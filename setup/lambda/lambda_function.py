import json
import logging
import boto3
import psycopg2
from psycopg2.extras import execute_values
import os

logger = logging.getLogger()
logger.setLevel(logging.INFO)

conn = None


def get_connection():
    global conn
    if conn and not conn.closed:
        return conn

    is_local = os.environ.get("LOCAL") == "true"
    logger.info(
        "running {} - using {}".format(
            "locally" if is_local else "in Lambda",
            "direct RDS endpoint" if is_local else "RDS proxy",
        )
    )

    client = boto3.client("secretsmanager", region_name="us-east-1")
    creds = json.loads(
        client.get_secret_value(SecretId="kopflab/db-credentials/microloggers")[
            "SecretString"
        ]
    )

    conn = psycopg2.connect(
        host=creds["host"] if is_local else creds["proxy"],
        port=5432,
        dbname="microloggers",
        user=creds["username"],
        password=creds["password"],
    )
    return conn


def error_response(status_code, message):
    return {"statusCode": status_code, "body": json.dumps({"error": message})}


def get_parameter(event, param_name, require=True):
    param = event.get(param_name)
    if require and param is None:
        logger.error("no '{}' provided in event {}".format(param_name, event))
        return None, {
            "statusCode": 400,
            "body": json.dumps({"error": "missing '{}' in payload".format(param_name)}),
        }
    return param, None


def update_usage_stats(cursor, core_id, payload_bytes):
    try:
        cursor.execute(
            """
            INSERT INTO device_usage_stats (core_id, year, month, recording_events_n, nonrecording_events_n)
            VALUES (
                %s,
                EXTRACT(YEAR FROM NOW())::INTEGER,
                EXTRACT(MONTH FROM NOW())::INTEGER,
                CASE WHEN EXISTS (
                    SELECT 1 FROM experiments e
                    JOIN experiment_devices ed ON e.exp_id = ed.exp_id
                    WHERE ed.core_id = %s
                      AND e.recording = TRUE
                      AND e.archived = FALSE
                ) THEN CEIL(%s::FLOAT / 1024)::INTEGER ELSE 0 END,
                CASE WHEN EXISTS (
                    SELECT 1 FROM experiments e
                    JOIN experiment_devices ed ON e.exp_id = ed.exp_id
                    WHERE ed.core_id = %s
                      AND e.recording = TRUE
                      AND e.archived = FALSE
                ) THEN 0 ELSE CEIL(%s::FLOAT / 1024)::INTEGER END
            )
            ON CONFLICT (core_id, year, month) DO UPDATE SET
                recording_events_n    = device_usage_stats.recording_events_n    + EXCLUDED.recording_events_n,
                nonrecording_events_n = device_usage_stats.nonrecording_events_n + EXCLUDED.nonrecording_events_n
            """,
            [core_id, core_id, payload_bytes, core_id, payload_bytes],
        )
        logger.info("updated usage stats for device {}".format(core_id))
    except Exception as e:
        logger.error("failed to update usage stats for {}: {}".format(core_id, str(e)))


def get_payload_type(core_id, payload):
    keys = set(payload.keys())
    if {"s", "e", "t", "v"}.issubset(keys):
        return "tree", None
    elif {"n", "s", "tb"}.issubset(keys):
        return "state", None
    elif {"n", "d", "t", "v"}.issubset(keys):
        return "values", None
    elif {"n", "b", "tb"}.issubset(keys):
        return "burst", None
    else:
        logger.error(
            "unrecognized payload type for core_id {}: {}".format(core_id, payload)
        )
        return None, error_response(
            400, "unrecognized payload type for core_id {}".format(core_id)
        )


def get_device_group(cursor, core_id):
    try:
        cursor.execute("SELECT group_id FROM devices WHERE core_id = %s", [core_id])
        row = cursor.fetchone()
        if not row:
            logger.error(
                "device {} is not registered with the database".format(core_id)
            )
            return None, error_response(
                404, "device {} is not registered with the database".format(core_id)
            )
        return row[0], None
    except Exception as e:
        logger.error("failed to get device group for {}: {}".format(core_id, str(e)))
        return None, error_response(
            500, "failed to get device group for {}: {}".format(core_id, str(e))
        )


def get_experiments(cursor, core_id, payload):
    # experiments linked to the device with a recording segment that overlaps the
    # time range of the burst (queued bursts can arrive long after they were recorded,
    # so this is based on when the data was recorded, not on what is recording right now)
    try:
        max_offset = max(
            [
                record.get("o") or 0
                for entry in payload.get("b", [])
                for records in entry.values()
                for record in records
            ]
            or [0]
        )
        cursor.execute(
            """
            SELECT DISTINCT e.exp_id
            FROM experiments e
            JOIN experiment_devices ed ON e.exp_id = ed.exp_id
            JOIN experiment_segments s ON s.exp_id = e.exp_id
            WHERE ed.core_id = %s
              AND s.start_datetime <= %s::timestamptz + %s * INTERVAL '1 millisecond'
              AND (s.end_datetime IS NULL OR s.end_datetime >= %s::timestamptz)
            ORDER BY e.exp_id
            """,
            [core_id, payload.get("tb"), max_offset, payload.get("tb")],
        )
        experiments = [{"exp_id": row[0]} for row in cursor.fetchall()]
        if experiments:
            logger.info(
                "found recording experiments {} for device {}".format(experiments, core_id)
            )
        else:
            logger.info(
                "no experiments linked to device {} were recording at this time".format(
                    core_id
                )
            )
            return None, {
                "statusCode": 200,
                "body": json.dumps(
                    {
                        "message": "no experiments linked to device {} were recording at this time".format(
                            core_id
                        )
                    }
                ),
            }
        return experiments, None
    except Exception as e:
        logger.error(
            "failed to check for active experiments for device {}: {}".format(
                core_id, str(e)
            )
        )
        return None, error_response(
            500,
            "failed to check for active experiments for device {}: {}".format(
                core_id, str(e)
            ),
        )


def process_tree(cursor, payload):
    try:
        cursor.execute(
            """
            INSERT INTO structures (type, version, tree_json)
            VALUES (%s, %s, %s)
            ON CONFLICT (type, version) DO UPDATE
            SET tree_json = EXCLUDED.tree_json
            """,
            [payload.get("t"), payload.get("v"), json.dumps(payload)],
        )
        logger.info(
            "upserted structure type '{}' version {}".format(
                payload.get("t"), payload.get("v")
            )
        )
        return None, None
    except Exception as e:
        logger.error("failed to process tree payload: {}".format(str(e)))
        return None, error_response(
            500, "failed to process tree payload: {}".format(str(e))
        )


def process_values(cursor, core_id, group_id, published_at, payload):
    try:
        cursor.execute(
            """
            INSERT INTO devices (core_id, group_id, published_at, type, version, values_json)
            VALUES (%s, %s, %s, %s, %s, %s)
            ON CONFLICT (core_id) DO UPDATE
            SET group_id = EXCLUDED.group_id,
                published_at = EXCLUDED.published_at,
                type = EXCLUDED.type,
                version = EXCLUDED.version,
                values_json = EXCLUDED.values_json
            """,
            [
                core_id,
                group_id,
                published_at,
                payload.get("t"),
                payload.get("v"),
                json.dumps(payload),
            ],
        )
        logger.info(
            "upserted device {} with type '{}' version {}".format(
                core_id, payload.get("t"), payload.get("v")
            )
        )
        return None, None
    except Exception as e:
        logger.error("failed to process values payload: {}".format(str(e)))
        return None, error_response(
            500, "failed to process values payload: {}".format(str(e))
        )


def process_burst(cursor, core_id, payload, experiments):
    try:
        core_name = payload.get("n")
        log_datetime = payload.get("tb")
        total_logs = 0

        # collect all records of the burst
        rows = []
        for entry in payload.get("b", []):
            data_path = next(iter(entry))
            for record in entry[data_path]:
                data_value = record.get("v")
                data_text = record.get("c")

                if data_value is None and data_text is None:
                    logger.warning(
                        "skipping record in {} for device {} - no 'v' or 'c' present".format(
                            data_path, core_id
                        )
                    )
                    continue

                rows.append(
                    (
                        core_id,
                        core_name,
                        log_datetime,
                        record.get("o"),
                        data_path,
                        None if data_text is not None else record.get("n"),
                        data_text,
                        data_value,
                        None if data_text is not None else record.get("s"),
                        record.get("u"),
                    )
                )

        # drop records that appear twice in the same burst (same path and offset), the
        # upsert below can't touch the same row twice in one statement
        unique_rows = {}
        for row in rows:
            unique_rows.setdefault((row[2], row[3] or 0, row[4]), row)
        rows = [r if r[3] is not None else r[:3] + (0,) + r[4:] for r in unique_rows.values()]

        # batch insert the logs; records that are already stored (e.g. uploaded from a
        # bluetooth download) are not inserted again, their existing log_id is returned
        # (the no-op update makes RETURNING include them) so they still get linked below (one statement per 1000 records instead of one per record)
        log_ids = []
        if rows:
            log_ids = [
                r[0]
                for r in execute_values(
                    cursor,
                    """
                    INSERT INTO logs (core_id, core_name, log_datetime, log_time_offset, data_path, data_n, data_text, data_value, data_sd, data_units)
                    VALUES %s
                    ON CONFLICT ON CONSTRAINT logs_unique_record
                    DO UPDATE SET core_id = logs.core_id
                    RETURNING log_id
                    """,
                    rows,
                    page_size=1000,
                    fetch=True,
                )
            ]
        total_logs = len(log_ids)

        # link the logs to the experiments
        if log_ids and experiments:
            # each record goes into the segment that was recording when it was recorded
            # (records from pauses are not linked; the latest segment should they overlap)
            cursor.execute(
                """
                INSERT INTO experiment_logs (exp_id, segment, log_id)
                SELECT DISTINCT ON (s.exp_id, l.log_id) s.exp_id, s.segment, l.log_id
                FROM logs l
                JOIN experiment_segments s ON s.exp_id = ANY(%s)
                  AND l.log_datetime + l.log_time_offset * INTERVAL '1 millisecond' >= s.start_datetime
                  AND (s.end_datetime IS NULL OR l.log_datetime + l.log_time_offset * INTERVAL '1 millisecond' <= s.end_datetime)
                WHERE l.log_id = ANY(%s)
                ORDER BY s.exp_id, l.log_id, s.segment DESC
                ON CONFLICT (exp_id, log_id) DO NOTHING
                """,
                [[exp["exp_id"] for exp in experiments], log_ids],
            )

        logger.info(
            "processed burst for device {} across experiments {}: {} logs".format(
                core_id, experiments, total_logs
            )
        )
        return total_logs, None
    except Exception as e:
        logger.error(
            "failed to process burst for device {}: {}".format(core_id, str(e))
        )
        return None, error_response(
            500, "failed to process burst for device {}: {}".format(core_id, str(e))
        )


def process_state(cursor, core_id, payload):
    try:
        cursor.execute(
            """
            INSERT INTO device_snapshots (core_id, core_name, snapshot_datetime, snapshot_json)
            VALUES (%s, %s, %s, %s)
            """,
            [
                core_id,
                payload.get("n"),
                payload.get("tb"),
                json.dumps(payload.get("s")),
            ],
        )
        logger.info("created snapshot for device {}".format(core_id))
        return None, None
    except Exception as e:
        logger.error(
            "failed to process state payload for device {}: {}".format(core_id, str(e))
        )
        return None, error_response(
            500,
            "failed to process state payload for device {}: {}".format(core_id, str(e)),
        )


def process_single_payload(cursor, db, core_id, group_id, published_at, payload):
    payload_type, err = get_payload_type(core_id, payload)
    if err:
        return {"payload_type": "unknown", "error": err["body"]}

    result = {"payload_type": payload_type}

    if payload_type == "tree":
        _, err = process_tree(cursor, payload)
        if err:
            result["error"] = err["body"]
        else:
            db.commit()

    elif payload_type == "values":
        _, err = process_values(cursor, core_id, group_id, published_at, payload)
        if err:
            result["error"] = err["body"]
        else:
            db.commit()

    elif payload_type == "burst":
        experiments, err = get_experiments(cursor, core_id, payload)
        if err:
            result["error"] = err["body"]
        else:
            total_logs, err = process_burst(cursor, core_id, payload, experiments)
            if err:
                result["error"] = err["body"]
            else:
                db.commit()
                result["logs_written"] = total_logs

    elif payload_type == "state":
        _, err = process_state(cursor, core_id, payload)
        if err:
            result["error"] = err["body"]
        else:
            db.commit()

    # failed? --> roll back the partial transaction, otherwise the connection stays in an aborted
    # transaction and all following payloads of this request fail as well
    if "error" in result:
        db.rollback()

    return result


def lambda_handler(event, context):
    try:
        # API Gateway passes body as a JSON string
        if isinstance(event.get("body"), str):
            event = json.loads(event["body"])

        core_id, err = get_parameter(event, "core_id")
        if err:
            return err

        payloads, err = get_parameter(event, "payload")
        if err:
            return err

        published_at, err = get_parameter(event, "published_at")
        if err:
            return err

        if not isinstance(payloads, list):
            payloads = [payloads]

        logger.info(
            "processing {} payload(s) for core_id {}".format(len(payloads), core_id)
        )

        db = get_connection()
        cursor = db.cursor()
        results = []
        try:
            group_id, err = get_device_group(cursor, core_id)
            if err:
                return err

            logger.info("core_id {} belongs to group {}".format(core_id, group_id))

            update_usage_stats(cursor, core_id, len(json.dumps(payloads)))
            db.commit()

            for payload in payloads:
                result = process_single_payload(
                    cursor, db, core_id, group_id, published_at, payload
                )
                results.append(result)

        finally:
            cursor.close()

        return {
            "statusCode": 200,
            "body": json.dumps(
                {"core_id": core_id, "num_payloads": len(payloads), "results": results}
            ),
        }

    except Exception as e:
        logger.error("unhandled exception: {}".format(str(e)))
        return error_response(500, str(e))
