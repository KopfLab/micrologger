import json
import logging
import boto3
import psycopg2
import os

# Logger
logger = logging.getLogger()
logger.setLevel(logging.INFO)

# Outside handler — reused on warm invocations
conn = None


def get_connection():
    global conn
    if conn and not conn.closed:
        return conn

    # local or on cloud?
    is_local = os.environ.get("LOCAL") == "true"
    if is_local:
        logger.info("running locally - using direct RDS endpoint")
    else:
        logger.info("running in Lambda - using RDS proxy")

    client = boto3.client("secretsmanager", region_name="us-east-1")
    secret = client.get_secret_value(SecretId="kopflab/db-credentials/microloggers")
    creds = json.loads(secret["SecretString"])

    conn = psycopg2.connect(
        # The proxy only works in the cloud, need to use the direct DB instance when running locally
        host=creds["host"] if is_local else creds["proxy"],
        port=5432,
        dbname="microloggers",
        user=creds["username"],
        password=creds["password"],
    )
    return conn


def get_parameter(event, param_name, require=True):
    param = event.get(param_name)
    if require and param is None:
        logger.error("no '{}' provided in event {}".format(param_name, event))
        return None, {
            "statusCode": 400,
            "body": json.dumps({"error": "missing '{}' in payload".format(param_name)}),
        }
    return param, None


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
        return None, {
            "statusCode": 400,
            "body": json.dumps(
                {"error": "unrecognized payload type for core_id {}".format(core_id)}
            ),
        }


def get_device_group(cursor, core_id):
    try:
        cursor.execute("SELECT group_id FROM devices WHERE core_id = %s", [core_id])
        row = cursor.fetchone()
        if not row:
            logger.error(
                "device {} is not registered with the database".format(core_id)
            )
            return None, {
                "statusCode": 404,
                "body": json.dumps(
                    {
                        "error": "device {} is not registered with the database".format(
                            core_id
                        )
                    }
                ),
            }
        return row[0], None
    except Exception as e:
        logger.error("failed to get device group for {}: {}".format(core_id, str(e)))
        return None, {
            "statusCode": 500,
            "body": json.dumps(
                {
                    "error": "failed to get device group for {}: {}".format(
                        core_id, str(e)
                    )
                }
            ),
        }


def get_experiments(cursor, core_id, required=True):
    try:
        cursor.execute(
            """
            SELECT e.exp_id
            FROM experiments e
            JOIN experiment_devices ed ON e.exp_id = ed.exp_id
            WHERE ed.core_id = %s
              AND e.recording = TRUE
              AND e.archived = FALSE
        """,
            [core_id],
        )
        rows = cursor.fetchall()
        exp_ids = [row[0] for row in rows]
        if exp_ids:
            logger.info(
                "found active experiments {} for device {}".format(exp_ids, core_id)
            )
        else:
            logger.info("no active experiments linked to device {}".format(core_id))
            if required:
                return None, {
                    "statusCode": 200,
                    "body": json.dumps(
                        {
                            "message": "no active experiments linked to device {} at the moment".format(
                                core_id
                            )
                        }
                    ),
                }
        return exp_ids, None
    except Exception as e:
        logger.error(
            "failed to check for active experiments for device {}: {}".format(
                core_id, str(e)
            )
        )
        return None, {
            "statusCode": 500,
            "body": json.dumps(
                {
                    "error": "failed to check for active experiments for device {}: {}".format(
                        core_id, str(e)
                    )
                }
            ),
        }


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
        return None, {
            "statusCode": 500,
            "body": json.dumps(
                {"error": "failed to process tree payload: {}".format(str(e))}
            ),
        }


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
        return None, {
            "statusCode": 500,
            "body": json.dumps(
                {"error": "failed to process values payload: {}".format(str(e))}
            ),
        }


def process_burst(cursor, core_id, payload, exp_ids):
    try:
        core_name = payload.get("n")
        log_datetime = payload.get("tb")
        burst = payload.get("b", [])
        total_logs = 0

        for entry in burst:
            data_path = next(iter(entry))
            records = entry[data_path]

            for record in records:
                log_time_offset = record.get("o")
                data_value = record.get("v")
                data_text = record.get("c")
                data_sd = record.get("s")
                data_n = record.get("n")
                data_units = record.get("u")

                if data_value is None and data_text is None:
                    logger.warning(
                        "skipping record in {} for device {} - no 'v' or 'c' present".format(
                            data_path, core_id
                        )
                    )
                    continue

                if data_text is not None:
                    data_n = None
                    data_sd = None

                cursor.execute(
                    """
                    INSERT INTO logs (core_id, core_name, log_datetime, log_time_offset, data_path, data_n, data_text, data_value, data_sd, data_units)
                    VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
                    RETURNING log_id
                """,
                    [
                        core_id,
                        core_name,
                        log_datetime,
                        log_time_offset,
                        data_path,
                        data_n,
                        data_text,
                        data_value,
                        data_sd,
                        data_units,
                    ],
                )

                log_id = cursor.fetchone()[0]
                total_logs += 1

                for exp_id in exp_ids:
                    cursor.execute(
                        """
                        INSERT INTO experiment_logs (exp_id, log_id)
                        VALUES (%s, %s)
                    """,
                        [exp_id, log_id],
                    )

        logger.info(
            "processed burst for device {} across experiments {}: {} logs".format(
                core_id, exp_ids, total_logs
            )
        )
        return total_logs, None
    except Exception as e:
        logger.error(
            "failed to process burst for device {}: {}".format(core_id, str(e))
        )
        return None, {
            "statusCode": 500,
            "body": json.dumps(
                {
                    "error": "failed to process burst for device {}: {}".format(
                        core_id, str(e)
                    )
                }
            ),
        }


def flatten_state(obj, prefix=""):
    records = []
    for key, value in obj.items():
        path = "{}.{}".format(prefix, key) if prefix else key
        if value is None:
            continue
        elif isinstance(value, dict):
            records.extend(flatten_state(value, path))
        elif isinstance(value, (int, float)):
            records.append((path, value, None))
        elif isinstance(value, str):
            records.append((path, None, value))
    return records


def process_state(cursor, core_id, payload, exp_ids):
    try:
        core_name = payload.get("n")
        log_datetime = payload.get("tb")

        cursor.execute(
            """
            INSERT INTO device_snapshots (core_id, snapshot_datetime)
            VALUES (%s, %s)
            RETURNING snapshot_id
        """,
            [core_id, log_datetime],
        )
        snapshot_id = cursor.fetchone()[0]
        logger.info("created snapshot {} for device {}".format(snapshot_id, core_id))

        records = flatten_state(payload.get("s", {}))
        total_logs = 0

        for data_path, data_value, data_text in records:
            cursor.execute(
                """
                INSERT INTO logs (core_id, core_name, log_datetime, log_time_offset, snapshot_id, data_path, data_value, data_text)
                VALUES (%s, %s, %s, %s, %s, %s, %s, %s)
                RETURNING log_id
            """,
                [
                    core_id,
                    core_name,
                    log_datetime,
                    0,
                    snapshot_id,
                    data_path,
                    data_value,
                    data_text,
                ],
            )

            log_id = cursor.fetchone()[0]
            total_logs += 1

            if exp_ids:
                for exp_id in exp_ids:
                    cursor.execute(
                        """
                        INSERT INTO experiment_logs (exp_id, log_id)
                        VALUES (%s, %s)
                    """,
                        [exp_id, log_id],
                    )

        logger.info(
            "processed state for device {} with snapshot {}: {} logs".format(
                core_id, snapshot_id, total_logs
            )
        )
        return total_logs, None
    except Exception as e:
        logger.error(
            "failed to process state payload for device {}: {}".format(core_id, str(e))
        )
        return None, {
            "statusCode": 500,
            "body": json.dumps(
                {
                    "error": "failed to process state payload for device {}: {}".format(
                        core_id, str(e)
                    )
                }
            ),
        }


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
        exp_ids, err = get_experiments(cursor, core_id, required=True)
        if err:
            result["error"] = err["body"]
        else:
            total_logs, err = process_burst(cursor, core_id, payload, exp_ids)
            if err:
                result["error"] = err["body"]
            else:
                db.commit()
                result["logs_written"] = total_logs

    elif payload_type == "state":
        exp_ids, err = get_experiments(cursor, core_id, required=False)
        if err:
            result["error"] = err["body"]
        else:
            total_logs, err = process_state(cursor, core_id, payload, exp_ids)
            if err:
                result["error"] = err["body"]
            else:
                db.commit()
                result["logs_written"] = total_logs

    return result


def lambda_handler(event, context):

    # start the procesing
    try:
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
        return {"statusCode": 500, "body": json.dumps({"error": str(e)})}
