# upload offline data (bluetooth downloads) ==========

#' Read a data bursts file
#'
#' Reads the data bursts JSON file saved by the SDDS browser after downloading
#' the data queued on a device via bluetooth (an array of data bursts, same
#' format as the cloud events). A single burst object is also accepted.
#' @param file path to the JSON file
#' @return tibble with one row per data record (core_name, tb, offset, path,
#'   n, text, value, sd, units)
#' @export
ml_read_data_bursts <- function(file) {
  file |>
    check_arg(
      !missing(file) && is_scalar_character(file) && file.exists(file),
      "must be the path to an existing file"
    )
  parsed <- tryCatch(
    jsonlite::read_json(file, simplifyVector = FALSE),
    error = function(e) {
      cli_abort("could not read {.file {basename(file)}} as JSON", parent = e)
    }
  )
  # single burst
  if (is.list(parsed) && !is.null(parsed[["b"]])) {
    parsed <- list(parsed)
  }
  bursts <- purrr::keep(parsed, ~ is.list(.x) && !is.null(.x[["b"]]) && !is.null(.x[["tb"]]))
  if (length(bursts) == 0) {
    cli_abort("{.file {basename(file)}} does not contain any data bursts")
  }

  # value or NA
  get <- function(x, key, default) {
    v <- x[[key]]
    if (is.null(v)) default else v
  }

  records <- purrr::map(bursts, function(burst) {
    purrr::map(burst[["b"]], function(entry) {
      path <- names(entry)[1]
      purrr::map(entry[[1]], function(r) {
        tibble(
          path = path,
          offset = as.integer(get(r, "o", 0L)),
          n = as.integer(get(r, "n", NA_integer_)),
          text = as.character(get(r, "c", NA_character_)),
          value = as.numeric(get(r, "v", NA_real_)),
          sd = as.numeric(get(r, "s", NA_real_)),
          units = as.character(get(r, "u", NA_character_))
        )
      }) |>
        bind_rows()
    }) |>
      bind_rows() |>
      mutate(
        core_name = as.character(get(burst, "n", NA_character_)),
        tb = as.character(burst[["tb"]]),
        .before = 1L
      )
  }) |>
    bind_rows()

  # same rules as the lambda function: skip records without value or text,
  # text records don't carry n/sd
  records |>
    filter(!is.na(.data$value) | !is.na(.data$text)) |>
    mutate(
      n = if_else(!is.na(.data$text), NA_integer_, .data$n),
      sd = if_else(!is.na(.data$text), NA_real_, .data$sd)
    )
}

#' Upload offline data into an experiment
#'
#' Adds the data records from a data bursts file (see [ml_read_data_bursts()])
#' of a device to an experiment. Only records that fall into one of the
#' experiment's recording segments are added (each to its segment). Records that
#' are already in the database (e.g. uploaded before or published via the cloud)
#' are not stored again, they are only linked to the experiment if they are not
#' already. Everything happens in a single transaction.
#' @param exp_id the experiment (single exp_id)
#' @param core_id the device (single core_id), must be linked to the experiment
#' @param records records from [ml_read_data_bursts()]
#' @param core_name the device name, if provided the records must all be from a device with this name
#' @param dry_run if TRUE, only reports what would happen
#' @param con database connection (defaults to `db()`)
#' @return a list with the number of records in the file (`total`), outside
#'   of the recording segments (`outside`), already linked to the experiment
#'   (`linked`), already in the database but not linked (`existing`) and new
#'   ones (`new`), plus the time range of the records (`from`, `to`)
#' @export
ml_upload_data_bursts <- function(
  exp_id,
  core_id,
  records,
  core_name = NULL,
  dry_run = FALSE,
  con = db()
) {
  # safety checks
  exp_id |>
    check_arg(
      !missing(exp_id) && is_scalar_integerish(exp_id),
      "must be a single exp_id"
    )
  core_id |>
    check_arg(
      !missing(core_id) && is_scalar_character(core_id),
      "must be a single core_id"
    )
  records |>
    check_arg(
      !missing(records) && is.data.frame(records) && nrow(records) > 0,
      "must be a data frame with at least one record"
    )
  if (!is.null(core_name)) {
    names_in_file <- unique(stats::na.omit(records$core_name))
    if (length(names_in_file) > 0 && !all(names_in_file == core_name)) {
      cli_abort(
        "the data is from {.field {names_in_file}}, not from device {.field {core_name}}"
      )
    }
  }
  linked <- sprintf(
    "SELECT 1 FROM experiment_devices WHERE exp_id = %s AND core_id = %s",
    to_sql(exp_id),
    to_sql(core_id)
  ) |>
    run_sql(con, execute_only = FALSE)
  if (nrow(linked) == 0) {
    cli_abort("device {.field {core_id}} is not linked to experiment {exp_id}")
  }

  # information
  cli_alert_info(
    "{if (dry_run) 'Checking' else 'Uploading'} {nrow(records)} record{?s} of {.field device} {col_blue(core_id)} for {.field experiment} {col_magenta(exp_id)}..."
  )

  # one record per key (the database can only hold each once)
  records <- records |>
    distinct(.data$tb, .data$offset, .data$path, .keep_all = TRUE) |>
    select("core_name", "tb", "o_ms" = "offset", "path", "n", "text", "value", "sd", "units")

  # everything in one transaction (the temp table is dropped at the end)
  # with the records in a temporary table, matched up with segments and existing logs
  result <- DBI::dbWithTransaction(con, {
    DBI::dbWriteTable(con, "_ml_upload_raw", records, temporary = TRUE, overwrite = TRUE)
    sprintf(
      "CREATE TEMP TABLE _ml_upload ON COMMIT DROP AS
      SELECT u.*, u.tb::timestamptz AS log_datetime, s.segment, l.log_id,
        (el.log_id IS NOT NULL) AS is_linked
      FROM (SELECT r.*, r.tb::timestamptz + r.o_ms * INTERVAL '1 millisecond' AS rec_time FROM _ml_upload_raw r) u
      -- the segment the record falls into (the latest one should segments overlap)
      LEFT JOIN LATERAL (
        SELECT segment FROM experiment_segments
        WHERE exp_id = %1$s AND u.rec_time >= start_datetime
          AND (end_datetime IS NULL OR u.rec_time <= end_datetime)
        ORDER BY segment DESC LIMIT 1
      ) s ON TRUE
      LEFT JOIN logs l ON l.core_id = %2$s AND l.log_datetime = u.tb::timestamptz
        AND l.log_time_offset = u.o_ms AND l.data_path = u.path
      LEFT JOIN experiment_logs el ON el.exp_id = %1$s AND el.log_id = l.log_id",
      to_sql(exp_id),
      to_sql(core_id)
    ) |>
      run_sql(con, .retry = FALSE)
    DBI::dbExecute(con, "DROP TABLE _ml_upload_raw")

    summary <- DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) AS total,
        COUNT(*) FILTER (WHERE segment IS NULL) AS outside,
        COUNT(*) FILTER (WHERE segment IS NOT NULL AND is_linked) AS linked,
        COUNT(*) FILTER (WHERE segment IS NOT NULL AND log_id IS NOT NULL AND NOT is_linked) AS existing,
        COUNT(*) FILTER (WHERE segment IS NOT NULL AND log_id IS NULL) AS new,
        MIN(rec_time) AS from, MAX(rec_time) AS to
      FROM _ml_upload"
    )

    if (!dry_run) {
      # store the new records (ON CONFLICT in case something arrived in the meantime)
      sprintf(
        "INSERT INTO logs (core_id, core_name, log_datetime, log_time_offset, data_path, data_n, data_text, data_value, data_sd, data_units)
        SELECT %s, core_name, log_datetime, o_ms, path, n, text, value, sd, units
        FROM _ml_upload WHERE segment IS NOT NULL AND log_id IS NULL
        ON CONFLICT ON CONSTRAINT logs_unique_record DO NOTHING",
        to_sql(core_id)
      ) |>
        run_sql(con, .retry = FALSE)
      # link all records in the segments to the experiment
      sprintf(
        "INSERT INTO experiment_logs (exp_id, segment, log_id)
        SELECT %1$s, u.segment, l.log_id
        FROM _ml_upload u
        JOIN logs l ON l.core_id = %2$s AND l.log_datetime = u.log_datetime
          AND l.log_time_offset = u.o_ms AND l.data_path = u.path
        WHERE u.segment IS NOT NULL
        ON CONFLICT (exp_id, log_id) DO NOTHING",
        to_sql(exp_id),
        to_sql(core_id)
      ) |>
        run_sql(con, .retry = FALSE)
    }
    DBI::dbExecute(con, "DROP TABLE _ml_upload")
    summary
  })

  out <- list(
    total = as.integer(result$total),
    outside = as.integer(result$outside),
    linked = as.integer(result$linked),
    existing = as.integer(result$existing),
    new = as.integer(result$new),
    from = result$from,
    to = result$to
  )
  cli_alert_success(
    "{out$new} new record{?s}, {out$existing} already stored, {out$linked} already in the experiment, {out$outside} outside the recording periods{if (dry_run) ' (nothing was changed)'}."
  )
  return(out)
}
