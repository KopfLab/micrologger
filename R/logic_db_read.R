#' Get user groups
#' @export
ml_get_groups <- function(group_id = NULL, con = db()) {
  # information
  cli_alert_info(
    "Retrieving group information{if(!is.null(group_id)) format_inline(' for group{?s} {.field {group_id}}')}..."
  )

  df <- tbl(con, "groups")

  if (!is.null(group_id)) {
    group_id <- stringr::str_to_upper(group_id)
    df <- df |>
      dplyr::filter(stringr::str_to_upper(group_id) %in% !!group_id)
  }

  df <- df |> collect_from_db()
  cli_alert_success("Found {nrow(df)} group{?s}.")
  return(df)
}

#' Retrieve devices including their structures and who is currently using them
#' @param group_id devices from which group(s) to fetch
#' @return devices
#' @export
ml_get_devices <- function(
  group_id = NULL,
  filter = NULL,
  select = everything(),
  con = db()
) {
  filter_quo <- enquo(filter)
  select_quo <- enquo(select)

  # information
  cli_alert_info(
    c(
      "Retrieving devices{if(!is.null(group_id)) format_inline(' for group{?s} {.field {group_id}}')}",
      "{if(!quo_is_null(filter_quo)) format_inline(' with filter {.code {quo_text(filter_quo)}}')}",
      "..."
    )
  )

  df <- tbl(con, "devices") |>
    left_join(tbl(con, "structures"), by = c("type", "version")) |>
    left_join(
      tbl(con, "experiments") |>
        dplyr::filter(!archived) |>
        dplyr::select("exp_id", "name", "user_id", "user_first", "user_last"),
      by = c("control_exp_id" = "exp_id")
    )

  if (!is.null(group_id)) {
    group_id <- stringr::str_to_upper(group_id)
    df <- df |>
      dplyr::filter(stringr::str_to_upper(group_id) %in% !!group_id)
  }

  if (!quo_is_null(filter_quo)) {
    df <- df |> dplyr::filter(!!filter_quo)
  }

  df <- df |> dplyr::select(!!select_quo) |> collect_from_db()
  cli_alert_success("Found {nrow(df)} device{?s}.")
  return(df)
}

#' Retrieve experiments
#' @param group_id experiments from which group to fetch
#' @return experiments
#' @export
ml_get_experiments <- function(
  group_id = NULL,
  filter = NULL,
  convert_to_TZ = Sys.timezone(),
  con = db()
) {
  filter_quo <- enquo(filter)

  # information
  cli_alert_info(
    c(
      "Retrieving experiments{if(!is.null(group_id)) format_inline(' for group{?s} {.field {group_id}}')}",
      "{if(!quo_is_null(filter_quo)) format_inline(' with filter {.code {quo_text(filter_quo)}}')}",
      "..."
    )
  )

  df <- tbl(con, "experiments")

  if (!is.null(group_id)) {
    group_id <- stringr::str_to_upper(group_id)
    df <- df |>
      dplyr::filter(stringr::str_to_upper(group_id) %in% !!group_id)
  }

  if (!quo_is_null(filter_quo)) {
    df <- df |> dplyr::filter(!!filter_quo)
  }

  df <- df |>
    collect_from_db() |>
    arrange(desc(recording), desc(last_recording_change))

  # local TZ conversion
  if (!is.null(convert_to_TZ)) {
    cli_alert_info(
      "Converting {.code last_recording_change} to timezone {col_blue(convert_to_TZ)}."
    )
    df <- mutate(
      df,
      last_recording_change = lubridate::with_tz(
        last_recording_change,
        convert_to_TZ
      )
    )
  }

  cli_alert_success("Found {nrow(df)} experiment{?s}.")

  return(df)
}

#' Retrieve experiment devices
#'
#' Returns experiment-devices merged with devices table
#'
#' @inheritParams ml_get_experiments
#' @return experiments_devices
#' @export
ml_get_experiment_devices <- function(
  exp_id = NULL,
  filter = NULL,
  select = everything(),
  con = db()
) {
  # safety check
  exp_id |>
    check_arg(
      !missing(exp_id) && is_integerish(exp_id) && length(exp_id) > 0,
      "must provide at least one experiment id"
    )
  select_quo <- enquo(select)
  filter_quo <- enquo(filter)

  # information
  cli_alert_info(
    c(
      "Retrieving experiment devices for {qty(length(exp_id))}{.field experiment{?s}} {col_magenta(exp_id)}",
      "{if(!quo_is_null(filter_quo)) format_inline(' with filter {.code {quo_text(filter_quo)}}')}",
      "..."
    )
  )

  df <-
    tbl(con, "experiment_devices") |>
    left_join(tbl(con, "devices"), by = "core_id") |>
    left_join(
      tbl(con, "experiments") |>
        dplyr::select(
          "control_exp_id" = "exp_id",
          "name",
          "user_id",
          "user_first",
          "user_last"
        ),
      by = "control_exp_id"
    ) |>
    dplyr::filter(.data$exp_id %in% !!exp_id)

  if (!quo_is_null(filter_quo)) {
    df <- df |> dplyr::filter(!!filter_quo)
  }

  df <- df |> dplyr::select(!!select_quo) |> collect_from_db()
  cli_alert_success("Found {nrow(df)} experiment device{?s}.")
  return(df)
}

# get device snapshots linked to devices in a particular time period of an experiment
ml_get_experiment_devices_snapshots <- function(
  exp_id,
  con = db(),
  parse = TRUE
) {
  # safety check
  exp_id |>
    check_arg(
      !missing(exp_id) && is_integerish(exp_id) && length(exp_id) > 0,
      "must provide at least one experiment id"
    )

  # query
  df <-
    tbl(con, "experiment_devices") |>
    inner_join(tbl(con, "device_snapshots"), by = "core_id") |>
    inner_join(tbl(con, "experiments"), by = "exp_id") |>
    dplyr::filter(
      .data$exp_id %in% !!exp_id,
      !is.null(first_recording_start)
    ) |>
    dplyr::filter(
      snapshot_datetime >= first_recording_start &
        (recording |
          snapshot_datetime <= last_recording_change)
    ) |>
    dplyr::select(
      "exp_id",
      "snapshot_id",
      "core_id",
      "core_name",
      "label",
      "snapshot_datetime",
      "snapshot_json"
    )

  df <- df |> collect_from_db()
  if (parse) {
    return(df |> ml_parse_experiment_devices_snapshots())
  }
  return(df)
}

#' read data logs
#' @param parse whether to parse the database logs
#' @param timezone what timezone to use for parsing
#' @param experiments tibble with experiment information if this should be merged in
#' @export
ml_get_logs <- function(
  exp_id,
  filter = NULL,
  select = everything(),
  max_rows = NULL,
  cache = TRUE,
  include_snapshots = TRUE,
  con = db(),
  parse = TRUE,
  timezone = Sys.timezone(),
  experiments = NULL
) {
  # safety check
  exp_id |>
    check_arg(
      !missing(exp_id) && is_integerish(exp_id) && length(exp_id) > 0,
      "must provide at least one experiment id"
    )
  filter_quo <- enquo(filter)
  select_quo <- enquo(select)

  # cache
  cache_path <- file.path("cache", sprintf("exp%s_logs.rds", exp_id))
  use_cache <- cache && file.exists(cache_path)
  logs <- tibble()
  if (use_cache) {
    logs <- readr::read_rds(cache_path)
  }
  if (nrow(logs) == 0 || !"log_id" %in% names(logs)) {
    use_cache <- FALSE
  }

  # information
  cli_alert_info(
    c(
      "Retrieving logs ",
      if (include_snapshots) "and snapshots ",
      "for {qty(length(exp_id))}{.field experiment{?s}} {col_magenta(exp_id)} from ",
      if (use_cache) "cache ({nrow(logs)} records cached) and from the ",
      "database",
      if (!quo_is_null(filter_quo)) {
        " with filter {.code {quo_text(filter_quo)}}"
      },
      "..."
    )
  )

  # query
  df <-
    tbl(con, "experiment_logs") |>
    inner_join(tbl(con, "logs"), by = c("log_id")) |>
    left_join(tbl(con, "experiment_devices"), by = c("exp_id", "core_id")) |>
    dplyr::filter(.data$exp_id %in% !!exp_id) |>
    arrange(log_id)

  if (!is.null(max_rows)) {
    df <- df |> dplyr::filter(row_number() <= !!max_rows)
  }

  if (use_cache) {
    max_log_id <- max(logs$log_id, na.rm = TRUE)
    df <- df |> dplyr::filter(log_id > !!max_log_id)
  }

  df <- df |>
    dplyr::select(!!select_quo) |>
    collect_from_db() |>
    mutate(snapshot_id = NA_integer_, .after = "log_id")

  # post-query processing
  cli_alert_success(
    "Received {nrow(df)}{if(use_cache) ' new'} {qty(nrow(df))}log{?s} from the database."
  )
  logs <- bind_rows(logs, df)
  if (cache && nrow(df) > 0) {
    if (!dir.exists(dirname(cache_path))) {
      dir.create(dirname(cache_path), recursive = TRUE)
    }
    logs |> readr::write_rds(cache_path)
    cli_alert_info("Caching all {nrow(logs)} records.")
  }

  # get snapshots only aftewards so we cache just raw logs
  if (include_snapshots) {
    snaps <- ml_get_experiment_devices_snapshots(exp_id, con = con)
    cli_alert_success(
      "Received {length(unique(snaps$snapshot_id))} device snapshots from the database."
    )
    if (nrow(snaps) > 0) {
      logs <- logs |>
        bind_rows(
          snaps |>
            rename(
              "log_datetime" = "snapshot_datetime",
              "data_path" = "path",
              "data_text" = "v_text",
              "data_value" = "v_num"
            ) |>
            mutate(log_time_offset = 0)
        )
    }
  }

  # apply custom filter only afterwards so we cache all raw logs
  if (!quo_is_null(filter_quo)) {
    logs <- logs |> dplyr::filter(!!filter_quo)
  }

  # parse?
  if (parse) {
    logs <- logs |>
      ml_parse_logs(experiments = experiments, timezone = timezone)
  }

  return(logs)
}

# clear cached logs
ml_clear_logs_cache <- function() {
  files <- list.files("cache", pattern = "_logs.rds", full.names = TRUE)
  unlink(files)
  cli_alert_success("Cleared cached logs from {length(files)} experiment{?s}.")
}
