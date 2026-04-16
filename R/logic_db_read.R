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

  df <- df |> collect()
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

  df <- df |> dplyr::select(!!select_quo) |> collect()
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

  df <- df |> arrange(desc(recording), desc(last_recording_change)) |> collect()

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
        dplyr::filter(!archived) |>
        dplyr::select(
          "control_exp_id" = "exp_id",
          "name",
          "user_id",
          "user_first",
          "user_last"
        ),
      by = "control_exp_id"
    ) |>
    dplyr::filter(exp_id %in% !!exp_id)

  if (!quo_is_null(filter_quo)) {
    df <- df |> dplyr::filter(!!filter_quo)
  }

  df <- df |> dplyr::select(!!select_quo) |> collect()
  cli_alert_success("Found {nrow(df)} experiment device{?s}.")
  return(df)
}

#' read data logs
#' @export
ml_get_logs <- function(
  exp_id,
  filter = NULL,
  select = everything(),
  max_rows = NULL,
  convert_to_TZ = Sys.timezone(),
  con = db()
) {
  # safety check
  exp_id |>
    check_arg(
      !missing(exp_id) && is_integerish(exp_id) && length(exp_id) > 0,
      "must provide at least one experiment id"
    )
  filter_quo <- enquo(filter)
  select_quo <- enquo(select)

  # information
  cli_alert_info(
    c(
      "Retrieving logs for {qty(length(exp_id))}{.field experiment{?s}} {col_magenta(exp_id)}",
      "{if(!quo_is_null(filter_quo)) format_inline(' with filter {.code {quo_text(filter_quo)}}')}",
      "..."
    )
  )

  df <-
    tbl(con, "experiment_logs") |>
    inner_join(tbl(con, "logs"), by = c("log_id")) |>
    left_join(tbl(con, "experiment_devices"), by = c("exp_id", "core_id")) |>
    dplyr::filter(exp_id %in% !!exp_id) |>
    arrange(log_id)

  if (!quo_is_null(filter_quo)) {
    df <- df |> dplyr::filter(!!filter_quo)
  }

  if (!is.null(max_rows)) {
    df <- df |> dplyr::filter(row_number() <= !!max_rows)
  }

  df <- df |> dplyr::select(!!select_quo) |> collect()
  cli_alert_success("Found {nrow(df)} log{?s}.")

  # local TZ conversion
  if (!is.null(convert_to_TZ) && "log_datetime" %in% names(df)) {
    cli_alert_info(
      "Converting {.code log_datetime} to timezone {col_blue(convert_to_TZ)}."
    )
    df <- df |>
      mutate(log_datetime = lubridate::with_tz(log_datetime, convert_to_TZ))
  }

  return(df)
}
