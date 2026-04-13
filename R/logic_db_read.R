#' Retrieve devices
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

  df <- tbl(con, "devices")

  if (!is.null(group_id)) {
    df <- df |> dplyr::filter(group_id %in% !!group_id)
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
    df <- df |> dplyr::filter(group_id %in% !!group_id)
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
#' Returns experiment-devices joined with experiment and devices tables so filter conditions can be applied on any of these as well.
#'
#' @inheritParams ml_get_experiments
#' @return experiments_devices
#' @export
ml_get_experiment_devices <- function(
  group_id = NULL,
  filter = NULL,
  select = c("exp_id", "core_id", "label", "recording"),
  con = db()
) {
  filter_quo <- enquo(filter)
  select_quo <- enquo(select)

  # information
  cli_alert_info(
    c(
      "Retrieving experiment devices{if(!is.null(group_id)) format_inline(' for group{?s} {.field {group_id}}')}",
      "{if(!quo_is_null(filter_quo)) format_inline(' with filter {.code {quo_text(filter_quo)}}')}",
      "..."
    )
  )

  df <- tbl(con, "experiment_devices") |>
    left_join(tbl(con, "devices"), by = "core_id") |>
    left_join(tbl(con, "experiments"), by = c("exp_id", "group_id"))

  if (!is.null(group_id)) {
    df <- df |> dplyr::filter(group_id %in% !!group_id)
  }

  if (!quo_is_null(filter_quo)) {
    df <- df |> dplyr::filter(!!filter_quo)
  }

  df <- df |> arrange(label) |> dplyr::select(!!select_quo) |> collect()
  cli_alert_success("Found {nrow(df)} experiment device{?s}.")
  return(df)
}
