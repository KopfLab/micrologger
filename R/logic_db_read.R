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
  group_id_value <- group_id

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
