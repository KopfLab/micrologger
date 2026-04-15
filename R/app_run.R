#' Run the user interface
#'
#' Set database credentials ahead of time via [ml_set_connection_credentials].
#'
#' @param token particle access token (retrieved from keyring by default), save a token in your keyring with particle_store_token()
#' @param timezone the timezone to user for datetime calculations
#' @inheritParams shiny::shinyApp
#' @export
ml_run_gui <- function(
  user_id,
  user_groups,
  user_is_admin = FALSE,
  user_first_name = NA_character_,
  user_last_name = NA_character_,
  token = keyring::key_get("particle"),
  timezone = Sys.timezone(),
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url"
) {
  # startup
  log_info("\n\n========================================================")
  log_info("starting micrologger GUI", if (shiny::in_devmode()) " in DEV mode")

  # safety check for parameters
  user_id |>
    check_arg(
      !missing(user_id) && is_scalar_character(user_id) && !is.na(user_id),
      "must be a string"
    )
  user_groups |>
    check_arg(
      !missing(user_groups) &&
        is_character(user_groups) &&
        length(user_groups) > 0,
      "must be at least one string"
    )
  user_is_admin |>
    check_arg(is_scalar_logical(user_is_admin), "must be TRUE or FALSE")
  user_first_name |>
    check_arg(is_scalar_character(user_first_name), "must be a string")
  user_last_name |>
    check_arg(is_scalar_character(user_last_name), "must be a string")
  token |>
    check_arg(is_scalar_character(token) && nzchar(token), "must be a string")
  timezone |>
    check_arg(
      is_scalar_character(timezone) && timezone %in% base::OlsonNames(),
      "must be an OlsonName"
    )

  # check DB connection
  cli_alert_info("Checking database connection...")
  force(db())
  cli_alert_success("Connected.")

  # get user groups and check we have at least one
  groups <- ml_get_groups(group_id = if (user_is_admin) NULL else user_groups)
  if (nrow(groups) == 0) {
    cli_abort(
      c(
        "none of the user groups are registered with the database",
        "i" = "groups: {user_groups}"
      )
    )
  }

  # load app ui and server
  ui <- ml_ui(
    timezone = timezone,
    user = if (!is.na(user_first_name)) user_first_name else user_id,
    groups = groups
  )
  server <- ml_server(
    token = token,
    user_id = user_id,
    # set the first group as the default
    user_group = groups$group_id[1],
    user_first_name = user_first_name,
    user_last_name = user_last_name,
    user_is_admin = user_is_admin
  )

  # generate app
  shinyApp(
    ui = ui,
    server = server,
    # onstart required to read the particle stream!
    onStart = sddsParticle::sdds_onstart(token = token),
    options = options,
    enableBookmarking = enableBookmarking,
    uiPattern = uiPattern
  )
}
