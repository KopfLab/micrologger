# experiments ==========

#' Add new experiment
#' @export
ml_add_experiment <- function(
  group_id,
  user_id,
  user_first = NA_character_,
  user_last = NA_character_,
  con = db()
) {
  # safety checks
  group_id |>
    check_arg(
      !missing(group_id) && is_scalar_character(group_id),
      "must be a scalar character"
    )
  user_id |>
    check_arg(
      !missing(user_id) && is_scalar_character(user_id),
      "must be a scalar character"
    )

  # information
  cli_alert_info(
    "Adding new experiment for user {col_blue(user_id)} in group {.field {group_id}}..."
  )

  data <- tibble(group_id, user_id, user_first, user_last)
  new_exp_id <- run_insert_sql(
    data,
    "experiments",
    con,
    return_column = "exp_id"
  )$exp_id

  # return
  data |>
    mutate(
      exp_id = new_exp_id,
      .before = 1L
    )
}

#' Update experiment information
#' @return whether the update was successful or not
#' @export
ml_update_experiment_info <- function(
  exp_id,
  name = NULL,
  description = NULL,
  notes = NULL,
  con = db()
) {
  if (missing(exp_id)) {
    stop("must supply an experiment id", call. = FALSE)
  }
  if (length(exp_id) != 1) {
    stop("must provide only one exp id", call. = FALSE)
  }

  # information
  cli_alert_info(
    "Updating info for {.field experiment} {col_magenta(exp_id)}..."
  )

  updates <- c(
    name = if (!is.null(name)) name,
    description = if (!is.null(description)) description,
    notes = if (!is.null(notes)) notes
  )
  if (is.null(updates) || length(updates) == 0) {
    cli_alert_warning("There is nothing to update.")
    return(FALSE)
  }

  sql <- sprintf(
    "UPDATE experiments SET %s WHERE exp_id = %s",
    to_sql(updates, named = TRUE),
    to_sql(exp_id)
  )
  result <- run_sql(sql, con)

  if (result > 0) {
    cli_alert_success("{result} record{?s} updated.")
  } else {
    cli_alert_warning("no records found, this experiment does not exist.")
  }

  return(result > 0)
}

# experiment devices =========

#' Add/update for the experiment_devices table
#' @export
ml_link_devices_to_experiment <- function(
  exp_id,
  core_ids,
  labels = NA_character_,
  experiment_devices = tibble(exp_id, core_id = core_ids, label = labels),
  con = db()
) {
  # safety checks
  if (!"exp_id" %in% names(experiment_devices)) {
    stop("must supply an existing experiment id", call. = FALSE)
  }
  if (!"core_id" %in% names(experiment_devices)) {
    stop("must supply core_id(s)", call. = FALSE)
  }

  # no duplicates
  experiment_devices <- distinct(experiment_devices)

  # information
  cli_alert_info(
    "Linking {qty(experiment_devices$core_id)}{.field device{?s}} {col_blue(experiment_devices$core_id)} to {.field experiment} {col_magenta(unique(experiment_devices$exp_id))}"
  )

  run_insert_sql(
    experiment_devices,
    "experiment_devices",
    # if record already exists, simply update it
    on_conflict_constraint = "experiment_devices_exp_id_core_id_key",
    on_conflict_do = "UPDATE SET label = EXCLUDED.label",
    con = con
  )
  return(invisible(experiment_devices))
}

#' Remove experiment devices
#'
#' @export
ml_unlink_devices_from_experiment <- function(
  exp_id,
  core_ids,
  con = db()
) {
  # safety checks
  exp_id |>
    check_arg(
      !missing(exp_id) && is_scalar_integerish(exp_id),
      "must be a single exp_id"
    )
  core_ids |>
    check_arg(
      !missing(core_ids) && is_character(core_ids),
      "must be a character vector"
    )

  # information
  cli_alert_info(
    "Unlinking {qty(core_ids)}{.field device{?s}} {col_blue(core_ids)} from {.field experiment} {col_magenta(exp_id)}"
  )

  deleted <-
    sprintf(
      "DELETE FROM experiment_devices WHERE exp_id = %s AND core_id IN (%s)",
      to_sql(exp_id),
      to_sql(core_ids)
    ) |>
    run_sql(con)

  # information
  cli_alert_success("{deleted} link{?s} deleted.")
  return(invisible(NULL))
}


#' @export
ml_experiment_start_recording <- function(exp_id, con = db()) {
  # safety checks
  exp_id |>
    check_arg(
      !missing(exp_id) && is_scalar_integerish(exp_id),
      "must be a single exp_id"
    )

  # information
  cli_alert_info(
    "Starting recording for {.field experiment} {col_magenta(exp_id)}."
  )

  # run SQL
  sql <- sprintf(
    "UPDATE experiments SET recording = true, 
    last_recording_change = CASE WHEN recording IS NOT TRUE THEN %s ELSE last_recording_change END, 
    current_segment = CASE WHEN recording IS NOT TRUE THEN current_segment + 1 ELSE current_segment END
    WHERE exp_id = %s",
    to_sql(format(lubridate::now('UTC'))),
    to_sql(exp_id)
  )
  result <- sql |> run_sql(con)
  if (result == 0) {
    cli_alert_warning("Something went wrong, no experiment was affected.")
    return(FALSE)
  }
  cli_alert_success("Experiment is recording.")
  return(TRUE)
}

#' @export
ml_experiment_stop_recording <- function(exp_id, con = db()) {
  # safety checks
  exp_id |>
    check_arg(
      !missing(exp_id) && is_scalar_integerish(exp_id),
      "must be a single exp_id"
    )

  # information
  cli_alert_info(
    "Stopping recording for {.field experiment} {col_magenta(exp_id)}."
  )

  # run SQL
  sql <- sprintf(
    "UPDATE experiments SET recording = false, 
    last_recording_change = CASE WHEN recording IS TRUE THEN %s ELSE last_recording_change END
    WHERE exp_id = %s",
    to_sql(format(lubridate::now('UTC'))),
    to_sql(exp_id)
  )
  result <- sql |> run_sql(con)
  if (result == 0) {
    cli_alert_warning("Something went wrong, no experiment was affected.")
    return(FALSE)
  }
  cli_alert_success("Experiment recording paused.")
  return(TRUE)
}

#' @export
ml_archive_experiment <- function(exp_id, con = db()) {
  # safety checks
  exp_id |>
    check_arg(
      !missing(exp_id) && is_scalar_integerish(exp_id),
      "must be a single exp_id"
    )

  # finishing
  cli_alert_info(
    "Archiving {.field experiment} {col_magenta(exp_id)} and releasing all its devices (if any)."
  )

  # release devices
  sql <- sprintf(
    "UPDATE devices SET control_exp_id = NULL WHERE control_exp_id = %s",
    to_sql(exp_id)
  )
  result <- sql |> run_sql(con)
  cli_alert_info("{result} devices{?s} were released.")

  # archive experiment
  sql <- sprintf(
    "UPDATE experiments SET archived = TRUE WHERE exp_id = %s",
    to_sql(exp_id)
  )
  result <- sql |> run_sql(con)
  if (result == 0) {
    cli_alert_warning("Something went wrong, no experiment was affected.")
    return(FALSE)
  }
  cli_alert_success("Experiment archived.")
  return(TRUE)
}
