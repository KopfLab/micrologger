# logic functions for the app ======

# simplify owner helper function
simplify_owner <- function(experiments, user_id) {
  experiments |>
    mutate(
      owner = case_when(
        user_id == !!user_id ~ "You",
        !is.na(user_first) & !is.na(user_last) ~ sprintf(
          "%s %s",
          user_first,
          user_last
        ),
        !is.na(user_first) ~ user_first,
        !is.na(user_last) ~ user_last,
        TRUE ~ user_id
      )
    )
}

# get experiments for table in app
get_experiments_for_table_in_app <- function(experiments, timezone, user_id) {
  experiments |>
    mutate(
      status = case_when(
        archived ~ "finished",
        recording ~ "recording",
        TRUE ~ "not recording"
      ),
      category = case_when(
        user_id == !!user_id & archived ~ "Your finished experiments",
        user_id == !!user_id ~ "Your active experiments",
        archived ~ "Other finished experiments in the group",
        TRUE ~ "Other active experiment in the group"
      ),
      last_recording_change = last_recording_change |>
        lubridate::with_tz(timezone) |>
        format("%b %d %Y %H:%M:%S"),
      .after = 1L
    ) |>
    simplify_owner(user_id = user_id) |>
    relocate("status", "owner", .after = "category") |>
    arrange(
      desc(user_id == !!user_id),
      archived,
      desc(recording),
      desc(last_recording_change)
    ) |>
    select(
      "exp_id",
      "category",
      "Status" = "status",
      "Owner" = "owner",
      "Experiment" = "name",
      "Description" = "description",
      "Last recording change" = "last_recording_change"
    )
}

# get experiemnt devices for table in app
get_experiment_devices_for_table_in_app <- function(
  experiment_devices,
  timezone
) {
  experiment_devices |>
    mutate(
      last_heard = .data$last_heard |>
        lubridate::with_tz(timezone) |>
        format("%b %d %Y %H:%M:%S")
    ) |>
    mutate(version = sddsParticle:::version_value_to_text(.data$version)) |>
    select(
      "core_id",
      Name = "name",
      Label = "label",
      Type = "type",
      Version = "version",
      `Last heard from` = "last_heard",
      Connected = "connected"
    )
}


# get devices for table in app
get_devices_for_table_in_app <- function(devices, timezone, user_id) {
  devices |>
    mutate(
      last_heard = .data$last_heard |>
        lubridate::with_tz(timezone) |>
        format("%b %d %Y %H:%M:%S")
    ) |>
    mutate(version = sddsParticle:::version_value_to_text(.data$version)) |>
    simplify_owner(user_id = user_id) |>
    select(
      "core_id",
      Name = "name",
      "In use by" = "owner",
      Type = "type",
      Version = "version",
      `Last heard from` = "last_heard",
      Connected = "connected"
    )
}
