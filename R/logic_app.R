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
        current_segment > 0 ~ "paused",
        TRUE ~ "new"
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
  timezone,
  user_id
) {
  experiment_devices |>
    mutate(
      last_heard = .data$last_heard |>
        lubridate::with_tz(timezone) |>
        format("%b %d %Y %H:%M:%S")
    ) |>
    simplify_owner(user_id = user_id) |>
    mutate(
      version = sddsParticle:::version_value_to_text(.data$version),
      name = case_when(
        is.na(control_exp_id) ~ "Not in use",
        control_exp_id == exp_id ~ "This experiment",
        TRUE ~ name
      ),
      owner = case_when(
        is_na(control_exp_id) ~ "None",
        TRUE ~ owner
      )
    ) |>
    select(
      "core_id",
      Device = "core_name",
      "Custom label" = "label",
      "Current experiment" = "name",
      "Current controller" = "owner",
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

# plotting functions for the app ======

get_empty_plot_in_app <- function(font_size) {
  ggplot() +
    annotate(
      "text",
      x = 0,
      y = 0,
      label = "no data",
      vjust = 0.5,
      hjust = 0.5,
      size = font_size
    ) +
    theme_void()
}

get_logs_plot_in_app <- function(logs, devices, ..., legend_position) {
  p <- logs |>
    filter(.data$device %in% !!devices) |>
    ml_plot_logs(...)

  # legend position
  if (legend_position == "bottom") {
    p <- p +
      theme(legend.position = "bottom", legend.direction = "vertical")
  } else if (legend_position == "hide") {
    p <- p + theme(legend.position = "none")
  }

  return(p)
}

get_logs_for_download_in_app <- function(logs, devices, filter_paths) {
  logs |>
    filter(.data$device %in% !!devices) |>
    filter(.data$path %in% filter_paths)
}
