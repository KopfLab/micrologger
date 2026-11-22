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
    simplify_owner(user_id = user_id) |>
    arrange(
      desc(user_id == !!user_id),
      archived,
      desc(recording),
      desc(is.na(last_recording_change)),
      desc(last_recording_change)
    ) |>
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
        format("%b %d %Y %H:%M:%S %Z"),
      .after = 1L
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

# get experiment devices for table in app
get_experiment_devices_for_table_in_app <- function(
  experiment_devices,
  timezone,
  user_id
) {
  experiment_devices |>
    mutate(
      last_heard = .data$last_heard |>
        lubridate::with_tz(timezone) |>
        format("%b %d %Y %H:%M:%S %Z")
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
    arrange(core_name) |>
    select(
      any_of(c(
        "core_id",
        Device = "core_name",
        "Custom label" = "label",
        "Current experiment" = "name",
        "Current controller" = "owner",
        "Recording" = "publishing.record",
        "Publish Interval" = "publishing.globalInterval_ms",
        Type = "type",
        Version = "version",
        `Last heard from` = "last_heard",
        Connected = "connected"
      ))
    )
}


# get devices for table in app
get_unlinked_devices_for_table_in_app <- function(devices, timezone, user_id) {
  devices |>
    mutate(
      last_heard = .data$last_heard |>
        lubridate::with_tz(timezone) |>
        format("%b %d %Y %H:%M:%S %Z")
    ) |>
    simplify_owner(user_id = user_id) |>
    mutate(
      version = sddsParticle:::version_value_to_text(.data$version),
      name = case_when(
        is.na(control_exp_id) ~ "Not in use",
        TRUE ~ name
      ),
      owner = case_when(
        is_na(control_exp_id) ~ "None",
        TRUE ~ owner
      )
    ) |>
    arrange(!is.na(control_exp_id), core_name) |>
    select(
      "core_id",
      Device = "core_name",
      "Current experiment" = "name",
      "Current controller" = "owner",
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
  if (legend_position == "hide") {
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = legend_position)
    if (legend_position %in% c("bottom", "top")) {
      p <- p +
        theme(legend.direction = "vertical")
    }
  }

  return(p)
}

get_logs_for_download_in_app <- function(logs, devices, filter_paths) {
  logs |>
    filter(.data$device %in% !!devices) |>
    filter(.data$path %in% filter_paths)
}


# device publishing settings ==========

# get the publishing settings (record, global interval) of devices from their
# SDDS SYSTEM variable. The particle variable is limited in size and the SYSTEM
# tree got big enough that the JSON can be cut off, so instead of parsing the
# whole JSON the two values are pulled out with regular expressions
# (missing ones are NA)
# @return tibble with core_id, publishing.record, publishing.globalInterval_ms
ml_get_devices_publishing <- function(core_ids, token) {
  core_ids |>
    purrr::map(function(core_id) {
      json <- tryCatch(
        sddsParticle::particle_get_sdds_system(core_id, token = token),
        error = function(e) {
          cli_warn(
            "could not get the SYSTEM information of device {.field {core_id}}",
            parent = e
          )
          NA_character_
        }
      )
      ml_extract_publishing(json) |>
        mutate(core_id = !!core_id, .before = 1L)
    }) |>
    bind_rows()
}

# pull the publishing settings out of a (possibly incomplete) SYSTEM json
ml_extract_publishing <- function(json) {
  json <- if (is.null(json) || length(json) == 0) NA_character_ else as.character(json[1])
  # the publishing settings (record/globalInterval_ms only exist there)
  record <- stringr::str_match(json, '"record"\\s*:\\s*"?([A-Za-z_]+)"?')[, 2]
  interval <- stringr::str_match(json, '"globalInterval_ms"\\s*:\\s*([0-9]+)')[, 2] |>
    as.numeric()
  tibble(
    publishing.record = record,
    publishing.globalInterval_ms = format_interval_ms(interval)
  )
}

# format an interval in ms, e.g. 1800000 -> "30 min", 5400000 -> "1 hour 30 min"
format_interval_ms <- function(ms) {
  purrr::map_chr(ms, function(x) {
    if (is.na(x)) {
      return(NA_character_)
    }
    if (x < 1000) {
      return(sprintf("%g ms", x))
    }
    secs <- x / 1000
    parts <- c(
      day = secs %/% 86400,
      hour = (secs %% 86400) %/% 3600,
      min = (secs %% 3600) %/% 60,
      sec = round(secs %% 60, 3)
    )
    parts <- parts[parts > 0]
    labels <- names(parts)
    labels[labels %in% c("day", "hour") & parts != 1] <- paste0(labels[labels %in% c("day", "hour") & parts != 1], "s")
    paste(sprintf("%g %s", parts, labels), collapse = " ")
  })
}
