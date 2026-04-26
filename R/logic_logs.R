# parse the json from the snapshots
ml_parse_experiment_devices_snapshots <- function(snapshots) {
  this_call <- current_call()
  snapshots |>
    mutate(
      data = purrr::map(
        snapshot_json,
        ~ {
          out <- sddsParticle::sdds_parse_state_snapshot(.x) |> try_catch_cnds()
          out |> show_cnds(.call = this_call)
          out$result
        }
      )
    ) |>
    select(-"snapshot_json") |>
    tidyr::unnest(data)
}

# Parse data logs
ml_parse_logs <- function(
  logs,
  experiments = NULL,
  timezone = Sys.timezone()
) {
  cli_alert_info("Parsing {nrow(logs)} {.field logs}.")
  logs <- logs |>
    mutate(
      # log datetime with the time offset
      log_datetime = .data$log_datetime +
        lubridate::milliseconds(.data$log_time_offset),
      # figure out the parent
      parent = .data$data_path |>
        stringr::str_remove("\\.[^.]+$") |>
        # remove all units
        stringr::str_remove_all("_[^.]+"),
      # figure out the measurement base unit
      units = case_when(
        !is.na(.data$data_units) ~ .data$data_units,
        TRUE ~ .data$data_path |>
          stringr::str_extract("(?<=_)([^.$]+)(?=(\\.|$))") |>
          stringr::str_replace_all("_", "/")
      ),
      # figure out the variable
      variable = .data$data_path |>
        stringr::str_extract("\\.[^.]+$") |>
        stringr::str_remove_all("(^\\.|_.*$)"),
      # root elements
      variable = if_else(is.na(.data$variable), .data$parent, .data$variable),
      # variale with units
      var_w_units = case_when(
        !is.na(.data$units) ~ sprintf("%s [%s]", .data$variable, .data$units),
        TRUE ~ .data$variable
      ),
      # device label
      device_label = if_else(!is.na(.data$label), .data$label, .data$core_name),
      # factors
      parent = factor(.data$parent),
      # snapshot
      is_snap = !is.na(.data$snapshot_id),
      # step
      is_step = FALSE
    ) |>
    # is this a discrete variable?
    mutate(
      .by = c("exp_id", "core_id", "data_path"),
      is_discrete = all(is.na(.data$data_n))
    )

  # create stepwise values for discrete variables (n = NA)
  if (nrow(logs) > 0) {
    logs <-
      logs |>
      bind_rows(
        logs |>
          mutate(
            # extend the last discrete data point to the latest data point we have for the experiment
            .by = c("exp_id"),
            .max_dt = max(.data$log_datetime)
          ) |>
          filter(.data$is_discrete) |>
          arrange(.data$log_datetime) |>
          mutate(
            # interpret these across segments
            .by = c("exp_id", "core_id", "data_path"),
            log_datetime = lead(
              .data$log_datetime,
              1,
              default = .data$.max_dt[1]
            ) -
              lubridate::milliseconds(1),
            is_step = TRUE
          ) |>
          select(-".max_dt")
      ) |>
      arrange(.data$log_datetime)
  }

  # duration
  if (nrow(logs) > 0) {
    logs <- logs |>
      mutate(
        .by = any_of("exp_id"),
        duration.sec = (.data$log_datetime - min(.data$log_datetime)) |>
          as.numeric("secs")
      )
  } else {
    logs$duration.sec <- numeric()
  }

  # local TZ conversion
  if (!is.null(timezone)) {
    cli_alert_info(
      "Converting {.code log_datetime} to timezone {col_blue(timezone)}."
    )
    logs <- logs |>
      mutate(
        log_datetime = .data$log_datetime |> lubridate::with_tz(timezone),
        data_text = case_when(
          !is.na(.data$data_units) &
            !is.na(.data$data_text) &
            .data$data_units == "dt" ~ .data$data_text |>
            lubridate::ymd_hms(quiet = TRUE) |>
            lubridate::with_tz(timezone) |>
            format("%b %d %Y %H:%M:%S"),
          TRUE ~ .data$data_text
        )
      )
  }

  # cleanup
  logs <- logs |>
    select(
      "exp_id",
      "segment",
      "log_id",
      "snapshot_id",
      "device" = "core_name",
      "label" = "device_label",
      "datetime" = "log_datetime",
      "duration.sec",
      "path" = "data_path",
      "parent",
      "variable",
      "units",
      "var_w_units",
      "text" = "data_text",
      "value" = "data_value",
      "n" = "data_n",
      "sd" = "data_sd",
      "is_snap",
      "is_discrete",
      "is_step"
    )

  # experiments
  if (!is.null(experiments)) {
    cli_alert_info(
      "Adding experiment information."
    )
    logs <- experiments |> right_join(logs, by = "exp_id")
  }

  return(logs)
}

#' summarize log devices
ml_summarize_log_devices <- function(
  logs,
  include_snapshot_only_values = FALSE,
  include_text_values = FALSE
) {
  if (!include_snapshot_only_values) {
    logs <- logs |>
      filter(
        .by = c("exp_id", "path"),
        any(!.data$is_snap)
      )
  }
  if (!include_text_values) {
    logs <- logs |> filter(!is.na(value))
  }
  logs |>
    summarize(
      .by = "device",
      # to make sure there's no problem with cached label
      label = tail(label, n = 1L),
      n = n()
    )
}

#' Summarize logs
#' @export
ml_summarize_logs <- function(
  logs,
  include_snapshot_only_values = FALSE,
  include_text_values = FALSE
) {
  if (!include_snapshot_only_values) {
    logs <- logs |>
      filter(
        .by = c("exp_id", "path"),
        any(!.data$is_snap)
      )
  }
  if (!include_text_values) {
    logs <- logs |> filter(!is.na(value))
  }
  logs |>
    summarize(
      .by = c("exp_id", "path", "parent", "variable", "units", "var_w_units"),
      n = n()
    ) |>
    arrange(parent, variable)
}

#' Plot data logs
#'
#' This function helps to visualize data logs retrieved via [ml_get_logs] or downloaded from the app.
#'
#' @param logs data logs
#' @param filter generic filter, can be any expression
#' @param filter_paths a character vector of paths to include in the visualization
#' @param include_snapshot_only_values if a variable path only has values from snapshot, should they still be plotted? By default NO unless `filter_paths` makes specific selections.
#' @export
ml_plot_logs <- function(
  logs,
  filter = TRUE,
  filter_paths = NULL,
  timescale = c("datetime", "duration"),
  timescale_lim = NULL,
  n_timescale_breaks = 5,
  color = label,
  linetype = NULL,
  panel = NULL,
  show_points = FALSE,
  show_error_range = FALSE,
  include_outliers = TRUE,
  include_snapshot_only_values = !is.null(filter_paths),
  scale_color = scale_color_brewer(palette = "Dark2"),
  scale_fill = scale_fill_brewer(palette = "Dark2"),
  text_size = 14
) {
  # safety checks
  timescale <- rlang::arg_match(timescale)
  timescale_lim |>
    check_arg(
      is.null(timescale_lim) || length(timescale_lim) == 2,
      "must be 2 values (min and max) if provided"
    )
  filter_paths |>
    check_arg(
      is.null(filter_paths) || is_character(filter_paths),
      "must be a vector of variable paths"
    )

  # plot df
  plot_df <- logs |>
    # use numeric values for now
    # TODO: provide factor based plotting for text-based values
    dplyr::filter(!is.na(value)) |>
    dplyr::filter({{ filter }}) |>
    # make sure data is in order
    arrange(datetime)

  # outliers
  # FIXME: implement
  # if (!include_outliers) {
  #   plot_df <- plot_df |>
  #     identify_data_outliers() |>
  #     mutate(data_value = ifelse(outlier, NA_real_, data_value))
  # }

  # specific paths
  if (!is.null(filter_paths)) {
    plot_df <- plot_df |>
      filter(.data$path %in% filter_paths)
  }

  # snapshot only
  if (!include_snapshot_only_values) {
    plot_df <- plot_df |>
      filter(
        .by = c("exp_id", "device", "path"),
        any(!.data$is_snap)
      )
  }

  # plot
  p <-
    ggplot() +
    aes(
      y = value,
      group = paste(exp_id, device, segment, path),
      color = {{ color }},
      linetype = {{ linetype }}
    ) +
    scale_color +
    scale_fill +
    facet_grid(
      rows = vars(var_w_units),
      cols = vars({{ panel }}),
      scales = "free"
    ) +
    theme_bw() +
    theme(text = element_text(size = text_size)) +
    labs(x = NULL, y = NULL, color = NULL, fill = NULL, linetype = NULL)

  # x axis
  if (timescale == "datetime") {
    p <- p +
      aes(x = datetime) +
      scale_x_datetime(
        breaks = scales::breaks_pretty(n = n_timescale_breaks),
        expand = if (!is.null(timescale_lim)) FALSE else waiver()
      )
  } else if (timescale == "duration") {
    p <- p +
      aes(x = duration.sec) +
      scale_x_continuous(
        breaks = breaks_pretty_duration(n = n_timescale_breaks),
        labels = labels_duration(),
        expand = if (!is.null(timescale_lim)) FALSE else waiver()
      )
  }

  # limits
  if (!is.null(timescale_lim)) {
    # figure out what's in range
    if (timescale == "datetime") {
      plot_df <- plot_df |>
        mutate(
          in_range = datetime >= timescale_lim[1] & datetime <= timescale_lim[2]
        )
    } else {
      plot_df <- plot_df |>
        mutate(
          in_range = duration.sec >= timescale_lim[1] &
            duration.sec <= timescale_lim[2]
        )
    }
    # for proper y scaling, select the data that's just before/after the selecting time scale
    plot_df <- plot_df |>
      mutate(
        .by = c("exp_id", "device", "path"),
        just_before = !in_range & lag(in_range, default = FALSE),
        just_after = !in_range & lead(in_range, default = FALSE)
      ) |>
      filter(in_range | just_before | just_after)

    p <- p + coord_cartesian(xlim = timescale_lim)
  }

  # error range
  if (show_error_range) {
    # figure out continuous stretches of error bars
    plot_df <- plot_df |>
      mutate(
        .by = c("exp_id", "device", "segment", "path"),
        has_error = !is.na(sd),
        error_group = cumsum(lag(!has_error, default = FALSE) & has_error)
      )
    p <- p +
      aes(fill = {{ color }}) +
      geom_ribbon(
        data = function(df) dplyr::filter(df, !is.na(sd)),
        mapping = aes(
          ymin = value - sd,
          ymax = value + sd,
          color = NULL,
          group = paste(exp_id, device, segment, path, error_group)
        ),
        show.legend = FALSE,
        alpha = 0.3
      )
  }

  # check data
  if (nrow(plot_df) == 0) {
    return(
      ggplot() +
        annotate(
          "text",
          x = 0,
          y = 0,
          label = "no data",
          vjust = 0.5,
          hjust = 0.5,
          size = text_size
        ) +
        theme_void()
    )
  }

  # add data
  p <- p + plot_df

  # add line
  # TODO: implement data that is stepwise here as well (if n=1)
  # or maybe this should happen in the prep step? probably there
  p <- p +
    # continous data
    geom_line(data = ~ .x |> filter(!is_discrete)) +
    # discrete data - plot across segment breaks
    geom_line(
      data = ~ .x |> filter(is_discrete),
      map = aes(group = paste(exp_id, device, path))
    )

  # add points
  if (show_points) {
    p <- p + geom_point(data = ~ .x |> filter(!is_step))
  }
  return(p)
}
