# data server taking care loading data only as needed
data_server <- function(
  id,
  particle_token,
  experiment_core_ids,
  get_timezone,
  get_user_id,
  get_user_first_name,
  get_user_last_name,
  get_group,
  is_admin
) {
  # actual module server
  moduleServer(id, function(input, output, session) {
    # namespace
    ns <- session$ns

    # reactive values keeping track of loaded data
    values <- reactiveValues(
      refresh_experiments = 0,
      has_any_exps = FALSE,
      has_exp_loaded = FALSE,
      current_exp_screen = "data",
      current_exp_id = NULL,
      current_exp = NULL,
      current_exp_owner = FALSE,
      refresh_exp_devices = 0,
      current_exp_devices = NULL,
      has_exp_device_selected = FALSE,
      selected_exp_device_core_id = NULL,
      selected_exp_device = NULL,
      refresh_unlinked_devices = 0,
      refresh_logs = 0
    )

    # EXPERIMENTS =======

    ## load experiments =====
    get_experiments <- reactive({
      values$refresh_experiments
      log_info(ns = ns, user_msg = "Fetching experiments")
      # safely call function
      out <-
        ml_get_experiments(group_id = get_group(), convert_to_TZ = NULL) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (is.null(out$result) || nrow(out$result) == 0L) {
        values$has_any_exps <- FALSE
        return(NULL)
      }
      values$has_any_exps <- TRUE
      return(out$result)
    })

    ## refresh experiments ======
    refresh_exps <- function() {
      # ensure everything is loaded from scratch
      values$refresh_experiments <- values$refresh_experiments + 1L
      values$has_any_exps <- FALSE
      values$has_exp_loaded <- FALSE
      values$current_exp_id <- NULL
      values$has_exp_device_selected <- FALSE
      values$selected_exp_device_core_id <- NULL
    }

    ## add experiment =======
    add_exp <- function() {
      log_info(ns = ns, user_msg = "Creating new experiment")
      out <- ml_add_experiment(
        group_id = get_group(),
        user_id = get_user_id(),
        user_first = get_user_first_name(),
        user_last = get_user_last_name()
      ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (!is.null(out$result)) {
        log_success(ns = ns, user_msg = "Experiment initiated")
        return(out$result$exp_id)
      }
      return(NULL)
    }

    ## load experiment =====
    load_exp <- function(exp_id) {
      # anything selected?
      if (!values$has_any_exps || is_empty(exp_id) || is_na(exp_id)) {
        values$has_exp_loaded <- FALSE
        return()
      }

      # new experiment selected?
      if (!identical(values$current_exp_id, exp_id)) {
        log_debug(ns = ns, "loading exp_id ", exp_id)
        values$current_exp_id <- exp_id
        values$current_exp <- get_experiments() |>
          filter(exp_id == !!exp_id) |>
          simplify_owner(user_id = get_user_id())
        values$current_exp_owner <- values$current_exp$user_id == get_user_id()
        values$current_exp_devices <- NULL
      }
      values$has_exp_loaded <- TRUE
    }

    has_exp_loaded <- reactive({
      values$has_exp_loaded
    })

    # keep track of which screen is loaded
    load_screen <- function(screen) {
      if (!screen %in% c("data", "configuration", "device_ctrl")) {
        cli_warn("invalid screen {.field {screen}}")
      }
      if (!identical(values$current_exp_screen, screen)) {
        values$current_exp_screen <- screen
      }
    }

    get_screen <- reactive({
      values$current_exp_screen
    })

    # check if current exp is owned by user or user is an admin
    is_owner_or_admin <- reactive({
      identical(values$current_exp_owner, TRUE) || is_admin()
    })

    # check if current experiment has undefined name
    is_unnamed <- reactive({
      is.na(values$current_exp$name) || !nzchar(values$current_exp$name)
    })

    # check if exp is recording
    is_exp_recording <- reactive({
      has_exp_loaded() && identical(values$current_exp$recording, TRUE)
    })

    ## save experiment ========

    save_exp <- function(..., refresh = TRUE) {
      if (!has_exp_loaded()) {
        log_error(ns = ns, user_msg = "No experiment loaded")
      }
      out <-
        ml_update_experiment_info(
          exp_id = values$current_exp_id,
          ...
        ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (!is.null(out$result)) {
        log_success(ns = ns, user_msg = "Experiment saved")
        if (refresh) {
          refresh_exps()
        }
      }
    }

    ## experiment recording ========

    start_exp_recording <- function() {
      if (!has_exp_loaded()) {
        log_error(ns = ns, user_msg = "No experiment loaded")
      }
      out <- ml_experiment_start_recording(exp_id = values$current_exp_id) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (identical(out$result, TRUE)) {
        log_success(ns = ns, user_msg = "Experiment is recording.")
        refresh_exps()
      }
    }

    stop_exp_recording <- function() {
      if (!has_exp_loaded()) {
        log_error(ns = ns, user_msg = "No experiment loaded")
      }
      out <- ml_experiment_stop_recording(exp_id = values$current_exp_id) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (identical(out$result, TRUE)) {
        log_success(ns = ns, user_msg = "Experiment recording is paused.")
        refresh_exps()
      }
    }

    archive_exp <- function() {
      if (!has_exp_loaded()) {
        log_error(ns = ns, user_msg = "No experiment loaded")
      }
      out <- ml_archive_experiment(exp_id = values$current_exp_id) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (identical(out$result, TRUE)) {
        log_success(
          ns = ns,
          user_msg = "Experiment is archived and its devices are released for use."
        )
        refresh_exps()
      }
    }

    # PARTICLE DEVICES ======

    ## get particle devices ============

    # get devies in app
    get_particle_devices <- reactive({
      values$refresh_exp_devices
      log_info(ns = ns, user_msg = "Fetching latest device statuses")
      out <-
        sddsParticle::particle_get_device_info(token = particle_token) |>
        # turn last heard into datetime (UTC), tz conversion happens in table prep, no need to trigger it here
        mutate(last_heard = lubridate::ymd_hms(.data$last_heard, tz = "UTC")) |>
        arrange(.data$name) |>
        rename("core_id" = "coreid", "core_name" = "name") |>
        try_catch_cnds(
          error_value = tibble(core_id = character(), core_name = character())
        )
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    # EXPERIMENT DEVICES ======

    ## refresh exp devices =========
    refresh_exp_devices <- function() {
      values$refresh_exp_devices <- values$refresh_exp_devices + 1L
      values$has_exp_device_selected <- FALSE
      values$selected_exp_device_core_id <- NULL
    }

    ## get experiment devices from DB =========

    get_exp_devices_links <- reactive({
      req(values$has_exp_loaded)
      req(values$current_exp_id)
      req(is_owner_or_admin())
      req(values$current_exp_screen %in% c("configuration", "device_ctrl"))
      values$refresh_exp_devices
      log_info(ns = ns, user_msg = "Fetching experiment devices")
      # safely call function
      out <-
        ml_get_experiment_devices(exp_id = values$current_exp_id) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      # success?
      result <- out$result
      if (!is.null(result)) {
        # store trees and values in the cache
        out <- result |>
          sddsParticle:::cache_trees() |>
          try_catch_cnds()
        out |> log_cnds(ns = ns)
        out <- result |>
          mutate(coreid = core_id) |>
          sddsParticle:::cache_treevalues() |>
          try_catch_cnds()
        out |> log_cnds(ns = ns)
        # tell sdds which core ids are accessible
        result |>
          filter(control_exp_id == values$current_exp_id) |>
          pull(core_id) |>
          experiment_core_ids()
      } else {
        # no core ids are accessible
        experiment_core_ids(c())
      }
      return(result)
    })

    ## experiment device info =========

    get_exp_devices_info <- reactive({
      req(get_exp_devices_links())
      req(values$current_exp_screen == "configuration")
      all_devices <- get_particle_devices()
      # safely call function
      out <-
        get_exp_devices_links() |>
        left_join(all_devices, by = "core_id") |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    ## get experiment device system info from paticle cloud

    ## load experiment device ========

    load_exp_device <- function(core_id) {
      if (is_empty(core_id)) {
        values$has_exp_device_selected <- FALSE
        values$selected_exp_device_core_id <- NULL
      } else {
        values$has_exp_device_selected <- TRUE
        values$selected_exp_device_core_id <- core_id
        values$selected_exp_device <- get_exp_devices_info() |>
          filter(core_id == !!core_id)
      }
    }

    ## save experiment device label =======
    save_exp_device_label <- function(label) {
      if (!has_exp_loaded() || !values$has_exp_device_selected) {
        log_error(ns = ns, user_msg = "No experiment device selected")
      }
      out <- ml_link_devices_to_experiment(
        exp_id = values$current_exp_id,
        core_ids = values$selected_exp_device_core_id,
        label = label
      ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (!is.null(out$result) && out$result == 1) {
        log_success(ns = ns, user_msg = "Device label updated.")
        refresh_exp_devices()
      }
    }

    ## manage experiment devices =========

    ## unlink core id from experiment (also makes sure it is released if necesasry)
    unlink_device <- function() {
      if (!has_exp_loaded() || !values$has_exp_device_selected) {
        log_error(ns = ns, user_msg = "No experiment device selected")
      }
      out <- ml_unlink_device_from_experiment(
        exp_id = values$current_exp_id,
        core_id = values$selected_exp_device_core_id
      ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (!is.null(out$result) && out$result >= 1) {
        log_success(ns = ns, user_msg = "Device unlinked.")
        refresh_exp_devices()
      } else {
        log_warning(ns = ns, user_msg = "Device could not be unlinked.")
      }
    }

    ## claim core id for expeirment
    claim_device <- function(core_id = NULL) {
      if (!has_exp_loaded() || !values$has_exp_device_selected) {
        log_error(ns = ns, user_msg = "No experiment device selected")
      }
      out <- ml_claim_device_for_experiment(
        exp_id = values$current_exp_id,
        core_id = values$selected_exp_device_core_id
      ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (identical(out$result, TRUE)) {
        log_success(ns = ns, user_msg = "Device claimed.")
        refresh_exp_devices()
      } else {
        log_warning(ns = ns, user_msg = "Device could not be claimed.")
      }
    }

    ## free core id from experiment
    release_device <- function() {
      if (!has_exp_loaded() || !values$has_exp_device_selected) {
        log_error(ns = ns, user_msg = "No experiment device selected")
      }
      out <- ml_release_device_from_experiment(
        exp_id = values$current_exp_id,
        core_id = values$selected_exp_device_core_id
      ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (identical(out$result, TRUE)) {
        log_success(ns = ns, user_msg = "Device released.")
        refresh_exp_devices()
      } else {
        log_warning(ns = ns, user_msg = "Device could not be released.")
      }
    }

    # UNLINKED DEVICES ======

    ## refresh unlinked devices ======
    refresh_unlinked_devices <- function() {
      values$refresh_unlinked_devices <- values$refresh_unlinked_devices + 1L
    }

    ## get unlinked devices ====
    get_unlinked_devices <- reactive({
      req(get_group())
      values$refresh_unlinked_devices
      all_devices <- isolate(get_particle_devices())
      log_info(ns = ns, user_msg = "Fetching unlinked devices")

      # get registered devices
      out <-
        ml_get_devices(group_id = get_group()) |>
        left_join(all_devices, by = "core_id") |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (is.null(out$result) || nrow(out$result) == 0L) {
        return(NULL)
      }
      available_to_link <- out$result
      already_linked <- isolate(get_exp_devices_info())
      if (!is.null(already_linked) && nrow(already_linked) > 0) {
        available_to_link <- available_to_link |>
          anti_join(already_linked, by = "core_id")
      }
      return(available_to_link)
    })

    ## link additional devices ======

    ## link core ids to experiment (can provide multiple core_ids) and claim the ones that are not currently used by another experiment
    link_and_claim_devices <- function(core_id) {
      if (!has_exp_loaded()) {
        log_error(ns = ns, user_msg = "No experiment loaded")
      }
      if (!is.character(core_id) || is_empty(core_id)) {
        log_error(ns = ns, user_msg = "No device ID provided")
      }

      # link devices
      out <- ml_link_devices_to_experiment(
        exp_id = values$current_exp_id,
        core_ids = core_id
      ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (is.null(out$result)) {
        return()
      }
      n_linked <- out$result

      # claim unclaimed devices
      out <- get_unlinked_devices() |>
        filter(core_id %in% !!core_id, is.na(control_exp_id)) |>
        pull(core_id) |>
        try_catch_cnds()
      out |> log_cnds()
      if (!is.null(out$result) && length(out$result) > 0) {
        for (core_id in out$result) {
          out <- ml_claim_device_for_experiment(
            exp_id = values$current_exp_id,
            core_id = core_id
          ) |>
            try_catch_cnds()
          out |> log_cnds(ns = ns)
        }
      }

      # final message
      log_success(
        ns = ns,
        user_msg = format_inline(
          "{n_linked} Device{?s} {?was/were} linked to this experiment."
        )
      )

      # refresh devices
      refresh_exp_devices()
    }

    # LOGS ======

    ## refresh logs ======
    refresh_logs <- function() {
      # ensure everything is loaded from scratch
      values$refresh_logs <- values$refresh_logs + 1L
    }

    # FIXME: this is not yet compatible with multiple exp loading!
    # but already brings in the experiment name (as "exp")
    get_logs <- reactive({
      req(values$has_exp_loaded)
      req(values$current_exp_id)
      req(values$current_exp_screen == "data")
      values$refresh_logs
      log_info(ns = ns, user_msg = "Fetching experiment logs")
      # safely call function
      out <-
        ml_get_logs(
          exp_id = values$current_exp_id,
          experiments = get_experiments() |>
            select("exp_id", "exp" = "name"),
          timezone = get_timezone()
        ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (is.null(out$result) || nrow(out$result) == 0L) {
        return(NULL)
      }
      log_success(
        ns = ns,
        user_msg = format_inline("Retrieved {nrow(out$result)} log records.")
      )
      return(out$result)
    })

    # RETURN ====
    list(
      ## admin
      get_user_id = get_user_id,
      get_group = get_group,
      get_timezone = get_timezone,
      ## experiments =======
      get_experiments = get_experiments,
      refresh_exps = refresh_exps,
      has_exp_loaded = has_exp_loaded,
      add_exp = add_exp,
      load_exp = load_exp,
      load_screen = load_screen,
      get_screen = get_screen,
      save_exp = save_exp,
      get_current_exp = reactive({
        values$current_exp
      }),
      is_owner_or_admin = is_owner_or_admin,
      is_unnamed = is_unnamed,
      is_exp_recording = is_exp_recording,
      start_exp_recording = start_exp_recording,
      stop_exp_recording = stop_exp_recording,
      archive_exp = archive_exp,
      ## exp devices =====
      refresh_exp_devices = refresh_exp_devices,
      get_exp_devices_links = get_exp_devices_links,
      get_exp_devices_info = get_exp_devices_info,
      load_exp_device = load_exp_device,
      has_exp_devices_selected = reactive({
        values$has_exp_device_selected
      }),
      get_selected_exp_device = reactive({
        values$selected_exp_device
      }),
      save_exp_device_label = save_exp_device_label,
      link_and_claim_devices = link_and_claim_devices,
      unlink_device = unlink_device,
      claim_device = claim_device,
      release_device = release_device,
      # unlinked devices =====
      refresh_unlinked_devices = refresh_unlinked_devices,
      get_unlinked_devices = get_unlinked_devices,
      ## logs =====
      refresh_logs = refresh_logs,
      get_logs = get_logs
    )
  })
}
