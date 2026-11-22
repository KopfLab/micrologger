# data server taking care loading data only as needed
data_server <- function(
  id,
  particle_token,
  experiment_core_ids,
  experiment_device_labels = function(...) NULL,
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
      refresh_group_devices = 0,
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
        if (screen == "device_ctrl") {
          # make sure device links are loaded when device control is selected
          get_exp_devices_links()
        }
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
        # do this last for snapshot
        send_recording_start_commands()
        refresh_exps()
      }
    }

    stop_exp_recording <- function() {
      if (!has_exp_loaded()) {
        log_error(ns = ns, user_msg = "No experiment loaded")
      }
      # do this first for snapshot
      send_recording_stop_commands()
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

    delete_exp <- function() {
      if (!has_exp_loaded()) {
        log_error(ns = ns, user_msg = "No experiment loaded")
        return()
      }
      out <- ml_delete_experiment(exp_id = values$current_exp_id) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (identical(out$result, TRUE)) {
        log_success(
          ns = ns,
          user_msg = "Experiment and all its data have been deleted."
        )
        # the experiment no longer exists - reset the loaded state
        values$current_exp_id <- NULL
        values$current_exp <- NULL
        values$has_exp_loaded <- FALSE
        refresh_exps()
      }
    }

    # PARTICLE DEVICES ======

    ## get particle devices ============

    # get devies in app
    get_particle_devices <- reactive({
      values$refresh_exp_devices
      values$refresh_group_devices
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

    ## send commands ot particle devices ==========

    ## send commands to particle device
    send_commands <- function(coreid, cmds, .call = caller_call()) {
      out <- coreid[1] |>
        sddsParticle::particle_send_sdds_commands(
          cmds = cmds,
          token = particle_token
        ) |>
        try_catch_cnds(error_value = list(success = FALSE))
      out |> log_cnds(ns = ns, .call = .call)
      return(all(out$result$success))
    }

    ## start recording
    send_recording_start_commands <- function() {
      n_links <- get_exp_devices_links() |> nrow()
      if (n_links == 0) {
        return()
      }
      core_ids <- experiment_core_ids()
      if (length(core_ids) < n_links) {
        log_warning(
          ns = ns,
          user_msg = "Not all devices under control",
          warning = format_inline(
            "{n_links - length(core_ids)} device{?s} {?is/are} NOT under control of this experiment and may not be recording."
          )
        )
      }
      if (length(core_ids) == 0) {
        return()
      }

      device_info <- get_exp_devices_info() |>
        dplyr::filter(.data$core_id %in% core_ids)
      success <- rep(FALSE, nrow(device_info))

      # try to start recording on connected devices
      if (any(device_info$connected)) {
        current_call <- current_call()
        success[device_info$connected] <- device_info$core_id[
          device_info$connected
        ] |>
          purrr::map_lgl(
            send_commands,
            cmds = c(
              "SYSTEM.publishing.record=ON",
              "SYSTEM.action=saveState",
              "SYSTEM.action=sendSddsState"
            ),
            .call = current_call
          )
      }

      # update
      if (all(success)) {
        log_success(
          ns = ns,
          user_msg = format_inline(
            "All devices under control of this experiment ({length(success)}) have started recording."
          )
        )
      } else {
        log_warning(
          ns = ns,
          user_msg = "Devices offline",
          warning = format_inline(
            "{sum(!success)}/{length(success)} of the {qty(length(success))}device{?s} under control of this experiment ({device_info$core_name[!success]}) could not be reached and {?is/are} not recording. If {?it is/they are} online, pause + resume the experiment recording to try again. If {?it is/they are} offline, connect to {?it/them} via bluetooth with the SDDS browser (devices.kopflab.org) and click {.strong Start recording} at the top of the page."
          )
        )
      }
    }

    ## stop recording
    send_recording_stop_commands <- function() {
      n_links <- get_exp_devices_links() |> nrow()
      if (n_links == 0) {
        return()
      }
      core_ids <- experiment_core_ids()
      if (length(core_ids) < n_links) {
        log_warning(
          ns = ns,
          user_msg = "Not all devices under control",
          warning = format_inline(
            "{n_links - length(core_ids)} device{?s} {?is/are} NOT under control of this experiment and may still be recording."
          )
        )
      }
      if (length(core_ids) == 0) {
        return()
      }

      device_info <- get_exp_devices_info() |>
        dplyr::filter(.data$core_id %in% core_ids)
      success <- rep(FALSE, nrow(device_info))

      # try to stop connected devices
      if (any(device_info$connected)) {
        current_call <- current_call()
        success[device_info$connected] <- device_info$core_id[
          device_info$connected
        ] |>
          purrr::map_lgl(
            send_commands,
            cmds = c(
              "SYSTEM.action=sendSddsState",
              "SYSTEM.publishing.record=OFF",
              "SYSTEM.action=saveState"
            ),
            .call = current_call
          )
      }

      # update
      if (all(success)) {
        log_success(
          ns = ns,
          user_msg = format_inline(
            "All devices under control of this experiment ({length(success)}) have stopped recording."
          )
        )
      } else {
        log_warning(
          ns = ns,
          user_msg = "Devices offline",
          warning = format_inline(
            "{sum(!success)}/{length(success)} of the {qty(length(success))}device{?s} under control of this experiment ({device_info$core_name[!success]}) could not be reached and may still be recording. If {?it is/they are} online, resume + pause the experiment recording to try to stop again. If {?it is/they are} offline, connect to {?it/them} via bluetooth with the SDDS browser (devices.kopflab.org) and click {.strong Stop recording} at the top of the page."
          )
        )
      }
    }

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
        # and the custom labels of the devices
        result |>
          filter(!is.na(.data$label) & nzchar(.data$label)) |>
          select(coreid = "core_id", "label") |>
          experiment_device_labels()
      } else {
        # no core ids are accessible
        experiment_core_ids(c())
        experiment_device_labels(tibble(coreid = character(), label = character()))
      }
      return(result)
    })

    ## experiment device info =========

    get_exp_devices_info <- reactive({
      req(get_exp_devices_links())
      req(values$current_exp_screen == "configuration")
      all_devices <- get_particle_devices()
      # safely get device links and join with all devices
      out <-
        get_exp_devices_links() |>
        left_join(all_devices, by = "core_id") |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      exp_devices <- out$result
      if (is.null(exp_devices) || nrow(exp_devices) == 0) {
        return(NULL)
      }

      # bring in system information for connected devices (function returns out with result and conditions)
      connected_devices <- dplyr::filter(exp_devices, .data$connected)$core_id
      if (length(connected_devices) > 0) {
        # only the publishing settings are needed: pulled straight out of the
        # SYSTEM json (which can be cut off since the SYSTEM tree got bigger)
        sys_info_out <-
          ml_get_devices_publishing(
            core_ids = connected_devices,
            token = particle_token
          ) |>
          try_catch_cnds()
        sys_info_out |> log_cnds(ns = ns)
        if (!is.null(sys_info_out$result)) {
          exp_devices <- exp_devices |>
            left_join(sys_info_out$result, by = "core_id")
        }
      }
      # devices that are not connected to the cloud can't be asked -> "offline"
      # (connected devices whose settings could not be read stay empty)
      for (col in c("publishing.record", "publishing.globalInterval_ms")) {
        if (!col %in% names(exp_devices)) {
          exp_devices[[col]] <- NA_character_
        }
        exp_devices[[col]] <- if_else(
          !is.na(exp_devices$connected) & exp_devices$connected,
          exp_devices[[col]],
          "offline"
        )
      }
      return(exp_devices)
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
        labels = label
      ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (!is.null(out$result) && out$result == 1) {
        log_success(ns = ns, user_msg = "Device label updated.")
        ml_clear_logs_cache() # clear cache to avoid label isses
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

    ## all devices of the group (devices overview) ====
    refresh_group_devices <- function() {
      values$refresh_group_devices <- values$refresh_group_devices + 1L
    }

    get_group_devices <- reactive({
      req(get_group())
      all_devices <- get_particle_devices()
      log_info(ns = ns, user_msg = "Fetching devices")
      out <-
        ml_get_devices(group_id = get_group()) |>
        left_join(all_devices, by = "core_id") |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (is.null(out$result) || nrow(out$result) == 0L) {
        return(NULL)
      }
      return(out$result)
    })

    ## start a new experiment with devices ====
    ## creates the experiment, links the devices and claims the ones that are
    ## not in use by another experiment; returns the new exp_id (NULL if it failed)
    start_exp_with_devices <- function(core_ids) {
      if (!is.character(core_ids) || is_empty(core_ids)) {
        log_error(ns = ns, user_msg = "No devices selected")
        return(NULL)
      }
      exp_id <- add_exp()
      if (is.null(exp_id)) {
        return(NULL)
      }

      # link devices
      out <- ml_link_devices_to_experiment(
        exp_id = exp_id,
        core_ids = core_ids
      ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (is.null(out$result)) {
        return(exp_id)
      }
      n_linked <- out$result

      # claim the devices that are not in use
      unclaimed <- get_group_devices() |>
        filter(.data$core_id %in% !!core_ids, is.na(.data$control_exp_id)) |>
        pull(.data$core_id)
      for (core_id in unclaimed) {
        out <- ml_claim_device_for_experiment(
          exp_id = exp_id,
          core_id = core_id
        ) |>
          try_catch_cnds()
        out |> log_cnds(ns = ns)
      }
      in_use <- length(core_ids) - length(unclaimed)
      log_success(
        ns = ns,
        user_msg = format_inline(
          "New experiment started with {n_linked} device{?s}{if (in_use > 0) format_inline(' ({in_use} {?is/are} in use by another experiment and cannot be controlled)') else ''}."
        )
      )
      refresh_group_devices()
      return(exp_id)
    }

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

    ## upload offline data =========

    ## read + check a data bursts file for the selected device (dry run)
    ## returns list(records, summary) or NULL if the file can't be used
    check_device_data_upload <- function(file) {
      if (!has_exp_loaded() || !values$has_exp_device_selected) {
        log_error(ns = ns, user_msg = "No experiment device selected")
        return(NULL)
      }
      out <- ml_read_data_bursts(file) |> try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (is.null(out$result)) {
        return(NULL)
      }
      records <- out$result
      out <- ml_upload_data_bursts(
        exp_id = values$current_exp_id,
        core_id = values$selected_exp_device_core_id,
        records = records,
        core_name = values$selected_exp_device$core_name,
        dry_run = TRUE
      ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (is.null(out$result)) {
        return(NULL)
      }
      list(records = records, summary = out$result)
    }

    ## upload checked records for the selected device
    upload_device_data <- function(records) {
      if (!has_exp_loaded() || !values$has_exp_device_selected) {
        log_error(ns = ns, user_msg = "No experiment device selected")
        return()
      }
      out <- ml_upload_data_bursts(
        exp_id = values$current_exp_id,
        core_id = values$selected_exp_device_core_id,
        records = records,
        core_name = values$selected_exp_device$core_name
      ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (!is.null(out$result)) {
        log_success(
          ns = ns,
          user_msg = format_inline(
            "Uploaded data: {out$result$new} new record{?s}, {out$result$existing} existing record{?s} added to the experiment."
          )
        )
        # the logs cache only fetches newer log ids, existing records that were
        # just linked would be missed -> reload this experiment's logs from scratch
        unlink(file.path("cache", sprintf("exp%s_logs.rds", values$current_exp_id)))
        refresh_logs()
      }
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
      delete_exp = delete_exp,
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
      refresh_group_devices = refresh_group_devices,
      get_group_devices = get_group_devices,
      start_exp_with_devices = start_exp_with_devices,
      ## logs =====
      refresh_logs = refresh_logs,
      check_device_data_upload = check_device_data_upload,
      upload_device_data = upload_device_data,
      get_logs = get_logs
    )
  })
}
