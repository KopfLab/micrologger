# data server taking care loading data only as needed
data_server <- function(
  id,
  particle,
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
      current_exp_id = NULL,
      current_exp = NULL,
      current_exp_owner = FALSE,
      refresh_exp_devices = 0,
      current_exp_devices = NULL
    )

    # change group ====
    observeEvent(get_group(), {
      refresh_exps()
    })

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
      particle$refresh_devices()
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

    # check if current exp is owned by user or user is an admin
    is_owner_or_admin <- reactive({
      identical(values$current_exp_owner, TRUE) || is_admin()
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

    # EXPERIMENT DEVICES ======

    ## refresh experiment devices ======
    refresh_exp_devices <- function() {
      values$refresh_exp_devices <- values$refresh_exp_devices + 1L
      particle$refresh_devices()
    }

    ## get experiment devices from DB =========

    get_exp_devices <- reactive({
      req(values$has_exp_loaded)
      req(values$current_exp_id)
      req(is_owner_or_admin())
      values$refresh_exp_devices
      all_devices <- particle$get_all_devices() |>
        rename("core_name" = "name")
      log_info(ns = ns, user_msg = "Fetching experiment devices")
      # safely call function
      out <-
        ml_get_experiment_devices(exp_id = values$current_exp_id) |>
        left_join(all_devices, by = c("core_id" = "coreid")) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    # REGISTERED DEVICES ========

    ## get registered devices ====
    get_registered_devices <- reactive({
      req(get_group())
      all_devices <- particle$get_all_devices()
      log_info(ns = ns, user_msg = "Fetching registered devices")
      # safely call function
      out <-
        ml_get_devices(group_id = get_group()) |>
        left_join(all_devices, by = c("core_id" = "coreid")) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (is.null(out$result) || nrow(out$result) == 0L) {
        return(NULL)
      }
      return(out$result)
    })

    # RETURN ====
    list(
      ## experiments =======
      get_experiments = get_experiments,
      refresh_exps = refresh_exps,
      has_exp_loaded = has_exp_loaded,
      add_exp = add_exp,
      load_exp = load_exp,
      save_exp = save_exp,
      get_current_exp = reactive({
        values$current_exp
      }),
      is_owner_or_admin = is_owner_or_admin,
      start_exp_recording = start_exp_recording,
      stop_exp_recording = stop_exp_recording,
      archive_exp = archive_exp,
      ## devices =====
      refresh_exp_devices = refresh_exp_devices,
      get_exp_devices = get_exp_devices,
      get_registered_devices = get_registered_devices
    )
  })
}
