# get_ function are all reactive
experiments_server <- function(
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

    # reactive values =======
    values <- reactiveValues(
      refresh_experiments = 0,
      current_exp_id = NULL,
      current_exp = NULL,
      current_exp_devices = NULL,
      current_exp_owner = FALSE
    )

    # general ownership info
    is_owner_or_admin <- reactive({
      identical(values$current_exp_owner, TRUE) || is_admin()
    })

    # experiment selection =============

    ## get experiments from DB
    get_experiments <- reactive({
      values$refresh_experiments
      log_info(ns = ns, user_msg = "Fetching experiments")
      # safely call function
      out <-
        ml_get_experiments(group_id = get_group(), convert_to_TZ = "UTC") |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    ## refresh
    observeEvent(input$refresh_experiments, {
      values$refresh_experiments <- values$refresh_experiments + 1L
    })

    ## prep experiments for table
    get_experiments_for_table <- reactive({
      # safety checks
      validate(need(
        !is.null(get_experiments()) && nrow(get_experiments()) > 0,
        "There are no experiments yet, please add one."
      ))
      # safely call function
      out <- get_experiments() |>
        get_experiments_for_table_in_app(
          timezone = get_timezone(),
          user_id = get_user_id()
        ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    ## setup experiments selector table
    experiments <- callModule(
      module_selector_table_server,
      "experiments",
      get_data = get_experiments_for_table,
      id_column = "exp_id",
      # row grouping
      extensions = "RowGroup",
      rowGroup = list(dataSrc = 1),
      # make id & category column invisible
      columnDefs = list(
        list(visible = FALSE, targets = 0:1)
      ),
      selection = "single",
      # view & scrolling
      allow_view_all = TRUE,
      initial_page_length = -1,
      dom = "ft",
      scrollX = TRUE,
      scrollY = "150px"
    )
    output$experiments_footer <- renderUI({
      if (!is.null(get_experiments()) && nrow(get_experiments()) > 0) {
        "Select the experiment you want to work with or add a new experiment."
      } else {
        spaces(1)
      }
    })

    # experiment editing =============

    ## add new experiment
    observeEvent(input$add_experiment, {
      log_info(ns = ns, user_msg = "Creating new experiment")
      # safely call function
      out <- ml_add_experiment(
        group_id = get_group(),
        user_id = get_user_id(),
        user_first = get_user_first_name(),
        user_last = get_user_last_name()
      ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (!is.null(out$result)) {
        values$refresh_experiments <- values$refresh_experiments + 1L
        log_success(ns = ns, user_msg = "Experiment initiated")
      }
    })

    ## edit experiment --> load experiment
    observe(
      {
        # hide experiment box
        if (
          nrow(get_experiments()) == 0 ||
            is_empty(experiments$get_selected_ids())
        ) {
          shinyjs::hide("experiment_box")
        }
        new_exp_id <- experiments$get_selected_ids()[1]
        values$current_exp_id <- new_exp_id
        print(new_exp_id)
        if (!is.null(new_exp_id)) {
          values$current_exp <- get_experiments() |>
            filter(exp_id == !!new_exp_id)
          values$current_exp_owner <- isolate(values$current_exp$user_id) ==
            get_user_id()
          linked$refresh <- isolate(linked$refresh) + 1L
        }
      },
      priority = 100
    )

    # show experiment box at end of load
    observeEvent(
      values$current_exp_id,
      {
        req(nrow(get_experiments() > 0))
        req(values$current_exp_id)
        req(values$current_exp)
        shinyjs::show("experiment_box")
      },
      priority = -100
    )

    ## generate experiment tabset
    output$experiment_tabset <- renderUI({
      if (
        nrow(get_experiments()) == 0 ||
          is.null(values$current_exp_id) ||
          is.null(values$current_exp)
      ) {
        return(NULL)
      }
      isolate({
        tabsetPanel(
          id = ns("tabset"),
          type = "tabs",
          selected = if (
            is_owner_or_admin() && is.na(values$current_exp$name)
          ) {
            "configuration"
          } else {
            "data"
          },
          generate_experiment_data_ui(ns = ns),
          if (is_owner_or_admin()) {
            generate_experiment_configuration_ui(
              ns = ns,
              owner = simplify_owner(
                values$current_exp,
                user_id = get_user_id()
              )$owner,
              archived = values$current_exp$archived,
              recording = values$current_exp$recording,
              exp_name = values$current_exp$name,
              exp_desc = values$current_exp$description,
              exp_notes = values$current_exp$notes
            )
          },
          if (is_owner_or_admin()) {
            generate_experiment_device_control_ui(ns = ns)
          }
        )
      })
    })

    ## start recording
    observeEvent(input$start_recording, {})

    # experiment devices =========

    ## get available devices from database and merge in the cloud information like name
    get_devices <- reactive({
      req(get_group())
      req(nrow(get_experiments() > 0))
      req(isolate(values$current_epx_id)) # don't run if there's nothing selected but don't triger based on this
      all_devices <- particle$get_all_devices()
      log_info(ns = ns, user_msg = "Fetching registered devices")
      # safely call function
      out <-
        ml_get_devices(group_id = get_group()) |>
        left_join(all_devices, by = c("core_id" = "coreid")) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    ## get avilable devices for table
    get_devices_for_table <- reactive({
      # safety checks
      validate(need(get_devices(), "No devices."))
      # safely call function
      out <- get_devices() |>
        get_devices_for_table_in_app(
          timezone = get_timezone(),
          user_id = get_user_id()
        ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    ## add devices modal
    add_experiment_devices_modal <- modalDialog(
      title = h3("Link devices to experiment"),
      module_selector_table_ui(ns("all_devices")),
      footer = tagList(
        actionButton(
          ns("modal_add_device"),
          "Link selected",
          icon = icon("plus"),
          style = "border: 0;"
        ) |>
          add_tooltip(
            "Link the selected devices."
          ),
        modalButton("Close")
      ),
      easyClose = TRUE,
      size = "l"
    )

    ## setup group devices selector table
    all_devices <- callModule(
      module_selector_table_server,
      "all_devices",
      get_data = get_devices_for_table,
      id_column = "core_id",
      # make id column invisible
      columnDefs = list(
        list(visible = FALSE, targets = 0)
      ),
      # view all & scrolling
      allow_view_all = TRUE,
      auto_reselect = FALSE,
      initial_page_length = -1,
      dom = "ft",
      scrollX = TRUE
    )

    ## trigger modal
    observeEvent(input$add_experiment_devices, {
      showModal(add_experiment_devices_modal)
    })

    ## reactive values for the linked devices
    linked <- reactiveValues(
      refresh = 0L,
      selected = c(),
    )

    ## select devices to link to experiment
    observeEvent(all_devices$get_selected_ids(), {
      newly_selected <- setdiff(all_devices$get_selected_ids(), linked$selected)
      if (is_empty(newly_selected)) {
        return()
      }
      device <- get_devices() |>
        filter(core_id == newly_selected) |>
        simplify_owner(user_id = get_user_id())
      if (!is.na(device$exp_id)) {
        log_warning(
          ns = ns,
          user_msg = sprintf("Cannot use %s", device$name),
          warning = sprintf(
            "This device is currently in use by %s and cannot be linked to this experiment. Ask them to finish their experiment so the device becomes available again.",
            device$owner
          )
        )
        # reset to linked selected
        all_devices$select_rows(ids = linked$selected)
      } else {
        # update selected
        linked$selected <- all_devices$get_selected_ids()
      }
    })

    ## get experiment devices from DB
    get_experiment_devices <- reactive({
      req(linked$refresh)
      req(nrow(isolate(get_experiments()) > 0))
      req(isolate(is_owner_or_admin()))
      all_devices <- isolate(particle$get_all_devices())
      log_info(ns = ns, user_msg = "Fetching experiment devices")
      # safely call function
      out <-
        ml_get_experiment_devices(exp_id = values$current_exp_id) |>
        left_join(all_devices, by = c("core_id" = "coreid")) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    ## prep experiment devices for table
    get_experiment_devices_for_table <- reactive({
      # safety checks
      req(get_experiment_devices())
      # safely call function
      out <- get_experiment_devices() |>
        get_experiment_devices_for_table_in_app(timezone = get_timezone()) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    ## setup experiment devices selector table
    experiment_devices <- callModule(
      module_selector_table_server,
      "experiment_devices",
      get_data = get_experiment_devices_for_table,
      id_column = "row_id",
      # make row_id column invisible
      columnDefs = list(
        list(visible = FALSE, targets = 0)
      ),
      selection = "single",
      auto_reselect = FALSE,
      # view & scrolling
      allow_view_all = TRUE,
      initial_page_length = -1,
      dom = "ft",
      no_data_message = ""
    )

    ## refresh experiment devices
    observeEvent(input$refresh_experiment_devices, {
      linked$refresh <- isolate(linked$refresh) + 1L
    })
  })
}
