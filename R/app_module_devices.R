# devices overview: all devices of the group and the experiments they are in,
# with the option to start a new experiment with the selected devices

devices_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        position = "left",
        width = "220",
        actionButton(
          ns("refresh_devices"),
          "Refresh",
          icon = icon("arrows-rotate"),
          style = "border: 0;"
        ) |>
          add_tooltip("Fetch the latest information about the devices."),
        actionButton(
          ns("start_experiment"),
          "Start new experiment with the selected devices",
          icon = icon("flask"),
          style = "border: 0;"
        ) |>
          shinyjs::disabled() |>
          add_tooltip(
            "Create a new experiment, link the selected devices to it and put it in control of the ones that are not in use by another experiment."
          )
      ),
      module_selector_table_ui(ns("devices"))
    ),
    bslib::card_footer(
      "All devices of this group and the experiments they are currently used by. Select devices to start a new experiment with them."
    )
  )
}

# @param on_new_experiment function called with the new exp_id after an experiment was started
devices_server <- function(id, data, get_timezone, on_new_experiment) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## refresh
    observeEvent(input$refresh_devices, data$refresh_group_devices())

    ## prep devices for table
    get_devices_for_table <- reactive({
      validate(need(
        data$get_group_devices(),
        "There are no devices in this group."
      ))
      out <- data$get_group_devices() |>
        get_unlinked_devices_for_table_in_app(
          timezone = get_timezone(),
          user_id = data$get_user_id()
        ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    ## devices table
    devices <- module_selector_table_server(
      "devices",
      get_data = get_devices_for_table,
      id_column = "core_id",
      # make id column invisible
      columnDefs = list(
        list(visible = FALSE, targets = 0)
      ),
      auto_reselect = FALSE,
      paging = FALSE,
      dom = "ft",
      no_data_message = "There are no devices in this group."
    )

    ## button
    observeEvent(
      devices$get_selected_ids(),
      {
        n <- length(devices$get_selected_ids())
        shinyjs::toggleState("start_experiment", condition = n > 0)
        updateActionButton(
          inputId = "start_experiment",
          label = if (n > 0) {
            format_inline("Start new experiment with the {n} selected device{?s}")
          } else {
            "Start new experiment with the selected devices"
          }
        )
      },
      ignoreNULL = FALSE
    )

    ## start new experiment
    observeEvent(input$start_experiment, {
      core_ids <- devices$get_selected_ids()
      req(length(core_ids) > 0)
      exp_id <- data$start_exp_with_devices(core_ids)
      if (!is.null(exp_id)) {
        devices$deselect_all()
        on_new_experiment(exp_id)
      }
    })

    list(
      refresh = data$refresh_group_devices
    )
  })
}
