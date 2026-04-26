# get_ function are all reactive
experiments_server <- function(
  id,
  data,
  sdds,
  get_timezone
) {
  # actual module server
  moduleServer(id, function(input, output, session) {
    # namespace
    ns <- session$ns

    # reactive values =======
    values <- reactiveValues(
      refresh_experiments = 0,
      new_exp_id = NULL,
      current_exp_id = NULL,
      current_exp = NULL,
      current_exp_devices = NULL,
      current_exp_owner = FALSE
    )

    # experiment selection =============

    ## refresh
    observeEvent(input$refresh_experiments, data$refresh_exps())

    ## prep experiments for table
    get_experiments_for_table <- reactive({
      # safety checks
      validate(need(
        data$get_experiments(),
        "There are no experiments yet, please add one."
      ))
      # safely call function
      out <- data$get_experiments() |>
        get_experiments_for_table_in_app(
          timezone = get_timezone(),
          user_id = data$get_user_id()
        ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    ## setup experiments selector table
    experiments <- module_selector_table_server(
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
      paging = FALSE,
      dom = "ft"
    )

    # experiment editing =============

    ## add new experiment
    observeEvent(input$add_experiment, {
      new_exp_id <- data$add_exp()
      if (!is.null(new_exp_id)) {
        values$new_exp_id <- new_exp_id
        experiments$deselect_all()
        data$refresh_exps()
      }
    })

    ## reload table --> select new experiment if there is one
    observeEvent(experiments$is_table_reloaded(), {
      req(values$new_exp_id)
      data$load_screen("configuration")
      experiments$select_rows(values$new_exp_id)
      values$new_exp_id <- NULL
    })

    ## change group
    observeEvent(data$get_group(), {
      # unselect experiments
      if (experiments$table_exists()) {
        experiments$deselect_all()
      }
    })

    ## show/load/hide experiment box (controls order of operations with priority)
    observe(
      {
        if (!data$has_exp_loaded()) shinyjs::hide("experiment_box")
      },
      priority = 100
    )
    observe(
      {
        data$load_exp(experiments$get_selected_ids()[1])
        isolate({
          if (data$has_exp_loaded()) {
            # these will only run if the correct screen is selected
            data$get_exp_devices_links()
            data$get_exp_devices_info()
            data$get_logs()
          }
        })
      },
      priority = 50
    )
    observe(
      if (data$has_exp_loaded()) shinyjs::show("experiment_box"),
      priority = -100
    )

    ## generate experiment tabset
    output$experiment_tabset <- renderUI({
      req(data$has_exp_loaded())
      bslib::navset_card_pill(
        id = ns("tabset"),
        selected = if (data$is_owner_or_admin() && data$is_unnamed()) {
          "configuration"
        } else if (data$is_owner_or_admin()) {
          isolate(data$get_screen())
        } else {
          "data"
        },
        bslib::nav_panel(
          value = "data",
          "Data",
          logs_plot_ui(ns("logs_plot"))
        ),
        if (data$is_owner_or_admin()) {
          generate_experiment_configuration_ui(
            ns = ns,
            exp = data$get_current_exp()
          )
        },
        if (data$is_owner_or_admin()) {
          generate_device_control_ui(ns = ns)
        }
      ) |>
        div(class = "centered-pills") |>
        bslib::as_fill_carrier()
    })

    ## update experiment tab
    observeEvent(
      input$tabset,
      {
        data$load_screen(input$tabset)
      },
      priority = 1000
    )

    ## save name
    observeEvent(input$exp_name_save, {
      log_info(ns = ns, user_msg = "Saving experiment name")
      data$save_exp(name = input$exp_name)
    })

    ## save description
    observeEvent(input$exp_desc_save, {
      log_info(ns = ns, user_msg = "Saving experiment description")
      data$save_exp(description = input$exp_desc)
    })

    ## save notes
    observeEvent(input$exp_notes_save, {
      log_info(ns = ns, user_msg = "Saving experiment notes")
      data$save_exp(notes = input$exp_notes)
    })

    ## start recording
    observeEvent(input$start_recording, {
      data$start_exp_recording()
    })

    ## stop recording
    observeEvent(input$stop_recording, {
      data$stop_exp_recording()
    })

    ## archive
    observeEvent(input$archive, {
      data$archive_exp()
    })

    # experiment devices =========

    ## prep experiment devices for table
    get_experiment_devices_for_table <- reactive({
      req(data$has_exp_loaded())
      validate(need(
        data$get_exp_devices_info(),
        "No devices are linked to this experiment yet. Link a device."
      ))
      # safely call function
      out <- data$get_exp_devices_info() |>
        get_experiment_devices_for_table_in_app(
          timezone = get_timezone(),
          user_id = data$get_user_id()
        ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    ## setup experiment devices selector table
    experiment_devices <- module_selector_table_server(
      "experiment_devices",
      get_data = get_experiment_devices_for_table,
      id_column = "core_id",
      # make id column invisible
      columnDefs = list(
        list(visible = FALSE, targets = 0)
      ),
      selection = "single",
      auto_reselect = FALSE,
      paging = FALSE,
      dom = "ft",
      no_data_message = "No devices are linked to this experiment yet. Link a device."
    )

    ## refresh experiment devices
    observeEvent(input$refresh_experiment_devices, {
      data$refresh_exp_devices()
    })

    ## button availability
    observeEvent(
      experiment_devices$get_selected_ids(),
      {
        # load the experiment device
        data$load_exp_device(experiment_devices$get_selected_ids())

        # disable if nothing is loaded
        if (!data$has_exp_loaded()) {
          shinyjs::disable("link_devices")
          shinyjs::disable("unlink_device")
          shinyjs::disable("claim_device")
          shinyjs::disable("release_device")
          shinyjs::hide("device_label_div")
          return()
        }

        # link device
        shinyjs::toggleState(
          "link_devices",
          condition = !data$is_exp_recording()
        )

        # label input
        exp_device_selected <- !is_empty(experiment_devices$get_selected_ids())
        if (data$has_exp_devices_selected()) {
          updateTextInput(
            inputId = "device_label",
            value = data$get_selected_exp_device()$label
          )
          shinyjs::show("device_label_div")
        } else {
          shinyjs::hide("device_label_div")
        }

        # nothing selected or recording?
        if (!data$has_exp_devices_selected() || data$is_exp_recording()) {
          shinyjs::disable("unlink_device")
          shinyjs::disable("claim_device")
          shinyjs::disable("release_device")
          return()
        }
        shinyjs::enable("unlink_device") # always available whether device is claimed or unclaimed
        control_exp_id <- data$get_selected_exp_device()$control_exp_id
        shinyjs::toggleState("claim_device", condition = is.na(control_exp_id))
        shinyjs::toggleState(
          "release_device",
          condition = !is.na(control_exp_id) &&
            data$get_current_exp()$exp_id == control_exp_id
        )
      },
      ignoreNULL = FALSE
    )

    ## update label
    observeEvent(input$device_label_save, {
      data$save_exp_device_label(input$device_label)
    })

    ## claim device
    observeEvent(input$claim_device, {
      data$claim_device()
    })

    ## free device
    observeEvent(input$release_device, {
      data$release_device()
    })

    ## unlink device
    observeEvent(input$unlink_device, {
      data$unlink_device()
      experiment_devices$deselect_all()
    })

    # unlinked devices ===========

    ## prep unlinked devices for table
    get_unlinked_devices_for_table <- reactive({
      req(data$has_exp_loaded())
      req(data$get_unlinked_devices())
      # safely call function
      out <- data$get_unlinked_devices() |>
        get_unlinked_devices_for_table_in_app(
          timezone = get_timezone(),
          user_id = data$get_user_id()
        ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    ## setup unlinked devices selector table
    unlinked_devices <- module_selector_table_server(
      "unlinked_devices",
      get_data = get_unlinked_devices_for_table,
      id_column = "core_id",
      # make id column invisible
      columnDefs = list(
        list(visible = FALSE, targets = 0)
      ),
      auto_reselect = FALSE,
      paging = FALSE,
      dom = "ft",
      no_data_message = "There are no devices that are not already linked to this experiment."
    )

    ## unlinked evices modal
    unlinked_devices_modal <- modalDialog(
      title = h3("Link devices to experiment"),
      bslib::card(
        full_screen = TRUE,
        class = "border-0",
        min_height = 300,
        module_selector_table_ui(ns("unlinked_devices"))
      ),
      footer = tagList(
        actionButton(
          ns("modal_add_device"),
          "Link devices",
          icon = icon("link"),
          style = "border: 0;"
        ) |>
          add_tooltip(
            "Link the selected device(s) to this experiment and try to put the experiment in control of them."
          ) |>
          shinyjs::disabled(),
        modalButton("Close")
      ),
      easyClose = TRUE,
      size = "l"
    )

    ## trigger modal
    observeEvent(input$link_devices, {
      data$refresh_unlinked_devices()
      showModal(unlinked_devices_modal)
    })

    ## react to unlinked device selection
    selected_unlinked <- reactiveVal(c())
    observeEvent(
      unlinked_devices$get_selected_ids(),
      {
        # update button
        shinyjs::toggleState(
          "modal_add_device",
          condition = !is_empty(unlinked_devices$get_selected_ids())
        )
        updateActionButton(
          inputId = "modal_add_device",
          label = format_inline(
            "Link {length(unlinked_devices$get_selected_ids())} device{?s}"
          )
        )
        # warn about unclaimable devices
        newly_selected <- setdiff(
          unlinked_devices$get_selected_ids(),
          selected_unlinked()
        )
        if (!is_empty(newly_selected)) {
          device <- data$get_unlinked_devices() |>
            filter(core_id == newly_selected)
          if (!is.na(device$control_exp_id)) {
            log_warning(
              ns = ns,
              user_msg = sprintf("Cannot control %s", device$core_name),
              warning = "This device is currently in use by another experiment. You can link it to your experiment but will not be able to control it."
            )
          }
        }
        selected_unlinked(unlinked_devices$get_selected_ids())
      },
      ignoreNULL = FALSE
    )

    ## trigger save
    observeEvent(input$modal_add_device, {
      removeModal()
      data$link_and_claim_devices(unlinked_devices$get_selected_ids())
    })

    # device control  ========

    # show data structure and common actions and disable/enable common actions
    observeEvent(sdds$devices$get_selected_ids(), {
      device_selected <- !is_empty(sdds$devices$get_selected_ids())
      shinyjs::toggleState("zero", condition = device_selected)
      shinyjs::toggleState("start_stirrer", condition = device_selected)
      shinyjs::toggleState("stop_stirrer", condition = device_selected)
      shinyjs::toggleState(
        "change_stirrer",
        condition = device_selected
      )
      shinyjs::toggleState("save", condition = device_selected)
      if (device_selected) {
        bslib::accordion_panel_open(
          id = "device_control_accordion",
          values = "Common actions"
        )
        bslib::accordion_panel_open(
          id = "device_control_accordion",
          values = "Data structures"
        )
      }
    })

    ## custom actions
    observeEvent(
      input$zero,
      sdds$edit_structure("sensor.action", value = "zero")
    )
    observeEvent(
      input$start_stirrer,
      sdds$edit_structure("stirrer.action", value = "start")
    )
    observeEvent(
      input$stop_stirrer,
      sdds$edit_structure("stirrer.action", value = "stop")
    )
    observeEvent(
      input$change_stirrer,
      sdds$edit_structure("stirrer.setpoint_rpm")
    )
    observeEvent(input$save, {
      sdds$edit_structure("SYSTEM.action", value = "saveState")
    })

    # logs ========
    logs <- logs_plot_server("logs_plot", data)
  })
}
