# get_ function are all reactive
logs_plot_server <- function(
  id,
  data
) {
  # actual module server
  moduleServer(id, function(input, output, session) {
    # namespace
    ns <- session$ns

    # reactive values  ====
    zoom_factor <- 2 # zoom in and out factor with each click
    zoom_move <- 0.5 # sideways move interval
    values <- reactiveValues(
      valid_fetch = FALSE,
      valid_plot = FALSE,
      selected_traces = c(),
      refresh_data_plot = NULL,
      zoom_stack = list(list(zoom = NULL, x_min = NULL, x_max = NULL))
    )

    # DATA ===========

    ## fetch data ====
    observeEvent(input$fetch_data, {
      values$valid_fetch <- TRUE
      data$refresh_logs()
      data$get_logs()
      shinyjs::show("traces_box")
      shinyjs::show("groups_box")
      shinyjs::show("options_box")

      # refresh existing plot
      if (values$valid_plot) {
        refresh_plot()
      }
    })

    ## reset cache ====

    observeEvent(input$reset_cache, {
      values$valid_fetch <- FALSE
      ml_clear_logs_cache()
      log_success(ns = ns, user_msg = "Cache cleared.")
    })

    ## devices ============

    ## get devices
    get_log_devices_for_table <- reactive({
      req(data$has_exp_loaded())
      req(data$get_logs())
      out <- data$get_logs() |>
        count(device, label) |>
        select(
          "Device" = "device",
          "Label" = "label",
          "Records" = "n"
        ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    ## devices selector table
    log_devices <- module_selector_table_server(
      "log_devices",
      get_data = get_log_devices_for_table,
      id_column = "Device",
      selection = "multiple",
      # view & scrolling
      allow_view_all = TRUE,
      initial_page_length = -1,
      dom = "t"
    )

    ## select all by default when we first load
    observeEvent(log_devices$is_table_reloaded(), {
      if (is_empty(log_devices$get_selected_ids())) {
        log_devices$select_all()
      }
    })

    ## traces ==========

    ## get traces
    get_log_traces_for_table <- reactive({
      req(data$has_exp_loaded())
      req(data$get_logs())
      out <- data$get_logs() |>
        ml_summarize_logs() |>
        select(
          "path",
          "category" = "parent",
          "Variable" = "var_w_units",
          "Records" = "n"
        ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    ## traces selector table
    log_traces <- module_selector_table_server(
      "log_traces",
      get_data = get_log_traces_for_table,
      id_column = "path",
      # row grouping
      extensions = "RowGroup",
      rowGroup = list(dataSrc = 1),
      # make path columns invisible
      columnDefs = list(
        list(visible = FALSE, targets = 0:1)
      ),
      selection = "multiple",
      # view & scrolling
      allow_view_all = TRUE,
      initial_page_length = -1,
      scrollY = "200px",
      dom = "ft"
    )

    # PLOT ===========

    ## reset =============

    observe({
      data$has_exp_loaded()
      data$get_current_exp()
      log_debug(ns = ns, "resetting plot")
      isolate({
        values$valid_fetch <- FALSE
        values$valid_plot <- FALSE
        values$zoom_stack <- list(list(zoom = NULL, x_min = NULL, x_max = NULL))
        refresh_plot()
      })
    })

    ## reset zoom stack
    observeEvent(input$time_axis, {
      values$zoom_stack <- list(list(zoom = NULL, x_min = NULL, x_max = NULL))
    })

    ## zoom buttons
    observeEvent(values$valid_plot, {
      req(data$has_exp_loaded())
      shinyjs::toggleState("zoom_all", condition = values$valid_plot)
      shinyjs::toggleState("zoom_move_left", condition = values$valid_plot)
      shinyjs::toggleState("zoom_move_right", condition = values$valid_plot)
      shinyjs::toggleState("zoom_back", condition = values$valid_plot)
      shinyjs::toggleState(
        "plot_download-download_dialog",
        condition = values$valid_plot
      )
      shinyjs::toggleState(
        "data_download-download_dialog",
        condition = values$valid_plot
      )
    })

    ## refresh ====

    refresh_plot <- function() {
      if (is.null(values$refresh_data_plot)) {
        values$refresh_data_plot <- 1
      } else {
        values$refresh_data_plot <- values$refresh_data_plot + 1
      }
    }

    observeEvent(input$plot_refresh, refresh_plot())
    observeEvent(input$traces_refresh, refresh_plot())
    observeEvent(input$groups_refresh, refresh_plot())
    observeEvent(input$options_refresh, refresh_plot())

    ## generate ========

    generate_plot <- eventReactive(values$refresh_data_plot, {
      req(data$has_exp_loaded())
      log_debug(ns = ns, "generating plot")
      this_call <- current_call()
      isolate({
        logs <- data$get_logs()
        color_aes <- input$color_aes
        # empty plot?
        if (
          is.null(logs) ||
            nrow(logs) == 0 ||
            is_empty(log_devices$get_selected_ids()) ||
            is_empty(log_traces$get_selected_ids())
        ) {
          values$valid_plot <- FALSE
          return(get_empty_plot_in_app(font_size = input$font_size))
        }
        # zoom
        timescale_lim <- NULL
        if (
          !is.null(get_last_zoom()$x_min) && !is.null(get_last_zoom()$x_max)
        ) {
          timescale_lim <- c(get_last_zoom()$x_min, get_last_zoom()$x_max)
          if (input$time_axis == "datetime") {
            timescale_lim <- as.POSIXct(timescale_lim)
          }
        }

        # generate plot
        out <-
          logs |>
          get_logs_plot_in_app(
            devices = log_devices$get_selected_ids(),
            filter_paths = log_traces$get_selected_ids(),
            timescale = input$time_axis,
            timescale_lim = timescale_lim,
            color = if (is.na(color_aes) || color_aes == "NA") {
              NULL
            } else {
              !!sym(color_aes)
            },
            show_points = input$show_points,
            show_error_range = input$show_errors,
            text_size = input$font_size,
            legend_position = input$legend_position
          ) |>
          try_catch_cnds()
        # report errors
        out |> log_cnds(ns = ns)
        values$valid_plot <- !is.null(out$result)
        return(out$result)
      })
    })

    output$data_plot <- renderPlot(
      generate_plot(),
      height = eventReactive(values$refresh_data_plot, input$plot_height)
    )

    ## zoom ========

    # add to zoom stack
    add_to_zoom_stack <- function(
      zoom,
      x_min,
      x_max,
      update = TRUE,
      only_add_if_new = TRUE
    ) {
      if (missing(zoom)) {
        zoom <- get_last_zoom()$zoom
      }
      if (missing(x_min)) {
        x_min <- get_last_zoom()$x_min
      }
      if (missing(x_max)) {
        x_max <- get_last_zoom()$x_max
      }
      new_zoom <- list(zoom = zoom, x_min = x_min, x_max = x_max)
      if (only_add_if_new && identical(get_last_zoom(), new_zoom)) {
        return(NULL)
      }
      log_debug(
        ns = ns,
        "adding to zoom stack: ",
        zoom,
        " time: ",
        x_min,
        " to ",
        x_max
      )
      values$zoom_stack <- c(values$zoom_stack, list(new_zoom))
      if (update) refresh_plot()
    }

    # load last zoom
    load_last_zoom <- function(update = TRUE) {
      last_element <- length(values$zoom_stack)
      if (last_element > 1) {
        values$zoom_stack[last_element] <- NULL
      }
      if (update) refresh_plot()
    }

    # get current zoom
    get_last_zoom <- function() {
      values$zoom_stack[[length(values$zoom_stack)]]
    }

    # zoom back
    observeEvent(input$zoom_back, load_last_zoom())
    observeEvent(input$data_plot_dblclick, load_last_zoom())
    # zoom whole data set
    observeEvent(input$zoom_all, {
      add_to_zoom_stack(zoom = NULL, x_min = NULL, x_max = NULL)
    })

    # # zoom fit
    # observeEvent(input$zoom_fit, {
    #   add_to_zoom_stack(zoom = NULL)
    # })
    # # zoom in
    # observeEvent(input$zoom_in, {
    #   if (is.null(get_last_zoom()$zoom)) add_to_zoom_stack(zoom = zoom_factor)
    #   else add_to_zoom_stack(zoom = get_last_zoom()$zoom * zoom_factor)
    # })
    # # zoom out
    # observeEvent(input$zoom_out, {
    #   if (is.null(get_last_zoom()$zoom)) add_to_zoom_stack(zoom = 1/zoom_factor)
    #   else add_to_zoom_stack(zoom = get_last_zoom()$zoom/zoom_factor)
    # })

    # time zoom
    observeEvent(input$data_plot_brush, {
      brush <- input$data_plot_brush
      if (!is.null(brush$xmin) && !is.null(brush$xmax)) {
        # convert to seconds
        add_to_zoom_stack(x_min = brush$xmin, x_max = brush$xmax)
      }
    })
    # left right movening
    move_zoom <- function(direction) {
      if (!is.null(get_last_zoom()$x_min) && !is.null(get_last_zoom()$x_max)) {
        add_to_zoom_stack(
          x_min = get_last_zoom()$x_min +
            direction *
              zoom_move *
              (get_last_zoom()$x_max - get_last_zoom()$x_min),
          x_max = get_last_zoom()$x_max +
            direction *
              zoom_move *
              (get_last_zoom()$x_max - get_last_zoom()$x_min)
        )
      }
    }
    observeEvent(input$zoom_move_left, move_zoom(-1))
    observeEvent(input$zoom_move_right, move_zoom(+1))

    # DOWNLOADS ============

    ## plot download ====
    download_handler <- callModule(
      plotDownloadServer,
      "plot_download",
      plot_func = generate_plot,
      filename_func = reactive({
        paste0(format(
          lubridate::now(),
          '%Y-%m-%d_%H-%M-%S_micrologger_plot.pdf'
        ))
      })
    )

    ## data download ====

    get_logs_for_download <- reactive({
      out <- data$get_logs() |>
        get_logs_for_download_in_app(
          devices = log_devices$get_selected_ids(),
          filter_paths = log_traces$get_selected_ids()
        ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    data_handler <- callModule(
      dataDownloadServer,
      "data_download",
      data_func = get_logs_for_download,
      filename_func = reactive({
        paste0(format(
          lubridate::now(),
          '%Y-%m-%d_%H-%M-%S_micrologger_data.zip'
        ))
      })
    )

    # SUMMARY (not implemented yet) ============
    # switch to bslib package

    # observeEvent(values$valid_plot, {
    #   toggle("summary_box", condition = values$valid_plot)
    #   toggle("data_box", condition = values$valid_plot)
    # })

    # # generate data table & summary =====

    # generate_data_summary <- eventReactive(values$refresh_data_plot, {
    #   logs <- get_plot_data_logs()
    #   if (nrow(logs) > 0) {
    #     logs <- logs |>
    #       ll_summarize_data_logs(
    #         slope_denom_units = "day",
    #         exclude_outliers = !input$show_outliers
    #       )
    #   }
    #   return(logs)
    # })

    # generate_data_table <- eventReactive(values$refresh_data_plot, {
    #   logs <- get_plot_data_logs() |>
    #     select(
    #       datetime,
    #       exp_id,
    #       device_name,
    #       data_key,
    #       data_units,
    #       data_value,
    #       data_sd,
    #       data_n
    #     ) |>
    #     mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))
    #   return(logs)
    # })

    # # summary table output ====

    # output$summary_table <- renderTable(
    #   {
    #     req(!is.null(input$digits) && is.numeric(input$digits))
    #     summary <- generate_data_summary()
    #     module_message(ns, "debug", "rendering plot data summary table")
    #     if (nrow(summary) > 0) {
    #       summary
    #     } else {
    #       tibble(` ` = "No data.")
    #     }
    #   },
    #   striped = TRUE,
    #   spacing = 'xs',
    #   width = '100%',
    #   align = NULL,
    #   digits = reactive(input$digits)
    # )

    # # data table output =====

    # output$data_table <- DT::renderDataTable({
    #   DT::datatable(
    #     generate_data_table(),
    #     options = list(orderClasses = TRUE, order = list(1, "desc")),
    #     filter = "bottom"
    #   )
    # })
  })
}


logs_plot_ui <- function(id, plot_height = 800) {
  ns <- NS(id)

  tagList(
    # plot box ------
    shinydashboard::box(
      title = "Data Plot",
      width = 8,
      status = "info",
      solidHeader = TRUE,
      collapsible = TRUE,
      div(
        style = paste0("min-height: ", plot_height, "px;"),
        div(
          id = ns("data_plot_actions"),
          fluidRow(
            column(
              width = 4,
              actionButton(
                ns("fetch_data"),
                NULL,
                icon = icon("cloud-download-alt")
              ) |>
                add_tooltip("Fetch the most recent data from the data base."),
              spaces(1),
              actionButton(
                ns("reset_cache"),
                NULL,
                icon = icon("unlink"),
              ) |>
                add_tooltip(
                  "Reset local cache (only necessary if experiment configuration changed)."
                )
            ),
            column(
              width = 4,
              align = "center",
              actionButton(
                ns("zoom_all"),
                "",
                icon = icon("resize-full", lib = "glyphicon")
              ) |>
                add_tooltip("Show all data") |>
                shinyjs::disabled(),
              actionButton(
                ns("zoom_move_left"),
                "",
                icon = icon("arrow-left")
              ) |>
                add_tooltip("Move back in time") |>
                shinyjs::disabled(),
              actionButton(
                ns("zoom_move_right"),
                "",
                icon = icon("arrow-right")
              ) |>
                add_tooltip("Move forward in time") |>
                shinyjs::disabled(),
              actionButton(
                ns("zoom_back"),
                "",
                icon = icon("rotate-left", verify_fa = FALSE)
              ) |>
                add_tooltip("Revert to previous view") |>
                shinyjs::disabled()
            ),
            column(
              width = 4,
              align = "right",
              actionButton(
                ns("plot_refresh"),
                "(Re)plot",
                icon = icon("sync")
              ) |>
                add_tooltip(
                  "Refresh the plot with the selected filters and plot options."
                ),
              spaces(1),
              plotDownloadLink(ns("plot_download"), label = NULL) |>
                shinyjs::disabled(),
              spaces(1),
              dataDownloadLink(ns("data_download"), label = NULL) |>
                shinyjs::disabled()
            )
          )
        ),
        div(
          id = ns("data_plot_messages"),
          h3(htmlOutput(ns("data_plot_message")))
        ),
        div(
          id = ns("data_plot_div"),
          plotOutput(
            ns("data_plot"),
            height = "100%",
            dblclick = ns("data_plot_dblclick"),
            brush = brushOpts(
              id = ns("data_plot_brush"),
              delayType = "debounce",
              direction = "x",
              resetOnNew = TRUE
            )
          ) |>
            shinycssloaders::withSpinner(
              type = 5,
              proxy.height = paste0(plot_height - 50, "px")
            )
        )
      )
    ),

    # devices ----
    div(
      id = ns("devices_box"),
      shinydashboard::box(
        title = "Select Devices",
        width = 4,
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        module_selector_table_ui(ns("log_devices"))
      )
    ),

    # variables box ----
    div(
      id = ns("traces_box"),
      shinydashboard::box(
        title = "Select data",
        width = 4,
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        module_selector_table_ui(ns("log_traces"))
        # footer = div(
        #   actionButton(
        #     ns("traces_refresh"),
        #     label = "Re-plot",
        #     icon = icon("sync")
        #   ) |>
        #     add_tooltip("Refresh plot with new data trace selection.")
        # )
      )
    ),

    # options box -----
    div(
      id = ns("options_box"),
      shinydashboard::box(
        title = "Plot Options",
        width = 4,
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        fluidRow(
          h4("Errors:") |> column(width = 4),
          checkboxInput(ns("show_errors"), NULL, value = FALSE) |>
            column(width = 2),
          h4("Show points:") |> column(width = 4),
          checkboxInput(ns("show_points"), NULL, value = FALSE) |>
            column(width = 2)
        ),
        # fluidRow(
        #   h4("Outliers:") |> column(width = 4),
        #   checkboxInput(ns("show_outliers"), NULL, value = TRUE) |>
        #     column(width = 2),
        #   h4("Overlay Exps:") |> column(width = 4),
        #   checkboxInput(ns("overlay_exps"), NULL, value = FALSE) |>
        #     column(width = 2)
        # ),
        fluidRow(
          h4("Time axis:") |> column(width = 4),
          radioButtons(
            ns("time_axis"),
            NULL,
            choices = c("date & time" = "datetime", "duration" = "duration"),
            selected = "datetime",
            inline = TRUE
          ) |>
            column(width = 8)
        ),
        fluidRow(
          h4("Color:") |> column(width = 4),
          selectInput(
            ns("color_aes"),
            NULL,
            choices = c(
              "None" = NA_character_,
              "Device" = "device",
              "Label" = "label"
            ),
            selected = "label"
          ) |>
            column(width = 8)
        ),
        # implement linetype and panel (for multi experiment views)
        #  fluidRow(
        #   h4("Linetype:") |> column(width = 4),
        #   selectInput(
        #     ns("linetype_aes"),
        #     NULL,
        #     choices = c("None" = NA_character_, "Device" = "device", "Label" = "label"),
        #     selected = NA_character_
        #   ) |>
        #     column(width = 8)
        # ),
        fluidRow(
          h4("Plot height:") |> column(width = 4),
          numericInput(
            ns("plot_height"),
            NULL,
            value = plot_height,
            min = 100,
            step = 50
          ) |>
            column(width = 8)
        ),
        fluidRow(
          h4("Legend:") |> column(width = 4),
          selectInput(
            ns("legend_position"),
            NULL,
            choices = c("right", "bottom", "hide"),
            selected = "right"
          ) |>
            column(width = 8)
        ),
        fluidRow(
          h4("Font Size:") |> column(width = 4),
          numericInput(ns("font_size"), NULL, value = 18, min = 6, step = 1) |>
            column(width = 8)
        )
        # footer = actionButton(
        #   ns("options_refresh"),
        #   label = "Re-plot",
        #   icon = icon("sync")
        # ) |>
        #   add_tooltip("Refresh plot with new plot settings.") |>
        #   shinyjs::disabled()
      )
    ),

    # summary box -----
    div(
      id = ns("summary_box"),
      shinydashboard::box(
        title = "Summary of Plotted Data",
        width = 12,
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        numericInput(
          ns("digits"),
          label = NULL,
          value = 2,
          step = 1
        ) |>
          add_tooltip("Enter number of digits to display."),
        tableOutput(ns("summary_table"))
      )
    ) |>
      shinyjs::hidden(),

    # data box ----

    div(
      id = ns("data_box"),
      shinydashboard::box(
        title = "All Plotted Data",
        width = 12,
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        DT::dataTableOutput(ns("data_table"))
      )
    ) |>
      shinyjs::hidden()
  )
}
