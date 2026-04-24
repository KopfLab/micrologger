ml_server <- function(
  token,
  user_id,
  user_first_name,
  user_last_name,
  user_is_admin,
  groups
) {
  # server function
  function(input, output, session) {
    # accessible core ids
    experiment_core_ids <- reactiveVal(value = character())

    # theme
    current_theme <- reactiveVal(NA_character_) # start with NA so serverside theme application make sure it loads in correct order
    observe({
      req(input$theme)
      isolate({
        if (!identical(input$theme, isolate(current_theme()))) {
          current_theme(input$theme)
          log_info(user_msg = paste("Loading theme", input$theme))
          session$setCurrentTheme(bslib::bs_theme(
            preset = input$theme,
            version = 5,
            "navbar-brand-font-size" = "1.5rem"
          ))
        }
      })
    })

    # group
    output$group_name <- renderUI({
      req(input$group)
      filter(groups, group_id == input$group)$group_desc
    })

    # particle module

    # TODO: make it possible to pass additional modules for the value editing:
    # example in get_structures() in app_module_sdds for Ohm should be coming from here
    # (both converter function and rendeirng module)
    sdds <- sddsParticle::sdds_server(
      "sdds",
      token,
      timezone = reactive(input$timezone),
      accessible_core_ids = experiment_core_ids
    )

    # data module
    data <- data_server(
      "data",
      token,
      experiment_core_ids = experiment_core_ids,
      get_timezone = reactive(input$timezone),
      get_user_id = reactive({
        user_id
      }),
      get_user_first_name = reactive({
        user_first_name
      }),
      get_user_last_name = reactive({
        user_last_name
      }),
      get_group = reactive({
        if (!is.null(input$group)) {
          # safety check
          filter(groups, group_id == input$group)$group_id
        } else {
          groups$group_id[1]
        }
      }),
      is_admin = reactive({
        user_is_admin
      })
    )

    # experiments module
    experiments <- experiments_server(
      "experiments",
      data = data,
      sdds = sdds,
      get_timezone = reactive(input$timezone)
    )

    # dev mode
    observeEvent(input$dev_mode_toggle, {
      if (shiny::in_devmode()) {
        shiny::devmode(FALSE)
      } else {
        shiny::devmode(TRUE)
      }
    })
  }
}
