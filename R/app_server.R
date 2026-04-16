ml_server <- function(
  token,
  user_id,
  user_group,
  user_first_name,
  user_last_name,
  user_is_admin
) {
  # server function
  function(input, output, session) {
    # accessible core ids
    experiment_core_ids <- reactiveVal(value = character())

    # particle module

    # TODO: make it possible to pass additional modules for the value editing:
    # example in get_structures() in app_module_sdds for Ohm should be coming from here
    # (both converter function and rendeirng module)
    particle <- sddsParticle::sdds_server(
      "sdds",
      token,
      timezone = reactive(input$timezone),
      accessible_core_ids = experiment_core_ids
    )

    # data module
    data <- data_server(
      "data",
      particle = particle,
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
          input$group
        } else {
          user_group
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
      particle = particle,
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
