#' experiments GUI module
#'
#' @param id module id
#' @export
experiments_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # experiments selection
    shinydashboard::box(
      title = span(
        "Experiments",
        div(
          style = "position: absolute; right: 50px; top: 5px;",
          actionButton(
            ns("add_experiment"),
            "Add new",
            icon = icon("plus"),
            style = "border: 0;"
          ) |>
            add_tooltip(
              "Create new experiment."
            ),
          actionButton(
            ns("refresh_experiments"),
            "Refresh",
            icon = icon("arrows-rotate"),
            style = "border: 0;"
          ) |>
            add_tooltip(
              "Refresh experiments list."
            )
        )
      ),
      width = 12,
      status = "info",
      solidHeader = TRUE,
      collapsible = TRUE,
      module_selector_table_ui(ns("experiments")),
      footer = tagList(
        uiOutput(ns("experiments_footer"))
      )
    ),
    # experiment details
    shinydashboard::box(
      title = "Experiment",
      width = 12,
      status = "info",
      solidHeader = TRUE,
      collapsible = TRUE,
      uiOutput(ns("experiment_tabset"))
    ) |>
      div(id = ns("experiment_box")) |>
      shinyjs::hidden()
  )
}


generate_experiment_configuration_ui <- function(
  ns,
  exp
) {
  # configuration =====
  tabPanel(
    value = "configuration",
    title = "Configuration",

    h4("Owner:", exp$owner),
    h4(
      "Status:",
      if (exp$archived) {
        "finished"
      } else if (exp$recording) {
        "recording"
      } else {
        "not recording"
      }
    ),
    if (!exp$archived && !exp$recording) {
      actionButton(
        ns("start_recording"),
        label = if (exp$current_segment > 0) {
          "Resume recording"
        } else {
          "Start recording"
        },
        icon = icon("play"),
        style = "color: #fff; background-color: #007f1f; border-color: #2e6da4"
      )
    } else if (!exp$archived && exp$recording) {
      actionButton(
        ns("stop_recording"),
        label = "Pause recording",
        icon = icon("stop"),
        style = "color: #fff; background-color: #f22e10; border-color: #2e6da4"
      )
    },
    if (!exp$archived) spaces(1),
    if (!exp$archived && !exp$recording && exp$current_segment > 0) {
      actionButton(
        ns("archive"),
        label = "Finish experiment",
        icon = icon("check-double")
      ) |>
        add_tooltip(
          "Finish the experiment. Frees up the devices used by this experiment. You will not be able to record more data with this experiment but will still have access to all the data."
        )
    },

    h4("Experiment:"),
    div_input_with_save(
      textInput(
        ns("exp_name"),
        NULL,
        value = if (!is.na(exp$name)) exp$name else "",
        placeholder = "Name your experiment"
      ),
      actionLink(ns("exp_name_save"), "Save", icon = icon("floppy-disk"))
    ),
    h4("Devices:"),
    # actionButton(
    #   ns("add_experiment_devices"),
    #   "Link devices",
    #   icon = icon("plus"),
    #   style = "border: 0;"
    # ) |>
    #   add_tooltip(
    #     "Link devices to this experiment."
    #   ),
    # spaces(1),
    actionButton(
      ns("refresh_experiment_devices"),
      "Refresh",
      icon = icon("arrows-rotate"),
      style = "border: 0;"
    ),
    module_selector_table_ui(ns("experiment_devices")),
    h4("Description:"),

    div_input_with_save(
      textAreaInput(
        ns("exp_desc"),
        NULL,
        value = if (!is.na(exp$description)) exp$description else "",
        placeholder = "Add a succinct description for your experiment",
        cols = 50,
        rows = 5,
        resize = "none"
      ),
      actionLink(ns("exp_desc_save"), "Save", icon = icon("floppy-disk"))
    ),
    h4("Notes:"),
    div_input_with_save(
      textAreaInput(
        ns("exp_notes"),
        NULL,
        value = if (!is.na(exp$notes)) exp$notes else "",
        placeholder = "Keep notes about your experiment",
        width = "100%",
        rows = 10,
        resize = "both"
      ),
      actionLink(ns("exp_notes_save"), "Save", icon = icon("floppy-disk"))
    )
  )
}

generate_experiment_device_control_ui <- function(ns) {
  tabPanel(
    value = "device_ctrl",
    title = "Device Control"
  )
}

generate_experiment_data_ui <- function(ns) {
  tabPanel(
    value = "data",
    "Data",
    br()
    #dataPlotUI(ns("exp_data_plot"))
  )
}
