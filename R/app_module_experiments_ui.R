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
  owner,
  archived,
  recording,
  exp_name,
  exp_desc,
  exp_notes
) {
  # configuration =====
  tabPanel(
    value = "configuration",
    title = "Configuration",

    h4("Owner:", owner),
    h4(
      "Status:",
      if (archived) {
        "finished"
      } else if (recording) {
        "recording"
      } else {
        "not recording"
      }
    ),
    if (!archived && !recording) {
      actionButton(
        ns("start_recording"),
        label = "Start Recording",
        icon = icon("play"),
        style = "color: #fff; background-color: #007f1f; border-color: #2e6da4"
      )
    } else if (!archived && recording) {
      actionButton(
        ns("stop_recording"),
        label = "Stop Recording",
        icon = icon("stop"),
        style = "color: #fff; background-color: #f22e10; border-color: #2e6da4"
      )
    },
    if (!archived) spaces(1),
    if (!archived) {
      actionButton(
        ns("finish"),
        label = "Finish Experiment",
        icon = icon("check-double")
      ) |>
        add_tooltip(
          "Finish the experiment. Frees up the devices used by this experiment. You will not be able to record more data with this experiment."
        )
    },
    h4("Experiment:"),
    textInput(
      ns("exp_name"),
      NULL,
      value = if (!is.na(exp_name)) exp_name else "",
      placeholder = "Name your experiment"
    ),
    h4("Devices:"),
    actionButton(
      ns("add_experiment_devices"),
      "Link devices",
      icon = icon("plus"),
      style = "border: 0;"
    ) |>
      add_tooltip(
        "Link devices to this experiment."
      ),
    spaces(1),
    actionButton(
      ns("refresh_experiment_devices"),
      "Refresh",
      icon = icon("arrows-rotate"),
      style = "border: 0;"
    ),
    module_selector_table_ui(ns("experiment_devices")),
    h4("Description:"),
    textAreaInput(
      ns("exp_desc"),
      NULL,
      value = if (!is.na(exp_desc)) exp_desc else "",
      placeholder = "Add a succinct description for your experiment",
      cols = 50,
      rows = 5,
      resize = "none"
    ),
    h4("Notes:"),
    textAreaInput(
      ns("exp_notes"),
      NULL,
      value = if (!is.na(exp_notes)) exp_notes else "",
      placeholder = "Keep notes about your experiment",
      width = "100%",
      rows = 10,
      resize = "both"
    ),
    actionButton(
      ns("save_info"),
      label = "Save",
      icon = icon("save")
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
