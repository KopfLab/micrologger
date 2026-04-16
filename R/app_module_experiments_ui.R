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
    actionButton(
      ns("refresh_experiment_devices"),
      "Refresh",
      icon = icon("arrows-rotate"),
      class = "btn-link"
    ) |>
      add_tooltip(
        "Fetch the latest updates on the linked devices from the cloud."
      ),
    spaces(2),
    if (!exp$archived) {
      actionButton(
        ns("link_device"),
        "Link device",
        icon = icon("plus"),
        class = "btn-link"
      ) |>
        shinyjs::disabled() |>
        add_tooltip(
          "Link a(nother) device to this experiment. You cannot do this while the experiment is recording."
        )
    },
    if (!exp$archived) {
      spaces(2)
    },
    actionButton(
      ns("change_label"),
      "Change label",
      icon = icon("pen-to-square"),
      class = "btn-link"
    ) |>
      shinyjs::disabled() |>
      add_tooltip(
        "Change the label of the device for data display purposes (does not affect data collection in any way and can be changed at any time)."
      ),
    spaces(2),
    if (!exp$archived) {
      actionButton(
        ns("claim_device"),
        "Claim device",
        icon = icon("flag"),
        class = "btn-link"
      ) |>
        shinyjs::disabled() |>
        add_tooltip(
          "Claim this device for your experiment (others will not be able to control it). You cannot do this while the experiment is recording."
        )
    },
    if (!exp$archived) {
      spaces(2)
    },
    if (!exp$archived) {
      actionButton(
        ns("release_device"),
        "Release device",
        icon = icon("dove"),
        class = "btn-link"
      ) |>
        shinyjs::disabled() |>
        add_tooltip(
          "Make this device available for others to use. You cannot do this while the experiment is recording."
        )
    },
    module_selector_table_ui(ns("experiment_devices")),
    div_input_with_save(
      id = ns("device_label_div"),
      textInput(
        ns("device_label"),
        NULL,
        placeholder = "Give this device a custom label for data visualization purposes"
      ),
      actionLink(ns("device_label_save"), "Save", icon = icon("floppy-disk"))
    ) |>
      shinyjs::hidden(),

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
    title = "Device Control",
    br(),
    sddsParticle::sdds_ui("sdds", device_list_title = "Devices")
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
