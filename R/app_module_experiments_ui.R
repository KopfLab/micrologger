#' experiments GUI module
#'
#' @param id module id
#' @export
experiments_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Experiment list ===========
    bslib::accordion(
      id = ns("accordion"),
      multiple = TRUE,
      bslib::accordion_panel(
        "Experiments",
        icon = icon("flask"),
        bslib::card(
          full_screen = TRUE,
          max_height = 320,
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              position = "left",
              width = "160",
              ml_help_link(
                "https://github.com/KopfLab/micrologger/wiki/Experiments",
                label = "Help",
                tooltip = "Open the experiments documentation",
                class = "btn btn-default",
                style = "border: 0;"
              ),
              actionButton(
                ns("add_experiment"),
                "Add new",
                icon = icon("plus"),
                style = "border: 0;"
              ) |>
                add_tooltip("Create new experiment."),
              actionButton(
                ns("refresh_experiments"),
                "Refresh",
                icon = icon("arrows-rotate"),
                style = "border: 0;"
              ) |>
                add_tooltip("Refresh experiments list."),
              actionButton(
                ns("hide_experiments"),
                "Hide",
                icon = icon("eye-slash"),
                style = "border: 0;"
              ) |>
                add_tooltip("Collapse the experiments list.")
            ),
            module_selector_table_ui(ns("experiments"))
          ),
          bslib::card_footer(
            "Select the experiment you want to work with or add a new experiment."
          )
        )
      )
    ),
    # Experiment details =======
    uiOutput(ns("experiment_tabset")) |>
      bslib::as_fill_carrier() |>
      div(
        id = ns("experiment_box"),
        class = "center-pills"
      ) |>
      bslib::as_fill_carrier() |>
      shinyjs::hidden()
  )
}

generate_experiment_configuration_ui <- function(
  ns,
  exp
) {
  # configuration =====
  bslib::nav_panel(
    value = "configuration",
    title = "Configuration",
    bslib::layout_sidebar(
      padding = 0,
      # DATA SELECTION =====
      sidebar = bslib::sidebar(
        position = "left",
        width = "160",
        fillable = TRUE,
        h5("Owner:", tags$br(), exp$owner, align = "center"),
        h5(
          "Status:",
          tags$br(),
          align = "center",
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
        if (!exp$archived && !exp$recording) {
          actionButton(
            ns("delete_experiment"),
            label = "Delete experiment",
            icon = icon("trash"),
            class = "btn-outline-danger"
          ) |>
            add_tooltip(
              "Permanently delete this experiment and all of its data."
            )
        }
      ),
      bslib::card_body(
        h4("Experiment:"),
        div_input_with_save(
          textInput(
            ns("exp_name"),
            NULL,
            value = if (!is.na(exp$name)) exp$name else "",
            placeholder = "Name your experiment"
          ),
          actionButton(ns("exp_name_save"), "Save", icon = icon("floppy-disk"))
        ),
        h4("Devices:"),
        if (!exp$archived && exp$recording) {
          "Pause recording to add/remove linked devices."
        },
        bslib::card(
          padding = 0,
          min_height = 320,
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              position = "right",
              width = "200",
              actionButton(
                ns("refresh_experiment_devices"),
                "Refresh",
                icon = icon("arrows-rotate")
              ) |>
                add_tooltip(
                  "Fetch the latest updates on the linked devices from the cloud."
                ),
              if (!exp$archived && !exp$recording) {
                actionButton(
                  ns("link_devices"),
                  "Link devices",
                  icon = icon("plus")
                ) |>
                  add_tooltip(
                    "Link additional device(s) to this experiment. You cannot do this while the experiment is recording."
                  )
              },
              if (!exp$archived && !exp$recording) {
                actionButton(
                  ns("unlink_device"),
                  "Unlink device",
                  icon = icon("link-slash")
                ) |>
                  shinyjs::disabled() |>
                  add_tooltip(
                    "Unlink the selected device from this experiment. You cannot do this while the experiment is recording."
                  )
              },
              if (!exp$archived && !exp$recording) {
                actionButton(
                  ns("claim_device"),
                  "Claim device",
                  icon = icon("flag")
                ) |>
                  shinyjs::disabled() |>
                  add_tooltip(
                    "Claim this device for your experiment (others will not be able to control it). You cannot do this while the experiment is recording."
                  )
              },
              if (!exp$archived && !exp$recording) {
                actionButton(
                  ns("release_device"),
                  "Release device",
                  icon = icon("dove")
                ) |>
                  shinyjs::disabled() |>
                  add_tooltip(
                    "Make this device available for others to use. You cannot do this while the experiment is recording."
                  )
              }
            ),
            module_selector_table_ui(ns("experiment_devices"))
          )
        ),

        div_input_with_save(
          id = ns("device_label_div"),
          textInput(
            ns("device_label"),
            NULL,
            placeholder = "Give this device a custom label for data visualization purposes"
          ),
          actionButton(
            ns("device_label_save"),
            "Save",
            icon = icon("floppy-disk")
          )
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
          actionButton(ns("exp_desc_save"), "Save", icon = icon("floppy-disk"))
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
          actionButton(ns("exp_notes_save"), "Save", icon = icon("floppy-disk"))
        )
      )
    )
  )
}

generate_device_control_ui <- function(ns) {
  bslib::nav_panel(
    value = "device_ctrl",
    title = "Device Control",
    # devices selection in the left sidebar, data structures as main content
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = "550",
        open = "open",
        # fillable so the devices card (and its table) fill the sidebar height
        fillable = TRUE,
        sddsParticle::sdds_ui_devices_card("sdds")
      ),
      sddsParticle::sdds_ui_structures_card(
        "sdds",
        quick_actions = ml_quick_actions()
      )
    )
  )
}

# the micrologger-specific quick actions shown in the "Quick actions" popover of
# the data structures card; pressing a button calls the sdds module's
# edit_structure(path, value) (the same list is passed to sddsParticle::sdds_server())
ml_quick_actions <- function() {
  list(
    sddsParticle::sdds_ui_quick_action(
      "zero",
      "Zero OD reader",
      icon = icon("0"),
      path = "sensor.action",
      value = "zero"
    ),
    sddsParticle::sdds_ui_quick_action(
      "change_read_interval",
      "Change read interval",
      icon = icon("vial-circle-check"),
      path = "sensor.reading.readInterval_ms"
    ),
    sddsParticle::sdds_ui_quick_action(
      "change_publish_interval",
      "Change publish interval",
      icon = icon("upload"),
      path = "SYSTEM.publishing.globalInterval_ms"
    ),
    sddsParticle::sdds_ui_quick_action_group(
      "stirring",
      "Set Stirring",
      icon = icon("spiral"),
      actions = list(
        sddsParticle::sdds_ui_quick_action(
          "start_stirrer",
          "Start stirring",
          icon = icon("play"),
          path = "stirrer.action",
          value = "start"
        ),
        sddsParticle::sdds_ui_quick_action(
          "stop_stirrer",
          "Stop stirring",
          icon = icon("stop"),
          path = "stirrer.action",
          value = "stop"
        ),
        sddsParticle::sdds_ui_quick_action(
          "change_stirrer",
          "Change speed",
          icon = icon("gauge"),
          path = "stirrer.setpoint_rpm"
        ),
        sddsParticle::sdds_ui_quick_action(
          "stir_through_od",
          "Stir through OD read",
          icon = icon("forward"),
          path = "sensor.reading.stopStirrer",
          value = "no"
        ),
        sddsParticle::sdds_ui_quick_action(
          "vortex_before_od",
          "Vortex before OD read",
          icon = icon("tornado"),
          path = "sensor.reading.vortex",
          value = "yes"
        )
      )
    ),
    sddsParticle::sdds_ui_quick_action_group(
      "illumination",
      "Set Illumination",
      icon = icon("lightbulb"),
      actions = list(
        sddsParticle::sdds_ui_quick_action(
          "set_intensity",
          "Set intensity",
          icon = icon("lightbulb"),
          path = "lights.intensity_percent"
        ),
        sddsParticle::sdds_ui_quick_action(
          "set_time_on",
          "Set time ON",
          icon = icon("play"),
          path = "lights.scheduleOn_sec"
        ),
        sddsParticle::sdds_ui_quick_action(
          "set_time_off",
          "Set time OFF",
          icon = icon("stop"),
          path = "lights.scheduleOff_sec"
        ),
        sddsParticle::sdds_ui_quick_action(
          "set_start_time",
          "Set start time",
          icon = icon("clock"),
          path = "lights.scheduleOnStart_HHMM"
        ),
        sddsParticle::sdds_ui_quick_action(
          "start_schedule",
          "Start schedule",
          icon = icon("hourglass-start"),
          path = "lights.action",
          value = "schedule"
        )
      )
    ),
    sddsParticle::sdds_ui_quick_action(
      "save",
      "Save state",
      icon = icon("floppy-disk"),
      path = "SYSTEM.action",
      value = "saveState"
    )
  )
}

# helpers =============

div_input_with_save <- function(...) {
  tags$div(class = "input-with-save", ...)
}
