#' Data Download Server
#' @param input,output,session standard Shiny module server arguments (supplied by [shiny::callModule()])
#' @param data_func reactive function providing the data
#' @param filename_func reactive function returning the default file name
dataDownloadServer <- function(
  input,
  output,
  session,
  data_func,
  filename_func
) {
  # namespace
  ns <- session$ns

  # save dialog
  save_dialog <- reactive({
    modalDialog(
      title = "Save data",
      fade = FALSE,
      easyClose = TRUE,
      size = "s",
      # Shiny clears the download button's own `disabled` state when it binds
      # the download link (renderValue removes the class), so we can't gate the
      # button directly. Instead we wrap it and block clicks on the wrapper via
      # a CSS class that Shiny never touches.
      tags$style(HTML(sprintf(
        "#%s.download-disabled {pointer-events: none; opacity: 0.65;}",
        ns("download_wrapper")
      ))),
      textInput(ns("save_name"), "Filename:", filename_func()),
      checkboxGroupInput(
        ns("format"),
        "Formats:",
        c(
          "R Data Storage (.rds)" = ".rds",
          "Excel (.xlsx)" = ".xlsx",
          "Comma Separated Values (.csv)" = ".csv"
        )
      ),
      footer = tagList(
        # starts disabled (via wrapper class); enabled once a format is checked
        tags$span(
          id = ns("download_wrapper"),
          class = "download-disabled",
          downloadButton(
            ns("download"),
            label = "Download",
            icon = icon("download")
          )
        ),
        modalButton("Close")
      )
    )
  })
  observeEvent(input$download_dialog, showModal(save_dialog()))
  # block the download until at least one format checkbox is selected;
  # ignoreNULL = FALSE so the button is re-disabled when all boxes are cleared
  observeEvent(
    input$format,
    shinyjs::toggleClass(
      "download_wrapper",
      "download-disabled",
      condition = length(input$format) == 0
    ),
    ignoreNULL = FALSE
  )

  # download handler
  output$download <- downloadHandler(
    filename = function() {
      isolate(stringr::str_replace(input$save_name, "(\\.zip)?$", ".zip"))
    },
    content = function(file) {
      save_name <- isolate(input$save_name)
      formats <- isolate(input$format)
      logs <- isolate(data_func())
      req(length(formats) > 0)
      log_debug(
        ns = ns,
        "saving data ",
        save_name,
        " (formats ",
        paste(formats, collapse = ", "),
        ")"
      )
      # write the individual format files into a temp directory and let
      # ml_write_logs_to_file() bundle them into the download zip (`file`)
      out <- try_catch_cnds({
        base <- basename(stringr::str_replace(save_name, "\\.zip$", ""))
        tmp_dir <- tempfile("ml_download_")
        dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        ml_write_logs_to_file(
          logs = logs,
          file_path = file.path(tmp_dir, paste0(base, formats)),
          zip = file
        )
      })
      out |> log_cnds(ns = ns)
    }
  )
}


#' Data Download Link
#' @param id module id
#' @param label Label for the download link
#' @param tooltip tooltip text shown on hover
dataDownloadLink <- function(
  id,
  label = "Save",
  tooltip = "Save the data in various formats"
) {
  ns <- NS(id)
  actionButton(ns("download_dialog"), label, icon = icon("save")) |>
    add_tooltip(tooltip)
}
