#' Plot Download Server
#' @param plot_func reactive function generating the plot
#' @param filename_func reactive function returning the default plot name
plotDownloadServer <- function(
  input,
  output,
  session,
  plot_func,
  filename_func
) {
  # namespace
  ns <- session$ns

  # save dialog
  save_dialog <- reactive({
    modalDialog(
      title = "Save plot",
      fade = FALSE,
      easyClose = TRUE,
      size = "s",
      textInput(ns("save_name"), "Filename:", filename_func()),
      numericInput(ns("save_width"), "Width [inches]:", 12),
      numericInput(ns("save_height"), "Height [inches]:", 8),

      footer = tagList(
        downloadButton(
          ns("download"),
          label = "Download",
          icon = icon("download")
        ),
        modalButton("Close")
      )
    )
  })
  observeEvent(input$download_dialog, showModal(save_dialog()))

  # download handler
  output$download <- downloadHandler(
    filename = function() {
      isolate(input$save_name)
    },
    content = function(filename) {
      log_debug(
        ns = ns,
        "saving plot ",
        input$save_name,
        " (",
        input$save_width,
        " by ",
        input$save_height,
        ")"
      )
      ggsave(
        file = filename,
        plot = plot_func(),
        width = isolate(input$save_width),
        height = isolate(input$save_height),
        device = "pdf"
      )
    }
  )
}


#' Plot Download Link
#' @param label Label for the download link
plotDownloadLink <- function(
  id,
  label = "Save",
  tooltip = "Save the plot as a PDF"
) {
  ns <- NS(id)
  actionButton(ns("download_dialog"), label, icon = icon("file-pdf")) |>
    add_tooltip(tooltip)
}
