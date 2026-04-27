ml_ui <- function(timezone, user, groups, default_theme) {
  # constants
  app_title <- "µLogger GUI"
  app_title_width <- 200
  app_color <- "green"
  spinner_color <- "#2c3b41"
  app_box_default <- "#2c3b41"

  # return ui function (request param required by shiny for bookmarking)
  function(request) {
    # set spinner color
    options(spinner.color = app_color)

    bslib::page_navbar(
      title = paste("Hello", user),
      theme = bslib::bs_theme(
        preset = default_theme,
        version = 5,
        "navbar-brand-font-size" = "1.5rem"
      ),
      fillable = TRUE,
      # HEADERS ===========
      header = tagList(
        sddsParticle::sdds_header(),
        tags$style(HTML(
          # center the nav-bar pills
          ".centered-pills .nav.nav-pills {justify-content: center;}",
          # align the save action buttons right next to inputs
          ".input-with-save {
            display: flex;
            align-items: flex-end;
            gap: 0.5rem;
          }
          .input-with-save .action-button {
            margin-bottom: 1rem;
          }"
        ))
      ),
      # TOP BAR ===========
      bslib::nav_spacer(), # pushes items to the right
      bslib::nav_item(
        if (nrow(groups) > 1) {
          uiOutput("group_name")
        } else {
          groups$group_desc[1]
        }
      ),
      bslib::nav_item(bslib::input_dark_mode(id = "color_mode", mode = NULL)),

      # main page
      bslib::nav_panel(
        title = NULL, # single nav panel
        padding = 0,
        # SIDE BAR ==============
        bslib::page_sidebar(
          sidebar = bslib::sidebar(
            open = nrow(groups) > 1, # collapsed by default
            h5(
              a(
                "µLogger GUI",
                href = "https://github.com/KopfLab/micrologger",
                target = "_blank"
              ),
              as.character(packageVersion("micrologger")),
              align = "center"
            ),
            if (nrow(groups) > 1) {
              selectInput(
                "group",
                label = "Group",
                choices = groups |>
                  select("group_desc", "group_id") |>
                  tibble::deframe()
              )
            },
            selectInput(
              "timezone",
              label = "Timezone",
              choices = OlsonNames(),
              selected = timezone
            ),
            selectInput(
              "theme",
              label = "Theme",
              choices = c(
                "flatly",
                "cosmo",
                "lumen",
                "minty",
                "sandstone",
                "darkly",
                "cyborg",
                "slate",
                "superhero",
                "solar"
              ),
              selected = default_theme
            )
          ),

          # EXPERIMENTS ==========
          experiments_ui("experiments")
        )
      )
    )

    # # header
    # header <- shinydashboard::dashboardHeader(
    #   title = user,
    #   titleWidth = app_title_width,
    #   # right side
    #   tags$li(
    #     class = "dropdown",
    #     if (nrow(groups) > 1) {
    #       selectInput(
    #         "group",
    #         label = NULL,
    #         choices = groups |>
    #           select("group_desc", "group_id") |>
    #           tibble::deframe()
    #       ) |>
    #         tags$div(class = "header-item")
    #     } else {
    #       groups$group_desc |> tags$div(class = "header-item")
    #     }
    #   )
    # )

    # # sidebar
    # sidebar <-
    #   shinydashboard::dashboardSidebar(
    #     collapsed = FALSE,
    #     disable = FALSE,
    #     width = app_title_width,
    #     sddsParticle::sdds_header(),
    #     use_app_utils(),
    #     tags$head(
    #       # css headers
    #       tags$style(
    #         type = "text/css",
    #         # for right side header (group title / dropdown)
    #         HTML(
    #           "
    #           .main-header .navbar .nav > li > .header-item {
    #             display: block;
    #             height: 50px;
    #             line-height: 50px;
    #             padding: 0 15px;
    #             color: #fff;
    #           }
    #         "
    #         ),
    #         HTML(paste(
    #           # body top padding
    #           ".box-body {padding-top: 5px; padding-bottom: 10px}",
    #           # custom background box
    #           sprintf(
    #             ".box.box-solid.box-info>.box-header{color:#fff; background: %s; background-color: %s;}",
    #             app_box_default,
    #             app_box_default
    #           ),
    #           sprintf(
    #             ".box.box-solid.box-info{border:1px solid %s;}",
    #             app_box_default
    #           ),
    #           sep = "\n"
    #         ))
    #       )
    #     ),
    #     h5(
    #       a(
    #         "µLogger GUI",
    #         href = "https://github.com/KopfLab/micrologger",
    #         target = "_blank"
    #       ),
    #       as.character(packageVersion("micrologger")),
    #       align = "center"
    #     ),
    #     h4("Timezone", align = "center", style = "margin: 0px;"),
    #     selectInput(
    #       "timezone",
    #       label = NULL,
    #       choices = OlsonNames(),
    #       selected = timezone
    #     ),
    #     tags$li(a(uiOutput("help", inline = TRUE))),
    #     if (shiny::in_devmode()) {
    #       actionButton("dev_mode_toggle", "Toggle Dev Mode")
    #     }
    #   )

    # # body
    # body <- shinydashboard::dashboardBody(
    #   experiments_ui("experiments")
    #   #   sddsParticle::sdds_ui(
    #   #   "sdds",
    #   #   device_list_title = "My devices",
    #   #   enable_add_remove_devices = TRUE
    #   # )
    # )

    # # dashboard page
    # shinydashboard::dashboardPage(
    #   title = app_title, # tab title
    #   skin = app_color, # styling
    #   header = header,
    #   sidebar = sidebar,
    #   body = body
    # )
  }
}
