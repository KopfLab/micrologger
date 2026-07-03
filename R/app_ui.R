ml_ui <- function(timezone, user, groups, default_theme) {
  # constants
  app_title <- "µLogger GUI"
  app_color <- "green"

  # return ui function (request param required by shiny for bookmarking)
  function(request) {
    # set spinner color
    options(spinner.color = "gray")

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
      # group / timezone / theme selectors (labels replaced with hover tooltips)
      bslib::nav_item(
        if (nrow(groups) > 1) {
          choices <- groups |>
            select("group_desc", "group_id") |>
            tibble::deframe()
          max_chars <- names(choices) |> nchar() |> max()
          selectInput(
            "group",
            label = NULL,
            choices = choices,
            width = sprintf("%dpx", max_chars * 9L)
          ) |>
            tagAppendAttributes(class = "mb-0") |>
            add_tooltip("Group whose loggers and experiments to show")
        } else {
          groups$group_desc[1]
        }
      ),
      bslib::nav_item(
        selectInput(
          "timezone",
          label = NULL,
          choices = OlsonNames(),
          selected = timezone,
          width = "200px"
        ) |>
          tagAppendAttributes(class = "mb-0") |>
          add_tooltip("Timezone used for all date and time displays")
      ),
      bslib::nav_item(
        selectInput(
          "theme",
          label = NULL,
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
          selected = default_theme,
          width = "150px"
        ) |>
          tagAppendAttributes(class = "mb-0") |>
          add_tooltip("Visual theme for the app")
      ),
      bslib::nav_item(bslib::input_dark_mode(id = "color_mode", mode = NULL)),
      bslib::nav_item(
        a(
          paste0("µLogger GUI v", as.character(packageVersion("micrologger"))),
          href = "https://github.com/KopfLab/micrologger",
          target = "_blank"
        )
      ),

      # main page
      bslib::nav_panel(
        title = NULL, # single nav panel
        padding = 0,
        # EXPERIMENTS ==========
        experiments_ui("experiments")
      )
    )
  }
}
