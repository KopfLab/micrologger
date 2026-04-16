# Selector table

#' Selector table server
#' @param get_data reactive context providing the data set
#' @param id_column name of the ID column - must have unique values!! make a rownumber or concatenated column if there is no unique identifier, this column does NOT have to be part of available_columns (but can be)
#' @param available_columns list of transmute statements to select columns to show
#' @param visible_columns integer vector of columns (from what the available_columns selects) that are visible when the table loads (can be changed with the table_columns_button action) - if empty, all columns are visible
#' @param container the container defining the table html layout (passed on to DT::datatable)
#' @param allow_view_all whether to allow the "all" option in the page lengths, default is FALSE
#' @param page_lengths page length options, first one will be selected
#' @param initial_page_length initially selected page length, first entry of the page_lengths by default
#' @param dom the available table control elements and their order
#' @param filter whether to include column filters - note that this does NOT work for restoring after reload so use with caution if that's a desired feature
#' @param ordering whether to allow column sorting, default TRUE
#' @param class styling of table see class parameter for datatable
#' @param escape_headers whether to HTML escape headers (turn off to have HTML rendered in the header columns)
#' @param selection see parameter for data table (none, single, multiple)
#' @param auto_reselect whether to reselect selected rows automatically after reloads
#' @param render_html list of columns which should NOT be html escaped (e.g. for links), use dplyr::everything() to render everything
#' @param formatting_calls list of lists with function and columns e.g. list(list(func = formatCurrency, columns = "x)) or a columns expressione.g. list(list(func = formatCurrencty, columns_expr = rlang::expr(matches("abc"))))
#' @param ... passed to options
module_selector_table_server <- function(
  input,
  output,
  session,
  get_data,
  id_column,
  available_columns = list(dplyr::across(dplyr::everything(), identity)),
  visible_columns = c(),
  container = NULL,
  allow_view_all = FALSE,
  page_lengths = list(
    c(5, 10, 20, 50, 100, if (allow_view_all) -1),
    c("5", "10", "20", "50", "100", if (allow_view_all) "All")
  ),
  initial_page_length = page_lengths[[1]][1],
  dom = "fltip",
  filter = c("none", "bottom", "top"),
  ordering = TRUE,
  class = "cell-border hover order-column",
  escape_headers = TRUE,
  selection = c("multiple", "single", "none"),
  auto_reselect = TRUE,
  enable_dblclick = FALSE,
  render_html = c(),
  formatting_calls = list(),
  editable = FALSE,
  extensions = list(),
  no_data_message = "No data available",
  ...
  # note: considered allowing editable option but it doesn't work so well for select tables
) {
  # safety checks
  stopifnot(!missing(get_data))
  stopifnot(!missing(id_column))

  # namespace
  ns <- session$ns

  # reactive values =========
  values <- reactiveValues(
    table_data = NULL,
    all_ids = c(),
    selected_ids = c(),
    selected_cells = c(), # only matters if in 'cell' selection mode
    update_selected = if (auto_reselect) -1L else 0L, # trigger selection update (circumventing circular triggers with user selection)
    rendering = TRUE, # whether the table is currently rendering
    table_exists = FALSE, # whether the table exists or not
    table_reloaded = 0L, # whether the table has re-loaded completely
    visible_cols = visible_columns,
    page_length = initial_page_length, # selected page length
    display_start = 0, # which display page to start on
    search = "", # search term
    order = list(), # ordering information
    filter = match.arg(filter), # filter setting
    formatting_calls = formatting_calls, # formatting calls
    options = list(...) #
  )

  # create table df =============
  get_table_df <- reactive({
    req(get_data())
    validate(need(has_data(), no_data_message))
    # get the table
    df <-
      tryCatch(
        isolate({
          # id values
          values$all_ids <- get_data()[[id_column]]
          if (any(duplicated(values$all_ids))) {
            abort("found duplicate IDs in data table")
          }

          # table df
          transmute_cols <- rlang::call_args(rlang::enquo(available_columns))
          df <- get_data() |>
            dplyr::transmute(!!!transmute_cols) |>
            as.data.frame()
          rownames(df) <- values$all_ids

          # select the transmuted cols to begin with
          if (length(values$visible_cols) == 0) {
            values$visible_cols <- seq_along(names(df))
          }

          return(df)
        }),
        error = function(e) {
          # try catch error
          log_error(
            ns = ns,
            user_msg = "Data could not be processed",
            error = e
          )
          return(NULL)
        }
      )

    # return
    return(df)
  })

  # set the visible columns, returns TRUE/FALSE if values have changed
  set_visible_columns <- function(visible_cols) {
    visible_cols <- as.integer(visible_cols)
    if (!identical(visible_cols, values$visible_cols)) {
      # got some new columns
      values$visible_cols <- visible_cols
      return(TRUE)
    }
    return(FALSE)
  }

  # reset visible columns (always isolate, trigger independently!)
  reset_visible_columns <- function() {
    isolate({
      if (length(values$visible_cols) > 0) {
        values$visible_cols <- c()
      }
    })
  }

  # get the table with the visible cols
  get_table_df_visible_cols <- reactive({
    return(get_table_df()[values$visible_cols])
  })

  # render data table ========
  render_html_expr <- rlang::enexpr(render_html)
  output$selection_table <- DT::renderDataTable(
    {
      # triggers
      get_table_df_visible_cols()
      values$filter
      # info
      log_info(
        ns = ns,
        "(re-) rendering selection table",
        user_msg = "Loading table"
      )
      # get the table
      table <-
        tryCatch(
          isolate({
            values$table_data <- get_table_df_visible_cols()
            # header columns
            if (is.null(container)) {
              container <- tags$table(
                DT::tableHeader(
                  names(values$table_data),
                  escape = escape_headers
                ),
                class = class
              )
            }
            # generate data table
            table <- DT::datatable(
              data = values$table_data,
              rownames = FALSE,
              filter = values$filter,
              class = class,
              container = container,
              selection = selection,
              #fillContainer = TRUE, # note this is useless inside DT:renderDataTable
              # (but also not necessary, use scrollY = "max(400px, calc(100vh - 200px))" instead)
              escape = if (
                rlang::is_call(render_html_expr) &&
                  rlang::call_name(render_html_expr) == "everything"
              ) {
                FALSE
              } else {
                setdiff(names(get_table_df_visible_cols()), render_html)
              },
              editable = editable,
              extensions = extensions,
              options = c(
                list(
                  order = values$order,
                  ordering = ordering,
                  pageLength = values$page_length,
                  search = list(
                    regex = FALSE,
                    caseInsensitive = TRUE,
                    search = values$search
                  ),
                  displayStart = values$display_start,
                  lengthMenu = page_lengths,
                  searchDelay = 100,
                  dom = dom,
                  #columns= values$columns, # this does not work to restore the search, breaks the table instead
                  # could maybe do it in javascript, ideas here: https://datatables.net/forums/discussion/53287/how-to-reset-values-in-individual-column-searching-text-inputs-at-a-button-click
                  stateSave = FALSE,
                  # disable the automatic state reload to avoid issues between different table instances
                  stateLoadParams = DT::JS(
                    "function (settings, data) { return false; }"
                  )
                ),
                values$options
              ),
              callback = if (enable_dblclick) {
                htmlwidgets::JS(
                  "table.on('dblclick', 'td',",
                  "  function() {",
                  "    var row = table.cell(this).index().row;",
                  "    var col = table.cell(this).index().column;",
                  sprintf(
                    "    Shiny.setInputValue('%s_dblclick', {dt_row: row, dt_col: col});",
                    ns("selection_table")
                  ),
                  "  }",
                  ");"
                )
              } else {
                htmlwidgets::JS("")
              }
            )

            # double click
            observeEvent(input$selection_table_dblclick, {
              print(input$selection_table_dblclick)
            })

            # formatting calls
            if (length(values$formatting_calls) > 0) {
              for (i in seq_along(values$formatting_calls)) {
                if (!"func" %in% names(values$formatting_calls[[i]])) {
                  abort(
                    "trying to apply formatting call without 'func' variable"
                  )
                }
                if (
                  !any(
                    c("columns", "columns_expr") %in%
                      names(values$formatting_calls[[i]])
                  )
                ) {
                  abort(
                    "trying to apply formatting call without either 'columns' or 'columns_expr' argument"
                  )
                }

                # columns
                existing_cols <-
                  if ("columns_expr" %in% names(values$formatting_calls[[i]])) {
                    tidyselect::eval_select(
                      values$formatting_calls[[i]]$columns_expr,
                      get_table_df_visible_cols(),
                      strict = FALSE
                    )
                  } else {
                    intersect(
                      values$formatting_calls[[i]]$columns,
                      names(get_table_df_visible_cols())
                    )
                  }

                # apply
                if (length(existing_cols) > 0) {
                  # run the renderer
                  table <- do.call(
                    values$formatting_calls[[i]]$func,
                    args = c(
                      list(table = table, columns = existing_cols),
                      values$formatting_calls[[i]][
                        -which(
                          names(values$formatting_calls[[i]]) %in%
                            c("func", "columns", "columns_expr")
                        )
                      ]
                    )
                  )
                }
              }
            }

            # return table
            table
          }),
          error = function(e) {
            # try catch error
            log_error(
              ns = ns,
              user_msg = "Data table couldn't be created",
              error = e
            )
            return(NULL)
          }
        )

      # wrap up
      validate(need(table, "Data table couldn't be created"))
      isolate({
        # keep track of rendering
        if (!values$rendering) {
          values$rendering <- TRUE
        }
      })
      return(table)
    },
    # make sure this is executed server side
    server = TRUE
  )

  # update data table formatting (always isolated! trigger independently)
  change_formatting_calls <- function(formatting_calls) {
    isolate({
      values$formatting_calls <- formatting_calls
    })
  }

  # update options (always isolated! trigger independently)
  update_options <- function(...) {
    isolate({
      values$options <- utils::modifyList(values$options, list(...))
    })
  }

  # edit data table ========================
  proxy = DT::dataTableProxy("selection_table")
  observeEvent(input$selection_table_cell_edit, {
    # this only makes sense for cell edit and immediate save
    if (nrow(input$selection_table_cell_edit) > 1) {
      abort("editable columns/rows not yet supported")
    }

    # new value
    row <- input$selection_table_cell_edit$row
    col <- input$selection_table_cell_edit$col
    value <- input$selection_table_cell_edit$value

    # info
    sprintf(
      "updating '%s', '%s' --> '%s'",
      get_id_from_index(row),
      get_col_from_index(col),
      value
    ) |>
      log_info(ns = ns)

    # FIXME: instead of updating the table, catch this even to save the update in the database
    # FIXME: make buttons for set yes/no/unknown for the teaching years

    # update
    values$table_data <- DT::editData(
      values$table_data,
      input$selection_table_cell_edit
    )

    # for some reason this does NOT work, it empties the table (even if previous is assigned with <<-)
    # maybe an issue with reactive values?
    #DT::replaceData(proxy, values$table_data)
  })

  # save row selection ========
  observeEvent(
    input$selection_table_rows_selected,
    {
      req(table_exists())
      req(has_data())
      req(get_all_ids())
      # don't trigger while we're first rendering
      req(!values$rendering)
      # avoid circular trigger iwth update_selected = FALSE
      select_rows(
        indices = input$selection_table_rows_selected,
        update_selected = FALSE
      )
    },
    ignoreNULL = FALSE
  )

  get_id_from_index <- function(indices) {
    return(values$all_ids[indices])
  }

  get_index_from_id <- function(ids) {
    return(which(values$all_ids %in% ids))
  }

  clean_ids <- function(ids) {
    # only return those not duplicated and actually in the dataset
    return(get_id_from_index(get_index_from_id(omit_duplicates(ids))))
  }

  select_rows <- function(
    ids = get_id_from_index(indices),
    indices = NULL,
    update_selected = TRUE
  ) {
    ids <- clean_ids(ids)
    if (!identical(ids, values$selected_ids)) {
      # there were actual changes
      values$selected_ids <- ids
    }
    if (update_selected) update_selected()
  }

  # save cell selection ========
  observeEvent(
    input$selection_table_cells_selected,
    {
      req(table_exists())
      req(has_data())
      req(values$all_ids)
      if (
        !is.null(input$selection_table_cells_selected) &&
          dim(input$selection_table_cells_selected)[2] > 1
      ) {
        select_cells(
          indices = input$selection_table_cells_selected[, 1],
          col_indices = input$selection_table_cells_selected[, 2]
        )
      } else {
        select_cells(indices = c(), col_indices = c())
      }
    },
    ignoreNULL = FALSE
  )

  get_col_from_index <- function(indices) {
    names(get_table_df_visible_cols())[indices + 1L]
  }

  select_cells <- function(
    ids = get_id_from_index(indices),
    indices = NULL,
    cols = get_col_from_index(col_indices),
    col_indices = NULL
  ) {
    # get selected ids and cols
    selected <-
      dplyr::tibble(id = ids, col = cols) |>
      dplyr::summarize(col = list(col), .by = id) |>
      dplyr::arrange(.data$id) |>
      tibble::deframe()
    ids <- names(selected)
    if (!identical(selected, values$selected_cells)) {
      # there were actual changes
      values$selected_ids <- ids
      values$selected_cells <- selected
      if (length(ids) > 0L) {
        log_debug(
          ns = ns,
          "saving cell selections: ",
          sprintf(
            "#%d = '%s' ('%s')",
            get_index_from_id(ids),
            ids,
            purrr::map_chr(selected, paste, collapse = "', '")
          ) |>
            paste0(collapse = ", ")
        )
      } else {
        log_debug(ns = ns, "saving cell selections: nothing")
      }
    }
  }

  # update selection =========
  update_selected <- function() {
    values$update_selected <- values$update_selected + 1L
  }
  observeEvent(
    values$update_selected,
    {
      if (values$update_selected > 0) {
        sprintf("(re-) selecting %d rows", length(values$selected_ids)) |>
          log_debug(ns = ns)
        proxy <- DT::dataTableProxy("selection_table")
        DT::selectRows(proxy, get_index_from_id(values$selected_ids))
      }
    },
    priority = 1000
  )

  # select all event ======
  observeEvent(input$select_all, {
    select_rows(
      ids = c(
        values$selected_ids,
        get_id_from_index(input$selection_table_rows_all)
      )
    )
  })

  # deselect all event ======
  observeEvent(input$deselect_all, {
    select_rows(c())
  })

  # set/pick columns event =====
  observeEvent(input$pick_cols, {
    req(get_data())
    dlg <- modalDialog(
      title = "Show columns",
      easyClose = TRUE,
      checkboxGroupInput(
        ns("visible_cols"),
        label = NULL,
        choiceNames = names(get_table_df()),
        choiceValues = seq_along(names(get_table_df())),
        selected = values$visible_cols
      ),
      footer = tagList(
        actionButton(ns("apply_cols"), "Apply") |>
          add_tooltip(
            "Switch to showing the selected column(s). Note that if the search is based on a column that is removed, different rows will show."
          ),
        spaces(1),
        modalButton("Cancel")
      )
    )
    showModal(dlg)
  })
  observeEvent(input$apply_cols, {
    if (set_visible_columns(input$visible_cols)) {
      log_info(
        ns = ns,
        "selecting table columns: ",
        sprintf(
          "%d (%s)",
          values$visible_cols,
          names(get_table_df())[values$visible_cols]
        ) |>
          paste(collapse = ", "),
        user_msg = "Switching columns"
      )
    }
    removeModal()
  })

  # toggle column search event =====
  observeEvent(input$col_search, {
    if (identical(values$filter, "none")) {
      log_info(
        ns = ns,
        "enabling top filter",
        user_msg = "Enabling column filters"
      )
      values$filter <- "top"
    } else if (identical(values$filter, "top")) {
      log_info(
        ns = ns,
        "removing top filter",
        user_msg = "Disabling column filters"
      )
      values$filter <- "none"
    }
  })

  # save state ========

  # save state
  observeEvent(input$selection_table_state, {
    if (values$rendering) {
      if (!values$table_exists) {
        values$table_exists <- TRUE
      }
      values$table_reloaded <- values$table_reloaded + 1L
      values$rendering <- FALSE
      # now that the renderin is done, revisit the selection
      if (auto_reselect) {
        # make sure selection stays the same
        update_selected()
      } else {
        # no selection
        if (length(values$selected_ids) > 0) {
          values$selected_ids <- c()
        }
      }
    }
    values$page_length <- input$selection_table_state$length
    values$display_start <- input$selection_table_state$start
    values$search <- input$selection_table_state$search$search
    values$order <- input$selection_table_state$order
    # Note: this doesn't work to restore the search fields
    #values$columns <- input$selection_table_state$columns
  })

  table_exists <- reactive(values$table_exists)
  is_table_reloaded <- reactive({
    req(table_exists())
    values$table_reloaded
  })

  # retrieve data ======
  get_all_ids <- reactive({
    req(table_exists())
    values$all_ids
  })

  has_data <- reactive({
    return(!is.null(get_data()) && nrow(get_data()) > 0L)
  })

  external_has_data <- reactive({
    req(table_exists())
    has_data()
  })

  get_selected_ids <- reactive({
    req(table_exists())
    return(values$selected_ids)
  })

  get_selected_cells <- reactive({
    req(table_exists())
    return(values$selected_cells)
  })

  get_selected_items <- reactive({
    # get the actual table items that are selected
    req(table_exists())
    return(get_data()[get_index_from_id(values$selected_ids), ])
  })

  # enable buttons =====
  observe({
    req(table_exists())
    toggle <- has_data() & length(input$selection_table_rows_all) > 0
    if (isolate(!is.null(input$select_all))) {
      shinyjs::toggleState("select_all", condition = toggle)
    }
  })
  observe({
    req(table_exists())
    toggle <- has_data()
    if (isolate(!is.null(input$deselect_all))) {
      shinyjs::toggleState("deselect_all", condition = toggle)
    }
  })
  observe({
    req(table_exists())
    toggle <- has_data() & length(input$visible_cols) > 0
    if (isolate(!is.null(input$apply_cols))) {
      shinyjs::toggleState("apply_cols", condition = toggle)
    }
  })

  # reactive trigger messsages ====

  # table_exists()
  observe({
    req(table_exists())
    log_debug(ns = ns, "table_exists() is now TRUE")
  })

  # has_data()
  observe({
    log_debug(ns = ns, "has_data() is now ", external_has_data())
  })

  # is_table_reloaded()
  observe({
    n <- is_table_reloaded()
    log_success(
      ns = ns,
      "is_table_reloaded() now returns ",
      n,
      user_msg = "Complete"
    )
  })

  # get_all_ids()
  observe({
    all_ids <- get_all_ids()
    if (is_empty(all_ids)) {
      log_debug(ns = ns, "get_all_ids() now returns an empty vector")
    } else {
      log_debug(
        ns = ns,
        "get_all_ids() now returns ",
        length(all_ids),
        " values"
      )
    }
  })

  # get_selected_ids()
  observe({
    ids <- get_selected_ids()
    if (is_empty(ids)) {
      log_debug(ns = ns, "get_selected_ids() now returns an empty vector")
    } else {
      log_debug(
        ns = ns,
        "get_selected_ids() now returns ",
        length(ids),
        " values: ",
        sprintf("#%d = '%s'", get_index_from_id(ids), ids) |>
          paste0(collapse = ", ")
      )
    }
  })

  # return functions =====
  list(
    # information functions
    table_exists = table_exists,
    is_table_reloaded = is_table_reloaded,
    has_data = external_has_data,
    get_all_ids = get_all_ids,
    get_selected_ids = get_selected_ids,
    get_selected_cells = get_selected_cells,
    get_selected_items = get_selected_items,
    # action functions
    select_rows = select_rows,
    set_visible_columns = set_visible_columns,
    reset_visible_columns = reset_visible_columns,
    change_formatting_calls = change_formatting_calls,
    update_options = update_options
  )
}


# Selector table
module_selector_table_ui <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("selection_table")) |> shinycssloaders::withSpinner()
}

# Selection buttons
module_selector_table_select_all_button <- function(id, border = TRUE) {
  ns <- NS(id)
  style <- if (!border) "border: 0;" else ""
  tagList(
    actionButton(
      ns("select_all"),
      "Select all",
      icon = icon("square-minus"),
      style = style
    ) |>
      add_tooltip(
        "Select all items that match the current search in addition to those already selected."
      )
  )
}

module_selector_table_deselect_all_button <- function(id, border = TRUE) {
  ns <- NS(id)
  style <- if (!border) "border: 0;" else ""
  tagList(
    actionButton(
      ns("deselect_all"),
      "Deselect",
      icon = icon("square"),
      style = style
    ) |>
      add_tooltip(
        "Deselect all items (even those not visible in the current search)"
      )
  )
}

# Column selector button
module_selector_table_columns_button <- function(id, border = TRUE) {
  ns <- NS(id)
  style <- if (!border) "border: 0;" else ""
  tagList(
    actionButton(
      ns("pick_cols"),
      "Adj. View",
      icon = icon("gear"),
      style = style
    ) |>
      add_tooltip("Pick which columns to show")
  )
}

# Column search button
module_selector_table_search_button <- function(id, border = TRUE) {
  ns <- NS(id)
  style <- if (!border) "border: 0;" else ""
  tagList(
    actionButton(
      ns("col_search"),
      "Adv. Search",
      icon = icon("search"),
      style = style
    ) |>
      add_tooltip("Toggle advanced column search option")
  )
}

# helper function
omit_duplicates <- function(x) {
  x[!duplicated(x)]
}
