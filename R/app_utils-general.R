# logging utilities ====
# note: fatal and trace are usually overkill

# call to use app util error formatting
use_app_utils <- function() {
  tagList(
    # adopt error color of the theme and make error validation larger
    tags$style(HTML(
      ".shiny-output-error-validation {
        color: var(--bs-danger) !important;
        font-size: 1.5rem;
      }"
    )) |>
      singleton(),
    # support ansii
    tags$style(HTML(paste(format(cli::ansi_html_style()), collapse = "\n"))) |>
      singleton(),
    # make card headings taller
    tags$style(HTML(".card-header { padding: 1rem 1.25rem;}")) |> singleton(),
    # preseve spacing of CLI errors that might make it to the gui
    tags$style(HTML(
      ".cli-inline-error {
        display: inline;
        white-space: pre-wrap;
      }"
    )) |>
      singleton()
  )
}

# call the other log functions instead for clarity in the code
# @param ... toaster parameters
log_any <- function(
  msg,
  log_fun,
  ns = NULL,
  toaster = NULL,
  position = "bottom-left",
  ...
) {
  ns <- if (!is.null(ns)) paste0("[", ns(NULL), "] ") else ""
  if (!is.null(toaster)) {
    log_fun(paste0(ns, msg, " [GUI msg: '", toaster, "']", collapse = ""))
    bslib::toast(
      HTML(cli::ansi_html(toaster)),
      position = position,
      ...,
    ) |>
      bslib::show_toast()
  } else {
    log_fun(paste0(ns, msg, collapse = ""))
  }
}

# calls log_warning and log_error for any encoutnered conditions
log_cnds <- function(
  cnds = tibble(),
  ns = NULL,
  user_msg = NULL,
  .call = caller_call()
) {
  # allow cnds to be a try_catch_cnds return object
  if (!is.data.frame(cnds) && is.data.frame(cnds$conditions)) {
    cnds <- cnds$conditions
  }
  if (nrow(cnds) == 0) {
    return()
  }

  # get call info
  call <- as.character(.call[1]) |>
    stringr::str_remove(stringr::fixed("<reactive:")) |>
    stringr::str_remove(stringr::fixed(">"))
  if (is_empty(call)) {
    call <- "unknown"
  }

  warnings <- cnds |> filter(type == "warning")
  if (nrow(warnings) > 0) {
    # toaster warnings
    for (message in warnings$message) {
      log_warning(
        ns = ns,
        user_msg = if (!is.null(user_msg)) {
          user_msg
        } else {
          format_inline(
            "Warning in {call}()"
          )
        },
        warning = message
      )
    }
  }
  errors <- cnds |> filter(type == "error")
  if (nrow(errors) > 0) {
    # full errors
    log_error(
      ns = ns,
      user_msg = if (!is.null(user_msg)) {
        user_msg
      } else {
        format_inline("{qty(nrow(errors))}Error{?s} in {call}()")
      },
      error = errors$condition |>
        purrr::map_chr(~ format(.x) |> paste(collapse = "\n"))
    )
  }
}

log_error <- function(..., ns = NULL, user_msg = NULL, error = NULL) {
  error_msg <-
    if (!is.null(error)) {
      gsub("\\n", "<br>", cli::ansi_html(error)) |> paste(collapse = "<br>")
    } else {
      ""
    }

  issue_title <- sprintf(
    "Version %s: %s",
    if (getPackageName() != ".GlobalEnv") {
      packageVersion(getPackageName())
    } else {
      "app"
    },
    user_msg
  )

  issue_body <- sprintf(
    "Please describe here what you were attempting to do in the app when this issue occured.\n\n## Trace (do NOT delete)\n\n<pre>%s</pre>",
    error_msg
  )

  issue_url <- sprintf(
    "https://github.com/KopfLab/sddsParticle/issues/new?title=%s&body=%s",
    URLencode(issue_title, reserved = TRUE),
    URLencode(HTML(issue_body), reserved = TRUE)
  )

  error_screen <- modalDialog(
    title = span(
      style = "color: red;",
      h2(user_msg, style = "color: red;"),
      h4(
        "Please try again. If the issue persists, please",
        tags$a("report this error", href = issue_url, target = "_blank"),
      )
    ),
    if (nchar(error_msg) > 0) pre(HTML(error_msg))
  )

  log_any(
    msg = paste0(
      ...,
      if (!is.null(error)) paste0("Encountered error:\n", error, "\n"),
      collapse = ""
    ),
    ns = ns,
    log_fun = rlog::log_error,
    toaster = user_msg,
    header = "Encountered error",
    type = "danger",
    duration_s = 10
  )

  if (!is.null(error)) {
    showModal(error_screen)
  }
}

log_warning <- function(..., ns = NULL, user_msg = NULL, warning = NULL) {
  msg <- paste0(..., collapse = "")
  if (!nzchar(msg) && !is.null(user_msg)) {
    msg <- user_msg
  }
  log_any(
    msg = msg,
    ns = ns,
    log_fun = rlog::log_warn,
    toaster = if (!is.null(warning)) warning else user_msg,
    header = if (!is.null(warning)) HTML(cli::ansi_html(user_msg)) else NULL,
    type = "warning",
    duration_s = 5
  )
}

log_info <- function(..., ns = NULL, user_msg = NULL) {
  msg <- paste0(..., collapse = "")
  if (!nzchar(msg) && !is.null(user_msg)) {
    msg <- user_msg
  }
  log_any(
    msg = msg,
    ns = ns,
    log_fun = rlog::log_info,
    toaster = user_msg,
    type = "info",
    duration_s = 2
  )
}

log_success <- function(..., ns = NULL, user_msg = NULL) {
  msg <- paste0(..., collapse = "")
  if (!nzchar(msg) && !is.null(user_msg)) {
    msg <- user_msg
  }
  log_any(
    msg = msg,
    ns = ns,
    log_fun = rlog::log_info,
    toaster = user_msg,
    type = "success",
    duration_s = 2
  )
}

log_debug <- function(..., ns = NULL) {
  log_any(msg = paste0(..., collapse = ""), ns = ns, log_fun = rlog::log_debug)
}

# ui elements ======

# convenience function for adding spaces (not the most elegant way but works)
spaces <- function(n = 1) {
  htmltools::HTML(rep("&nbsp;", n))
}

# adding an inline UI item
inline <- function(...) {
  htmltools::div(style = "display: inline-block;", ...)
}

# convenience function to add tooltip
add_tooltip <- function(widget, ...) {
  bslib::tooltip(widget, ...)
}
