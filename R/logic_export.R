#' Save device data logs
#'
#' Write device data logs to an .rds, .xlsx (excel), and/or .csv file and optionally compress them into a zip archive. Note that target files will be overwritten if they already exist.
#'
#' @param logs data logs to save
#' @param file_path path to the .rds, .xlsx and/or .csv files (can be multiple). All folders must already exist.
#' @param generate_rmd whether to generate an RMarkdown file for analysis. If both .rds and .xlsx file paths are provided, reads from the one listed first. Use \code{generate_rmd = TRUE} to have the function infer the Rmd file name from the .rds/.xlsx file path, or alternatively provide a file path with a specific name for the Rmd file (\code{generate_rmd = file.path(...)}).
#' @param zip whether to zip the resulting file(s). Use \code{zip = TRUE} to have the function infer the zip file name from the .rds/.xlsx file path, or alternatively provide a file path with a specific name for the zip file (\code{zip = file.path(...)}).
#' @return returns the logs invisibly for piping
#' @family data logs functions
#' @export
ml_write_logs_to_file <- function(
  logs,
  file_path,
  zip = FALSE
) {
  # write log files
  files <- write_logs_to_file(logs, file_path)
  zip_file <- if (is.character(zip)) zip else paste0(files$base_path[1], ".zip")

  # info message
  cli_alert_info(
    format_inline(
      "Info: writing device data logs",
      if (zip != FALSE) " into zip archive ('{basename(zip_file)}')" else "",
      ":\n - {paste0(basename(files$file_path), collapse = '\n - ')}"
    )
  )

  # zip file
  if (zip != FALSE) {
    zip_files(
      files$file_path,
      zip_file = zip_file,
      cleanup_after_compression = TRUE
    )
  }

  return(invisible(logs))
}


# convenience function to write logs to file, returns the files data frame
write_logs_to_file <- function(logs, file_path) {
  # safety checks
  if (missing(logs) || !is.data.frame(logs)) {
    stop("no data logs provided", call. = FALSE)
  }
  if (missing(file_path) || length(file_path) == 0) {
    stop("no file paths provided", call. = FALSE)
  }

  # file info
  supported <- c("rds", "xlsx", "csv")
  files <- tibble(
    file_path = file_path,
    ext = stringr::str_match(file_path, "\\.(\\w+)$")[, 2],
    base_path = stringr::str_replace(file_path, sprintf("\\.%s$", ext), ""),
    folder = dirname(file_path),
    dir_ok = dir.exists(folder),
    ext_ok = ext %in% supported
  )

  # check supported extensions
  if (!all(files$ext_ok)) {
    # fixme: simplify
    cli_abort(
      c(
        "unknown file extension(s): {paste0(filter(files, !ext_ok)$ext, collapse = ', ') |> unique()}",
        " (supported: {paste0(supported, collapse = ', ')})"
      )
    )
  }

  # check folders
  if (!all(files$dir_ok)) {
    # fixme: simplify
    cli_abort(
      c(
        "missing folder(s) - please make sure all directories already exist: ",
        "{paste0(filter(files, !dir_ok)$folder |> unique(), collapse = ', ')}"
      )
    )
  }

  # save files
  save_func <- function(file_path, ext) {
    if (ext == "rds") {
      readr::write_rds(logs, file_path)
    } else if (ext == "xlsx") {
      openxlsx::write.xlsx(logs, file_path)
    } else if (ext == "csv") {
      readr::write_csv(logs, file_path)
    } else {
      stop("shouldn't get here", call. = FALSE)
    }
    return(TRUE)
  }

  mutate(files, saved = purrr::map2_lgl(file_path, ext, save_func))
}

# helper function to zip up files with a simple file path
# @param cleanup_after_compression whether to remove the original files
zip_files <- function(
  file_path,
  zip_file = paste0(tempfile(), ".zip"),
  cleanup_after_compression = FALSE
) {
  files_exist <- file.exists(file_path)
  if (any(!files_exist)) {
    cli_warn(
      c(
        "some files do not exist ",
        "({paste(file_path[!files_exist], collapse = ', ')}) ",
        "and will be exluded from the zip archive"
      )
    )
  }
  zip_files <- file_path[files_exist]
  zip_files_in_wd <- basename(zip_files)
  file.copy(from = zip_files, to = zip_files_in_wd)
  zip_file_in_wd <- basename(zip_file)
  zip::zip(zip_file_in_wd, files = zip_files_in_wd)
  file.copy(from = zip_file_in_wd, to = zip_file)
  if (cleanup_after_compression) {
    unlink(unique(c(zip_files, zip_files_in_wd, zip_file_in_wd)))
  } else {
    unlink(unique(c(zip_files_in_wd, zip_file_in_wd)))
  }
  return(zip_file)
}
