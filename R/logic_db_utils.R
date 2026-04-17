# Connecion Credentials ==========

#' Securley store the access information using your OS keyring
#' Call [ml_set_connection_credentials] for a new R session to transfer these to environmental variables.
#' @export
ml_store_connection_credentials_host <- function() {
  keyring::key_set("MICROLOGGER_DB_HOST", prompt = "DB Host")
}

#' Securley store the access information using your OS keyring
#' Call [ml_set_connection_credentials] for a new R session to transfer these to environmental variables.
#' @export
ml_store_connection_credentials_user <- function() {
  keyring::key_set("MICROLOGGER_DB_USER", prompt = "DB User")
}

#' Securley store the access information using your OS keyring
#' Call [ml_set_connection_credentials] for a new R session to transfer these to environmental variables.
#' @export
ml_store_connection_credentials_password <- function() {
  keyring::key_set("MICROLOGGER_DB_PASSWORD", prompt = "DB Password")
}


#' Set DB connection credentials
#' @export
ml_set_connection_credentials <- function(
  host = keyring::key_get("MICROLOGGER_DB_HOST"),
  user = keyring::key_get("MICROLOGGER_DB_USER"),
  password = keyring::key_get("MICROLOGGER_DB_PASSWORD")
) {
  Sys.setenv(
    MICROLOGGER_DB_HOST = host,
    MICROLOGGER_DB_USER = user,
    MICROLOGGER_DB_PASSWORD = password
  )
}

# Connection Management =======

.db_env <- new.env(parent = emptyenv())
.db_env$conn <- NULL

get_connection <- function() {
  host <- Sys.getenv("MICROLOGGER_DB_HOST")
  user <- Sys.getenv("MICROLOGGER_DB_USER")
  password <- Sys.getenv("MICROLOGGER_DB_PASSWORD")
  stopifnot(
    "env. variable `MICROLOGGER_DB_HOST` not set" = nzchar(host),
    "env. variable `MICROLOGGER_DB_USER` not set" = nzchar(user),
    "env. variable `MICROLOGGER_DB_PASSWORD` not set" = nzchar(password)
  )
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = host,
    port = 5432,
    dbname = "microloggers",
    user = user,
    password = password
  )
}

db <- function() {
  if (is.null(.db_env$conn) || !DBI::dbIsValid(.db_env$conn)) {
    .db_env$conn <- get_connection()
  }
  .db_env$conn
}


# SQL converters ==========

# convert data to sql compatible
to_sql <- function(..., named = FALSE) {
  values <- list(...)
  if (length(values) == 1 && is.list(values[1])) {
    values <- values[[1]]
  }
  convert_class_to_sql <- function(.x) {
    if (is.null(.x) || is.na(.x)) {
      "NULL"
    } else if (is.character(.x)) {
      sprintf("'%s'", stringr::str_replace(.x, stringr::fixed("'"), "''"))
    } else if (is.numeric(.x)) {
      as.character(.x)
    } else if (is.logical(.x)) {
      if (.x) 'true' else 'false'
    } else {
      stop(glue("unsupported value type: {class(.x)[1]}"), call. = FALSE)
    }
  }
  sql_values <- purrr::map_chr(values, convert_class_to_sql)
  if (named) {
    if (is.null(names(values)) || any(names(values) == "")) {
      stop("must provide names for each value", call. = FALSE)
    }
    sql_values <- sprintf("%s=%s", names(values), sql_values)
  }
  paste(sql_values, collapse = ", ")
}

# convert whole df to sql compatible list of values
df_to_sql <- function(df) {
  df |>
    ungroup() |>
    mutate(rowid = row_number()) |>
    tidyr::nest(data = c(-rowid)) |>
    mutate(sql = purrr::map_chr(data, ~ to_sql(as.list(.x)))) |>
    pull(sql) |>
    paste(collapse = "), (") |>
    sprintf(fmt = "(%s)")
}

# make insert statement from data frame
df_to_insert_sql <- function(df, table) {
  sprintf(
    "INSERT INTO %s (%s) VALUES %s",
    table,
    paste(names(df), collapse = ", "),
    df_to_sql(df)
  )
}

# run a dbplyr query
collect_from_db <- function(df, .env = caller_env(), .retry = TRUE) {
  result <-
    tryCatch(
      df |> collect(),
      error = function(e) {
        # retry once in case we have a connection time out
        if (.retry) {
          cli_warn("Database query failed, retrying once...")
          return(df |> collect_from_db(.env = .env, .retry = FALSE))
        }
        # query failed
        cli_abort(
          c("Failed to fetch data from database"),
          parent = e,
          call = .env
        )
      }
    )
  return(result)
}

# run sql with error catching
# @param execute_only - if set, runs DBExecute (return value is number of rows affecgted), if FALSE, runs dbGetQuery and returns whatever the query specifes
run_sql <- function(
  sql,
  con = db(),
  .env = caller_env(),
  execute_only = TRUE,
  .retry = TRUE
) {
  result <-
    tryCatch(
      {
        if (execute_only) {
          DBI::dbExecute(con, as.character(sql))
        } else {
          DBI::dbGetQuery(con, as.character(sql))
        }
      },
      error = function(e) {
        # retry once in case we have a connection time out
        if (.retry) {
          cli_warn("Database query failed, retrying once...")
          return(run_sql(
            sql,
            con = con,
            .env = .env,
            execute_only = execute_only,
            .retry = FALSE
          ))
        }
        # query failed
        cli_abort(
          c("SQL statement failed", "i" = "{sql}"),
          parent = e,
          call = .env
        )
      }
    )
  return(result)
}

# run insert sql
run_insert_sql <- function(
  df,
  table,
  con = db(),
  on_conflict_constraint = NULL,
  on_conflict_do = "nothing",
  return_column = NULL
) {
  sql <- df_to_insert_sql(df, table)
  if (!is.null(on_conflict_constraint)) {
    sql <- sql |>
      paste(
        "ON CONFLICT ON CONSTRAINT",
        on_conflict_constraint,
        "DO",
        on_conflict_do
      )
  }
  if (!is.null(return_column)) {
    sql <- sql |>
      paste("RETURNING", return_column)
  }

  # run
  result <- sql |> run_sql(con, execute_only = is.null(return_column))

  # info
  if (is.null(return_column)) {
    cli::cli_alert_success(
      "{result} record{?s} created{if (!is.null(on_conflict_constraint)) ' or updated'}."
    )
  } else {
    cli::cli_alert_success(
      "Created{if (!is.null(on_conflict_constraint)) ' or updated'} {.field {table}} record with id {col_magenta(result[[1]])}."
    )
  }
  return(result)
}
