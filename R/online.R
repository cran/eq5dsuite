# Online mode: the settings that apply when the app is deployed on a public
# server, and nothing when it is run locally.
#
# Every online-only behaviour in the app hangs off one setting, so there is one
# place to look and one thing to test. run_app() resolves it once and stores
# the result; the app reads it once at startup.
#
# All of this is internal: it configures the app rather than analysing EQ-5D
# data, so it stays out of eq5dsuite::. The app and DEPLOY.md reach it through
# eq5dsuite:::.

# The defaults. Each is overridable by an R option, then by an environment
# variable, in that order of precedence.
.online_defaults <- list(
  max_upload_mb  = 10,
  max_rows       = 50000L,
  max_cols       = 200L,
  idle_minutes   = 30L,
  allowed_types  = c("csv", "xlsx", "xls")
)

# option -> environment variable -> default
.online_setting <- function(name, default, as = identity) {
  opt <- getOption(paste0("eq5dsuite.", name))
  if (!is.null(opt)) return(as(opt))
  env <- Sys.getenv(paste0("EQ5DSUITE_", toupper(name)), unset = NA_character_)
  if (!is.na(env) && nzchar(env)) return(as(env))
  default
}

.as_flag <- function(x) {
  if (is.logical(x)) return(isTRUE(x))
  isTRUE(tolower(trimws(as.character(x))) %in% c("true", "yes", "1", "on"))
}
.as_types <- function(x) {
  if (length(x) > 1L) return(tolower(trimws(as.character(x))))
  tolower(trimws(strsplit(as.character(x), "[,;[:space:]]+")[[1]]))
}

#' Settings for the app's online mode
#'
#' The Shiny app runs in one of two modes. Locally -- the default -- it behaves
#' as it always has. Deployed on a public server it limits what can be
#' uploaded, keeps every file it writes inside a per-session folder, ends idle
#' sessions, and tells the user what happens to their data.
#'
#' @details
#' Online mode is off unless it is asked for, by
#' \code{run_app(online = TRUE)} or by setting \code{EQ5DSUITE_ONLINE=true} in
#' the environment the app starts in. The argument wins over the environment
#' variable.
#'
#' Each limit can be set with an R option or an environment variable, the
#' option taking precedence:
#'
#' \tabular{lll}{
#'   \strong{setting} \tab \strong{option} \tab \strong{environment} \cr
#'   maximum upload, MB \tab \code{eq5dsuite.max_upload_mb} \tab \code{EQ5DSUITE_MAX_UPLOAD_MB} \cr
#'   maximum rows \tab \code{eq5dsuite.max_rows} \tab \code{EQ5DSUITE_MAX_ROWS} \cr
#'   maximum columns \tab \code{eq5dsuite.max_cols} \tab \code{EQ5DSUITE_MAX_COLS} \cr
#'   idle timeout, minutes \tab \code{eq5dsuite.idle_minutes} \tab \code{EQ5DSUITE_IDLE_MINUTES} \cr
#'   accepted file types \tab \code{eq5dsuite.allowed_types} \tab \code{EQ5DSUITE_ALLOWED_TYPES} \cr
#'   online mode \tab \code{eq5dsuite.online} \tab \code{EQ5DSUITE_ONLINE}
#' }
#'
#' The defaults are 10 MB, 50,000 rows, 200 columns, 30 minutes, and CSV and
#' Excel files. \code{.rds} is deliberately not accepted online: reading a
#' serialised R object from an untrusted source is not a safe operation. It
#' remains accepted locally.
#'
#' See \code{DEPLOY.md} in the installed package
#' (\code{system.file("shiny", "DEPLOY.md", package = "eq5dsuite")}) for what
#' the server itself has to be told.
#'
#' @param online Whether online mode is on. \code{NULL} (default) resolves it
#'   from the \code{eq5dsuite.online} option, then \code{EQ5DSUITE_ONLINE}.
#' @return A list: \code{enabled}, \code{max_upload_mb}, \code{max_rows},
#'   \code{max_cols}, \code{idle_minutes} and \code{allowed_types}. The limits
#'   are reported whether or not online mode is on, so a deployment can check
#'   what it would apply.
#' @seealso \code{\link{eq5d_check_upload}}, \code{\link{eq5d_online_notice}},
#'   \code{\link{run_app}}
#' @keywords internal
eq5d_online_config <- function(online = NULL) {
  enabled <- if (!is.null(online)) .as_flag(online)
             else .online_setting("online", FALSE, .as_flag)

  list(
    enabled       = enabled,
    max_upload_mb = .online_setting("max_upload_mb",
                                    .online_defaults$max_upload_mb, as.numeric),
    max_rows      = .online_setting("max_rows",
                                    .online_defaults$max_rows, as.integer),
    max_cols      = .online_setting("max_cols",
                                    .online_defaults$max_cols, as.integer),
    idle_minutes  = .online_setting("idle_minutes",
                                    .online_defaults$idle_minutes, as.integer),
    allowed_types = .online_setting("allowed_types",
                                    .online_defaults$allowed_types, .as_types)
  )
}

#' Check an uploaded dataset against the online limits
#'
#' Applied by the Shiny app after a file has been read, so the limits hold
#' whatever the browser allowed through. Returns the reason the file cannot be
#' used, in words the person who uploaded it can act on, or \code{NULL} when
#' there is nothing wrong.
#'
#' @param df The data frame that was read. May be \code{NULL} when only the
#'   file type is being checked.
#' @param type The file's type, usually its extension.
#' @param config Settings, as returned by \code{\link{eq5d_online_config}}.
#'   When \code{config$enabled} is \code{FALSE} nothing is checked and
#'   \code{NULL} is returned: the limits are an online measure.
#' @return A single string explaining the problem, or \code{NULL}.
#' @seealso \code{\link{eq5d_online_config}}
#' @keywords internal
eq5d_check_upload <- function(df, type, config = eq5d_online_config()) {
  if (!isTRUE(config$enabled)) return(NULL)

  locally <- paste0(
    "There is no limit when you run the app on your own machine: install ",
    "eq5dsuite and run eq5dsuite::run_app().")

  type <- tolower(trimws(as.character(type)))
  if (length(type) != 1L || is.na(type) || !nzchar(type) ||
      !type %in% config$allowed_types) {
    return(paste0(
      "This app accepts ",
      paste(toupper(config$allowed_types), collapse = ", "),
      " files", if (identical(type, "rds"))
        paste0(". Reading a saved R object from an uploaded file is not safe ",
               "on a shared server, so .rds is accepted only locally")
      else "",
      ". ", locally))
  }

  if (is.null(df)) return(NULL)
  if (!is.data.frame(df))
    return(paste0("That file did not read as a table of data. ", locally))

  if (nrow(df) > config$max_rows)
    return(paste0(
      "This file has ", format(nrow(df), big.mark = ","), " rows. The online ",
      "app accepts up to ", format(config$max_rows, big.mark = ","),
      ", so that it stays responsive for everyone using it. ", locally))

  if (ncol(df) > config$max_cols)
    return(paste0(
      "This file has ", format(ncol(df), big.mark = ","), " columns. The ",
      "online app accepts up to ", format(config$max_cols, big.mark = ","),
      ". ", locally))

  NULL
}

#' What the app tells users about their data online
#'
#' The wording of the notices shown in online mode, in one place so that it can
#' be changed without touching the app. Each can be overridden with an option,
#' which is how a deployment adopts its own organisation's wording.
#'
#' @param which Which notice: \code{"data"} (what happens to an uploaded
#'   dataset), \code{"upload"} (the short line beside the file picker), or
#'   \code{"local"} (how to run the app privately).
#' @return A single string.
#' @details
#' Set \code{options(eq5dsuite.online.notice.data = "...")}, and likewise
#' \code{...notice.upload} and \code{...notice.local}, to replace the wording.
#' @seealso \code{\link{eq5d_online_config}}
#' @keywords internal
eq5d_online_notice <- function(which = c("data", "upload", "local")) {
  which <- match.arg(which)
  override <- getOption(paste0("eq5dsuite.online.notice.", which))
  if (!is.null(override)) return(as.character(override))

  switch(which,
    # Accurate about the disk (review I05): Shiny writes an upload to a
    # temporary file on the server, and the Word report and the archive are
    # built in temporary files too. The app keeps no copy and deletes them
    # when the session ends.
    data = paste0(
      "Your data are processed only for this session. While you work, the ",
      "file you upload and any report you download are held in temporary ",
      "files on the server; they are deleted when the session ends or times ",
      "out, and no copy is kept. Please upload de-identified data only."),
    upload = paste0(
      "Processed for this session only: held in temporary files on the ",
      "server and deleted when the session ends. Please upload ",
      "de-identified data only."),
    local = paste0(
      "For sensitive or large datasets, run the app on your own machine: ",
      "install eq5dsuite and run eq5dsuite::run_app(). Nothing leaves your ",
      "computer, and no limits apply.")
  )
}

# Is this call happening inside an app that is running online? Used by the
# package functions that must not act on a public server.
.online_now <- function() isTRUE(getOption("eq5dsuite.online.active", FALSE))

# Resolve the mode and put the settings in force. Returns the config and the
# options as they were, for a caller that wants to restore them.
.online_begin <- function(online) {
  config <- eq5d_online_config(online = online)
  opts <- list(eq5dsuite.online.config = config,
               eq5dsuite.online.active = config$enabled)
  if (isTRUE(config$enabled)) {
    # Shiny refuses a larger upload before a byte reaches the app.
    opts$shiny.maxRequestSize <- config$max_upload_mb * 1024^2
    # An uncaught error reaches the browser as a generic message; the detail
    # stays in the log, where the data in it cannot reach another user.
    opts$shiny.sanitize.errors <- TRUE
  }
  list(config = config, old = options(opts))
}

#' The app as an object, for a server to run
#'
#' Returns the Shiny application rather than starting it, which is what a
#' server such as Shiny Server, Posit Connect or shinyapps.io needs: each runs
#' the application directory itself, so an \code{app.R} that called
#' \code{\link{run_app}} would be a \code{runApp()} inside a
#' \code{runApp()}, which Shiny refuses.
#'
#' Use \code{\link{run_app}} to start the app on your own machine, and this
#' to deploy it.
#'
#' @details
#' A whole \code{app.R} for a server is one line:
#'
#' \preformatted{
#' eq5dsuite:::eq5d_app(online = TRUE)
#' }
#'
#' Three colons, because \code{eq5d_app()} is internal: with two the server
#' cannot find it. \code{DEPLOY.md} says the same.
#'
#' The settings this puts in force -- the upload limit and error sanitising --
#' stay in force for the R process that serves the app, which is what a
#' deployment wants. \code{run_app()} restores them when it returns, which is
#' what an interactive session wants.
#'
#' See \code{DEPLOY.md} in the installed package
#' (\code{system.file("shiny", "DEPLOY.md", package = "eq5dsuite")}).
#'
#' @param online Whether to run in online mode. \code{NULL} (default) takes it
#'   from the \code{eq5dsuite.online} option and then \code{EQ5DSUITE_ONLINE}.
#'   A deployment should pass \code{TRUE} rather than rely on the environment.
#' @return A Shiny application object, as \code{\link[shiny]{shinyAppDir}}
#'   returns.
#' @seealso \code{\link{run_app}}, \code{\link{eq5d_online_config}}
#' @keywords internal
eq5d_app <- function(online = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE))
    stop("Package 'shiny' is required to run the app.", call. = FALSE)

  app_dir <- system.file("shiny", package = "eq5dsuite")
  if (!nzchar(app_dir))
    stop("Could not find the bundled Shiny app in the installed eq5dsuite ",
         "package. Please reinstall eq5dsuite and try again.", call. = FALSE)

  # Deliberately not restored: these must hold for as long as the process
  # serves the app.
  .online_begin(online)

  shiny::shinyAppDir(app_dir)
}
