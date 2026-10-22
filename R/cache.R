#' Get the eq5dsuite cache directory
#'
#' Returns the path to the user-specific cache directory used by
#' eq5dsuite to store the date of the last value set check.
#' The directory is created if it does not exist, unless \code{create = FALSE}.
#'
#' @param create Create the directory if it does not exist. Only callers that
#'   are about to write pass \code{TRUE}: reading a file that is not there
#'   needs no directory, and creating one at load time would leave a trace in
#'   the user's home for nothing.
#' @return A character string giving the path to the cache directory.
#' @keywords internal
get_cache_dir <- function(create = TRUE) {
  # The package environment's `cache_path`, where it has been set, so that a
  # caller which has redirected the cache -- the test suite, through
  # local_eq_env() -- redirects this too. Without it, last_checked.txt and
  # applied_migrations.rds were written to the real user cache whatever the
  # test did, which no test can be allowed to do.
  pkgenv <- getOption("eq.env")
  dir <- if (!is.null(pkgenv) && is.environment(pkgenv) &&
             !is.null(pkgenv$cache_path) && nzchar(pkgenv$cache_path))
           pkgenv$cache_path
         else find_cache_dir("eq5dsuite")
  if (isTRUE(create) && !dir.exists(dir))
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

#' Get the date of the last value set check
#'
#' Reads the cached date of the last value set update check from the
#' user cache directory. Returns \code{1970-01-01} if no cache file
#' exists (i.e., the check has never been run).
#'
#' @return A \code{Date} object.
#' @keywords internal
get_last_checked <- function() {
  cache_file <- file.path(get_cache_dir(create = FALSE), "last_checked.txt")
  if (!file.exists(cache_file)) return(as.Date("1970-01-01"))
  as.Date(readLines(cache_file, warn = FALSE)[1])
}

#' Set the date of the last value set check
#'
#' Writes the given date to the user cache directory so that future
#' calls to \code{get_last_checked()} and \code{is_update_due()} can
#' determine how long ago the check was performed.
#'
#' @param date A \code{Date} object. Defaults to \code{Sys.Date()}.
#' @return Invisibly returns \code{date}.
#' @keywords internal
set_last_checked <- function(date = Sys.Date()) {
  cache_file <- file.path(get_cache_dir(), "last_checked.txt")
  writeLines(format(date, "%Y-%m-%d"), cache_file)
  invisible(date)
}

#' Check whether a value set update check is due
#'
#' Compares the date of the last value set check (from the user cache)
#' against \code{Sys.Date()}. Returns \code{TRUE} if more than
#' \code{threshold_days} days have elapsed, indicating that the user
#' should be prompted to run \code{update_value_sets()}.
#'
#' @param threshold_days A single positive integer giving the number of
#'   days after which an update check is considered overdue. Defaults
#'   to \code{60}.
#' @return A single logical value.
#' @keywords internal
is_update_due <- function(threshold_days = 60) {
  as.numeric(Sys.Date() - get_last_checked()) > threshold_days
}
