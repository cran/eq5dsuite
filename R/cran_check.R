# Is a newer eq5dsuite on CRAN? For the Shiny app's version label.
#
# The check must never hold up the interface and must not contact CRAN over
# and over, so:
#
# - It is asynchronous. .cran_check_start() queues one request on a curl pool
#   and returns at once; .cran_check_poll() advances it without waiting. The
#   app calls the poll from a timer until the answer is in.
# - There is one check per R process, shared by every session in it. A
#   success is kept for a day and a failure for an hour before CRAN is asked
#   again, and a check already under way is never started twice.
# - It fails quietly. With no connection, a timeout, an HTTP error or a
#   DESCRIPTION without a usable Version, the state is "failed" and the app
#   shows nothing. It never says the package is up to date, because a failed
#   check does not know that.
#
# CRAN's per-package DESCRIPTION is a few kilobytes, rather than the whole
# package index.

.cran_description_url <- paste0(
  "https://cloud.r-project.org/web/packages/eq5dsuite/DESCRIPTION")

# How long an answer is kept, in seconds.
.CRAN_TTL_OK     <- 24 * 60 * 60
.CRAN_TTL_FAILED <- 60 * 60

# One state per process. Fields: state ("idle", "pending", "done",
# "failed"), cran (a package_version or NULL), checked (POSIXct), pool.
.cran_state <- new.env(parent = emptyenv())
.cran_state$state <- "idle"

# Reset, for tests.
.cran_check_reset <- function() {
  rm(list = ls(.cran_state, all.names = TRUE), envir = .cran_state)
  .cran_state$state <- "idle"
  invisible(NULL)
}

# The request and the pump, separate so the tests can replace them.
.cran_request <- function(url, done, fail, pool) {
  h <- curl::new_handle(timeout = 10, connecttimeout = 5)
  curl::curl_fetch_multi(url, done = done, fail = fail, pool = pool,
                         handle = h)
}
.cran_pump <- function(pool) {
  curl::multi_run(timeout = 0, poll = FALSE, pool = pool)
}

# The Version field of a DESCRIPTION, as a package_version, or an error.
.parse_cran_version <- function(text) {
  d <- tryCatch(read.dcf(textConnection(text), fields = c("Package", "Version")),
                error = function(e) NULL)
  if (is.null(d) || nrow(d) < 1L || is.na(d[1L, "Version"]))
    stop("no Version field", call. = FALSE)
  if (!identical(unname(d[1L, "Package"]), "eq5dsuite"))
    stop("not the eq5dsuite DESCRIPTION", call. = FALSE)
  package_version(unname(d[1L, "Version"]))
}

.cran_fresh <- function(now = Sys.time()) {
  st <- .cran_state$state
  if (!st %in% c("done", "failed") || is.null(.cran_state$checked))
    return(FALSE)
  ttl <- if (identical(st, "done")) .CRAN_TTL_OK else .CRAN_TTL_FAILED
  as.numeric(difftime(now, .cran_state$checked, units = "secs")) < ttl
}

# Start a check unless one is under way or a recent answer is in hand.
# Returns the state, invisibly. Never waits for the network.
.cran_check_start <- function(now = Sys.time()) {
  if (identical(.cran_state$state, "pending") || .cran_fresh(now))
    return(invisible(.cran_state$state))
  finish <- function(state, cran = NULL) {
    .cran_state$state   <- state
    .cran_state$cran    <- cran
    .cran_state$checked <- Sys.time()
    .cran_state$pool    <- NULL
  }
  pool <- curl::new_pool()
  .cran_state$state <- "pending"
  .cran_state$pool  <- pool
  ok <- tryCatch({
    .cran_request(
      .cran_description_url,
      done = function(res) {
        v <- if (identical(as.integer(res$status_code), 200L))
          tryCatch(.parse_cran_version(rawToChar(res$content)),
                   error = function(e) NULL)
        if (is.null(v)) finish("failed") else finish("done", v)
      },
      fail = function(msg) finish("failed"),
      pool = pool)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) finish("failed")
  invisible(.cran_state$state)
}

# Advance a pending check without waiting. Returns the state.
.cran_check_poll <- function() {
  if (identical(.cran_state$state, "pending") && !is.null(.cran_state$pool)) {
    tryCatch(.cran_pump(.cran_state$pool), error = function(e) {
      .cran_state$state   <- "failed"
      .cran_state$checked <- Sys.time()
      .cran_state$pool    <- NULL
    })
  }
  .cran_state$state
}

#' Whether a newer eq5dsuite is on CRAN, as far as is known
#'
#' @param installed The installed version.
#' @return A list: \code{state} ("idle", "pending", "done" or "failed"),
#'   \code{installed}, \code{cran} (\code{NULL} unless known) and
#'   \code{newer} (\code{TRUE} only when CRAN's version is known and higher).
#' @keywords internal
#' @noRd
.cran_update_status <- function(installed = utils::packageVersion("eq5dsuite")) {
  cran <- .cran_state$cran
  installed <- package_version(installed)
  list(state = .cran_state$state, installed = installed, cran = cran,
       newer = identical(.cran_state$state, "done") && !is.null(cran) &&
               isTRUE(cran > installed))
}
