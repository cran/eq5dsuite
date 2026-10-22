#' @keywords internal
.onAttach <- function(libname, pkgname) {

  # Check if a value set update is due
  # Uses cached date — no internet request is made here
  tryCatch({
    if (is_update_due(threshold_days = 60)) {
      packageStartupMessage(
        "eq5dsuite: Value sets were last checked more than ",
        "2 months ago.\n",
        "Run eq5dsuite::update_value_sets() to check for ",
        "new value sets."
      )
    }
  },
  error = function(e) {
    # Fail silently — never block package loading
    invisible(NULL)
  })
}
