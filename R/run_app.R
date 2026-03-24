#' Launch the eq5dsuite Shiny Application
#'
#' Opens an interactive Shiny application for uploading, processing,
#' analysing, and exporting EQ-5D data using the eq5dsuite package.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}},
#'   such as \code{port} or \code{launch.browser}.
#' @return Called for its side effect of launching a Shiny application.
#'   Returns invisibly.
#' @export
#' @examples
#' \dontrun{
#'   eq5dsuite::run_app()
#' }
run_app <- function(...) {
  app_packages <- c("shiny", "bslib", "DT", "readxl")
  
  missing_packages <- app_packages[
    !vapply(app_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  
  if (length(missing_packages) > 0) {
    stop(
      paste0(
        "To run the eq5dsuite Shiny app, please install the following package",
        if (length(missing_packages) > 1) "s" else "",
        ":\n\n",
        paste0("  - ", missing_packages, collapse = "\n"),
        "\n\nYou can install them with:\n\n",
        "install.packages(c(",
        paste(sprintf('"%s"', missing_packages), collapse = ", "),
        "))"
      ),
      call. = FALSE
    )
  }
  
  app_dir <- system.file("shiny", package = "eq5dsuite")
  
  if (!nzchar(app_dir)) {
    stop(
      "Could not find the bundled Shiny app in the installed eq5dsuite package. ",
      "Please reinstall eq5dsuite and try again.",
      call. = FALSE
    )
  }
  
  shiny::runApp(app_dir, ...)
}
