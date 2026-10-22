#' Launch the eq5dsuite Shiny Application
#'
#' Opens an interactive Shiny application for uploading, validating, analysing
#' and exporting EQ-5D data.
#'
#' @details
#' The app has seven pages, in the order the work is done:
#'
#' \describe{
#'   \item{Home}{What the app does, what data it needs, and a button to load
#'     the bundled \code{\link{example_data}}.}
#'   \item{Data}{Upload a \code{.csv}, \code{.xlsx} or \code{.rds} file, set
#'     the instrument version, and select its variables ("Confirm
#'     variables"). Only the five EQ-5D
#'     dimensions are required; a timepoint, patient ID, group, EQ VAS score,
#'     age, sex and an existing EQ-5D value column are each optional and each
#'     unlock further analyses.}
#'   \item{Validation}{Range, missingness and duplicate-ID checks, then
#'     "Proceed", which fixes the working dataset.}
#'   \item{Calculate EQ-5D values}{Turn the responses into values and append
#'     them as a column. One Method selector covers every way the package has
#'     of doing it: a published value set directly, a crosswalk
#'     (\code{\link{eqxw}}, \code{\link{eqxwr}}), or the NICE Decision
#'     Support Unit's UK mapping (\code{\link{eqxw_UK}},
#'     \code{\link{eqxwr_UK}}). The direction of a mapping follows the
#'     instrument version and is stated in the method's label and on the page:
#'     EQ-5D-3L responses give EQ-5D-5L values, and EQ-5D-5L responses give
#'     EQ-5D-3L values.
#'
#'     The DSU mapping depends on the respondent's age and sex as well as the
#'     health state, so choosing it asks for those columns. Where the age
#'     column holds bands rather than exact ages, the midpoint of each band is
#'     used and the page says so: the DSU's age bands begin at 35, 45, 55 and
#'     65, which are exactly the midpoints of ten-year bands, so a band
#'     straddling a boundary is placed in the upper one. Supply exact ages
#'     where they are available.
#'
#'     Only the EQ-5D value analyses need a value column; the profile and EQ
#'     VAS analyses can be run without one. Several columns can be added and
#'     compared.
#'
#'     The working dataset can be downloaded from here, as CSV, XLSX or RDS,
#'     with every column added during the session. EQ-5D values are shown to
#'     three decimal places throughout the app, but written at full precision.}
#'   \item{Analysis}{All the analysis functions of Devlin et al. (2020), in
#'     one place. Pick a component (EQ-5D profiles, EQ-5D values or EQ VAS),
#'     then an output, then Run -- or open "See catalogue of available
#'     analyses", which lists, in one tab per component, only what the data
#'     can support: a plain description of each and an example of its output
#'     on the bundled example data, with "Select this". An output whose variables are not selected says
#'     what it needs rather than failing. The EQ-5D value analyses take a
#'     Utility column, offering every value column in the dataset; the chosen
#'     one is passed to the analysis as \code{name_utility} and nothing is
#'     recalculated.}
#'   \item{Results and export}{Every analysis that has been run, numbered,
#'     each with controls to move it up or down or remove it, its own
#'     downloads (CSV for a table, PNG and PDF for a figure), and "View",
#'     which shows the table or figure and the \code{eq5dsuite} code that
#'     produced it. That order is the export order for all three exports: a
#'     Word report, one section per result with its title and its table
#'     or figure; an archive of CSV tables and PNG figures with a
#'     \code{manifest.csv}; and an R script
#'     that repeats the whole session. The Word report uses the template
#'     bundled with the package and needs Pandoc, which ships with RStudio.
#'
#'     The script loads the data, runs the same checks, calculates the same
#'     EQ-5D values and produces the same results in the same order. Apart
#'     from the analysis functions themselves it calls nothing from
#'     \code{eq5dsuite}: reading the file, renaming the columns, the checks,
#'     the age-band midpoints and the display formatting are all written out
#'     in the script, so it can be read, changed and run wherever eq5dsuite is
#'     installed, with no part of the app involved. It is built from a
#'     structured record of what was done, so every value is quoted safely.}
#' }
#'
#' @section Running it on a server:
#' \code{run_app(online = TRUE)}, or \code{EQ5DSUITE_ONLINE=true} in the
#' environment the app starts in, puts the app in online mode. That limits what
#' can be uploaded, keeps every file the app writes inside a folder of its own
#' that is deleted when the session ends, ends idle sessions, sanitises errors,
#' and tells the user what happens to their data. Locally -- the default --
#' none of it applies and the app behaves as it always has.
#'
#' The limits and the wording are set with options or environment variables.
#' \code{DEPLOY.md} in the installed package
#' (\code{system.file("shiny", "DEPLOY.md", package = "eq5dsuite")}) covers
#' what the server itself has to be told.
#'
#' The app does not attach eq5dsuite, so it needs no \code{library()} call
#' first; \code{run_app()} is enough.
#'
#' @seealso \code{\link{eq5d}} and \code{\link{eqxwr_UK}} for the EQ-5D
#'   values the app calculates, and \code{\link{eq5d_profile_level_summary}}
#'   for the analyses it runs.
#'
#' @param online Whether to run in online mode, for a public deployment.
#'   \code{NULL} (default) takes it from the \code{eq5dsuite.online} option
#'   and then the \code{EQ5DSUITE_ONLINE} environment variable, both of which
#'   default to off. The limits it puts in force are listed in
#'   \code{DEPLOY.md}.
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}},
#'   such as \code{port} or \code{launch.browser}.
#' @return Called for its side effect of launching a Shiny application.
#'   Returns invisibly.
#' @references
#' Devlin N, Parkin D, Janssen B (2020).
#' \emph{Methods for Analyzing and Reporting EQ-5D Data}. Springer.
#' \doi{10.1007/978-3-030-47622-9}
#' @seealso \code{\link{eq5d}} for value calculation,
#'   \code{\link{eqxwr_UK}} and \code{\link{eqxw_UK}} for the UK mapping.
#' @export
#' @examples
#' \dontrun{
#'   # Locally.
#'   eq5dsuite::run_app()
#'
#'   # On a server, with the defaults.
#'   eq5dsuite::run_app(online = TRUE)
#'
#'   # On a server, with its own limits.
#'   options(eq5dsuite.max_rows = 20000, eq5dsuite.idle_minutes = 15)
#'   eq5dsuite::run_app(online = TRUE)
#' }
run_app <- function(online = NULL, ...) {
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

  # Resolve the mode once, here, and leave it where the app and the package
  # functions that must behave differently online can both read it. An
  # interactive session gets its options back when the app stops.
  started <- .online_begin(online)
  on.exit(options(started$old), add = TRUE)

  if (isTRUE(started$config$enabled))
    message("eq5dsuite: online mode. Uploads limited to ",
            started$config$max_upload_mb, " MB, ",
            format(started$config$max_rows, big.mark = ","), " rows and ",
            started$config$max_cols, " columns; idle sessions end after ",
            started$config$idle_minutes, " minutes.")

  shiny::runApp(app_dir, ...)
}
