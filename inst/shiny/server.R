# server.R — main server function

server <- function(input, output, session) {

  # ── Shared reactive state ─────────────────────────────────────────────────
  rv <- shiny::reactiveValues(
    raw_data       = NULL,   # data.frame as uploaded
    mapping        = NULL,   # named list of column mapping + version + country
    processed_data = NULL,   # data.frame with standardised column names
    results        = list()  # list of saved analysis results
  )

  # ── Initialise: hide conditional analysis tabs ─────────────────────────────
  shiny::hideTab("analysis_main_tabs", "tab_vas",     session = session)
  shiny::hideTab("analysis_main_tabs", "tab_utility", session = session)

  # ── Module servers ─────────────────────────────────────────────────────────
  mod_data_server("data",             rv)
  mod_validation_server("validation", rv)
  mod_analysis_profile_server("profile", rv)
  mod_analysis_vas_server("vas",         rv)
  mod_analysis_utility_server("utility", rv)
  mod_results_server("results", rv)
  mod_export_server("export",   rv)

  # ── Conditional analysis tab visibility ────────────────────────────────────
  shiny::observe({
    mapping <- rv$mapping
    if (is.null(mapping)) return()

    # EQ-VAS tab: show when VAS is mapped
    has_vas <- !is.null(mapping$name_vas) && nzchar(mapping$name_vas)
    if (has_vas) {
      shiny::showTab("analysis_main_tabs", "tab_vas", session = session)
    } else {
      shiny::hideTab("analysis_main_tabs", "tab_vas", session = session)
    }

    # Utility tab: show when utility column is mapped OR was calculated on
    # the Data page (rv$mapping$name_utility updated by mod_data_server).
    has_utility <- !is.null(mapping$name_utility) && nzchar(mapping$name_utility)
    if (has_utility) {
      shiny::showTab("analysis_main_tabs", "tab_utility", session = session)
    } else {
      shiny::hideTab("analysis_main_tabs", "tab_utility", session = session)
    }
  })

  # ── Provide flags to conditionalPanel (suspendWhenHidden = FALSE required) ─
  output$app_has_data      <- shiny::reactive(!is.null(rv$processed_data))
  output$app_has_timepoint <- shiny::reactive(
    !is.null(rv$mapping) && !is.null(rv$mapping$name_fu) && nzchar(rv$mapping$name_fu)
  )
  output$app_has_group <- shiny::reactive(
    !is.null(rv$mapping) &&
      !is.null(rv$mapping$name_groupvar) && nzchar(rv$mapping$name_groupvar)
  )
  output$app_has_id <- shiny::reactive(
    !is.null(rv$mapping) &&
      !is.null(rv$mapping$name_id) && nzchar(rv$mapping$name_id)
  )
  shiny::outputOptions(output, "app_has_data",      suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "app_has_timepoint", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "app_has_group",     suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "app_has_id",        suspendWhenHidden = FALSE)
}
