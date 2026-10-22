# server.R — main server function

server <- function(input, output, session) {

  # ── Shared reactive state ─────────────────────────────────────────────────
  rv <- shiny::reactiveValues(
    raw_data       = NULL,   # data.frame as uploaded
    mapping        = NULL,   # named list of column mapping + version
    processed_data = NULL,   # data.frame with standardised column names
    results        = list(), # list of saved analysis results
    steps          = list(), # structured record of the session, for the script
    value_cols     = character(0), # EQ-5D value columns in processed_data
    load_example   = 0L,     # bumped by the Home page's example button
    revision       = 0L      # moved on whenever the analysed data change
  )

  # ── Module servers ─────────────────────────────────────────────────────────
  mod_version_server("version")
  mod_home_server("home",             rv)
  mod_data_server("data",             rv)
  mod_validation_server("validation", rv)
  mod_values_server("values",         rv)
  mod_analysis_server("analysis",     rv)
  mod_export_server("export",         rv)   # the Results and export page

  # ── Online only ─────────────────────────────────────────────────────────
  if (ONLINE$enabled) {
    # Everything this session wrote goes when the session does.
    session$onSessionEnded(function() {
      d <- session$userData$eq5d_dir
      if (!is.null(d) && dir.exists(d)) unlink(d, recursive = TRUE, force = TRUE)
    })

    # Idle sessions end, with a minute's warning first, because closing one
    # loses the results it holds.
    idle_warn_after <- (ONLINE$idle_minutes - 1) * 60
    shiny::observeEvent(input$eq5d_idle_warning, {
      shiny::showModal(shiny::modalDialog(
        title = "Still there?",
        shiny::p("This session will end in about a minute, to free the server ",
                 "for other people."),
        shiny::p(shiny::strong("Your results are not saved."), " Anything you ",
                 "have run is lost when the session ends. Download what you ",
                 "need from the Results and export page first \u2014 the Word report, the ",
                 "archive, or the R script."),
        footer = shiny::tagList(
          shiny::modalButton("Close"),
          shiny::actionButton("eq5d_stay", "Stay connected",
                              class = "btn-primary")),
        easyClose = FALSE))
    })
    shiny::observeEvent(input$eq5d_stay, {
      shiny::removeModal()
      session$sendCustomMessage("eq5d_idle_reset", list())
    })

    # The watch itself lives in the browser: the server cannot see a user
    # reading a table.
    session$sendCustomMessage("eq5d_idle_start", list(
      warnAfter = idle_warn_after,
      endAfter  = ONLINE$idle_minutes * 60))
  }
}
