# mod_home.R — landing page
#
# Three things only: what the app does, what data it needs, and how to start.
# One centred column, no card chrome.

mod_home_ui <- function(id) {
  ns <- shiny::NS(id)

  need_row <- function(col, adds) {
    shiny::tags$tr(shiny::tags$td(shiny::tags$strong(col)),
                   shiny::tags$td(adds))
  }

  shiny::div(
    class = "home",

    shiny::h1("eq5dsuite", class = "home-title"),
    shiny::p("Analyse EQ-5D data and export the results", class = "home-sub"),

    shiny::p(
      class = "home-lede",
      "Upload a dataset of EQ-5D responses and select its variables once. The app ",
      "then produces the analyses set out in Devlin, Parkin and Janssen ",
      "(2020), ",
      shiny::tags$em("Methods for Analyzing and Reporting EQ-5D Data"),
      " — health profiles, EQ-5D values and the EQ VAS — for the ",
      "EQ-5D-3L, EQ-5D-5L and EQ-5D-Y-3L. It calculates EQ-5D values from ",
      "any published value set, and maps between the EQ-5D-3L and ",
      "EQ-5D-5L. Every result is shown alongside the ",
      shiny::code("eq5dsuite", .noWS = "outside"),
      " R code that produced it, so any analysis ",
      "can be reproduced outside the app."
    ),

    # Online only: what happens to an uploaded dataset, and how to avoid
    # uploading one at all.
    online_note("data"),
    online_note("local"),

    shiny::h2("What your data needs", class = "home-h2"),
    shiny::p(
      "One row per observation, as ",
      shiny::code(".csv", .noWS = "outside"), ", ",
      shiny::code(".xlsx", .noWS = "outside"), " or ",
      shiny::code(".rds", .noWS = "outside"), "."
    ),
    shiny::p(
      shiny::tags$strong("Required"), " — the five EQ-5D dimensions: ",
      "mobility, self-care, usual activities, pain/discomfort and ",
      "anxiety/depression, as EQ-5D-3L or EQ-5D-Y-3L (levels 1–3), or ",
      "EQ-5D-5L (levels 1–5)."
    ),
    shiny::p(shiny::tags$strong("Optional", .noWS = "outside"),
             ", each one unlocking more analyses:"),
    shiny::tags$table(
      class = "home-needs",
      shiny::tags$tbody(
        need_row("Timepoint",
                 "change over time, PCHC, level sum scores, level frequency scores"),
        need_row("Patient ID",
                 "paired analyses of the same respondent across timepoints"),
        need_row("Group", "any analysis split by subgroup"),
        need_row("EQ VAS", "the EQ VAS analyses"),
        need_row("Age and sex",
                 "the UK EQ-5D-3L ↔ EQ-5D-5L mapping"),
        need_row("An existing value or index column",
                 "otherwise values can be calculated in the app")
      )
    ),

    shiny::div(
      class = "home-start",
      shiny::actionButton(ns("goto_data"), "Upload your data",
                          class = "btn-primary btn-lg",
                          icon = shiny::icon("arrow-right")),
      shiny::div(
        class = "home-start-example",
        shiny::actionButton(ns("load_example"), "Load the example dataset",
                            class = "btn-outline-secondary btn-lg"),
        shiny::p(
          class = "hint",
          "10,000 NHS PROMs records — 5,000 patients measured before and ",
          "after hip replacement, knee replacement, groin hernia or varicose ",
          "vein surgery, with EQ-5D-3L responses, an EQ VAS score, and age ",
          "band and sex for 9,030 of them."
        )
      )
    ),

    shiny::hr(),
    shiny::p(
      class = "home-foot",
      shiny::textOutput(ns("version"), inline = TRUE),
      " · Analyses follow Devlin NJ, Parkin D, Janssen B (2020), ",
      shiny::tags$em("Methods for Analyzing and Reporting EQ-5D Data"),
      ", Springer, ",
      shiny::tags$a(href = "https://doi.org/10.1007/978-3-030-47622-9",
                    target = "_blank", "doi:10.1007/978-3-030-47622-9")
    )
  )
}

mod_home_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    output$version <- shiny::renderText(
      paste("eq5dsuite", as.character(utils::packageVersion("eq5dsuite")))
    )

    shiny::observeEvent(input$goto_data, goto_page(session, "data"))

    # The data are loaded by the Data module, not here: bump a counter it
    # observes, so there is only one copy of the loading code.
    shiny::observeEvent(input$load_example, {
      rv$load_example <- (rv$load_example %||% 0L) + 1L
      goto_page(session, "data")
    })
  })
}
