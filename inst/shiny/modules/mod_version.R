# mod_version.R — the package version in the navbar, and a newer one on CRAN
#
# "eq5dsuite vX", with X read from the installed package. When CRAN has a
# higher version, a small icon follows it; clicking the icon says which.
#
# The check is eq5dsuite:::.cran_check_start() / .cran_check_poll() (see
# R/cran_check.R): asynchronous, once per process and shared by all sessions,
# and silent when it fails -- the label then shows the version alone, which
# claims nothing about being up to date. options(eq5dsuite.check_cran = FALSE)
# turns it off.

app_version <- function() as.character(utils::packageVersion("eq5dsuite"))

mod_version_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$span(
    class = "nav-version",
    shiny::tags$a(class = "nav-ext",
                  href = "https://github.com/MathsInHealth/eq5dsuite",
                  target = "_blank",
                  paste0("eq5dsuite v", app_version())),
    shiny::uiOutput(ns("update"), inline = TRUE))
}

# The sentence shown when a newer version is available.
update_message <- function(installed, cran) {
  paste0("You are using eq5dsuite version ", installed, ". Version ", cran,
         " is available on CRAN.")
}

mod_version_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    status <- shiny::reactiveVal(eq5dsuite:::.cran_update_status())

    if (isTRUE(getOption("eq5dsuite.check_cran", TRUE))) {
      eq5dsuite:::.cran_check_start()
      # Advance the request a step at a time until it is answered; nothing
      # here waits on the network. Once answered, the observer stops asking.
      shiny::observe({
        st <- eq5dsuite:::.cran_check_poll()
        status(eq5dsuite:::.cran_update_status())
        if (identical(st, "pending")) shiny::invalidateLater(500)
      })
    }

    output$update <- shiny::renderUI({
      s <- status()
      if (!isTRUE(s$newer)) return(NULL)
      shiny::actionLink(
        ns("info"), label = NULL, icon = shiny::icon("circle-arrow-up"),
        class = "nav-update",
        title = "A newer version of eq5dsuite is available",
        `aria-label` = "A newer version of eq5dsuite is available")
    })

    shiny::observeEvent(input$info, {
      s <- status()
      shiny::req(isTRUE(s$newer))
      shiny::showModal(shiny::modalDialog(
        title = "A newer version is available",
        shiny::p(update_message(s$installed, s$cran)),
        shiny::p(class = "hint",
                 "Update with install.packages(\"eq5dsuite\")."),
        easyClose = TRUE, footer = shiny::modalButton("Close")))
    })
  })
}
