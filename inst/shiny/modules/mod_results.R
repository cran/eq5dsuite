# mod_results.R — Results explorer module

# ── UI ────────────────────────────────────────────────────────────────────────

mod_results_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(4, 8),

      # Left: results list
      bslib::card(
        bslib::card_header("Saved Results"),
        bslib::card_body(
          shiny::uiOutput(ns("results_list"))
        )
      ),

      # Right: result viewer
      bslib::card(
        bslib::card_header(shiny::uiOutput(ns("viewer_title"))),
        bslib::card_body(
          shiny::uiOutput(ns("result_viewer"))
        )
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_results_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Currently selected result id
    selected_id <- shiny::reactiveVal(NULL)

    # ── Results list ──────────────────────────────────────────────────────────
    output$results_list <- shiny::renderUI({
      results <- rv$results
      if (length(results) == 0L) {
        return(shiny::p("No results saved yet. Run analyses and they will appear here.",
                        class = "text-muted small"))
      }

      # Build a button list (most recent first)
      btns <- lapply(rev(results), function(r) {
        icon_name <- switch(r$result_type,
                            table = "table",
                            plot  = "chart-bar",
                            both  = "layer-group",
                            "file")
        shiny::actionButton(
          ns(paste0("view_", r$id)),
          label = shiny::tagList(
            shiny::icon(icon_name), " ",
            r$label,
            shiny::tags$br(),
            shiny::tags$small(
              class = "text-muted",
              format(r$timestamp, "%H:%M:%S %d %b %Y")
            )
          ),
          class = "btn btn-outline-secondary w-100 text-start mb-1",
          style = "white-space: normal;"
        )
      })
      shiny::div(btns)
    })

    # Observe click on any result button
    shiny::observe({
      results <- rv$results
      lapply(results, function(r) {
        shiny::observeEvent(input[[paste0("view_", r$id)]], {
          selected_id(r$id)
        }, ignoreInit = TRUE)
      })
    })

    # ── Viewer title ──────────────────────────────────────────────────────────
    output$viewer_title <- shiny::renderUI({
      id  <- selected_id()
      res <- find_result(rv$results, id)
      if (is.null(res)) return("Result Viewer")
      shiny::tagList(res$label,
                     shiny::tags$small(
                       class = "text-muted ms-2",
                       format(res$timestamp, "%d %b %Y %H:%M")
                     ))
    })

    # ── Result viewer ─────────────────────────────────────────────────────────
    output$result_viewer <- shiny::renderUI({
      id  <- selected_id()
      res <- find_result(rv$results, id)

      if (is.null(res)) {
        return(shiny::p("Select a result from the list on the left.",
                        class = "text-muted"))
      }

      content <- list()

      # Show table
      if (res$result_type %in% c("table", "both") && !is.null(res$data)) {
        content <- c(content, list(
          shiny::h6("Table"),
          DT::DTOutput(ns("viewer_table"))
        ))
      }

      # Show plot
      if (res$result_type %in% c("plot", "both") && !is.null(res$plot)) {
        content <- c(content, list(
          shiny::h6("Plot"),
          shiny::plotOutput(ns("viewer_plot"), height = "420px")
        ))
      }

      # Show R call
      content <- c(content, list(
        shiny::hr(),
        shiny::tags$details(
          shiny::tags$summary(shiny::tags$small("R code used", class = "text-muted")),
          shiny::verbatimTextOutput(ns("viewer_call"), placeholder = TRUE)
        )
      ))

      shiny::tagList(content)
    })

    # Render selected result table
    output$viewer_table <- DT::renderDT({
      id  <- selected_id()
      res <- find_result(rv$results, id)
      shiny::req(res, !is.null(res$data))
      DT::datatable(res$data,
                    options  = list(pageLength = 15L, scrollX = TRUE, dom = "tip"),
                    rownames = FALSE,
                    class    = "table-sm table-striped")
    })

    # Render selected result plot
    output$viewer_plot <- shiny::renderPlot({
      id  <- selected_id()
      res <- find_result(rv$results, id)
      shiny::req(res, !is.null(res$plot))
      res$plot
    })

    # Render selected result R call
    output$viewer_call <- shiny::renderText({
      id  <- selected_id()
      res <- find_result(rv$results, id)
      shiny::req(res)
      res$fn_call
    })
  })
}

# ── Helper ────────────────────────────────────────────────────────────────────

find_result <- function(results, id) {
  if (is.null(id) || length(results) == 0L) return(NULL)
  idx <- which(vapply(results, function(r) identical(r$id, id), logical(1L)))
  if (length(idx) == 0L) return(NULL)
  results[[idx[1L]]]
}
