# mod_analysis_vas.R — EQ-VAS analysis module

# ── UI ────────────────────────────────────────────────────────────────────────

mod_analysis_vas_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("guard")),
    shiny::conditionalPanel(
      condition = "output.app_has_data",
      bslib::accordion(
        open = "acc_21",

        # Table 2.1 — summary stats by timepoint
        bslib::accordion_panel(
          title = "Table 2.1 \u2014 VAS summary statistics by timepoint",
          value = "acc_21",
          shiny::p("Descriptive statistics for the EQ-VAS score, grouped by timepoint."),
          shiny::uiOutput(ns("note_fu_21")),
          shiny::uiOutput(ns("grp_sel_ui_21")),
          shiny::actionButton(ns("run_21"), "Run",
                              class = "btn-sm btn-outline-primary"),
          shiny::br(), shiny::br(),
          shiny::uiOutput(ns("out_21"))
        ),

        # Table 2.2 — frequency of mid-points
        bslib::accordion_panel(
          title = "Table 2.2 \u2014 VAS frequency of mid-points",
          value = "acc_22",
          shiny::p("Frequency distribution of EQ-VAS scores by mid-point intervals."),
          shiny::actionButton(ns("run_22"), "Run",
                              class = "btn-sm btn-outline-primary"),
          shiny::br(), shiny::br(),
          shiny::uiOutput(ns("out_22"))
        ),

        # Figure 2.1 — histogram
        bslib::accordion_panel(
          title = "Figure 2.1 \u2014 VAS distribution (histogram)",
          value = "acc_fig21",
          shiny::p("Histogram of the EQ-VAS distribution."),
          shiny::actionButton(ns("run_fig21"), "Run",
                              class = "btn-sm btn-outline-primary"),
          shiny::br(), shiny::br(),
          shiny::uiOutput(ns("out_fig21"))
        ),

        # Figure 2.2 — box plot by midpoints
        bslib::accordion_panel(
          title = "Figure 2.2 \u2014 VAS distribution by midpoints (box plot)",
          value = "acc_fig22",
          shiny::p("Box plots of EQ-VAS score distribution by midpoint intervals."),
          shiny::actionButton(ns("run_fig22"), "Run",
                              class = "btn-sm btn-outline-primary"),
          shiny::br(), shiny::br(),
          shiny::uiOutput(ns("out_fig22"))
        )
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_analysis_vas_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$guard <- shiny::renderUI({
      if (is.null(rv$processed_data)) {
        bslib::card(bslib::card_body(
          shiny::p(shiny::icon("circle-info"),
                   " Please complete data upload and validation first.",
                   class = "text-muted")
        ))
      }
    })

    # ── Info note: no timepoint mapped ────────────────────────────────────────
    output$note_fu_21 <- shiny::renderUI({
      shiny::req(rv$mapping)
      has_fu <- !is.null(rv$mapping$name_fu) && nzchar(rv$mapping$name_fu)
      if (!has_fu) {
        shiny::div(
          class = "alert alert-info small py-2",
          shiny::icon("circle-info"), " ",
          "No timepoint variable mapped. ",
          "Results will be shown for all observations combined."
        )
      }
    })

    # ── Optional group filter for Table 2.1 ───────────────────────────────────
    output$grp_sel_ui_21 <- shiny::renderUI({
      shiny::req(rv$processed_data, rv$mapping)
      has_group <- !is.null(rv$mapping$name_groupvar) &&
                   nzchar(rv$mapping$name_groupvar)
      if (!has_group) return(NULL)
      df       <- rv$processed_data
      grp_vals <- if ("groupvar" %in% names(df)) {
        sort(unique(as.character(df[["groupvar"]])))
      } else {
        character(0L)
      }
      if (length(grp_vals) == 0L) return(NULL)
      shiny::selectInput(
        ns("grp_sel_21"), "Filter by group",
        choices  = c("All" = "All", stats::setNames(grp_vals, grp_vals)),
        selected = "All"
      )
    })

    # ── Table 2.1 ─────────────────────────────────────────────────────────────
    r21 <- shiny::reactiveValues(data = NULL, call = NULL)

    shiny::observeEvent(input$run_21, {
      shiny::req(rv$processed_data)
      df      <- rv$processed_data
      # Optional group filter
      grp_sel <- input$grp_sel_21
      if (!is.null(grp_sel) && grp_sel != "All" && "groupvar" %in% names(df)) {
        df <- df[df[["groupvar"]] == grp_sel, , drop = FALSE]
      }
      fu_info  <- ensure_fu(df, rv$mapping)
      args     <- list(name_vas = "vas", name_fu = fu_info$name_fu)
      call_str <- format_call("table_2_1", list(df = "data", name_vas = "vas"))
      tryCatch({
        r21$data <- do.call(table_2_1, c(list(df = fu_info$df), args))
        r21$call <- call_str
        save_result(rv, "VAS summary stats (2.1)", call_str, "table", data = r21$data)
      }, error = function(e) err_notify(e))
    })

    output$out_21 <- shiny::renderUI({
      if (is.null(r21$data)) return(NULL)
      shiny::tagList(analysis_table_output(ns("tbl_21")), call_display(ns("call_21")))
    })
    output$tbl_21  <- render_analysis_table(shiny::reactive(r21$data))
    output$call_21 <- shiny::renderText(r21$call)

    # ── Table 2.2 ─────────────────────────────────────────────────────────────
    r22 <- shiny::reactiveValues(data = NULL, call = NULL)

    shiny::observeEvent(input$run_22, {
      shiny::req(rv$processed_data)
      args     <- list(name_vas = "vas")
      call_str <- format_call("table_2_2", c(list(df = "data"), args))
      tryCatch({
        r22$data <- do.call(table_2_2, c(list(df = rv$processed_data), args))
        r22$call <- call_str
        save_result(rv, "VAS mid-points (2.2)", call_str, "table", data = r22$data)
      }, error = function(e) err_notify(e))
    })

    output$out_22 <- shiny::renderUI({
      if (is.null(r22$data)) return(NULL)
      shiny::tagList(analysis_table_output(ns("tbl_22")), call_display(ns("call_22")))
    })
    output$tbl_22  <- render_analysis_table(shiny::reactive(r22$data))
    output$call_22 <- shiny::renderText(r22$call)

    # ── Figure 2.1 ────────────────────────────────────────────────────────────
    rfig21 <- shiny::reactiveValues(plot = NULL, call = NULL)

    shiny::observeEvent(input$run_fig21, {
      shiny::req(rv$processed_data)
      args     <- list(name_vas = "vas")
      call_str <- format_call("figure_2_1", c(list(df = "data"), args))
      tryCatch({
        res         <- do.call(figure_2_1, c(list(df = rv$processed_data), args))
        rfig21$plot <- res$p
        rfig21$call <- call_str
        save_result(rv, "VAS histogram (fig. 2.1)", call_str, "plot", plot = rfig21$plot)
      }, error = function(e) err_notify(e))
    })

    output$out_fig21 <- shiny::renderUI({
      if (is.null(rfig21$plot)) return(NULL)
      shiny::tagList(
        shiny::plotOutput(ns("plt_fig21"), height = "400px"),
        call_display(ns("call_fig21"))
      )
    })
    output$plt_fig21  <- shiny::renderPlot(rfig21$plot)
    output$call_fig21 <- shiny::renderText(rfig21$call)

    # ── Figure 2.2 ────────────────────────────────────────────────────────────
    rfig22 <- shiny::reactiveValues(plot = NULL, call = NULL)

    shiny::observeEvent(input$run_fig22, {
      shiny::req(rv$processed_data)
      args     <- list(name_vas = "vas")
      call_str <- format_call("figure_2_2", c(list(df = "data"), args))
      tryCatch({
        res         <- do.call(figure_2_2, c(list(df = rv$processed_data), args))
        rfig22$plot <- res$p
        rfig22$call <- call_str
        save_result(rv, "VAS box plot (fig. 2.2)", call_str, "plot", plot = rfig22$plot)
      }, error = function(e) err_notify(e))
    })

    output$out_fig22 <- shiny::renderUI({
      if (is.null(rfig22$plot)) return(NULL)
      shiny::tagList(
        shiny::plotOutput(ns("plt_fig22"), height = "400px"),
        call_display(ns("call_fig22"))
      )
    })
    output$plt_fig22  <- shiny::renderPlot(rfig22$plot)
    output$call_fig22 <- shiny::renderText(rfig22$call)
  })
}
