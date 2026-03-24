# mod_analysis_utility.R — EQ-5D Utility (index) analysis module

# ── UI ────────────────────────────────────────────────────────────────────────

mod_analysis_utility_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("guard")),
    shiny::conditionalPanel(
      condition = "output.app_has_data",

      # Default country card
      bslib::card(
        bslib::card_header(
          shiny::tags$span(shiny::icon("globe"), " Default Country / Value Set")
        ),
        bslib::card_body(
          shiny::p(
            class = "text-muted small",
            "Select a default country to pre-fill all analysis country pickers below."
          ),
          shiny::uiOutput(ns("default_country_ui"))
        )
      ),
      shiny::br(),

      bslib::accordion(
        open = "acc_u31",

        # Table 3.1 — summary stats (uses ensure_fu)
        bslib::accordion_panel(
          title = "Table 3.1 \u2014 Utility summary statistics",
          value = "acc_u31",
          shiny::p("Descriptive statistics for the EQ-5D utility index, grouped by timepoint."),
          shiny::uiOutput(ns("note_fu_31")),
          shiny::uiOutput(ns("cpick_31")),
          shiny::actionButton(ns("run_31"), "Run", class = "btn-sm btn-outline-primary"),
          shiny::br(), shiny::br(),
          shiny::uiOutput(ns("out_31"))
        ),

        # Figure 3.4 — bar chart (no fu/group required)
        bslib::accordion_panel(
          title = "Figure 3.4 \u2014 Utility distribution (bar chart)",
          value = "acc_ufig34",
          shiny::p("Bar chart of the EQ-5D utility index distribution."),
          shiny::uiOutput(ns("cpick_fig34")),
          shiny::actionButton(ns("run_fig34"), "Run", class = "btn-sm btn-outline-primary"),
          shiny::br(), shiny::br(),
          shiny::uiOutput(ns("out_fig34"))
        ),

        # Table 3.2 — group required (body rendered conditionally)
        bslib::accordion_panel(
          title = "Table 3.2 \u2014 Mean utility by group",
          value = "acc_u32",
          shiny::uiOutput(ns("body_32"))
        ),

        # Figure 3.2 — group required (body rendered conditionally)
        bslib::accordion_panel(
          title = "Figure 3.2 \u2014 Mean utility and 95% CI by group (bar chart)",
          value = "acc_ufig32",
          shiny::uiOutput(ns("body_fig32"))
        ),

        # Figure 3.1 — fu required (body rendered conditionally)
        bslib::accordion_panel(
          title = "Figure 3.1 \u2014 Utility by timepoint (box plots)",
          value = "acc_ufig31",
          shiny::uiOutput(ns("body_fig31"))
        ),

        # Figure 3.3 — group + fu required (body rendered conditionally)
        bslib::accordion_panel(
          title = "Figure 3.3 \u2014 Utility by timepoint and group",
          value = "acc_ufig33",
          shiny::uiOutput(ns("body_fig33"))
        ),

        # Figure 3.5 — VAS required (body rendered conditionally)
        bslib::accordion_panel(
          title = "Figure 3.5 \u2014 Utility vs EQ-VAS",
          value = "acc_ufig35",
          shiny::uiOutput(ns("body_fig35"))
        )
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_analysis_utility_server <- function(id, rv) {
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

    # ── Country helpers ───────────────────────────────────────────────────────
    country_choices <- shiny::reactive({
      shiny::req(rv$mapping$eq5d_version)
      get_country_choices(rv$mapping$eq5d_version)
    })

    # reactiveVal holds the currently selected default country
    user_default_country <- shiny::reactiveVal("")

    # Seed from mapping country when mapping is first confirmed
    shiny::observe({
      shiny::req(rv$mapping)
      cty <- rv$mapping$country
      if (!is.null(cty) && nzchar(cty) && !nzchar(user_default_country())) {
        user_default_country(cty)
      }
    })

    # Default picker UI
    output$default_country_ui <- shiny::renderUI({
      ch <- country_choices()
      if (length(ch) == 0L) {
        return(shiny::p("No value sets available for this version.", class = "text-muted small"))
      }
      shiny::selectizeInput(
        ns("default_country"), NULL,
        choices  = c("(select)" = "", ch),
        selected = user_default_country()
      )
    })

    # When default picker changes, propagate to reactiveVal
    shiny::observeEvent(input$default_country, {
      user_default_country(input$default_country)
    })

    # Factory: per-analysis country picker; re-renders when default changes
    make_cpick <- function(picker_id, label = "Country / Value set") {
      shiny::renderUI({
        ch  <- country_choices()
        sel <- user_default_country()   # reactive dependency — re-renders on default change
        if (length(ch) == 0L) {
          return(shiny::p("No value sets found.", class = "text-danger small"))
        }
        shiny::selectizeInput(
          ns(picker_id), label,
          choices  = c("(select)" = "", ch),
          selected = sel
        )
      })
    }

    output$cpick_31    <- make_cpick("country_31")
    output$cpick_fig34 <- make_cpick("country_fig34")
    output$cpick_32    <- make_cpick("country_32")
    output$cpick_fig32 <- make_cpick("country_fig32")
    output$cpick_fig31 <- make_cpick("country_fig31")
    output$cpick_fig33 <- make_cpick("country_fig33")
    output$cpick_fig35 <- make_cpick("country_fig35")

    # ── Base args reactive ────────────────────────────────────────────────────
    base_args <- shiny::reactive({
      shiny::req(rv$mapping)
      list(names_eq5d = DIMS_STD, eq5d_version = rv$mapping$eq5d_version)
    })

    # ── Reusable warning notes ────────────────────────────────────────────────
    no_group_note <- shiny::div(
      class = "alert alert-warning small py-2",
      shiny::icon("triangle-exclamation"), " ",
      "This analysis requires a Group variable. ",
      "Please map a group column on the Data tab."
    )

    no_fu_note <- shiny::div(
      class = "alert alert-info small py-2",
      shiny::icon("circle-info"), " ",
      "No timepoint variable mapped. ",
      "Results will be shown for all observations combined."
    )

    no_fu_required_note <- shiny::div(
      class = "alert alert-warning small py-2",
      shiny::icon("triangle-exclamation"), " ",
      "This analysis requires a Timepoint variable. ",
      "Please map a follow-up column on the Data tab."
    )

    no_vas_note <- shiny::div(
      class = "alert alert-warning small py-2",
      shiny::icon("triangle-exclamation"), " ",
      "This analysis requires an EQ-VAS variable. ",
      "Please map a VAS column on the Data tab."
    )

    # ── Table 3.1 ─────────────────────────────────────────────────────────────
    r31 <- shiny::reactiveValues(data = NULL, call = NULL)

    output$note_fu_31 <- shiny::renderUI({
      shiny::req(rv$mapping)
      has_fu <- !is.null(rv$mapping$name_fu) && nzchar(rv$mapping$name_fu)
      if (!has_fu) no_fu_note
    })

    shiny::observeEvent(input$run_31, {
      shiny::req(rv$processed_data, nzchar(input$country_31 %||% ""))
      fu_info  <- ensure_fu(rv$processed_data, rv$mapping)
      args     <- c(base_args(),
                    list(name_fu = fu_info$name_fu, country = input$country_31))
      call_str <- format_call("table_3_1",
                              c(list(df = "data"), base_args(),
                                list(country = input$country_31)))
      tryCatch({
        r31$data <- do.call(table_3_1, c(list(df = fu_info$df), args))
        r31$call <- call_str
        save_result(rv, "Utility summary stats (3.1)", call_str, "table", data = r31$data)
      }, error = function(e) err_notify(e))
    })

    output$out_31 <- shiny::renderUI({
      if (is.null(r31$data)) return(NULL)
      shiny::tagList(analysis_table_output(ns("tbl_31")), call_display(ns("call_31")))
    })
    output$tbl_31  <- render_analysis_table(shiny::reactive(r31$data))
    output$call_31 <- shiny::renderText(r31$call)

    # ── Figure 3.4 ────────────────────────────────────────────────────────────
    rfig34 <- shiny::reactiveValues(plot = NULL, call = NULL)

    shiny::observeEvent(input$run_fig34, {
      shiny::req(rv$processed_data, nzchar(input$country_fig34 %||% ""))
      args     <- c(base_args(), list(country = input$country_fig34))
      call_str <- format_call("figure_3_4", c(list(df = "data"), args))
      tryCatch({
        res          <- do.call(figure_3_4, c(list(df = rv$processed_data), args))
        rfig34$plot  <- res$p
        rfig34$call  <- call_str
        save_result(rv, "Utility bar chart (fig. 3.4)", call_str, "plot", plot = rfig34$plot)
      }, error = function(e) err_notify(e))
    })

    output$out_fig34 <- shiny::renderUI({
      if (is.null(rfig34$plot)) return(NULL)
      shiny::tagList(
        shiny::plotOutput(ns("plt_fig34"), height = "420px"),
        call_display(ns("call_fig34"))
      )
    })
    output$plt_fig34  <- shiny::renderPlot(rfig34$plot)
    output$call_fig34 <- shiny::renderText(rfig34$call)

    # ── Table 3.2 (group required) ────────────────────────────────────────────
    r32 <- shiny::reactiveValues(data = NULL, call = NULL)

    output$body_32 <- shiny::renderUI({
      shiny::req(rv$mapping)
      has_group <- !is.null(rv$mapping$name_groupvar) &&
                   nzchar(rv$mapping$name_groupvar)
      if (!has_group) return(shiny::tagList(shiny::br(), no_group_note))
      shiny::tagList(
        shiny::p("Mean utility (with 95% CI) overall and by group."),
        shiny::uiOutput(ns("cpick_32")),
        shiny::actionButton(ns("run_32"), "Run", class = "btn-sm btn-outline-primary"),
        shiny::br(), shiny::br(),
        shiny::uiOutput(ns("out_32"))
      )
    })

    shiny::observeEvent(input$run_32, {
      shiny::req(rv$processed_data, rv$mapping$name_groupvar,
                 nzchar(input$country_32 %||% ""))
      args     <- c(base_args(),
                    list(name_groupvar = "groupvar", country = input$country_32))
      call_str <- format_call("table_3_2", c(list(df = "data"), args))
      tryCatch({
        r32$data <- do.call(table_3_2, c(list(df = rv$processed_data), args))
        r32$call <- call_str
        save_result(rv, "Utility by group (3.2)", call_str, "table", data = r32$data)
      }, error = function(e) err_notify(e))
    })

    output$out_32 <- shiny::renderUI({
      if (is.null(r32$data)) return(NULL)
      shiny::tagList(analysis_table_output(ns("tbl_32")), call_display(ns("call_32")))
    })
    output$tbl_32  <- render_analysis_table(shiny::reactive(r32$data))
    output$call_32 <- shiny::renderText(r32$call)

    # ── Figure 3.2 (group required) ───────────────────────────────────────────
    rfig32 <- shiny::reactiveValues(plot = NULL, call = NULL)

    output$body_fig32 <- shiny::renderUI({
      shiny::req(rv$mapping)
      has_group <- !is.null(rv$mapping$name_groupvar) &&
                   nzchar(rv$mapping$name_groupvar)
      if (!has_group) return(shiny::tagList(shiny::br(), no_group_note))
      shiny::tagList(
        shiny::p("Bar chart of mean utility with 95% confidence intervals by group."),
        shiny::uiOutput(ns("cpick_fig32")),
        shiny::actionButton(ns("run_fig32"), "Run", class = "btn-sm btn-outline-primary"),
        shiny::br(), shiny::br(),
        shiny::uiOutput(ns("out_fig32"))
      )
    })

    shiny::observeEvent(input$run_fig32, {
      shiny::req(rv$processed_data, rv$mapping$name_groupvar,
                 nzchar(input$country_fig32 %||% ""))
      args     <- c(base_args(),
                    list(name_groupvar = "groupvar", country = input$country_fig32))
      call_str <- format_call("figure_3_2", c(list(df = "data"), args))
      tryCatch({
        res          <- do.call(figure_3_2, c(list(df = rv$processed_data), args))
        rfig32$plot  <- res$p
        rfig32$call  <- call_str
        save_result(rv, "Utility CI bar chart (fig. 3.2)", call_str, "plot", plot = rfig32$plot)
      }, error = function(e) err_notify(e))
    })

    output$out_fig32 <- shiny::renderUI({
      if (is.null(rfig32$plot)) return(NULL)
      shiny::tagList(
        shiny::plotOutput(ns("plt_fig32"), height = "420px"),
        call_display(ns("call_fig32"))
      )
    })
    output$plt_fig32  <- shiny::renderPlot(rfig32$plot)
    output$call_fig32 <- shiny::renderText(rfig32$call)

    # ── Figure 3.1 (fu required) ──────────────────────────────────────────────
    rfig31 <- shiny::reactiveValues(plot = NULL, call = NULL)

    output$body_fig31 <- shiny::renderUI({
      shiny::req(rv$mapping)
      has_fu <- !is.null(rv$mapping$name_fu) && nzchar(rv$mapping$name_fu)
      if (!has_fu) return(shiny::tagList(shiny::br(), no_fu_required_note))
      shiny::tagList(
        shiny::p("Box plots of the utility index at each timepoint."),
        shiny::uiOutput(ns("cpick_fig31")),
        shiny::actionButton(ns("run_fig31"), "Run", class = "btn-sm btn-outline-primary"),
        shiny::br(), shiny::br(),
        shiny::uiOutput(ns("out_fig31"))
      )
    })

    shiny::observeEvent(input$run_fig31, {
      shiny::req(rv$processed_data, rv$mapping$name_fu,
                 nzchar(input$country_fig31 %||% ""))
      args     <- c(base_args(), list(name_fu = "fu", country = input$country_fig31))
      call_str <- format_call("figure_3_1", c(list(df = "data"), args))
      tryCatch({
        res          <- do.call(figure_3_1, c(list(df = rv$processed_data), args))
        rfig31$plot  <- res$p
        rfig31$call  <- call_str
        save_result(rv, "Utility box plots (fig. 3.1)", call_str, "plot", plot = rfig31$plot)
      }, error = function(e) err_notify(e))
    })

    output$out_fig31 <- shiny::renderUI({
      if (is.null(rfig31$plot)) return(NULL)
      shiny::tagList(
        shiny::plotOutput(ns("plt_fig31"), height = "420px"),
        call_display(ns("call_fig31"))
      )
    })
    output$plt_fig31  <- shiny::renderPlot(rfig31$plot)
    output$call_fig31 <- shiny::renderText(rfig31$call)

    # ── Figure 3.3 (group + fu required) ─────────────────────────────────────
    rfig33 <- shiny::reactiveValues(plot = NULL, call = NULL)

    output$body_fig33 <- shiny::renderUI({
      shiny::req(rv$mapping)
      has_fu    <- !is.null(rv$mapping$name_fu)       && nzchar(rv$mapping$name_fu)
      has_group <- !is.null(rv$mapping$name_groupvar) && nzchar(rv$mapping$name_groupvar)
      if (!has_fu || !has_group) {
        missing <- c(if (!has_fu) "Timepoint", if (!has_group) "Group")
        return(shiny::tagList(
          shiny::br(),
          shiny::div(
            class = "alert alert-warning small py-2",
            shiny::icon("triangle-exclamation"), " ",
            paste("This analysis requires:",
                  paste(missing, collapse = " and "),
                  "variable(s). Please map them on the Data tab.")
          )
        ))
      }
      shiny::tagList(
        shiny::p("Mean utility with 95% CI by timepoint, coloured by group."),
        shiny::uiOutput(ns("cpick_fig33")),
        shiny::actionButton(ns("run_fig33"), "Run", class = "btn-sm btn-outline-primary"),
        shiny::br(), shiny::br(),
        shiny::uiOutput(ns("out_fig33"))
      )
    })

    shiny::observeEvent(input$run_fig33, {
      shiny::req(rv$processed_data, rv$mapping$name_fu, rv$mapping$name_groupvar,
                 nzchar(input$country_fig33 %||% ""))
      args     <- c(base_args(),
                    list(name_fu       = "fu",
                         name_groupvar = "groupvar",
                         country       = input$country_fig33))
      call_str <- format_call("figure_3_3", c(list(df = "data"), args))
      tryCatch({
        res          <- do.call(figure_3_3, c(list(df = rv$processed_data), args))
        rfig33$plot  <- res$p
        rfig33$call  <- call_str
        save_result(rv, "Utility by timepoint & group (fig. 3.3)", call_str,
                    "plot", plot = rfig33$plot)
      }, error = function(e) err_notify(e))
    })

    output$out_fig33 <- shiny::renderUI({
      if (is.null(rfig33$plot)) return(NULL)
      shiny::tagList(
        shiny::plotOutput(ns("plt_fig33"), height = "420px"),
        call_display(ns("call_fig33"))
      )
    })
    output$plt_fig33  <- shiny::renderPlot(rfig33$plot)
    output$call_fig33 <- shiny::renderText(rfig33$call)

    # ── Figure 3.5 (VAS required) ─────────────────────────────────────────────
    rfig35 <- shiny::reactiveValues(plot = NULL, call = NULL)

    output$body_fig35 <- shiny::renderUI({
      shiny::req(rv$mapping)
      has_vas <- !is.null(rv$mapping$name_vas) && nzchar(rv$mapping$name_vas)
      if (!has_vas) return(shiny::tagList(shiny::br(), no_vas_note))
      shiny::tagList(
        shiny::p("Scatter plot of EQ-5D utility index values against EQ-VAS scores."),
        shiny::uiOutput(ns("cpick_fig35")),
        shiny::actionButton(ns("run_fig35"), "Run", class = "btn-sm btn-outline-primary"),
        shiny::br(), shiny::br(),
        shiny::uiOutput(ns("out_fig35"))
      )
    })

    shiny::observeEvent(input$run_fig35, {
      shiny::req(rv$processed_data, rv$mapping$name_vas,
                 nzchar(input$country_fig35 %||% ""))
      args     <- c(base_args(), list(name_vas = "vas", country = input$country_fig35))
      call_str <- format_call("figure_3_5", c(list(df = "data"), args))
      tryCatch({
        res          <- do.call(figure_3_5, c(list(df = rv$processed_data), args))
        rfig35$plot  <- res$p
        rfig35$call  <- call_str
        save_result(rv, "Utility vs VAS (fig. 3.5)", call_str, "plot", plot = rfig35$plot)
      }, error = function(e) err_notify(e))
    })

    output$out_fig35 <- shiny::renderUI({
      if (is.null(rfig35$plot)) return(NULL)
      shiny::tagList(
        shiny::plotOutput(ns("plt_fig35"), height = "420px"),
        call_display(ns("call_fig35"))
      )
    })
    output$plt_fig35  <- shiny::renderPlot(rfig35$plot)
    output$call_fig35 <- shiny::renderText(rfig35$call)
  })
}

# Null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0L) a else b
