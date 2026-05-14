# mod_analysis_profile.R — EQ-5D Profile analysis module

DIMS_STD <- c("mo", "sc", "ua", "pd", "ad")

# ── UI ────────────────────────────────────────────────────────────────────────

mod_analysis_profile_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("guard")),
    shiny::conditionalPanel(
      condition = "output.app_has_data",
      shiny::tabsetPanel(
        id       = ns("profile_tabs"),
        type     = "pills",
        selected = "cs",
        
        # ── 1. Cross-sectional ───────────────────────────────────────────────
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("table"), " Cross-sectional"),
          value = "cs",
          shiny::br(),
          bslib::accordion(
            open     = "acc_111",
            multiple = TRUE,
            
            bslib::accordion_panel(
              title = shiny::tagList(shiny::icon("table"),
                                     " Table 1.1.1 \u2014 Level frequencies by dimension"),
              value = "acc_111",
              shiny::p(class = "text-muted small",
                       "Frequency (n and %) of each response level across all five EQ-5D dimensions."),
              shiny::actionButton(ns("run_111"), "Run",
                                  class = "btn-sm btn-outline-primary", icon = shiny::icon("play")),
              shiny::br(), shiny::br(),
              shiny::uiOutput(ns("out_111"))
            ),
            
            bslib::accordion_panel(
              title = shiny::tagList(shiny::icon("table"),
                                     " Table 1.1.2 \u2014 Level frequencies by group"),
              value = "acc_112",
              shiny::p(class = "text-muted small",
                       "Frequency of response levels stratified by group. Requires a Group variable."),
              shiny::conditionalPanel(
                condition = "!output.app_has_group",
                shiny::div(class = "alert alert-warning small py-2",
                           shiny::icon("triangle-exclamation"),
                           " Group variable not mapped. Please add one on the Data tab.")
              ),
              shiny::conditionalPanel(
                condition = "output.app_has_group",
                shiny::actionButton(ns("run_112"), "Run",
                                    class = "btn-sm btn-outline-primary", icon = shiny::icon("play"))
              ),
              shiny::br(), shiny::br(),
              shiny::uiOutput(ns("out_112"))
            ),
            
            bslib::accordion_panel(
              title = shiny::tagList(shiny::icon("star"),
                                     " Table 1.1.3 \u2014 Most common health states"),
              value = "acc_113",
              shiny::p(class = "text-muted small",
                       "The most frequently observed EQ-5D health states in the dataset."),
              bslib::layout_columns(
                col_widths = c(3, 9),
                shiny::numericInput(ns("n_113"), "Top N states",
                                    value = 10L, min = 1L, max = 50L, step = 1L),
                shiny::div()
              ),
              shiny::actionButton(ns("run_113"), "Run",
                                  class = "btn-sm btn-outline-primary", icon = shiny::icon("play")),
              shiny::br(), shiny::br(),
              shiny::uiOutput(ns("out_113"))
            )
          )
        ),
        
        # ── 2. Longitudinal ──────────────────────────────────────────────────
        # NOTE: this tab is hidden by default on module init (see server below)
        # and shown only when rv$mapping$name_fu is a non-empty string.
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("clock"), " Longitudinal"),
          value = "long",
          shiny::br(),
          bslib::accordion(
            open     = "acc_121",
            multiple = TRUE,
            
            # Table 1.2.1
            bslib::accordion_panel(
              title = shiny::tagList(shiny::icon("table"),
                                     " Table 1.2.1 \u2014 Level frequencies by timepoint"),
              value = "acc_121",
              shiny::p(class = "text-muted small",
                       "Frequency of response levels at each timepoint."),
              shiny::actionButton(ns("run_121"), "Run",
                                  class = "btn-sm btn-outline-primary", icon = shiny::icon("play")),
              shiny::br(), shiny::br(),
              shiny::uiOutput(ns("out_121"))
            ),
            
            # Table 1.2.2
            bslib::accordion_panel(
              title = shiny::tagList(shiny::icon("chart-bar"),
                                     " Table 1.2.2 \u2014 PCHC health change"),
              value = "acc_122",
              shiny::p(class = "text-muted small",
                       "Paretian Classification of Health Change (PCHC). Requires Patient ID. ",
                       "If no group variable is mapped, analysis runs on the full population."),
              shiny::conditionalPanel(
                condition = "!output.app_has_id",
                shiny::div(class = "alert alert-warning small py-2",
                           shiny::icon("triangle-exclamation"), " Patient ID not mapped.")
              ),
              shiny::conditionalPanel(
                condition = "output.app_has_id",
                shiny::uiOutput(ns("group_note_122")),
                shiny::actionButton(ns("run_122"), "Run",
                                    class = "btn-sm btn-outline-primary", icon = shiny::icon("play"))
              ),
              shiny::br(), shiny::br(),
              shiny::uiOutput(ns("out_122"))
            ),
            
            # Table 1.2.3 (id gated — body rendered conditionally by server)
            bslib::accordion_panel(
              title = shiny::tagList(shiny::icon("chart-bar"),
                                     " Table 1.2.3 \u2014 PCHC (accounting for no problems)"),
              value = "acc_123",
              shiny::p(class = "text-muted small",
                       "PCHC accounting for those reporting no problems at baseline. ",
                       "Requires Patient ID."),
              shiny::uiOutput(ns("panel_123_body")),
              shiny::br(),
              shiny::uiOutput(ns("out_123"))
            ),
            
            # Figures 1.2.1–1.2.4 (grouped in one panel)
            bslib::accordion_panel(
              title = shiny::tagList(shiny::icon("chart-bar"),
                                     " Figures 1.2.1\u20131.2.4 \u2014 PCHC visualisations"),
              value = "acc_12figs",
              shiny::p(class = "text-muted small",
                       "Bar charts of PCHC results by group and dimension. Requires Patient ID. ",
                       "If no group variable is mapped, uses the full population."),
              shiny::conditionalPanel(
                condition = "!output.app_has_id",
                shiny::div(class = "alert alert-warning small py-2",
                           shiny::icon("triangle-exclamation"), " Patient ID not mapped.")
              ),
              shiny::conditionalPanel(
                condition = "output.app_has_id",
                shiny::uiOutput(ns("group_note_12figs")),
                shiny::tags$div(style = "margin-top:0.5rem;"),
                shiny::h6("Figure 1.2.1 \u2014 PCHC categories by group",
                          class = "fw-semibold mt-2"),
                shiny::actionButton(ns("run_121fig"), "Run Fig. 1.2.1",
                                    class = "btn-sm btn-outline-primary", icon = shiny::icon("play")),
                shiny::br(), shiny::br(),
                shiny::uiOutput(ns("out_121fig")),
                shiny::hr(style = "margin: 0.5rem 0;"),
                shiny::h6("Figure 1.2.2 \u2014 Improvements by group and dimension",
                          class = "fw-semibold"),
                shiny::actionButton(ns("run_122fig"), "Run Fig. 1.2.2",
                                    class = "btn-sm btn-outline-primary", icon = shiny::icon("play")),
                shiny::br(), shiny::br(),
                shiny::uiOutput(ns("out_122fig")),
                shiny::hr(style = "margin: 0.5rem 0;"),
                shiny::h6("Figure 1.2.3 \u2014 Worsenings by group and dimension",
                          class = "fw-semibold"),
                shiny::actionButton(ns("run_123fig"), "Run Fig. 1.2.3",
                                    class = "btn-sm btn-outline-primary", icon = shiny::icon("play")),
                shiny::br(), shiny::br(),
                shiny::uiOutput(ns("out_123fig")),
                shiny::hr(style = "margin: 0.5rem 0;"),
                shiny::h6("Figure 1.2.4 \u2014 Mixed changes by group and dimension",
                          class = "fw-semibold"),
                shiny::actionButton(ns("run_124fig"), "Run Fig. 1.2.4",
                                    class = "btn-sm btn-outline-primary", icon = shiny::icon("play")),
                shiny::br(), shiny::br(),
                shiny::uiOutput(ns("out_124fig"))
              )
            ),
            
            # Figure 1.2.5 — HPG
            bslib::accordion_panel(
              title = shiny::tagList(shiny::icon("chart-line"),
                                     " Figure 1.2.5 \u2014 Health Profile Grid (HPG)"),
              value = "acc_125fig",
              shiny::p(class = "text-muted small",
                       "HPG scatter plot comparing health profiles between two timepoints. ",
                       "Requires Patient ID and a country value set. Select exactly two timepoints."),
              shiny::conditionalPanel(
                condition = "!output.app_has_id",
                shiny::div(class = "alert alert-warning small py-2",
                           shiny::icon("triangle-exclamation"), " Patient ID not mapped.")
              ),
              shiny::conditionalPanel(
                condition = "output.app_has_id",
                bslib::layout_columns(
                  col_widths = c(6, 6),
                  shiny::uiOutput(ns("fu_picker_125")),
                  shiny::uiOutput(ns("country_picker_125"))
                ),
                shiny::actionButton(ns("run_125fig"), "Run",
                                    class = "btn-sm btn-outline-primary", icon = shiny::icon("play"))
              ),
              shiny::br(), shiny::br(),
              shiny::uiOutput(ns("out_125fig"))
            ),
            
            # Table 1.2.4 (3L only)
            bslib::accordion_panel(
              title = shiny::tagList(shiny::icon("arrows-left-right"),
                                     " Table 1.2.4 \u2014 Level change between timepoints [3L only]"),
              value = "acc_124",
              shiny::p(class = "text-muted small",
                       "Proportion improving, stable, or worsening per dimension. ",
                       "Only available for EQ-5D-3L. Requires Patient ID."),
              shiny::uiOutput(ns("panel_124_body")),
              shiny::br(), shiny::br(),
              shiny::uiOutput(ns("out_124"))
            )
          )
        ),
        
        # ── 3. Summarizing Severity ──────────────────────────────────────────
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("layer-group"),
                                 " Summarizing Severity"),
          value = "severity",
          shiny::br(),
          shiny::p(shiny::em("Summarizing severity of EQ-5D profiles"),
                   class = "text-muted mb-3"),
          bslib::accordion(
            open     = "acc_131",
            multiple = TRUE,
            
            bslib::accordion_panel(
              title = shiny::tagList(shiny::icon("table"),
                                     " Table 1.3.1 \u2014 Summary statistics by Level Sum Score (LSS)"),
              value = "acc_131",
              shiny::p(class = "text-muted small",
                       "Summary statistics for EQ-5D utility values at each Level Sum Score."),
              shiny::uiOutput(ns("country_picker_131")),
              shiny::actionButton(ns("run_131"), "Run",
                                  class = "btn-sm btn-outline-primary", icon = shiny::icon("play")),
              shiny::br(), shiny::br(),
              shiny::uiOutput(ns("out_131"))
            ),
            
            bslib::accordion_panel(
              title = shiny::tagList(shiny::icon("chart-line"),
                                     " Figure 1.3.1 \u2014 EQ-5D values vs. Level Sum Score"),
              value = "acc_131fig",
              shiny::p(class = "text-muted small",
                       "Plot of EQ-5D utility values against the Level Sum Score (LSS)."),
              shiny::uiOutput(ns("country_picker_131fig")),
              shiny::actionButton(ns("run_131fig"), "Run",
                                  class = "btn-sm btn-outline-primary", icon = shiny::icon("play")),
              shiny::br(), shiny::br(),
              shiny::uiOutput(ns("out_131fig"))
            ),
            
            bslib::accordion_panel(
              title = shiny::tagList(shiny::icon("table"),
                                     " Table 1.3.2 \u2014 Distribution by Level Frequency Score (LFS)"),
              value = "acc_132",
              shiny::p(class = "text-muted small",
                       "Distribution of EQ-5D health states by Level Frequency Score."),
              shiny::actionButton(ns("run_132"), "Run",
                                  class = "btn-sm btn-outline-primary", icon = shiny::icon("play")),
              shiny::br(), shiny::br(),
              shiny::uiOutput(ns("out_132"))
            ),
            
            bslib::accordion_panel(
              title = shiny::tagList(shiny::icon("table"),
                                     " Table 1.3.4 \u2014 Summary statistics by Level Frequency Score (LFS)"),
              value = "acc_134",
              shiny::p(class = "text-muted small",
                       "Summary statistics of EQ-5D utility values by Level Frequency Score."),
              shiny::uiOutput(ns("country_picker_134")),
              shiny::actionButton(ns("run_134"), "Run",
                                  class = "btn-sm btn-outline-primary", icon = shiny::icon("play")),
              shiny::br(), shiny::br(),
              shiny::uiOutput(ns("out_134"))
            ),
            
            bslib::accordion_panel(
              title = shiny::tagList(shiny::icon("chart-line"),
                                     " Figure 1.3.2 \u2014 EQ-5D values vs. Level Frequency Score"),
              value = "acc_132fig",
              shiny::p(class = "text-muted small",
                       "Plot of EQ-5D utility values against the Level Frequency Score (LFS)."),
              shiny::uiOutput(ns("country_picker_132fig")),
              shiny::actionButton(ns("run_132fig"), "Run",
                                  class = "btn-sm btn-outline-primary", icon = shiny::icon("play")),
              shiny::br(), shiny::br(),
              shiny::uiOutput(ns("out_132fig"))
            )
          )
        ),
        
        # ── 4. Informativity ─────────────────────────────────────────────────
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("chart-bar"), " Informativity"),
          value = "informativity",
          shiny::br(),
          shiny::p(shiny::em("Informativity of EQ-5D profile data"),
                   class = "text-muted mb-3"),
          bslib::accordion(
            open = "acc_141",
            
            bslib::accordion_panel(
              title = shiny::tagList(shiny::icon("chart-bar"),
                                     " Figure 1.4.1 \u2014 Health State Density Index (HSDI)"),
              value = "acc_141",
              shiny::p(class = "text-muted small",
                       "Scatter plot of the cumulative distribution of observed health profiles."),
              shiny::actionButton(ns("run_141"), "Run",
                                  class = "btn-sm btn-outline-primary", icon = shiny::icon("play")),
              shiny::br(), shiny::br(),
              shiny::uiOutput(ns("out_141"))
            )
          )
        )
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_analysis_profile_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Guard ─────────────────────────────────────────────────────────────────
    output$guard <- shiny::renderUI({
      if (is.null(rv$processed_data)) {
        bslib::card(bslib::card_body(
          shiny::p(shiny::icon("circle-info"),
                   " Please complete data upload and validation first.",
                   class = "text-muted")
        ))
      }
    })
    
    # ── Show/hide Longitudinal tab ────────────────────────────────────────────
    # Hide immediately on module init so the tab is never visible before
    # rv$mapping is set (prevents flash of the tab on first render).
    shiny::hideTab(inputId = "profile_tabs", target = "long", session = session)
    
    # React to rv$mapping changes (ignoreNULL = FALSE so hideTab fires
    # when mapping is cleared / reset, not just when it first appears).
    shiny::observeEvent(rv$mapping, {
      has_fu <- !is.null(rv$mapping$name_fu) &&
        nzchar(rv$mapping$name_fu %||% "")
      if (has_fu) {
        shiny::showTab(inputId = "profile_tabs", target = "long", session = session)
      } else {
        shiny::hideTab(inputId = "profile_tabs", target = "long", session = session)
      }
    }, ignoreNULL = FALSE)
    
    # ── Shared helpers ────────────────────────────────────────────────────────
    
    # Version shortcut
    eq5d_ver <- shiny::reactive({
      shiny::req(rv$mapping)
      rv$mapping$eq5d_version
    })
    
    is_3L <- shiny::reactive({
      identical(eq5d_ver(), "3L")
    })
    
    # Country choices for value-set analyses
    country_choices <- shiny::reactive({
      get_country_choices(eq5d_ver())
    })
    
    # Factory: render a country selectizeInput for a given namespaced id
    make_country_picker <- function(picker_id) {
      shiny::renderUI({
        ch <- country_choices()
        if (length(ch) == 0L) {
          return(shiny::p("No value sets available.", class = "text-muted small"))
        }
        default_c <- if (!is.null(rv$mapping) && nzchar(rv$mapping$country %||% ""))
          rv$mapping$country else ""
        shiny::selectizeInput(
          ns(picker_id), "Country / Value set",
          choices  = c("(select)" = "", ch),
          selected = default_c
        )
      })
    }
    
    output$country_picker_125    <- make_country_picker("country_125")
    output$country_picker_131    <- make_country_picker("country_131")
    output$country_picker_131fig <- make_country_picker("country_131fig")
    output$country_picker_134    <- make_country_picker("country_134")
    output$country_picker_132fig <- make_country_picker("country_132fig")
    
    # Factory: info note shown when no group variable is mapped
    make_group_note <- function() {
      shiny::renderUI({
        has_gv <- !is.null(rv$mapping) &&
          !is.null(rv$mapping$name_groupvar) &&
          nzchar(rv$mapping$name_groupvar %||% "")
        if (!has_gv) {
          shiny::div(class = "alert alert-info small py-2 mb-2",
                     shiny::icon("circle-info"),
                     " No group variable mapped. Analysis will run on the full population.")
        }
      })
    }
    
    output$group_note_122    <- make_group_note()
    output$group_note_12figs <- make_group_note()
    output$group_note_123    <- make_group_note()
    
    # Add synthetic "groupvar" = "All" when group not mapped
    ensure_groupvar <- function(df) {
      if (!"groupvar" %in% names(df)) df[["groupvar"]] <- "All"
      df
    }
    
    # Timepoints available in processed data (for Fig 1.2.5 picker)
    timepoints_avail <- shiny::reactive({
      df <- rv$processed_data
      if (is.null(df) || !"fu" %in% names(df)) return(character(0L))
      sort(as.character(unique(df[["fu"]])))
    })
    
    # ── Table 1.2.4 conditional body (3L + id gated) ──────────────────────────
    output$panel_124_body <- shiny::renderUI({
      if (!isTRUE(is_3L())) {
        shiny::div(class = "alert alert-info small py-2",
                   shiny::icon("circle-info"),
                   " This analysis is only available for EQ-5D-3L data.")
      } else if (is.null(rv$mapping$name_id) || !nzchar(rv$mapping$name_id %||% "")) {
        shiny::div(class = "alert alert-warning small py-2",
                   shiny::icon("triangle-exclamation"), " Patient ID not mapped.")
      } else {
        shiny::actionButton(ns("run_124"), "Run",
                            class = "btn-sm btn-outline-primary", icon = shiny::icon("play"))
      }
    })
    
    # ── Table 1.2.3 conditional body (id gated only) ──────────────────────────
    output$panel_123_body <- shiny::renderUI({
      if (is.null(rv$mapping$name_id) || !nzchar(rv$mapping$name_id %||% "")) {
        shiny::div(class = "alert alert-warning small py-2",
                   shiny::icon("triangle-exclamation"), " Patient ID not mapped.")
      } else {
        shiny::tagList(
          shiny::uiOutput(ns("group_note_123")),
          shiny::actionButton(ns("run_123"), "Run",
                              class = "btn-sm btn-outline-primary", icon = shiny::icon("play"))
        )
      }
    })
    
    # ── Figure 1.2.5 timepoint picker ─────────────────────────────────────────
    output$fu_picker_125 <- shiny::renderUI({
      tp <- timepoints_avail()
      if (length(tp) < 2L) {
        return(shiny::p("At least 2 timepoints required.", class = "text-warning small"))
      }
      shiny::selectizeInput(
        ns("fu_levels_125"), "Select exactly two timepoints",
        choices  = tp,
        selected = tp[seq_len(min(2L, length(tp)))],
        multiple = TRUE,
        options  = list(maxItems = 2L, placeholder = "Choose 2 timepoints...")
      )
    })
    
    # ═══════════════════════════════════════════════════════════════════════════
    # CROSS-SECTIONAL
    # ═══════════════════════════════════════════════════════════════════════════
    
    # ── Table 1.1.1 ───────────────────────────────────────────────────────────
    r111 <- shiny::reactiveValues(data = NULL, call = NULL)
    
    shiny::observeEvent(input$run_111, {
      shiny::req(rv$processed_data)
      ver  <- eq5d_ver()
      args <- list(names_eq5d = DIMS_STD, eq5d_version = ver)
      cs   <- format_call("eq5d_profile_level_summary", c(list(df = "data"), args))
      tryCatch({
        r111$data <- do.call(eq5d_profile_level_summary, c(list(df = rv$processed_data), args))
        r111$call <- cs
        save_result(rv, "Level frequencies (1.1.1)", cs, "table", data = r111$data)
      }, error = function(e) err_notify(e))
    })
    
    output$out_111 <- shiny::renderUI({
      if (is.null(r111$data)) return(NULL)
      shiny::tagList(analysis_table_output(ns("tbl_111")), call_display(ns("call_111")))
    })
    output$tbl_111  <- render_analysis_table(shiny::reactive(r111$data))
    output$call_111 <- shiny::renderText(r111$call)
    
    # ── Table 1.1.2 ───────────────────────────────────────────────────────────
    r112 <- shiny::reactiveValues(data = NULL, call = NULL)
    
    shiny::observeEvent(input$run_112, {
      shiny::req(rv$processed_data, rv$mapping$name_groupvar)
      ver  <- eq5d_ver()
      args <- list(names_eq5d = DIMS_STD, name_cat = "groupvar", eq5d_version = ver)
      cs   <- format_call("eq5d_profile_level_summary_by_group", c(list(df = "data"), args))
      tryCatch({
        r112$data <- do.call(eq5d_profile_level_summary_by_group, c(list(df = rv$processed_data), args))
        r112$call <- cs
        save_result(rv, "Level freq. by group (1.1.2)", cs, "table", data = r112$data)
      }, error = function(e) err_notify(e))
    })
    
    output$out_112 <- shiny::renderUI({
      if (is.null(r112$data)) return(NULL)
      shiny::tagList(analysis_table_output(ns("tbl_112")), call_display(ns("call_112")))
    })
    output$tbl_112  <- render_analysis_table(shiny::reactive(r112$data))
    output$call_112 <- shiny::renderText(r112$call)
    
    # ── Table 1.1.3 ───────────────────────────────────────────────────────────
    r113 <- shiny::reactiveValues(data = NULL, call = NULL)
    
    shiny::observeEvent(input$run_113, {
      shiny::req(rv$processed_data, input$n_113)
      ver  <- eq5d_ver()
      args <- list(names_eq5d = DIMS_STD, eq5d_version = ver, n = as.integer(input$n_113))
      cs   <- format_call("eq5d_profile_top_states", c(list(df = "data"), args))
      tryCatch({
        r113$data <- do.call(eq5d_profile_top_states, c(list(df = rv$processed_data), args))
        r113$call <- cs
        save_result(rv, "Most common states (1.1.3)", cs, "table", data = r113$data)
      }, error = function(e) err_notify(e))
    })
    
    output$out_113 <- shiny::renderUI({
      if (is.null(r113$data)) return(NULL)
      shiny::tagList(analysis_table_output(ns("tbl_113")), call_display(ns("call_113")))
    })
    output$tbl_113  <- render_analysis_table(shiny::reactive(r113$data))
    output$call_113 <- shiny::renderText(r113$call)
    
    # ═══════════════════════════════════════════════════════════════════════════
    # LONGITUDINAL
    # ═══════════════════════════════════════════════════════════════════════════
    
    # ── Table 1.2.1 ───────────────────────────────────────────────────────────
    r121 <- shiny::reactiveValues(data = NULL, call = NULL)
    
    shiny::observeEvent(input$run_121, {
      shiny::req(rv$processed_data, rv$mapping$name_fu)
      ver  <- eq5d_ver()
      args <- list(names_eq5d = DIMS_STD, name_fu = "fu", eq5d_version = ver)
      cs   <- format_call("eq5d_profile_change_summary", c(list(df = "data"), args))
      tryCatch({
        r121$data <- do.call(eq5d_profile_change_summary, c(list(df = rv$processed_data), args))
        r121$call <- cs
        save_result(rv, "Level freq. by timepoint (1.2.1)", cs, "table", data = r121$data)
      }, error = function(e) err_notify(e))
    })
    
    output$out_121 <- shiny::renderUI({
      if (is.null(r121$data)) return(NULL)
      shiny::tagList(analysis_table_output(ns("tbl_121")), call_display(ns("call_121")))
    })
    output$tbl_121  <- render_analysis_table(shiny::reactive(r121$data))
    output$call_121 <- shiny::renderText(r121$call)
    
    # ── Table 1.2.4 ───────────────────────────────────────────────────────────
    r124 <- shiny::reactiveValues(data = NULL, call = NULL)
    
    shiny::observeEvent(input$run_124, {
      shiny::req(rv$processed_data, rv$mapping$name_fu, rv$mapping$name_id, is_3L())
      args <- list(name_id = "id", names_eq5d = DIMS_STD, name_fu = "fu")
      cs   <- format_call("eq5d_profile_dimension_change_table", c(list(df = "data"), args))
      tryCatch({
        r124$data <- do.call(eq5d_profile_dimension_change_table, c(list(df = rv$processed_data), args))
        r124$call <- cs
        save_result(rv, "Level change (1.2.4)", cs, "table", data = r124$data)
      }, error = function(e) err_notify(e))
    })
    
    output$out_124 <- shiny::renderUI({
      if (is.null(r124$data)) return(NULL)
      shiny::tagList(analysis_table_output(ns("tbl_124")), call_display(ns("call_124")))
    })
    output$tbl_124  <- render_analysis_table(shiny::reactive(r124$data))
    output$call_124 <- shiny::renderText(r124$call)
    
    # ── Table 1.2.2 ───────────────────────────────────────────────────────────
    r122 <- shiny::reactiveValues(data = NULL, call = NULL)
    
    shiny::observeEvent(input$run_122, {
      shiny::req(rv$processed_data, rv$mapping$name_fu, rv$mapping$name_id)
      df_gv <- ensure_groupvar(rv$processed_data)
      args  <- list(name_id = "id", name_groupvar = "groupvar",
                    names_eq5d = DIMS_STD, name_fu = "fu")
      cs    <- format_call("eq5d_profile_pchc_table", c(list(df = "data"), args))
      tryCatch({
        r122$data <- do.call(eq5d_profile_pchc_table, c(list(df = df_gv), args))
        r122$call <- cs
        save_result(rv, "PCHC (1.2.2)", cs, "table", data = r122$data)
      }, error = function(e) err_notify(e))
    })
    
    output$out_122 <- shiny::renderUI({
      if (is.null(r122$data)) return(NULL)
      shiny::tagList(analysis_table_output(ns("tbl_122")), call_display(ns("call_122")))
    })
    output$tbl_122  <- render_analysis_table(shiny::reactive(r122$data))
    output$call_122 <- shiny::renderText(r122$call)
    
    # ── Table 1.2.3 (3L only) ────────────────────────────────────────────────
    r123 <- shiny::reactiveValues(data = NULL, call = NULL)
    
    shiny::observeEvent(input$run_123, {
      shiny::req(rv$processed_data, rv$mapping$name_fu, rv$mapping$name_id, is_3L())
      df_gv <- ensure_groupvar(rv$processed_data)
      args  <- list(name_id = "id", name_groupvar = "groupvar",
                    names_eq5d = DIMS_STD, name_fu = "fu")
      cs    <- format_call("eq5d_profile_pchc_with_no_problems_table", c(list(df = "data"), args))
      tryCatch({
        r123$data <- do.call(eq5d_profile_pchc_with_no_problems_table, c(list(df = df_gv), args))
        r123$call <- cs
        save_result(rv, "PCHC no-problems (1.2.3)", cs, "table", data = r123$data)
      }, error = function(e) err_notify(e))
    })
    
    output$out_123 <- shiny::renderUI({
      if (is.null(r123$data)) return(NULL)
      shiny::tagList(analysis_table_output(ns("tbl_123")), call_display(ns("call_123")))
    })
    output$tbl_123  <- render_analysis_table(shiny::reactive(r123$data))
    output$call_123 <- shiny::renderText(r123$call)
    
    # ── Figure 1.2.1 ─────────────────────────────────────────────────────────
    r121fig <- shiny::reactiveValues(plot = NULL, call = NULL)
    
    shiny::observeEvent(input$run_121fig, {
      shiny::req(rv$processed_data, rv$mapping$name_fu, rv$mapping$name_id)
      df_gv <- ensure_groupvar(rv$processed_data)
      args  <- list(name_id = "id", name_groupvar = "groupvar",
                    names_eq5d = DIMS_STD, name_fu = "fu")
      cs    <- format_call("eq5d_profile_pchc_by_group_plot", c(list(df = "data"), args))
      tryCatch({
        res          <- do.call(eq5d_profile_pchc_by_group_plot, c(list(df = df_gv), args))
        r121fig$plot <- res$p
        r121fig$call <- cs
        save_result(rv, "PCHC bar chart (fig. 1.2.1)", cs, "plot", plot = res$p)
      }, error = function(e) err_notify(e))
    })
    
    output$out_121fig <- shiny::renderUI({
      if (is.null(r121fig$plot)) return(NULL)
      shiny::tagList(shiny::plotOutput(ns("plt_121fig"), height = "420px"),
                     call_display(ns("call_121fig")))
    })
    output$plt_121fig  <- shiny::renderPlot(r121fig$plot)
    output$call_121fig <- shiny::renderText(r121fig$call)
    
    # ── Figure 1.2.2 ─────────────────────────────────────────────────────────
    r122fig <- shiny::reactiveValues(plot = NULL, call = NULL)
    
    shiny::observeEvent(input$run_122fig, {
      shiny::req(rv$processed_data, rv$mapping$name_fu, rv$mapping$name_id)
      df_gv <- ensure_groupvar(rv$processed_data)
      args  <- list(name_id = "id", name_groupvar = "groupvar",
                    names_eq5d = DIMS_STD, name_fu = "fu")
      cs    <- format_call("eq5d_profile_better_dimensions_by_group_plot", c(list(df = "data"), args))
      tryCatch({
        res          <- do.call(eq5d_profile_better_dimensions_by_group_plot, c(list(df = df_gv), args))
        r122fig$plot <- res$p
        r122fig$call <- cs
        save_result(rv, "Improvements chart (fig. 1.2.2)", cs, "plot", plot = res$p)
      }, error = function(e) err_notify(e))
    })
    
    output$out_122fig <- shiny::renderUI({
      if (is.null(r122fig$plot)) return(NULL)
      shiny::tagList(shiny::plotOutput(ns("plt_122fig"), height = "420px"),
                     call_display(ns("call_122fig")))
    })
    output$plt_122fig  <- shiny::renderPlot(r122fig$plot)
    output$call_122fig <- shiny::renderText(r122fig$call)
    
    # ── Figure 1.2.3 ─────────────────────────────────────────────────────────
    r123fig <- shiny::reactiveValues(plot = NULL, call = NULL)
    
    shiny::observeEvent(input$run_123fig, {
      shiny::req(rv$processed_data, rv$mapping$name_fu, rv$mapping$name_id)
      df_gv <- ensure_groupvar(rv$processed_data)
      args  <- list(name_id = "id", name_groupvar = "groupvar",
                    names_eq5d = DIMS_STD, name_fu = "fu")
      cs    <- format_call("eq5d_profile_worse_dimensions_by_group_plot", c(list(df = "data"), args))
      tryCatch({
        res          <- do.call(eq5d_profile_worse_dimensions_by_group_plot, c(list(df = df_gv), args))
        r123fig$plot <- res$p
        r123fig$call <- cs
        save_result(rv, "Worsenings chart (fig. 1.2.3)", cs, "plot", plot = res$p)
      }, error = function(e) err_notify(e))
    })
    
    output$out_123fig <- shiny::renderUI({
      if (is.null(r123fig$plot)) return(NULL)
      shiny::tagList(shiny::plotOutput(ns("plt_123fig"), height = "420px"),
                     call_display(ns("call_123fig")))
    })
    output$plt_123fig  <- shiny::renderPlot(r123fig$plot)
    output$call_123fig <- shiny::renderText(r123fig$call)
    
    # ── Figure 1.2.4 ─────────────────────────────────────────────────────────
    r124fig <- shiny::reactiveValues(plot = NULL, call = NULL)
    
    shiny::observeEvent(input$run_124fig, {
      shiny::req(rv$processed_data, rv$mapping$name_fu, rv$mapping$name_id)
      df_gv <- ensure_groupvar(rv$processed_data)
      args  <- list(name_id = "id", name_groupvar = "groupvar",
                    names_eq5d = DIMS_STD, name_fu = "fu")
      cs    <- format_call("eq5d_profile_mixed_dimensions_by_group_plot", c(list(df = "data"), args))
      tryCatch({
        res          <- do.call(eq5d_profile_mixed_dimensions_by_group_plot, c(list(df = df_gv), args))
        r124fig$plot <- res$p
        r124fig$call <- cs
        save_result(rv, "Mixed change chart (fig. 1.2.4)", cs, "plot", plot = res$p)
      }, error = function(e) err_notify(e))
    })
    
    output$out_124fig <- shiny::renderUI({
      if (is.null(r124fig$plot)) return(NULL)
      shiny::tagList(shiny::plotOutput(ns("plt_124fig"), height = "420px"),
                     call_display(ns("call_124fig")))
    })
    output$plt_124fig  <- shiny::renderPlot(r124fig$plot)
    output$call_124fig <- shiny::renderText(r124fig$call)
    
    # ── Figure 1.2.5 — HPG ───────────────────────────────────────────────────
    r125fig <- shiny::reactiveValues(plot = NULL, call = NULL)
    
    shiny::observeEvent(input$run_125fig, {
      shiny::req(rv$processed_data, rv$mapping$name_fu, rv$mapping$name_id,
                 nzchar(input$country_125 %||% ""))
      tp_sel <- input$fu_levels_125
      if (is.null(tp_sel) || length(tp_sel) != 2L) {
        shiny::showNotification(
          "Please select exactly two timepoints for the HPG plot.",
          type = "warning", duration = 5
        )
        return()
      }
      ver  <- eq5d_ver()
      args <- list(names_eq5d = DIMS_STD, name_fu = "fu", levels_fu = tp_sel,
                   name_id = "id", eq5d_version = ver, country = input$country_125)
      cs   <- format_call("eq5d_profile_health_profile_grid", c(list(df = "data"), args))
      tryCatch({
        res          <- do.call(eq5d_profile_health_profile_grid, c(list(df = rv$processed_data), args))
        r125fig$plot <- res$p
        r125fig$call <- cs
        save_result(rv, "HPG scatter (fig. 1.2.5)", cs, "plot", plot = res$p)
      }, error = function(e) err_notify(e))
    })
    
    output$out_125fig <- shiny::renderUI({
      if (is.null(r125fig$plot)) return(NULL)
      shiny::tagList(shiny::plotOutput(ns("plt_125fig"), height = "500px"),
                     call_display(ns("call_125fig")))
    })
    output$plt_125fig  <- shiny::renderPlot(r125fig$plot)
    output$call_125fig <- shiny::renderText(r125fig$call)
    
    # ═══════════════════════════════════════════════════════════════════════════
    # SUMMARIZING SEVERITY
    # ═══════════════════════════════════════════════════════════════════════════
    
    # ── Table 1.3.1 ───────────────────────────────────────────────────────────
    r131 <- shiny::reactiveValues(data = NULL, call = NULL)
    
    shiny::observeEvent(input$run_131, {
      shiny::req(rv$processed_data, nzchar(input$country_131 %||% ""))
      ver  <- eq5d_ver()
      args <- list(names_eq5d = DIMS_STD, eq5d_version = ver, country = input$country_131)
      cs   <- format_call("eq5d_profile_lss_utility_summary", c(list(df = "data"), args))
      tryCatch({
        r131$data <- do.call(eq5d_profile_lss_utility_summary, c(list(df = rv$processed_data), args))
        r131$call <- cs
        save_result(rv, "LSS summary stats (1.3.1)", cs, "table", data = r131$data)
      }, error = function(e) err_notify(e))
    })
    
    output$out_131 <- shiny::renderUI({
      if (is.null(r131$data)) return(NULL)
      shiny::tagList(analysis_table_output(ns("tbl_131")), call_display(ns("call_131")))
    })
    output$tbl_131  <- render_analysis_table(shiny::reactive(r131$data))
    output$call_131 <- shiny::renderText(r131$call)
    
    # ── Figure 1.3.1 ─────────────────────────────────────────────────────────
    r131fig <- shiny::reactiveValues(plot = NULL, call = NULL)
    
    shiny::observeEvent(input$run_131fig, {
      shiny::req(rv$processed_data, nzchar(input$country_131fig %||% ""))
      ver  <- eq5d_ver()
      args <- list(names_eq5d = DIMS_STD, eq5d_version = ver, country = input$country_131fig)
      cs   <- format_call("eq5d_profile_lss_utility_plot", c(list(df = "data"), args))
      tryCatch({
        res          <- do.call(eq5d_profile_lss_utility_plot, c(list(df = rv$processed_data), args))
        r131fig$plot <- res$p
        r131fig$call <- cs
        save_result(rv, "LSS scatter (fig. 1.3.1)", cs, "plot", plot = res$p)
      }, error = function(e) err_notify(e))
    })
    
    output$out_131fig <- shiny::renderUI({
      if (is.null(r131fig$plot)) return(NULL)
      shiny::tagList(shiny::plotOutput(ns("plt_131fig"), height = "420px"),
                     call_display(ns("call_131fig")))
    })
    output$plt_131fig  <- shiny::renderPlot(r131fig$plot)
    output$call_131fig <- shiny::renderText(r131fig$call)
    
    # ── Table 1.3.2 ───────────────────────────────────────────────────────────
    r132 <- shiny::reactiveValues(data = NULL, call = NULL)
    
    shiny::observeEvent(input$run_132, {
      shiny::req(rv$processed_data)
      ver  <- eq5d_ver()
      args <- list(names_eq5d = DIMS_STD, eq5d_version = ver)
      cs   <- format_call("eq5d_profile_lfs_distribution", c(list(df = "data"), args))
      tryCatch({
        r132$data <- do.call(eq5d_profile_lfs_distribution, c(list(df = rv$processed_data), args))
        r132$call <- cs
        save_result(rv, "LFS distribution (1.3.2)", cs, "table", data = r132$data)
      }, error = function(e) err_notify(e))
    })
    
    output$out_132 <- shiny::renderUI({
      if (is.null(r132$data)) return(NULL)
      shiny::tagList(analysis_table_output(ns("tbl_132")), call_display(ns("call_132")))
    })
    output$tbl_132  <- render_analysis_table(shiny::reactive(r132$data))
    output$call_132 <- shiny::renderText(r132$call)
    
    # ── Table 1.3.4 ───────────────────────────────────────────────────────────
    r134 <- shiny::reactiveValues(data = NULL, call = NULL)
    
    shiny::observeEvent(input$run_134, {
      shiny::req(rv$processed_data, nzchar(input$country_134 %||% ""))
      ver  <- eq5d_ver()
      args <- list(names_eq5d = DIMS_STD, eq5d_version = ver, country = input$country_134)
      cs   <- format_call("eq5d_profile_lfs_utility_summary", c(list(df = "data"), args))
      tryCatch({
        r134$data <- do.call(eq5d_profile_lfs_utility_summary, c(list(df = rv$processed_data), args))
        r134$call <- cs
        save_result(rv, "LFS summary stats (1.3.4)", cs, "table", data = r134$data)
      }, error = function(e) err_notify(e))
    })
    
    output$out_134 <- shiny::renderUI({
      if (is.null(r134$data)) return(NULL)
      shiny::tagList(analysis_table_output(ns("tbl_134")), call_display(ns("call_134")))
    })
    output$tbl_134  <- render_analysis_table(shiny::reactive(r134$data))
    output$call_134 <- shiny::renderText(r134$call)
    
    # ── Figure 1.3.2 ─────────────────────────────────────────────────────────
    r132fig <- shiny::reactiveValues(plot = NULL, call = NULL)
    
    shiny::observeEvent(input$run_132fig, {
      shiny::req(rv$processed_data, nzchar(input$country_132fig %||% ""))
      ver  <- eq5d_ver()
      args <- list(names_eq5d = DIMS_STD, eq5d_version = ver, country = input$country_132fig)
      cs   <- format_call("eq5d_profile_lfs_utility_plot", c(list(df = "data"), args))
      tryCatch({
        res          <- do.call(eq5d_profile_lfs_utility_plot, c(list(df = rv$processed_data), args))
        r132fig$plot <- res$p
        r132fig$call <- cs
        save_result(rv, "LFS scatter (fig. 1.3.2)", cs, "plot", plot = res$p)
      }, error = function(e) err_notify(e))
    })
    
    output$out_132fig <- shiny::renderUI({
      if (is.null(r132fig$plot)) return(NULL)
      shiny::tagList(shiny::plotOutput(ns("plt_132fig"), height = "420px"),
                     call_display(ns("call_132fig")))
    })
    output$plt_132fig  <- shiny::renderPlot(r132fig$plot)
    output$call_132fig <- shiny::renderText(r132fig$call)
    
    # ═══════════════════════════════════════════════════════════════════════════
    # INFORMATIVITY
    # ═══════════════════════════════════════════════════════════════════════════
    
    # ── Figure 1.4.1 ─────────────────────────────────────────────────────────
    r141 <- shiny::reactiveValues(plot = NULL, call = NULL)
    
    shiny::observeEvent(input$run_141, {
      shiny::req(rv$processed_data)
      ver  <- eq5d_ver()
      args <- list(names_eq5d = DIMS_STD, eq5d_version = ver)
      cs   <- format_call("eq5d_profile_density_curve", c(list(df = "data"), args))
      tryCatch({
        res       <- do.call(eq5d_profile_density_curve, c(list(df = rv$processed_data), args))
        r141$plot <- res$p
        r141$call <- cs
        save_result(rv, "HSDI (fig. 1.4.1)", cs, "plot", plot = r141$plot)
      }, error = function(e) err_notify(e))
    })
    
    output$out_141 <- shiny::renderUI({
      if (is.null(r141$plot)) return(NULL)
      shiny::tagList(shiny::plotOutput(ns("plt_141"), height = "450px"),
                     call_display(ns("call_141")))
    })
    output$plt_141  <- shiny::renderPlot(r141$plot)
    output$call_141 <- shiny::renderText(r141$call)
  })
}

# ── Shared rendering helpers (module-local) ───────────────────────────────────

analysis_table_output <- function(output_id) {
  shiny::tagList(DT::DTOutput(output_id), shiny::br())
}

build_profile_container <- function(df) {
  col_names <- names(df)
  if (length(col_names) < 2L || col_names[1L] != "level") return(NULL)
  
  rest <- col_names[-1L]
  if (!all(grepl("^(n|freq)_", rest))) return(NULL)
  
  parsed <- lapply(rest, function(cn) {
    parts <- strsplit(cn, "_")[[1L]]
    if (length(parts) < 3L) return(NULL)
    list(
      metric = parts[1L],
      dim    = parts[length(parts)],
      fu     = paste(parts[2L:(length(parts) - 1L)], collapse = "_")
    )
  })
  if (any(vapply(parsed, is.null, logical(1L)))) return(NULL)
  
  dims <- vapply(parsed, `[[`, character(1L), "dim")
  if (!all(dims %in% names(DIM_LABELS))) return(NULL)
  
  fu_vals   <- vapply(parsed, `[[`, character(1L), "fu")
  multi_fu  <- length(unique(fu_vals)) > 1L
  dim_order <- unique(dims)
  
  th_level <- shiny::tags$th(rowspan = 2L, "Level")
  th_dims  <- lapply(dim_order, function(d) {
    shiny::tags$th(
      colspan = sum(dims == d),
      style   = "text-align: center; border-bottom: 0;",
      DIM_LABELS[[d]]
    )
  })
  th_sub <- lapply(seq_along(rest), function(i) {
    p      <- parsed[[i]]
    symbol <- if (p$metric == "freq") "%" else "n"
    label  <- if (multi_fu) paste0(p$fu, " ", symbol) else symbol
    shiny::tags$th(label, style = "text-align: right;")
  })
  
  shiny::tags$table(
    class = "table table-sm table-striped display",
    shiny::tags$thead(
      shiny::tags$tr(th_level, th_dims),
      shiny::tags$tr(th_sub)
    )
  )
}

render_analysis_table <- function(data_reactive) {
  DT::renderDT({
    df <- data_reactive()
    shiny::req(df)
    
    container      <- build_profile_container(df)
    freq_col_names <- names(df)[grepl("^freq_", names(df))]
    # Bare proportion columns produced by eq5d_profile_top_states / eq5d_profile_*_table functions
    pct_col_names  <- intersect(c("p", "cum_p"), names(df))
    all_pct        <- c(freq_col_names, pct_col_names)
    # Round remaining double columns (e.g. mean, sd) to 2 decimal places
    dbl_col_names  <- setdiff(
      names(df)[vapply(df, is.double, logical(1L))],
      all_pct
    )
    
    opts <- list(pageLength = 15L, scrollX = TRUE, dom = "tip")
    
    dt <- if (!is.null(container)) {
      DT::datatable(df, container = container, options = opts, rownames = FALSE)
    } else {
      DT::datatable(df, options = opts, rownames = FALSE,
                    class = "table-sm table-striped")
    }
    
    if (length(freq_col_names) > 0L)
      dt <- DT::formatPercentage(dt, columns = freq_col_names, digits = 1L)
    if (length(pct_col_names) > 0L)
      dt <- DT::formatPercentage(dt, columns = pct_col_names, digits = 1L)
    if (length(dbl_col_names) > 0L)
      dt <- DT::formatRound(dt, columns = dbl_col_names, digits = 2L)
    
    dt
  })
}

call_display <- function(output_id) {
  shiny::tagList(
    shiny::tags$details(
      shiny::tags$summary(shiny::tags$small("Show R code", class = "text-muted")),
      shiny::verbatimTextOutput(output_id, placeholder = TRUE)
    ),
    shiny::br()
  )
}

err_notify <- function(e) {
  shiny::showNotification(
    paste("Analysis error:", conditionMessage(e)),
    type = "error", duration = 8
  )
}
