# ui.R — main UI definition

# ── Shared footer (defined once) ──────────────────────────────────────────────
mih_footer <- shiny::tags$div(
  class = "mih-footer",
  style = "
    display:flex;
    flex-direction:column;
    align-items:center;
    justify-content:center;
    text-align:center;
    gap:0.4rem;
  ",
  
  # # Logo (big and centered)
  # shiny::tags$a(
  #   href   = "https://www.mathsinhealth.com",
  #   target = "_blank",
  #   shiny::tags$img(
  #     src = "logo.png",
  #     height = "20px",
  #     style = "margin-bottom:4px;"
  #   )
  # ),
  
  # Text below logo
  shiny::tags$div(
    style = "font-size:0.9rem;",
    "Developed by ",
    shiny::tags$a(
      href   = "https://www.mathsinhealth.com",
      target = "_blank",
      shiny::tags$strong("Maths in Health")
    )
  )
)

# ── Navbar brand with MiH logo ────────────────────────────────────────────────
navbar_brand <- shiny::tags$span(
  class = "mih-brand-logo",
  shiny::tags$img(
    src = "logo.png",
    height = "28px",
    style = "margin-right: 8px;"
  ),
  shiny::tags$span(
    shiny::tags$strong("eq5dsuite"),
    shiny::tags$span(class = "mih-label", "\u00a0|\u00a0EQ-5D Analysis")
  )
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- shiny::tagList(
  bslib::page_navbar(
    title = navbar_brand,
    theme = bslib::bs_theme(
      version    = 5,
      bootswatch = "flatly",
      primary    = "#142036", #"#4f6f9f",
      success    = "#5c8f6f"
    ),
    # Load custom stylesheet
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    window_title = "eq5dsuite | Maths in Health",

    # ── Home ────────────────────────────────────────────────────────────────
    bslib::nav_panel(
      title = shiny::tagList(shiny::icon("house"), " Home"),
      value = "home",
      bslib::layout_columns(
        col_widths = c(7, 5),

        # Left: welcome + workflow
        bslib::card(
          bslib::card_header(
            shiny::tags$span(shiny::icon("chart-line"), "Welcome to eq5dsuite")
          ),
          bslib::card_body(
            shiny::p(
              style = "font-size:2.88rem; color:#344055; margin-bottom:0.6rem;",
              "Welcome to eq5dsuite"
            ),
            shiny::p(
              style = "font-size:0.88rem; color:#344055; margin-bottom:0.6rem;",
              "Upload, validate, analyse, and export EQ-5D data using the ",
              shiny::code("eq5dsuite"), " R package. ",
              "Analyses follow ",
              shiny::tags$em("Devlin et al. (2020) — Methods for Analyzing and Reporting EQ-5D Data.")
            ),
            shiny::tags$div(
              style = "font-size:0.8rem; font-weight:600; color:#6b7280;
                       text-transform:uppercase; letter-spacing:0.05em;
                       margin-bottom:0.4rem;",
              "Workflow"
            ),
            shiny::tags$div(
              class = "workflow-steps",
              # Step 1
              shiny::tags$div(
                class = "workflow-step",
                shiny::tags$div(class = "workflow-step-num", "1"),
                shiny::tags$div(
                  class = "workflow-step-body",
                  shiny::tags$strong(shiny::icon("upload"), " Data"),
                  shiny::tags$span("Upload file & map columns")
                )
              ),
              # Step 2
              shiny::tags$div(
                class = "workflow-step",
                shiny::tags$div(class = "workflow-step-num", "2"),
                shiny::tags$div(
                  class = "workflow-step-body",
                  shiny::tags$strong(shiny::icon("shield-check"), " Validation"),
                  shiny::tags$span("Review data quality")
                )
              ),
              # Step 3
              shiny::tags$div(
                class = "workflow-step",
                shiny::tags$div(class = "workflow-step-num", "3"),
                shiny::tags$div(
                  class = "workflow-step-body",
                  shiny::tags$strong(shiny::icon("magnifying-glass-chart"), " Analysis"),
                  shiny::tags$span("Profile, VAS & utility")
                )
              ),
              # Step 4
              shiny::tags$div(
                class = "workflow-step",
                shiny::tags$div(class = "workflow-step-num", "4"),
                shiny::tags$div(
                  class = "workflow-step-body",
                  shiny::tags$strong(shiny::icon("table-list"), " Results"),
                  shiny::tags$span("Review saved outputs")
                )
              ),
              # Step 5
              shiny::tags$div(
                class = "workflow-step",
                shiny::tags$div(class = "workflow-step-num", "5"),
                shiny::tags$div(
                  class = "workflow-step-body",
                  shiny::tags$strong(shiny::icon("download"), " Export"),
                  shiny::tags$span("Download tables & plots")
                )
              )
            )
          )
        ),

        # Right: quick start
        bslib::card(
          bslib::card_header(
            shiny::tags$span(shiny::icon("circle-info"), " Quick start")
          ),
          bslib::card_body(
            shiny::tags$div(
              style = "font-size:0.8rem; font-weight:600; color:#6b7280;
                       text-transform:uppercase; letter-spacing:0.05em;
                       margin-bottom:0.35rem;",
              "Accepted formats"
            ),
            shiny::tags$div(
              class = "qs-item",
              shiny::tags$span(class = "qs-dot", "\u25cf"),
              shiny::tags$span(
                shiny::code(".csv"), ", ", shiny::code(".xlsx"), ", ",
                shiny::code(".rds")
              )
            ),
            shiny::tags$div(
              class = "qs-item",
              shiny::tags$span(class = "qs-dot", "\u25cf"),
              "EQ-5D-3L and EQ-5D-5L"
            ),
            shiny::tags$div(
              class = "qs-item",
              shiny::tags$span(class = "qs-dot", "\u25cf"),
              "Cross-sectional and longitudinal"
            ),
            shiny::tags$hr(style = "margin: 0.55rem 0;"),
            shiny::tags$div(
              style = "font-size:0.8rem; font-weight:600; color:#6b7280;
                       text-transform:uppercase; letter-spacing:0.05em;
                       margin-bottom:0.35rem;",
              "Required"
            ),
            shiny::tags$div(
              class = "qs-item",
              shiny::tags$span(class = "qs-dot", "\u25cf"),
              "Five EQ-5D dimensions: MO, SC, UA, PD, AD"
            ),
            shiny::tags$hr(style = "margin: 0.55rem 0;"),
            shiny::tags$div(
              style = "font-size:0.8rem; font-weight:600; color:#6b7280;
                       text-transform:uppercase; letter-spacing:0.05em;
                       margin-bottom:0.35rem;",
              "Optional"
            ),
            shiny::tags$div(
              class = "qs-item",
              shiny::tags$span(class = "qs-dot", "\u25cf"),
              shiny::tags$strong("Timepoint"), " — unlocks longitudinal analyses"
            ),
            shiny::tags$div(
              class = "qs-item",
              shiny::tags$span(class = "qs-dot", "\u25cf"),
              shiny::tags$strong("Group"), " — enables stratified analyses"
            ),
            shiny::tags$div(
              class = "qs-item",
              shiny::tags$span(class = "qs-dot", "\u25cf"),
              shiny::tags$strong("Patient ID"), " — matched analyses (PCHC)"
            ),
            shiny::tags$div(
              class = "qs-item",
              shiny::tags$span(class = "qs-dot", "\u25cf"),
              shiny::tags$strong("VAS score"), " — EQ-VAS analyses"
            ),
            shiny::tags$div(
              class = "qs-item",
              shiny::tags$span(class = "qs-dot", "\u25cf"),
              shiny::tags$strong("Utility / index"), " — or calculate on Data page"
            )
          )
        )
      )
    ),

    # ── Data ──────────────────────────────────────────────────────────────────
    bslib::nav_panel(
      title = shiny::tagList(shiny::icon("upload"), " Data"),
      value = "data",
      mod_data_ui("data")
    ),

    # ── Validation ────────────────────────────────────────────────────────────
    bslib::nav_panel(
      title = shiny::tagList(shiny::icon("shield-check"), " Validation"),
      value = "validation",
      mod_validation_ui("validation")
    ),

    # ── Analysis ──────────────────────────────────────────────────────────────
    bslib::nav_panel(
      title = shiny::tagList(shiny::icon("chart-bar"), " Analysis"),
      value = "analysis",
      shiny::tabsetPanel(
        id   = "analysis_main_tabs",
        type = "tabs",
        shiny::tabPanel(
          title = "EQ-5D Profile",
          value = "tab_profile",
          mod_analysis_profile_ui("profile")
        ),
        shiny::tabPanel(
          title = "EQ-VAS",
          value = "tab_vas",
          mod_analysis_vas_ui("vas")
        ),
        shiny::tabPanel(
          title = "EQ-5D Utility",
          value = "tab_utility",
          mod_analysis_utility_ui("utility")
        )
      )
    ),

    # ── Results ───────────────────────────────────────────────────────────────
    bslib::nav_panel(
      title = shiny::tagList(shiny::icon("table-list"), " Results"),
      value = "results",
      mod_results_ui("results")
    ),

    # ── Export ────────────────────────────────────────────────────────────────
    bslib::nav_panel(
      title = shiny::tagList(shiny::icon("download"), " Export"),
      value = "export",
      mod_export_ui("export")
    ),

    # Right-aligned nav items
    bslib::nav_spacer(),
    bslib::nav_item(
      shiny::tags$small(
        shiny::tags$a(
          href   = "https://github.com/MathsInHealth/eq5dsuite",
          target = "_blank",
          style  = "color: rgba(255,255,255,0.5); font-size:0.75rem;",
          "eq5dsuite package"
        )
      )
    )
  )

  # ── Global footer (defined once, visible on all pages) ────────────────────
  # mih_footer
)
