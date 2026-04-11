# mod_data.R — Data upload, variable mapping, utility calculation, and download

# ── UI ────────────────────────────────────────────────────────────────────────

mod_data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Row 1: Upload + Preview
    bslib::layout_columns(
      col_widths = c(5, 7),
      
      # Left panel: upload + mapping controls
      bslib::card(
        bslib::card_header(
          shiny::tags$span(shiny::icon("upload"), " Upload & Variable Mapping")
        ),
        bslib::card_body(
          
          # File upload
          shiny::fileInput(
            ns("file"), "Choose file",
            accept      = c(".csv", ".xlsx", ".rds"),
            placeholder = "CSV, XLSX, or RDS"
          ),
          shiny::div(
            class = "d-flex align-items-center my-2",
            shiny::tags$span(class = "text-muted small me-2", "or"),
            shiny::actionButton(
              ns("use_example"), "Try example data",
              icon  = shiny::icon("database"),
              class = "btn-outline-secondary btn-sm"
            )
          ),

          # EQ-5D version
          shiny::selectInput(
            ns("version"), "EQ-5D version",
            choices  = c("3L", "5L"),
            selected = "3L"
          ),
          
          shiny::hr(style = "margin: 0.6rem 0;"),
          
          # Column mapping (rendered after upload)
          # NOTE: Country selector has been removed from here.
          # It now lives exclusively in the Calculate Utility section.
          shiny::uiOutput(ns("mapping_ui")),
          
          shiny::hr(style = "margin: 0.6rem 0;"),
          shiny::actionButton(
            ns("confirm"), "Confirm Mapping",
            class = "btn-primary w-100",
            icon  = shiny::icon("check")
          )
        )
      ),
      
      # Right panel: data preview
      bslib::card(
        bslib::card_header(
          shiny::tags$span(shiny::icon("table"), " Data Preview")
        ),
        bslib::card_body(
          shiny::uiOutput(ns("preview_info")),
          DT::DTOutput(ns("preview_table"))
        )
      )
    ),
    
    # Row 2: Utility calculation (shown after mapping confirmed)
    shiny::uiOutput(ns("utility_section_ui")),
    
    # Row 3: Download (shown when data is ready)
    shiny::uiOutput(ns("download_section_ui"))
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_data_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Reactive: read uploaded file ─────────────────────────────────────────
    active_data <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$file, {
      ext <- tools::file_ext(input$file$name)
      tryCatch(
        active_data(read_uploaded_file(input$file$datapath, ext)),
        error = function(e) {
          shiny::showNotification(paste("Error reading file:", conditionMessage(e)),
                                  type = "error", duration = 8)
        }
      )
    })

    shiny::observeEvent(input$use_example, {
      active_data(eq5dsuite::example_data)
      shiny::showNotification("Example dataset loaded (10 000 rows).",
                              type = "message", duration = 3)
    })

    uploaded <- shiny::reactive(active_data())
    
    # ── Reactive: smart column suggestions ───────────────────────────────────
    suggestions <- shiny::reactive({
      df <- uploaded()
      if (is.null(df)) return(list())
      suggest_mapping(names(df))
    })
    
    # ── Render: column mapping selectors ─────────────────────────────────────
    output$mapping_ui <- shiny::renderUI({
      df <- uploaded()
      if (is.null(df)) {
        return(shiny::p(shiny::em("Upload a file to map columns."),
                        class = "text-muted"))
      }
      cols      <- names(df)
      cols_none <- c("(none)" = "")
      sug       <- suggestions()
      
      sel <- function(label, inputId, choices, selected, ...) {
        shiny::selectInput(
          ns(inputId), label,
          choices  = choices,
          selected = selected %||% "",
          ...
        )
      }
      
      shiny::tagList(
        shiny::h6("EQ-5D Dimensions", class = "text-muted fw-semibold mt-2"),
        shiny::p(shiny::em("All five dimensions are required."),
                 class = "small text-muted"),
        bslib::layout_columns(
          col_widths = c(6, 6),
          sel("Mobility (MO)",           "col_mo", cols, sug$mo),
          sel("Self-care (SC)",          "col_sc", cols, sug$sc),
          sel("Usual activities (UA)",   "col_ua", cols, sug$ua),
          sel("Pain/discomfort (PD)",    "col_pd", cols, sug$pd),
          sel("Anxiety/depression (AD)", "col_ad", cols, sug$ad),
          shiny::div()   # placeholder to keep 2-col layout tidy
        ),
        
        shiny::h6("Optional Variables", class = "text-muted fw-semibold mt-3"),
        bslib::layout_columns(
          col_widths = c(6, 6),
          sel("Timepoint / Follow-up", "col_fu",
              c(cols_none, cols), sug$name_fu),
          sel("Group / Category",      "col_groupvar",
              c(cols_none, cols), sug$name_groupvar),
          sel("Patient ID",            "col_id",
              c(cols_none, cols), sug$name_id),
          sel("EQ-VAS score",          "col_vas",
              c(cols_none, cols), sug$name_vas),
          sel("Utility / Index column", "col_utility",
              c(cols_none, cols), sug$name_utility)
        )
      )
    })
    
    # ── Render: data preview ──────────────────────────────────────────────────
    # FIXED: preview now uses local_data() (which reflects processed_data if
    # available) rather than the raw uploaded() reactive.  This means the
    # table refreshes immediately whenever a utility column is added.
    output$preview_info <- shiny::renderUI({
      df <- local_data()
      if (is.null(df)) {
        # Fall back to raw upload count before mapping is confirmed
        df_raw <- uploaded()
        if (is.null(df_raw)) return(NULL)
        df <- df_raw
      }
      shiny::p(
        shiny::strong(format(nrow(df), big.mark = ",")), " rows \u00d7 ",
        shiny::strong(ncol(df)), " columns",
        class = "text-muted small mb-2"
      )
    })
    
    output$preview_table <- DT::renderDT({
      # Use local_data() when mapping is confirmed (includes utility cols);
      # fall back to raw uploaded data while mapping is still being set up.
      df <- tryCatch(local_data(), error = function(e) NULL)
      if (is.null(df)) df <- uploaded()
      shiny::req(df)
      DT::datatable(
        head(df, 200L),
        options  = list(pageLength = 10L, scrollX = TRUE, dom = "tip"),
        rownames = FALSE,
        class    = "table-sm table-striped"
      )
    })
    
    # ── Validation helpers ────────────────────────────────────────────────────
    mapping_complete <- shiny::reactive({
      if (is.null(uploaded())) return(FALSE)
      dims <- c(input$col_mo, input$col_sc, input$col_ua, input$col_pd, input$col_ad)
      all(nzchar(dims)) && length(unique(dims)) == 5L
    })
    
    # ── Confirm mapping ───────────────────────────────────────────────────────
    shiny::observeEvent(input$confirm, {
      if (!isTRUE(mapping_complete())) {
        shiny::showNotification(
          "Please map all five EQ-5D dimension columns (MO, SC, UA, PD, AD) \
           to distinct columns before confirming.",
          type = "warning", duration = 5
        )
        return()
      }
      
      rv$raw_data <- uploaded()
      rv$mapping  <- list(
        eq5d_version  = input$version,
        names_eq5d    = c(input$col_mo, input$col_sc,
                          input$col_ua, input$col_pd, input$col_ad),
        name_fu       = if (nzchar(input$col_fu))       input$col_fu       else NULL,
        name_groupvar = if (nzchar(input$col_groupvar)) input$col_groupvar else NULL,
        name_id       = if (nzchar(input$col_id))       input$col_id       else NULL,
        name_vas      = if (nzchar(input$col_vas))      input$col_vas      else NULL,
        name_utility  = if (nzchar(input$col_utility))  input$col_utility  else NULL,
        country       = ""   # country now lives only in the utility section
      )
      # Reset downstream state when mapping changes
      rv$processed_data <- NULL
      
      shiny::showNotification(
        "Mapping confirmed. Proceed to the Validation tab or calculate utility below.",
        type = "message", duration = 4
      )
    })
    
    # ── Local working dataset (mapped, may include added utility cols) ─────────
    local_data <- shiny::reactive({
      shiny::req(rv$raw_data, rv$mapping)
      if (!is.null(rv$processed_data)) rv$processed_data else apply_mapping(rv$raw_data, rv$mapping)
    })
    
    # ── Render: utility section (after mapping confirmed) ─────────────────────
    # REDESIGNED: stacked layout with 2-per-row controls at wider widths,
    # no cramped 3-column layout.  Country selector is now here exclusively.
    output$utility_section_ui <- shiny::renderUI({
      shiny::req(rv$mapping)
      
      version         <- rv$mapping$eq5d_version
      has_utility_col <- !is.null(rv$mapping$name_utility) &&
        nzchar(rv$mapping$name_utility %||% "")
      
      # Status banner: no utility column detected vs already present
      status_banner <- if (!has_utility_col) {
        shiny::tags$div(
          class = "alert alert-warning small py-2 mb-3",
          shiny::icon("triangle-exclamation"), " ",
          "No utility column detected. You can calculate utility values below."
        )
      } else {
        shiny::tags$div(
          class = "alert alert-success small py-2 mb-3",
          shiny::icon("circle-check"), " ",
          "Utility column mapped: ",
          shiny::strong(rv$mapping$name_utility), ". ",
          "You can add more utility columns with different value sets below."
        )
      }
      
      method_choices <- get_utility_method_choices(version)
      
      bslib::card(
        bslib::card_header(
          shiny::tags$span(shiny::icon("calculator"), " Calculate Utility")
        ),
        bslib::card_body(
          style = "padding: 1.25rem;",
          
          status_banner,
          
          shiny::p(
            class = "text-muted small mb-3",
            "Compute a utility index from the EQ-5D dimensions and append it as ",
            "a new column. You can add multiple utility columns with different ",
            "value sets or methods."
          ),
          
          # Controls row: Method + Country + Output column name
          # Using fluidRow/column which resolves reliably inside renderUI
          # (bslib::layout_columns can produce overlapping labels there)
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::selectInput(
                ns("utility_method"), "Method",
                choices  = method_choices,
                selected = "direct"
              )
            ),
            shiny::column(
              4,
              shiny::uiOutput(ns("utility_country_picker"))
            ),
            shiny::column(
              4,
              shiny::textInput(
                ns("utility_col_name"), "Output column name",
                value       = "utility",
                placeholder = "e.g. utility_uk, utility_es"
              )
            )
          ),
          
          # Action button
          shiny::actionButton(
            ns("add_utility"), "Add utility column",
            class = "btn-primary",
            icon  = shiny::icon("plus")
          ),
          
          # Status output (appears after column is added)
          shiny::div(
            class = "mt-3",
            shiny::uiOutput(ns("utility_status"))
          )
        )
      )
    })
    
    # ── Render: dynamic method choices (re-render when version changes) ───────
    shiny::observe({
      shiny::req(rv$mapping)
      choices <- get_utility_method_choices(rv$mapping$eq5d_version)
      shiny::updateSelectInput(session, "utility_method", choices = choices,
                               selected = "direct")
    })
    
    # ── Render: utility country picker ────────────────────────────────────────
    output$utility_country_picker <- shiny::renderUI({
      shiny::req(rv$mapping)
      method <- input$utility_method %||% "direct"
      ver <- switch(method,
                    direct = rv$mapping$eq5d_version,
                    xw     = "3L",
                    xwr    = "5L",
                    rv$mapping$eq5d_version
      )
      ch <- get_country_choices(ver)
      if (length(ch) == 0L) {
        return(shiny::p("No value sets found for this method.",
                        class = "text-danger small"))
      }
      shiny::selectizeInput(
        ns("utility_country"), "Country / Value set",
        choices  = c("(select)" = "", ch),
        selected = rv$mapping$country %||% ""
      )
    })
    
    # ── Add utility column ────────────────────────────────────────────────────
    shiny::observeEvent(input$add_utility, {
      shiny::req(
        rv$mapping,
        rv$raw_data,
        nzchar(input$utility_country %||% ""),
        nzchar(trimws(input$utility_col_name %||% ""))
      )
      
      col_name <- trimws(input$utility_col_name)
      method   <- input$utility_method
      country  <- input$utility_country
      df       <- local_data()
      
      if (col_name %in% names(df)) {
        shiny::showNotification(
          paste0('Column "', col_name, '" already exists. ',
                 "Rename the output column or it will be overwritten."),
          type = "warning", duration = 4
        )
      }
      
      tryCatch({
        util_vals <- compute_utility_col(df, method, country, rv$mapping$eq5d_version)
        df[[col_name]] <- util_vals
        
        # FIXED: update processed_data and mapping atomically so that:
        #   (a) preview_table re-renders with the new column immediately
        #   (b) downstream modules see the correct utility column name
        #   (c) if no utility column existed before, this one becomes the default
        new_mapping <- rv$mapping
        new_mapping$name_utility <- col_name
        rv$processed_data <- df
        rv$mapping        <- new_mapping
        
        # Also refresh the utility column selector in the mapping panel so the
        # user sees the newly created column if they revisit the mapping UI.
        shiny::updateSelectInput(
          session, "col_utility",
          choices  = c("(none)" = "", names(df)),
          selected = col_name
        )
        
        shiny::showNotification(
          paste0(
            'Column "', col_name, '" added (',
            sum(!is.na(util_vals)), "/", nrow(df), " non-missing values)."
          ),
          type = "message", duration = 4
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("Error computing utility:", conditionMessage(e)),
          type = "error", duration = 8
        )
      })
    })
    
    # ── Render: utility status (shows after column is added) ──────────────────
    output$utility_status <- shiny::renderUI({
      shiny::req(rv$processed_data)
      col_name <- trimws(input$utility_col_name %||% "")
      if (!nzchar(col_name) || !col_name %in% names(rv$processed_data)) return(NULL)
      vals  <- rv$processed_data[[col_name]]
      n_ok  <- sum(!is.na(vals))
      n_tot <- length(vals)
      mn    <- round(min(vals, na.rm = TRUE), 3L)
      mx    <- round(max(vals, na.rm = TRUE), 3L)
      shiny::div(
        class = "alert alert-success small py-2 mb-0",
        shiny::icon("circle-check"), " ",
        shiny::strong(col_name), ": ",
        n_ok, "/", n_tot, " non-missing. Range: ", mn, " to ", mx, "."
      )
    })
    
    # ── Render: download section ──────────────────────────────────────────────
    # REDESIGNED: uses a flex-wrap row so buttons never overlap on narrow widths.
    output$download_section_ui <- shiny::renderUI({
      shiny::req(rv$processed_data)
      bslib::card(
        bslib::card_header(
          shiny::tags$span(shiny::icon("file-arrow-down"), " Download Dataset")
        ),
        bslib::card_body(
          style = "padding: 1.25rem;",
          shiny::p(
            class = "text-muted small mb-3",
            "Download the current working dataset, including any calculated ",
            "utility columns."
          ),
          # d-flex + flex-wrap + gap-2 ensures buttons wrap cleanly at any width
          shiny::div(
            class = "d-flex flex-wrap gap-2",
            shiny::downloadButton(
              ns("download_csv"),  "Download CSV",
              class = "btn-outline-secondary",
              icon  = shiny::icon("file-csv")
            ),
            shiny::downloadButton(
              ns("download_xlsx"), "Download XLSX",
              class = "btn-outline-secondary",
              icon  = shiny::icon("file-excel")
            ),
            shiny::downloadButton(
              ns("download_rds"),  "Download RDS",
              class = "btn-outline-secondary",
              icon  = shiny::icon("r-project")
            )
          )
        )
      )
    })
    
    # ── Download handlers ─────────────────────────────────────────────────────
    
    output$download_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("eq5dsuite_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        shiny::req(rv$processed_data)
        utils::write.csv(rv$processed_data, file, row.names = FALSE)
      }
    )
    
    output$download_xlsx <- shiny::downloadHandler(
      filename = function() {
        paste0("eq5dsuite_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      },
      content = function(file) {
        shiny::req(rv$processed_data)
        if (!requireNamespace("writexl", quietly = TRUE)) {
          stop("Package 'writexl' is required for XLSX export. ",
               "Install it with: install.packages('writexl')", call. = FALSE)
        }
        writexl::write_xlsx(rv$processed_data, file)
      }
    )
    
    output$download_rds <- shiny::downloadHandler(
      filename = function() {
        paste0("eq5dsuite_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        shiny::req(rv$processed_data)
        saveRDS(rv$processed_data, file)
      }
    )
  })
}

# Null-coalescing operator (safe for module scope)
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0L) a else b