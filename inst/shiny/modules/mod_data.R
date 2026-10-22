# mod_data.R — upload a dataset and select its variables
#
# Sidebar: source, version, the five dimension pickers, optional columns in a
# disclosure, Confirm. Main area: a preview of the working dataset.
# Value calculation, and downloading the working dataset, have their own
# page (mod_values.R).

mod_data_ui <- function(id) {
  ns <- shiny::NS(id)

  page_shell(
    sidebar_title = "Data",
    sidebar = shiny::tagList(
      shiny::fileInput(ns("file"), "Upload a file",
                       accept = paste0(".", ONLINE$allowed_types_ui),
                       buttonLabel = "Browse", placeholder = "No file"),
      shiny::actionLink(ns("use_example"), "or load the example dataset"),
      online_note("upload"),

      disclosure(
        "What can I upload?",
        # The formats actually accepted, which online excludes .rds.
        shiny::p(accepted_types_text(), ", one row per observation."),
        shiny::p(shiny::tags$strong("Required:"), " the five EQ-5D dimensions, ",
                 "EQ-5D-3L or EQ-5D-Y-3L (levels 1–3), or EQ-5D-5L (levels 1–5)."),
        shiny::p(shiny::tags$strong("Optional:"), " timepoint, patient ID, ",
                 "group, EQ VAS, age and sex, an existing value column. Each ",
                 "unlocks further analyses."),
        shiny::p("Cross-sectional and longitudinal data are both supported.")
      ),

      shiny::hr(),
      shiny::uiOutput(ns("mapping_ui")),
      shiny::uiOutput(ns("confirm_ui"))
    ),

    shiny::uiOutput(ns("main"))
  )
}

mod_data_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    active_data <- shiny::reactiveVal(NULL)
    # How the data on screen were loaded. It becomes the session's load step
    # only when the mapping is confirmed: choosing a file must not change the
    # record of the data the current results came from.
    pending_load <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$file, {
      ext <- tolower(tools::file_ext(input$file$name))

      # The type is checked here, on the server, not only by the browser's
      # file picker, which a caller can ignore.
      refuse <- eq5dsuite:::eq5d_check_upload(NULL, ext, ONLINE)
      if (!is.null(refuse)) {
        shiny::showNotification(refuse, type = "error", duration = 15)
        return()
      }
      # The reading options are fixed here; the generated script writes them
      # out so they can be changed there.
      read_args <- if (ext %in% c("xlsx", "xls")) list(sheet = 1)
                   else if (identical(ext, "csv")) list(sep = ",", dec = ".",
                                                        fileEncoding = "")
                   else list()
      tryCatch({
        d <- do.call(eq5dsuite:::eq5d_read_data,
                     c(list(input$file$datapath, type = ext), read_args))

        # And the size of what came out, which the browser cannot know.
        refuse <- eq5dsuite:::eq5d_check_upload(d, ext, ONLINE)
        if (!is.null(refuse)) {
          shiny::showNotification(refuse, type = "error", duration = 15)
          return()
        }

        active_data(d)
        pending_load(list(source = "file", file = input$file$name,
                          ext = ext, read_args = read_args))
      }, error = function(e) {
        # The message can quote the file's contents, so online it is shown to
        # the person who uploaded it and goes no further.
        shiny::showNotification(
          if (ONLINE$enabled)
            paste0("That file could not be read. Check that it is a ",
                   "comma-separated or Excel file with a row of column names.")
          else paste("Error reading file:", conditionMessage(e)),
          type = "error", duration = 8)
      })
    })

    load_example <- function() {
      active_data(eq5dsuite::example_data)
      pending_load(list(source = "example"))
      shiny::showNotification(
        "Example dataset loaded (10,000 rows). Select its variables and confirm.",
        type = "message", duration = 3)
    }

    shiny::observeEvent(input$use_example, load_example())

    # The Home page's "Load the example dataset" button bumps this counter
    # rather than loading the data itself. The counter starts at 0, so the
    # observer's first run is a no-op; ignoreInit is deliberately not used,
    # because it also swallows the first run under shiny::testServer().
    shiny::observeEvent(rv$load_example, {
      if ((rv$load_example %||% 0L) > 0L) load_example()
    })

    uploaded <- shiny::reactive(active_data())

    suggestions <- shiny::reactive({
      df <- uploaded()
      if (is.null(df)) return(list())
      suggest_mapping(names(df))
    })

    # ── Sidebar: the column pickers ──────────────────────────────────────────
    output$mapping_ui <- shiny::renderUI({
      df <- uploaded()
      if (is.null(df)) {
        return(hint("Upload a file, or load the example dataset, to select ",
                    "its variables."))
      }
      cols      <- names(df)
      cols_none <- c("(none)" = "")
      sug       <- suggestions()

      sel <- function(label, inputId, choices, selected) {
        shiny::selectInput(ns(inputId), label, choices = choices,
                           selected = selected %||% "")
      }

      shiny::tagList(
        shiny::radioButtons(ns("version"), "EQ-5D version",
                            choices = c("EQ-5D-3L" = "3L", "EQ-5D-5L" = "5L",
                                        "EQ-5D-Y-3L" = "Y3L"),
                            selected = input$version %||% "3L", inline = TRUE),

        shiny::tags$p("Dimensions", class = "sidebar-label"),
        sel("Mobility",           "col_mo", cols, sug$mo),
        sel("Self-care",          "col_sc", cols, sug$sc),
        sel("Usual activities",   "col_ua", cols, sug$ua),
        sel("Pain/discomfort",    "col_pd", cols, sug$pd),
        sel("Anxiety/depression", "col_ad", cols, sug$ad),

        disclosure(
          "Optional columns",
          sel("Timepoint",  "col_fu",       c(cols_none, cols), sug$name_fu),
          shiny::uiOutput(ns("fu_order_ui")),
          sel("Group",      "col_groupvar", c(cols_none, cols), sug$name_groupvar),
          sel("Patient ID", "col_id",       c(cols_none, cols), sug$name_id),
          sel("EQ VAS",     "col_vas",      c(cols_none, cols), sug$name_vas),
          sel("Age",        "col_age",      c(cols_none, cols), sug$name_age),
          sel("Sex",        "col_sex",      c(cols_none, cols), sug$name_sex),
          sel("Existing EQ-5D value column", "col_utility",
              c(cols_none, cols), sug$name_utility)
        )
      )
    })

    output$confirm_ui <- shiny::renderUI({
      if (is.null(uploaded())) return(NULL)
      shiny::tagList(
        shiny::hr(),
        shiny::actionButton(ns("confirm"), "Confirm variables",
                            class = "btn-primary w-100",
                            icon = shiny::icon("check"))
      )
    })

    # The order of the timepoints decides which way round "change between
    # timepoints" is read, so it is set here rather than left to the order the
    # rows happen to be in. Defaults to the order they appear in the file.
    output$fu_order_ui <- shiny::renderUI({
      df <- uploaded()
      col <- input$col_fu
      if (is.null(df) || is.null(col) || !nzchar(col) || !col %in% names(df))
        return(NULL)
      lv <- unique(as.character(df[[col]]))
      lv <- lv[!is.na(lv)]
      if (length(lv) < 2L) return(NULL)
      chosen <- shiny::isolate(input$fu_order)
      sel <- if (!is.null(chosen) && setequal(chosen, lv)) chosen else lv
      shiny::tagList(
        shiny::selectizeInput(ns("fu_order"), "Timepoint order",
                              choices = lv, selected = sel, multiple = TRUE,
                              options = list(plugins = list("drag_drop"))),
        hint("First is the baseline. Change between timepoints is reported ",
             "as later minus earlier.")
      )
    })

    # The order actually used: the user's, if it still matches the column.
    fu_levels <- shiny::reactive({
      df <- uploaded()
      col <- input$col_fu
      if (is.null(df) || is.null(col) || !nzchar(col) || !col %in% names(df))
        return(NULL)
      lv <- unique(as.character(df[[col]]))
      lv <- lv[!is.na(lv)]
      chosen <- input$fu_order
      if (!is.null(chosen) && setequal(chosen, lv)) chosen else lv
    })

    mapping_complete <- shiny::reactive({
      if (is.null(uploaded())) return(FALSE)
      dims <- c(input$col_mo, input$col_sc, input$col_ua, input$col_pd, input$col_ad)
      all(nzchar(dims)) && length(unique(dims)) == 5L
    })

    shiny::observeEvent(input$confirm, {
      if (!isTRUE(mapping_complete())) {
        shiny::showNotification(
          "Select a different column for each of the five EQ-5D dimensions before confirming.",
          type = "warning", duration = 5
        )
        return()
      }

      opt <- function(x) if (!is.null(x) && nzchar(x)) x else NULL

      rv$raw_data <- uploaded()
      rv$mapping  <- list(
        eq5d_version  = input$version,
        names_eq5d    = c(input$col_mo, input$col_sc,
                          input$col_ua, input$col_pd, input$col_ad),
        name_fu       = opt(input$col_fu),
        levels_fu     = fu_levels(),
        name_groupvar = opt(input$col_groupvar),
        name_id       = opt(input$col_id),
        name_vas      = opt(input$col_vas),
        name_age      = opt(input$col_age),
        name_sex      = opt(input$col_sex),
        name_utility  = opt(input$col_utility),
        country       = ""   # set on the Calculate EQ-5D values page
      )
      # A new revision: results, value columns and value steps from the
      # previous data or mapping are cleared (see start_revision()). An
      # existing value column becomes "utility" in the working dataset, and is
      # the first entry in the list the Analysis page offers.
      cleared <- start_revision(rv, pending_load(), rv$mapping)
      rv$value_cols <- if (is.null(opt(input$col_utility))) character(0L)
                       else "utility"

      gone <- c(if (cleared$results > 0L)
                  sprintf("%d saved result%s", cleared$results,
                          if (cleared$results == 1L) "" else "s"),
                if (cleared$values > 0L)
                  sprintf("%d calculated value column%s", cleared$values,
                          if (cleared$values == 1L) "" else "s"))
      shiny::showNotification(
        paste0("Variables confirmed. Next: Validation.",
               if (length(gone))
                 paste0(" ", paste(gone, collapse = " and "),
                        " from the previous data were cleared.")),
        type = if (length(gone)) "warning" else "message",
        duration = if (length(gone)) 8 else 4
      )
    })

    # The working dataset: processed_data once it exists (it carries any
    # calculated value columns), otherwise the freshly mapped upload.
    local_data <- shiny::reactive({
      shiny::req(rv$raw_data, rv$mapping)
      if (!is.null(rv$processed_data)) rv$processed_data
      else eq5dsuite:::eq5d_apply_mapping(rv$raw_data, rv$mapping)
    })

    # ── Main area ────────────────────────────────────────────────────────────
    output$main <- shiny::renderUI({
      if (is.null(uploaded())) {
        return(bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, 
          shiny::p(shiny::icon("circle-info"), " ",
                   "No data yet. Upload a file, or load the example dataset, ",
                   "using the controls on the left.", class = "text-muted")
        )))
      }
      bslib::card(
        full_screen = TRUE, fill = FALSE,
        bslib::card_header("Preview"),
        bslib::card_body(fillable = FALSE,
          shiny::uiOutput(ns("preview_info")),
          DT::DTOutput(ns("preview_table"))
        )
      )
    })

    output$preview_info <- shiny::renderUI({
      df <- tryCatch(local_data(), error = function(e) NULL) %||% uploaded()
      shiny::req(df)
      hint(shiny::strong(format(nrow(df), big.mark = ",")), " rows × ",
           shiny::strong(ncol(df)), " columns",
           if (nrow(df) > 200L) " — first 200 shown" else "")
    })

    output$preview_table <- DT::renderDT({
      df <- tryCatch(local_data(), error = function(e) NULL) %||% uploaded()
      shiny::req(df)
      preview_table(head(df, 200L), value_columns(rv))
    })

  })
}
