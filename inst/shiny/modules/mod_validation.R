# mod_validation.R — Data validation and preprocessing module

# ── UI ────────────────────────────────────────────────────────────────────────

mod_validation_ui <- function(id) {
  ns <- shiny::NS(id)
  page_shell(
    sidebar_title = "Validation",
    sidebar = shiny::uiOutput(ns("sidebar")),
    shiny::uiOutput(ns("main"))
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_validation_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Reactive: run validation when mapping is confirmed ───────────────────
    validation <- shiny::reactive({
      shiny::req(rv$raw_data, rv$mapping)
      # The checks come back as plain text; the markup is the app's.
      eq5dsuite:::eq5d_validate(rv$raw_data, rv$mapping, quiet = TRUE)
    })

    # ── Render: main UI (shown only when mapping exists) ─────────────────────
    output$sidebar <- shiny::renderUI({
      if (is.null(rv$mapping)) {
        return(shiny::tagList(
          hint("Upload data and confirm its variables first."),
          shiny::actionButton(ns("goto_data"), "Go to Data",
                              class = "btn-primary w-100",
                              icon = shiny::icon("arrow-right"))
        ))
      }
      shiny::tagList(
        shiny::tags$p("Variables", class = "sidebar-label"),
        shiny::uiOutput(ns("mapping_summary")),
        shiny::hr(),
        shiny::actionButton(ns("proceed"), "Proceed",
                            class = "btn-primary w-100",
                            icon = shiny::icon("arrow-right"))
      )
    })

    shiny::observeEvent(input$goto_data, goto_page(session, "data"))

    output$main <- shiny::renderUI({
      if (is.null(rv$mapping)) {
        return(bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, hint(
          "Nothing to check yet. Select your EQ-5D variables on the Data page and ",
          "the checks will appear here."))))
      }
      shiny::tagList(
        bslib::card(
          fill = FALSE,
          bslib::card_header("Data checks"),
          bslib::card_body(fillable = FALSE, shiny::uiOutput(ns("validation_msgs")))
        ),
        bslib::card(
          full_screen = TRUE, fill = FALSE,
          bslib::card_header("Processed data"),
          bslib::card_body(fillable = FALSE,
            shiny::uiOutput(ns("processed_info")),
            DT::DTOutput(ns("processed_table"))
          )
        )
      )
    })

    # ── Render: validation messages ───────────────────────────────────────────
    # Each finding is one short line. Where eq5d_validate() has more to say --
    # the values found, the records affected, what a percentage is of -- a
    # "See details" toggle reveals it on demand. The details come from the
    # same checks the analyses use, and nothing here changes the data.
    details <- shiny::reactive(attr(validation(), "details"))

    output$validation_msgs <- shiny::renderUI({
      v <- validation()
      d <- details()
      shiny::tagList(lapply(seq_len(nrow(v)), function(i) {
        cls <- switch(v$type[i],
          ok      = "note note-ok",
          warning = "note note-warning",
          error   = "note note-error",
          "note note-info"
        )
        icon <- switch(v$type[i],
          ok = "circle-check", warning = "triangle-exclamation",
          error = "circle-exclamation", "circle-info"
        )
        shiny::div(class = cls, role = "alert",
                   shiny::icon(icon), " ", v$message[i],
                   finding_details(ns, i, d[[i]]))
      }))
    })

    # The tables behind each finding: long lists are searchable and paged.
    shiny::observe({
      d <- details()
      for (i in seq_along(d)) local({
        ii <- i
        dd <- d[[ii]]
        if (is.null(dd)) return()
        if (!is.null(dd$summary) && nrow(dd$summary) > SMALL_TABLE_ROWS)
          output[[paste0("det_", ii, "_summary")]] <-
            DT::renderDT(details_table(dd$summary))
        if (!is.null(dd$records))
          output[[paste0("det_", ii, "_records")]] <-
            DT::renderDT(details_table(dd$records))
      })
    })

    # ── Render: mapping summary ───────────────────────────────────────────────
    output$mapping_summary <- shiny::renderUI({
      m <- rv$mapping
      shiny::req(m)
      items <- list(
        list("EQ-5D version", m$eq5d_version),
        list("Dimensions",    paste(m$names_eq5d, collapse = ", ")),
        list("Timepoint",     m$name_fu       %||% "(not selected)"),
        list("Group",         m$name_groupvar %||% "(not selected)"),
        list("Patient ID",    m$name_id       %||% "(not selected)"),
        list("EQ VAS",        m$name_vas      %||% "(not selected)"),
        list("Age",           m$name_age      %||% "(not selected)"),
        list("Sex",           m$name_sex      %||% "(not selected)"),
        list("EQ-5D value",   m$name_utility  %||% "(not selected)")
      )
      shiny::tags$dl(
        class = "map-summary",
        lapply(items, function(x) {
          shiny::tagList(
            shiny::tags$dt(x[[1]]),
            shiny::tags$dd(x[[2]])
          )
        })
      )
    })

    # ── Reactive: apply mapping to produce processed_data preview ─────────────
    preview_data <- shiny::reactive({
      shiny::req(rv$raw_data, rv$mapping)
      eq5dsuite:::eq5d_apply_mapping(rv$raw_data, rv$mapping)
    })

    # Use rv$processed_data when available (preserves utility columns added on
    # the Data page), otherwise fall back to the freshly-mapped preview.
    current_data <- shiny::reactive({
      if (!is.null(rv$processed_data)) rv$processed_data else preview_data()
    })

    output$processed_info <- shiny::renderUI({
      df <- current_data()
      hint(shiny::strong(format(nrow(df), big.mark = ",")), " rows \u00d7 ",
           shiny::strong(ncol(df)), " columns",
           if (nrow(df) > 200L) " \u2014 first 200 shown" else "")
    })

    output$processed_table <- DT::renderDT({
      df <- current_data()
      shiny::req(df)
      DT::datatable(
        head(df, 200L),
        options  = list(pageLength = 10L, scrollX = TRUE, dom = "tip"),
        rownames = FALSE,
        class    = "table-sm table-striped"
      )
    })

    # ── Proceed: finalise processed_data and store in rv ──────────────────────
    shiny::observeEvent(input$proceed, {
      v <- validation()
      has_error <- any(v$type == "error")
      if (has_error) {
        shiny::showNotification(
          "Please resolve data errors before proceeding.",
          type = "error", duration = 5
        )
        return()
      }
      # Preserve any utility columns already added on the Data page.
      # Only set from raw mapping if processed_data not yet initialised.
      if (is.null(rv$processed_data)) {
        rv$processed_data <- preview_data()
        next_revision(rv)
      }
      shiny::showNotification(
        "Dataset validated.", type = "message", duration = 3
      )
      # Next in the workflow is calculating EQ-5D values. The profile and EQ
      # VAS analyses do not need them, so the page says so rather than this
      # being a required step.
      goto_page(session, "values")
    })
  })
}

# ── Helpers ───────────────────────────────────────────────────────────────────

# Up to this many rows a summary is a plain table; beyond it, a paged one.
SMALL_TABLE_ROWS <- 12L

# The "See details" part of one finding, collapsed until asked for.
finding_details <- function(ns, i, d) {
  if (is.null(d)) return(NULL)
  shiny::tags$details(
    class = "finding-details",
    shiny::tags$summary("See details"),
    if (length(d$emphasis))
      lapply(d$emphasis, function(x) shiny::p(shiny::strong(x))),
    lapply(d$notes, shiny::p),
    if (!is.null(d$denominator))
      shiny::p(class = "hint", d$denominator),
    if (!is.null(d$summary)) {
      if (nrow(d$summary) > SMALL_TABLE_ROWS)
        DT::DTOutput(ns(paste0("det_", i, "_summary")))
      else small_table(d$summary)
    },
    if (!is.null(d$records)) shiny::tagList(
      shiny::p(class = "details-label",
               sprintf("Records affected (%s)",
                       format(nrow(d$records), big.mark = ","))),
      DT::DTOutput(ns(paste0("det_", i, "_records"))))
  )
}

# A short table as plain HTML.
small_table <- function(df) {
  shiny::tags$table(
    class = "table table-sm details-table",
    shiny::tags$thead(shiny::tags$tr(lapply(names(df), shiny::tags$th))),
    shiny::tags$tbody(lapply(seq_len(nrow(df)), function(r)
      shiny::tags$tr(lapply(df[r, , drop = TRUE], function(x)
        shiny::tags$td(format(x, big.mark = ",")))))))
}

# A long table: searchable and paged.
details_table <- function(df) {
  DT::datatable(df, rownames = FALSE, class = "table-sm table-striped",
                options = list(pageLength = 10L, scrollX = TRUE,
                               dom = "ftip"))
}

