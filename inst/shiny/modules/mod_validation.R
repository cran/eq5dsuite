# mod_validation.R — Data validation and preprocessing module

# ── UI ────────────────────────────────────────────────────────────────────────

mod_validation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("main_ui"))
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_validation_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Reactive: run validation when mapping is confirmed ───────────────────
    validation <- shiny::reactive({
      shiny::req(rv$raw_data, rv$mapping)
      run_validation(rv$raw_data, rv$mapping)
    })

    # ── Render: main UI (shown only when mapping exists) ─────────────────────
    output$main_ui <- shiny::renderUI({
      if (is.null(rv$mapping)) {
        return(bslib::card(
          bslib::card_body(shiny::p(
            shiny::icon("circle-info"),
            " Please upload data and confirm mapping on the Data tab first.",
            class = "text-muted"
          ))
        ))
      }

      bslib::layout_columns(
        col_widths = c(5, 7),

        # Left: validation summary
        bslib::card(
          bslib::card_header(
            shiny::tags$span(shiny::icon("clipboard-check"), " Validation Summary")
          ),
          bslib::card_body(
            shiny::uiOutput(ns("validation_msgs")),
            shiny::hr(),
            shiny::h6("Mapping confirmed"),
            shiny::uiOutput(ns("mapping_summary")),
            shiny::hr(),
            shiny::actionButton(
              ns("proceed"), "Proceed to Analysis",
              class = "btn-success w-100",
              icon  = shiny::icon("arrow-right")
            )
          )
        ),

        # Right: processed data preview
        bslib::card(
          bslib::card_header(
            shiny::tags$span(shiny::icon("table"), " Processed Data Preview")
          ),
          bslib::card_body(
            shiny::uiOutput(ns("processed_info")),
            DT::DTOutput(ns("processed_table"))
          )
        )
      )
    })

    # ── Render: validation messages ───────────────────────────────────────────
    output$validation_msgs <- shiny::renderUI({
      v <- validation()
      shiny::tagList(
        lapply(v$messages, function(msg) {
          cls <- switch(msg$type,
            ok      = "alert alert-success",
            warning = "alert alert-warning",
            error   = "alert alert-danger",
            "alert alert-info"
          )
          shiny::div(class = cls, role = "alert", shiny::HTML(msg$text))
        })
      )
    })

    # ── Render: mapping summary ───────────────────────────────────────────────
    output$mapping_summary <- shiny::renderUI({
      m <- rv$mapping
      shiny::req(m)
      items <- list(
        list("EQ-5D version", m$eq5d_version),
        list("Dimensions",    paste(m$names_eq5d, collapse = ", ")),
        list("Timepoint",     m$name_fu       %||% "(not mapped)"),
        list("Group",         m$name_groupvar %||% "(not mapped)"),
        list("Patient ID",    m$name_id       %||% "(not mapped)"),
        list("EQ-VAS",        m$name_vas      %||% "(not mapped)"),
        list("Utility",       m$name_utility  %||% "(not mapped)"),
        list("Country",       if (nzchar(m$country)) m$country else "(not selected)")
      )
      shiny::tags$dl(
        class = "row small mb-0",
        lapply(items, function(x) {
          shiny::tagList(
            shiny::tags$dt(class = "col-sm-5 text-muted", x[[1]]),
            shiny::tags$dd(class = "col-sm-7", x[[2]])
          )
        })
      )
    })

    # ── Reactive: apply mapping to produce processed_data preview ─────────────
    preview_data <- shiny::reactive({
      shiny::req(rv$raw_data, rv$mapping)
      apply_mapping(rv$raw_data, rv$mapping)
    })

    # Use rv$processed_data when available (preserves utility columns added on
    # the Data page), otherwise fall back to the freshly-mapped preview.
    current_data <- shiny::reactive({
      if (!is.null(rv$processed_data)) rv$processed_data else preview_data()
    })

    output$processed_info <- shiny::renderUI({
      df <- current_data()
      shiny::p(
        shiny::strong(format(nrow(df), big.mark = ",")), " rows \u00d7 ",
        shiny::strong(ncol(df)), " columns",
        class = "text-muted small mb-2"
      )
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
      has_error <- any(vapply(v$messages, function(m) m$type == "error", logical(1L)))
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
      }
      shiny::showNotification(
        "Dataset validated. You can now run analyses.",
        type = "message", duration = 4
      )
    })
  })
}

# Null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0L) a else b

# ── Validation logic (pure function, not reactive) ────────────────────────────

run_validation <- function(df, mapping) {
  msgs <- list()

  add_msg <- function(type, text) {
    msgs[[length(msgs) + 1L]] <<- list(type = type, text = text)
  }

  n_rows <- nrow(df)
  add_msg("ok", paste0("<strong>", format(n_rows, big.mark = ","),
                       "</strong> rows loaded."))

  # Check EQ-5D column existence
  eq5d_cols   <- mapping$names_eq5d
  missing_cols <- eq5d_cols[!eq5d_cols %in% names(df)]
  if (length(missing_cols) > 0L) {
    add_msg("error",
            paste("EQ-5D columns not found in data:",
                  paste(missing_cols, collapse = ", ")))
  } else {
    # Check value range
    max_level <- if (mapping$eq5d_version == "3L") 3L else 5L
    out_of_range <- vapply(eq5d_cols, function(col) {
      vals <- suppressWarnings(as.integer(df[[col]]))
      any(!is.na(vals) & (vals < 1L | vals > max_level))
    }, logical(1L))

    if (any(out_of_range)) {
      add_msg("warning",
              paste0(
                "Some values in [",
                paste(eq5d_cols[out_of_range], collapse = ", "),
                "] are outside the expected range (1\u2013", max_level,
                ") and will be set to NA."
              ))
    } else {
      add_msg("ok", paste0(
        "All EQ-5D values within expected range (1\u2013", max_level, ")."
      ))
    }

    # Missing EQ-5D values
    n_complete <- sum(complete.cases(
      lapply(eq5d_cols, function(col) suppressWarnings(as.integer(df[[col]])))
    ))
    n_miss <- n_rows - n_complete
    if (n_miss > 0L) {
      pct <- round(100 * n_miss / n_rows, 1)
      add_msg("warning",
              paste0("<strong>", n_miss, "</strong> rows (",
                     pct, "%) have missing EQ-5D values."))
    } else {
      add_msg("ok", "No missing EQ-5D values.")
    }
  }

  # Duplicate ID check
  if (!is.null(mapping$name_id) && nzchar(mapping$name_id)) {
    if (mapping$name_id %in% names(df)) {
      ids   <- df[[mapping$name_id]]
      n_dup <- sum(duplicated(ids))
      if (n_dup == 0L) {
        add_msg("ok", "No duplicate patient IDs.")
      } else {
        has_fu  <- !is.null(mapping$name_fu) && nzchar(mapping$name_fu) &&
                   mapping$name_fu %in% names(df)
        if (has_fu) {
          fu_vals  <- df[[mapping$name_fu]]
          pairs    <- paste(ids, fu_vals, sep = "\u00b7")
          n_dup_pairs <- sum(duplicated(pairs))
          n_tp     <- length(unique(fu_vals))
          if (n_dup_pairs == 0L) {
            add_msg("ok",
                    paste0("Repeated patient IDs detected across ",
                           "<strong>", n_tp, "</strong> timepoints \u2014 ",
                           "this is expected for longitudinal data. ",
                           "All ID\u2013timepoint combinations are unique."))
          } else {
            add_msg("warning",
                    paste0("<strong>", n_dup_pairs, "</strong> ID\u2013timepoint ",
                           "combinations are duplicated. Check for duplicate records."))
          }
        } else {
          add_msg("warning",
                  paste0("<strong>", n_dup, "</strong> repeated patient IDs. ",
                         "For cross-sectional data each row should have a unique ID. ",
                         "If this is longitudinal data, map a Timepoint variable."))
        }
      }
    }
  }

  # VAS range check
  if (!is.null(mapping$name_vas) && nzchar(mapping$name_vas) &&
      mapping$name_vas %in% names(df)) {
    vas_vals <- suppressWarnings(as.numeric(df[[mapping$name_vas]]))
    out_vas  <- any(!is.na(vas_vals) & (vas_vals < 0 | vas_vals > 100))
    if (out_vas) {
      add_msg("warning", "Some VAS values are outside the expected range (0\u2013100).")
    } else {
      add_msg("ok", "VAS values within expected range (0\u2013100).")
    }
  }

  list(messages = msgs)
}
