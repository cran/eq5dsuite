# mod_export.R — Export module

# ── UI ────────────────────────────────────────────────────────────────────────

mod_export_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(8, 4),
      
      # Main: export list
      bslib::card(
        bslib::card_header("Export Results"),
        bslib::card_body(
          shiny::uiOutput(ns("export_list"))
        )
      ),
      
      # Sidebar: bulk export
      bslib::card(
        bslib::card_header("Bulk Export"),
        bslib::card_body(
          shiny::p("Download all tables (CSV) and figures (PNG) as a ZIP file."),
          shiny::downloadButton(ns("download_all_zip"),
                                "All results (.zip)",
                                class = "btn-outline-primary w-100 mb-2"),
          shiny::hr()
          # shiny::p("Export all results to a Word document."),
          # shiny::downloadButton(ns("download_word"),
          #                       "Word report (.docx)",
          #                       class = "btn-outline-primary w-100 mb-2")
        )
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_export_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Export list ───────────────────────────────────────────────────────────
    output$export_list <- shiny::renderUI({
      results <- rv$results
      if (length(results) == 0L) {
        return(shiny::p("No results to export. Run analyses first.",
                        class = "text-muted small"))
      }
      
      rows <- lapply(seq_along(results), function(i) {
        r    <- results[[i]]
        dl_buttons <- list()
        
        if (r$result_type %in% c("table", "both") && !is.null(r$data)) {
          dl_buttons <- c(dl_buttons, list(
            shiny::downloadButton(
              ns(paste0("dl_csv_", i)),
              label = "CSV",
              class = "btn-sm btn-outline-secondary me-1"
            )
          ))
        }
        
        if (r$result_type %in% c("plot", "both") && !is.null(r$plot)) {
          dl_buttons <- c(dl_buttons, list(
            shiny::downloadButton(
              ns(paste0("dl_png_", i)),
              label = "PNG",
              class = "btn-sm btn-outline-secondary me-1"
            ),
            shiny::downloadButton(
              ns(paste0("dl_pdf_", i)),
              label = "PDF",
              class = "btn-sm btn-outline-secondary"
            )
          ))
        }
        
        shiny::div(
          class = "d-flex align-items-center justify-content-between border-bottom py-2",
          shiny::div(
            shiny::strong(r$label),
            shiny::tags$br(),
            shiny::tags$small(
              class = "text-muted",
              format(r$timestamp, "%d %b %Y %H:%M"),
              " \u2014 ", r$result_type
            )
          ),
          shiny::div(dl_buttons)
        )
      })
      
      shiny::tagList(rows)
    })
    
    # ── Dynamic download handlers ─────────────────────────────────────────────
    # We observe rv$results and register handlers each time it changes.
    shiny::observe({
      results <- rv$results
      lapply(seq_along(results), function(i) {
        r <- results[[i]]
        
        # CSV download
        if (r$result_type %in% c("table", "both") && !is.null(r$data)) {
          local({
            local_r <- r
            local_i <- i
            output[[paste0("dl_csv_", local_i)]] <- shiny::downloadHandler(
              filename = function() {
                paste0(safe_filename(local_r$label), "_",
                       format(local_r$timestamp, "%Y%m%d"), ".csv")
              },
              content = function(file) {
                utils::write.csv(local_r$data, file, row.names = FALSE)
              }
            )
          })
        }
        
        # PNG download
        if (r$result_type %in% c("plot", "both") && !is.null(r$plot)) {
          local({
            local_r <- r
            local_i <- i
            output[[paste0("dl_png_", local_i)]] <- shiny::downloadHandler(
              filename = function() {
                paste0(safe_filename(local_r$label), "_",
                       format(local_r$timestamp, "%Y%m%d"), ".png")
              },
              content = function(file) {
                ggplot2::ggsave(file, plot = local_r$plot,
                                width = 8, height = 5, dpi = 150)
              }
            )
          })
          
          # PDF download
          local({
            local_r <- r
            local_i <- i
            output[[paste0("dl_pdf_", local_i)]] <- shiny::downloadHandler(
              filename = function() {
                paste0(safe_filename(local_r$label), "_",
                       format(local_r$timestamp, "%Y%m%d"), ".pdf")
              },
              content = function(file) {
                ggplot2::ggsave(file, plot = local_r$plot,
                                width = 8, height = 5, device = "pdf")
              }
            )
          })
        }
      })
    })
    
    # ── Bulk ZIP download (tables + plots) ────────────────────────────────────
    output$download_all_zip <- shiny::downloadHandler(
      filename = function() {
        paste0("eq5dsuite_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        results  <- rv$results
        tmp_dir  <- file.path(tempdir(), paste0("eq5dzip_", format(Sys.time(), "%H%M%S%OS3")))
        dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
        
        out_files <- character(0L)
        
        # Tables → CSV
        for (r in results) {
          if (r$result_type %in% c("table", "both") && !is.null(r$data)) {
            fname <- file.path(tmp_dir, paste0(safe_filename(r$label), ".csv"))
            utils::write.csv(r$data, fname, row.names = FALSE)
            out_files <- c(out_files, fname)
          }
        }
        
        # Plots → PNG
        for (r in results) {
          if (r$result_type %in% c("plot", "both") && !is.null(r$plot)) {
            fname <- file.path(tmp_dir, paste0(safe_filename(r$label), ".png"))
            tryCatch(
              ggplot2::ggsave(fname, plot = r$plot, width = 8, height = 5, dpi = 150),
              error = function(e) NULL
            )
            if (file.exists(fname) && file.size(fname) > 0L) {
              out_files <- c(out_files, fname)
            }
          }
        }
        
        if (length(out_files) == 0L) {
          placeholder <- file.path(tmp_dir, "no_results.txt")
          writeLines("No results to export.", placeholder)
          out_files <- placeholder
        }
        
        utils::zip(file, files = out_files, flags = "-j")
      }
    )
    
  })
}

# ── Helper ────────────────────────────────────────────────────────────────────

safe_filename <- function(label) {
  gsub("[^A-Za-z0-9_-]", "_", label)
}
