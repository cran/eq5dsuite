# mod_export.R — Results and export
#
# One page for the saved results and everything that leaves the app. The
# list on the right is the order every export follows -- the Word report,
# the ZIP archive and the R script -- and each result in it can be moved,
# removed, downloaded on its own, or viewed inline.
#
# Every control is keyed by the result's id (save_result()), never by its
# position, so moving or removing one result cannot make a button act on
# another. A file's name does use the position -- "01_<label>" -- worked out
# when the file is downloaded, so it always matches the archive.

# ── UI ────────────────────────────────────────────────────────────────────────

mod_export_ui <- function(id) {
  ns <- shiny::NS(id)
  page_shell(
    sidebar_title = "Results and export",
    sidebar = shiny::tagList(
      hint("Every saved result, in the order of the list."),
      shiny::downloadButton(ns("download_docx"), "Word report (.docx)",
                            class = "btn-primary w-100 mb-2"),
      shiny::downloadButton(ns("download_all_zip"), "Tables and figures (.zip)",
                            class = "btn-outline-secondary w-100 mb-2"),
      shiny::downloadButton(ns("download_script"), "R script (.R)",
                            class = "btn-outline-secondary w-100"),
      disclosure(
        "About the R script",
        shiny::p("An R script that repeats this session: loading the data, ",
                 "the checks, the EQ-5D values, and every saved result in the ",
                 "order of the list."),
        shiny::p("It calls eq5dsuite's analysis functions and writes ",
                 "everything else out in full, so it runs on its own ",
                 "wherever the package is installed, and you can read and ",
                 "change every step.")),
      disclosure(
        "About the Word report",
        shiny::p("One section per result, in order, each with its title and ",
                 "its table or figure."),
        shiny::p("The document uses the template bundled with the package, ",
                 "so it carries the same styles, header and footer as other ",
                 "Maths in Health reports."))
    ),
    shiny::tagList(
      bslib::card(
        fill = FALSE,
        bslib::card_header("Results"),
        bslib::card_body(fillable = FALSE, shiny::uiOutput(ns("export_list")))
      ),
      bslib::card(
        fill = FALSE,
        bslib::card_header("R script"),
        bslib::card_body(fillable = FALSE,
          hint("What the download contains."),
          shiny::verbatimTextOutput(ns("script_preview")))
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_export_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # The results shown expanded, by id.
    expanded <- shiny::reactiveVal(character(0L))

    # ── The list ────────────────────────────────────────────────────────────
    output$export_list <- shiny::renderUI({
      results <- rv$results
      if (length(results) == 0L) {
        return(hint("No results yet. Every analysis you run is saved here, ",
                    "in the order you run them, ready to export."))
      }
      open <- expanded()
      n <- length(results)
      rows <- lapply(seq_len(n), function(i) {
        result_row(ns, results[[i]], i, n, results[[i]]$id %in% open)
      })
      shiny::tagList(rows,
                     hint("Results are exported in this order: the Word ",
                          "report, the archive and the R script all follow ",
                          "it."))
    })

    # ── Per-result controls, downloads and previews ─────────────────────────
    # Wired once for each id. A result's buttons, its files and its preview
    # find it by id when used, so they follow it wherever it moves.
    wired <- shiny::reactiveVal(character(0L))
    shiny::observe({
      ids <- vapply(rv$results, function(r) r$id, character(1L))
      new_ids <- setdiff(ids, shiny::isolate(wired()))
      for (the_id in new_ids) local({
        rid <- the_id
        current <- function() find_result(shiny::isolate(rv$results), rid)
        position <- function() result_index(shiny::isolate(rv$results), rid)

        shiny::observeEvent(input[[paste0("view_", rid)]], {
          open <- shiny::isolate(expanded())
          expanded(if (rid %in% open) setdiff(open, rid) else c(open, rid))
        }, ignoreInit = TRUE)
        shiny::observeEvent(input[[paste0("up_", rid)]],
                            move_result(rv, rid, -1L), ignoreInit = TRUE)
        shiny::observeEvent(input[[paste0("down_", rid)]],
                            move_result(rv, rid, 1L), ignoreInit = TRUE)
        shiny::observeEvent(input[[paste0("rm_", rid)]], {
          expanded(setdiff(shiny::isolate(expanded()), rid))
          remove_result(rv, rid)
        }, ignoreInit = TRUE)

        file_name <- function(ext) function()
          paste0(result_file_stem(position(), current()), ".", ext)
        output[[paste0("dl_csv_", rid)]] <- shiny::downloadHandler(
          filename = file_name("csv"),
          content = function(file)
            utils::write.csv(current()$data, file, row.names = FALSE))
        output[[paste0("dl_png_", rid)]] <- shiny::downloadHandler(
          filename = file_name("png"),
          content = function(file)
            ggplot2::ggsave(file, plot = current()$plot,
                            width = 8, height = 5, dpi = 150))
        output[[paste0("dl_pdf_", rid)]] <- shiny::downloadHandler(
          filename = file_name("pdf"),
          content = function(file)
            ggplot2::ggsave(file, plot = current()$plot,
                            width = 8, height = 5, device = "pdf"))

        output[[paste0("tbl_", rid)]] <- DT::renderDT({
          r <- find_result(rv$results, rid)
          shiny::req(r, !is.null(r$data))
          DT::datatable(r$data,
                        options  = list(pageLength = 15L, scrollX = TRUE,
                                        dom = "tip"),
                        rownames = FALSE, class = "table-sm table-striped")
        })
        output[[paste0("plot_", rid)]] <- shiny::renderPlot({
          r <- find_result(rv$results, rid)
          shiny::req(r, !is.null(r$plot))
          r$plot
        })
        output[[paste0("call_", rid)]] <- shiny::renderText({
          r <- find_result(rv$results, rid)
          shiny::req(r)
          r$fn_call
        })
      })
      if (length(new_ids)) wired(c(shiny::isolate(wired()), new_ids))
    })

    # ── R script ──────────────────────────────────────────────────────────────
    script_lines <- shiny::reactive({
      eq5dsuite:::script_from_session(rv$steps %||% list(), rv$results)
    })

    output$script_preview <- shiny::renderText(
      paste(script_lines(), collapse = "\n"))

    output$download_script <- shiny::downloadHandler(
      filename = function() {
        paste0("eq5d_analysis_", format(Sys.Date(), "%Y%m%d"), ".R")
      },
      content = function(file) writeLines(script_lines(), file)
    )

    # ── Word report ───────────────────────────────────────────────────────────
    output$download_docx <- shiny::downloadHandler(
      filename = function() {
        paste0("eq5dsuite_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx")
      },
      content = function(file) {
        results <- rv$results
        if (length(results) == 0L)
          stop("There are no saved results to put in the report.", call. = FALSE)
        # The same formatting the Analysis page shows on screen, so the
        # document and the app do not disagree about what a number means.
        write_results_docx(results, file,
                           format_table = eq5dsuite:::eq5d_format_table)
      }
    )

    # ── Bulk ZIP download (tables + plots) ────────────────────────────────────
    output$download_all_zip <- shiny::downloadHandler(
      filename = function() {
        paste0("eq5dsuite_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        results  <- rv$results
        # A name no other session can collide with, removed once the archive
        # is built rather than left in tempdir() for the life of the process.
        # Inside this session's own folder, which goes when the session does.
        tmp_dir  <- session_file(session, "eq5dzip_", "")
        dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

        built <- build_results_archive(results, tmp_dir)
        if (built$failed > 0L)
          shiny::showNotification(
            sprintf(paste0("%d plot%s could not be drawn and %s not in the ",
                           "archive. manifest.csv says which, and why."),
                    built$failed, if (built$failed == 1L) "" else "s",
                    if (built$failed == 1L) "is" else "are"),
            type = "warning", duration = 10)
        out_files <- built$files

        utils::zip(file, files = out_files, flags = "-j")
      }
    )
    
  })
}

# ── Helpers ───────────────────────────────────────────────────────────────────

# One entry of the list: its number, title and when it was run; below the
# title its downloads and "View"; on the right, move up, move down, remove;
# and, when expanded, the result itself beneath.
result_row <- function(ns, r, i, n, open) {
  rid <- r$id
  small <- function(id, icon, title, disabled = FALSE) {
    b <- shiny::actionButton(ns(paste0(id, "_", rid)), label = NULL,
                             icon = shiny::icon(icon), title = title,
                             `aria-label` = title,
                             class = "btn btn-sm btn-outline-secondary")
    if (disabled) b$attribs$disabled <- "disabled"
    b
  }
  dl <- function(kind, label)
    shiny::downloadButton(ns(paste0("dl_", kind, "_", rid)), label = label,
                          class = "btn-sm btn-outline-secondary me-1")
  is_table <- r$result_type %in% c("table", "both") && !is.null(r$data)
  is_plot  <- r$result_type %in% c("plot", "both") && !is.null(r$plot)

  shiny::div(
    class = paste("result-row", if (open) "result-row-open"),
    shiny::div(
      class = "result-row-head",
      shiny::div(
        class = "result-row-main",
        shiny::div(
          class = "result-row-title",
          shiny::span(class = "result-n", i), " ",
          shiny::icon(result_icon(r$result_type)), " ",
          shiny::strong(r$label)),
        shiny::tags$small(class = "hint",
                          format(r$timestamp, "%d %b %Y %H:%M")),
        shiny::div(
          class = "result-row-actions",
          if (is_table) dl("csv", "CSV"),
          if (is_plot) shiny::tagList(dl("png", "PNG"), dl("pdf", "PDF")),
          shiny::actionLink(
            ns(paste0("view_", rid)),
            if (open) "Hide" else "View",
            icon = shiny::icon(if (open) "chevron-up" else "chevron-down"),
            class = "result-view",
            `aria-expanded` = if (open) "true" else "false"))),
      shiny::div(
        class = "result-row-controls",
        small("up", "arrow-up", "Move up", i == 1L),
        small("down", "arrow-down", "Move down", i == n),
        small("rm", "xmark", "Remove"))),
    if (open) shiny::div(
      class = "result-preview",
      if (is_table) DT::DTOutput(ns(paste0("tbl_", rid))),
      if (is_plot) plot_frame(ns(paste0("plot_", rid))),
      call_display(ns(paste0("call_", rid))))
  )
}

safe_filename <- function(label) {
  gsub("[^A-Za-z0-9_-]", "_", label)
}

#' The name of a result's files, without the extension
#'
#' Its position in the user's order -- the order of the results list, the
#' Word report and the generated script -- then its label:
#' "01_Utility_summary_stats__3_1_". The app gives every run of an analysis
#' the same label, so the label alone named two runs alike. The individual
#' downloads and the bulk archive use the same names.
result_file_stem <- function(i, r) {
  sprintf("%02d_%s", i, safe_filename(r$label %||% "result"))
}

#' Write the files of the bulk archive
#'
#' Each file is named from the result's position in the user's order -- the
#' order of the results list, the Word report and the generated script -- and
#' then its label: "01_Utility_summary_stats__3_1_.csv". The app gives every
#' run of an analysis the same label, so the label alone let a second run
#' overwrite the first. manifest.csv lists every file with the label, the call
#' that produced it and the name the generated script gives that result. A
#' plot that cannot be drawn is listed there as failed, rather than left out
#' without a word.
#'
#' @return A list: `files`, the paths to put in the archive, and `failed`, the
#'   number of plots that could not be drawn.
build_results_archive <- function(results, dir) {
  if (!length(results)) {
    placeholder <- file.path(dir, "no_results.txt")
    writeLines("No results to export.", placeholder)
    return(list(files = placeholder, failed = 0L))
  }

  objs <- eq5dsuite:::.result_object_names(results)
  rows <- list()
  files <- character(0L)
  add <- function(i, r, type, file, status) {
    rows[[length(rows) + 1L]] <<- data.frame(
      position = i, file = file, label = r$label %||% "", type = type,
      status = status, script_object = objs[i],
      call = paste(r$fn_call %||% "", collapse = "\n"),
      saved = if (is.null(r$timestamp)) NA_character_
              else format(r$timestamp, "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE)
  }

  for (i in seq_along(results)) {
    r <- results[[i]]
    stem <- result_file_stem(i, r)

    if (r$result_type %in% c("table", "both") && !is.null(r$data)) {
      f <- paste0(stem, ".csv")
      utils::write.csv(r$data, file.path(dir, f), row.names = FALSE)
      files <- c(files, file.path(dir, f))
      add(i, r, "table", f, "ok")
    }
    if (r$result_type %in% c("plot", "both") && !is.null(r$plot)) {
      f <- paste0(stem, ".png")
      path <- file.path(dir, f)
      err <- tryCatch({
        ggplot2::ggsave(path, plot = r$plot, width = 8, height = 5, dpi = 150)
        if (!file.exists(path) || file.size(path) == 0L) "no image was written"
        else NULL
      }, error = function(e) conditionMessage(e))
      if (is.null(err)) {
        files <- c(files, path)
        add(i, r, "plot", f, "ok")
      } else {
        unlink(path)
        add(i, r, "plot", f,
            paste0("failed: ", gsub("\\s+", " ", trimws(err))))
      }
    }
  }

  manifest <- do.call(rbind, rows)
  mpath <- file.path(dir, "manifest.csv")
  utils::write.csv(manifest, mpath, row.names = FALSE)
  list(files = c(mpath, files),
       failed = sum(startsWith(manifest$status, "failed")))
}
