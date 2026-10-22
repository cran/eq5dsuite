# mod_catalogue.R — the catalogue of available analyses, with examples
#
# The Analysis page's "See catalogue of available analyses" opens a modal with
# one tab per component. Each tab lists the analyses the confirmed data can
# support (available_analyses() in mod_analysis.R), each with its plain title
# and description (CATALOGUE), an "Example output" and "Select this".
#
# The examples are the real output of each analysis, produced by running the
# registry entry -- its own prep() and function, with the inputs a user would
# choose -- on the bundled example_data. They are made once, by
# build_catalogue_previews(), and shipped in inst/shiny/catalogue/: a table
# as an .rds of the full result, a figure as a .png. Opening the catalogue
# only reads them, once per R process, and never runs an analysis on the
# user's data. test-analysis-catalogue.R rebuilds the tables and checks they
# match what is shipped, so a changed analysis cannot leave a stale example.
#
# To rebuild after changing an analysis, from the package directory:
#   Rscript -e 'devtools::load_all(); source("tests/testthat/helper-shiny-app.R");
#     test_path <- function(...) file.path("tests/testthat", ...);
#     app_env()$build_catalogue_previews()'

# Rows of a table shown before "View full example".
CATALOGUE_PREVIEW_ROWS <- 5L

# Where the shipped examples live.
catalogue_dir <- function() system.file("shiny", "catalogue", package = "eq5dsuite")

# The URL prefix the browser fetches the example figures from.
CATALOGUE_URL <- "eq5d-catalogue"
if (nzchar(catalogue_dir()))
  shiny::addResourcePath(CATALOGUE_URL, catalogue_dir())

# ── Building the examples ────────────────────────────────────────────────────

# example_data as the app holds it once the variables are confirmed and an
# EQ-5D value column has been added: EQ-5D-3L, timepoint, patient ID, group
# and EQ VAS selected, and UK values in "utility".
catalogue_example_rv <- function() {
  m <- list(eq5d_version = "3L", names_eq5d = DIMS_STD,
            name_fu = "time", levels_fu = c("Pre-op", "Post-op"),
            name_groupvar = "procedure", name_id = "id", name_vas = "vas",
            name_age = NULL, name_sex = NULL, name_utility = NULL, country = "")
  pd <- eq5dsuite:::eq5d_apply_mapping(eq5dsuite::example_data, m)
  pd$utility <- suppressWarnings(
    compute_utility_col(pd, "direct", "GB", "3L"))
  m$name_utility <- "utility"
  m$country <- "GB"
  list(mapping = m, processed_data = pd, value_cols = "utility")
}

# The inputs a user would choose for each kind of option.
catalogue_example_input <- function() {
  list(utility_col = "utility", country = "GB", topn = 10L,
       fu_levels = c("Pre-op", "Post-op"), group_filter = GROUP_FILTER_ALL)
}

# Run one registry entry on the example, as the Analysis page runs it.
catalogue_example_output <- function(spec, rv = catalogue_example_rv(),
                                     input = catalogue_example_input()) {
  p <- spec$prep(input, rv)
  out <- suppressWarnings(suppressMessages(
    do.call(pkg_fn(spec$fn), c(list(df = p$df), p$args))))
  if (identical(spec$type, "plot")) out$p else out
}

#' Make every example and write it to `dir`
build_catalogue_previews <- function(
    dir = file.path("inst", "shiny", "catalogue")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  rv <- catalogue_example_rv()
  input <- catalogue_example_input()
  for (s in ANALYSES) {
    out <- catalogue_example_output(s, rv, input)
    if (identical(s$type, "plot")) {
      ggplot2::ggsave(file.path(dir, paste0(s$id, ".png")), plot = out,
                      width = 8, height = 5, dpi = 100, bg = "white")
    } else {
      saveRDS(out, file.path(dir, paste0(s$id, ".rds")), compress = "xz")
    }
  }
  invisible(dir)
}

# ── Reading them, once per process ──────────────────────────────────────────

CATALOGUE_CACHE <- new.env(parent = emptyenv())

# The example for one registry id: list(type, data | src), or NULL. Tables
# are read from disk the first time and kept for every later session.
catalogue_preview <- function(id) {
  dir <- catalogue_dir()
  if (!nzchar(dir)) return(NULL)
  if (!is.null(CATALOGUE_CACHE[[id]])) return(CATALOGUE_CACHE[[id]])
  rds <- file.path(dir, paste0(id, ".rds"))
  png <- file.path(dir, paste0(id, ".png"))
  out <- if (file.exists(rds)) {
    CATALOGUE_CACHE$reads <- (CATALOGUE_CACHE$reads %||% 0L) + 1L
    list(type = "table", data = readRDS(rds))
  } else if (file.exists(png)) {
    list(type = "plot", src = paste0(CATALOGUE_URL, "/", id, ".png"))
  }
  if (!is.null(out)) assign(id, out, envir = CATALOGUE_CACHE)
  out
}

# ── Showing them ────────────────────────────────────────────────────────────

# A table as plain HTML, in the app's display formatting, inside a container
# that scrolls sideways when the table is wider than the modal: every column
# is kept.
example_table <- function(df) {
  f <- eq5dsuite:::eq5d_format_table(df)
  shiny::div(
    class = "example-table-wrap", tabindex = "0",
    role = "region", `aria-label` = "Example table, scrolls sideways",
    shiny::tags$table(
      class = "table table-sm example-table",
      shiny::tags$thead(shiny::tags$tr(lapply(names(f), shiny::tags$th,
                                              scope = "col"))),
      shiny::tags$tbody(lapply(seq_len(nrow(f)), function(r)
        shiny::tags$tr(lapply(seq_along(f), function(j)
          shiny::tags$td(as.character(f[r, j]))))))))
}

# The example block for one analysis. It depends only on the shipped files,
# so it is built once per process and reused by every session.
catalogue_example <- function(spec) {
  key <- paste0("html_", spec$id)
  # Kept as rendered HTML, so later opens do not render it again. Its only
  # dependency, the Enlarge icon's Font Awesome, is on every page already.
  if (is.null(CATALOGUE_CACHE[[key]]))
    assign(key, shiny::HTML(as.character(build_catalogue_example(spec))),
           envir = CATALOGUE_CACHE)
  CATALOGUE_CACHE[[key]]
}

build_catalogue_example <- function(spec) {
  ex <- catalogue_preview(spec$id)
  if (is.null(ex)) return(NULL)
  title <- CATALOGUE[[spec$id]]$title
  body <- if (identical(ex$type, "table")) {
    df <- ex$data
    n <- nrow(df)
    k <- min(n, CATALOGUE_PREVIEW_ROWS)
    shiny::tagList(
      example_table(df[seq_len(k), , drop = FALSE]),
      shiny::p(class = "example-caption",
               sprintf("Showing %d of %d rows", k, n)),
      if (n > k) shiny::tags$details(
        class = "example-full",
        shiny::tags$summary("View full example"),
        example_table(df)))
  } else {
    shiny::tagList(
      shiny::tags$a(
        href = ex$src, target = "_blank", rel = "noopener",
        title = "Open the full-size figure in a new tab",
        shiny::tags$img(src = ex$src, class = "example-plot", loading = "lazy",
                        alt = paste("Example output:", title))),
      shiny::p(class = "example-caption",
               shiny::tags$a(href = ex$src, target = "_blank", rel = "noopener",
                             shiny::icon("up-right-and-down-left-from-center"),
                             " Enlarge")))
  }
  shiny::div(
    class = "catalogue-example",
    shiny::p(class = "example-label", "Example output",
             shiny::span(class = "hint",
                         " — from the example dataset bundled with eq5dsuite")),
    body)
}

catalogue_item <- function(spec, pick) {
  cat <- CATALOGUE[[spec$id]]
  shiny::div(
    class = "catalogue-item",
    shiny::div(
      class = "catalogue-head",
      shiny::div(
        class = "catalogue-text",
        shiny::tags$h6(cat$title),
        shiny::p(cat$about),
        shiny::tags$small(class = "hint", sub(" —.*$", "", spec$label))),
      shiny::tags$button(
        type = "button", class = "btn btn-sm btn-primary catalogue-select",
        `aria-label` = paste("Select", cat$title),
        onclick = sprintf(
          "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
          pick, spec$id),
        "Select this")),
    catalogue_example(spec))
}

# The modal: one tab per component, each listing that component's available
# analyses. "Select this" reports the id through one input, so a single
# observer handles every entry.
catalogue_modal <- function(ns, specs) {
  pick <- ns("catalogue_pick")
  tabs <- lapply(names(COMPONENTS), function(comp_label) {
    comp <- COMPONENTS[[comp_label]]
    these <- Filter(function(s) identical(s$component, comp), specs)
    bslib::nav_panel(
      title = shiny::tagList(comp_label,
                             shiny::span(class = "catalogue-count",
                                         length(these))),
      value = comp,
      shiny::div(
        class = "catalogue-list",
        if (length(these)) lapply(these, catalogue_item, pick = pick)
        else hint("None of these analyses can run on your data yet. ",
                  "Select the variables they need on the Data page, or ",
                  "calculate EQ-5D values.")))
  })
  shiny::modalDialog(
    title = "Analyses available for your data",
    size = "xl", easyClose = TRUE,
    footer = shiny::modalButton("Close"),
    shiny::div(
      class = "catalogue",
      if (!length(specs))
        hint("No analysis can run yet. Confirm your variables on the Data ",
             "page and press Proceed on the Validation page."),
      do.call(bslib::navset_underline,
              c(tabs, list(id = ns("catalogue_tab"))))))
}
