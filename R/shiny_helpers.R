# Internal helpers used by the package's Shiny application.
# None of these functions are part of the public API.

# ── EQ-5D full dimension labels ───────────────────────────────────────────────

DIM_LABELS <- c(
  mo = "Mobility",
  sc = "Self-care",
  ua = "Usual activities",
  pd = "Pain/discomfort",
  ad = "Anxiety/depression"
)

# ── ensure_fu ─────────────────────────────────────────────────────────────────

#' Ensure a follow-up column exists (adding a synthetic one when absent)
#'
#' Package functions such as \code{eq5d_vas_summary} and \code{eq5d_utility_summary} internally
#' require a follow-up column.  When the user has not mapped a timepoint
#' variable, this helper adds a single-level factor column \code{".fu_all."}
#' (value \code{"All"}) so those functions can run cross-sectionally without
#' error.
#'
#' @param df A data frame (the processed Shiny dataset).
#' @param mapping A named list; \code{mapping$name_fu} should be non-\code{NULL}
#'   and non-empty when a follow-up variable was mapped.
#' @return A named list with elements \code{df} (possibly augmented with
#'   \code{".fu_all."}) and \code{name_fu} (the column name to pass to package
#'   functions).
#' @keywords internal
#' @noRd
ensure_fu <- function(df, mapping) {
  has_fu <- !is.null(mapping) &&
            !is.null(mapping[["name_fu"]]) &&
            nzchar(mapping[["name_fu"]]) &&
            "fu" %in% names(df)
  if (has_fu) {
    list(df = df, name_fu = "fu")
  } else {
    df[[".fu_all."]] <- factor("All")
    list(df = df, name_fu = ".fu_all.")
  }
}

# ── prettify_table_columns ────────────────────────────────────────────────────

#' Rename profile-table columns using full EQ-5D dimension labels
#'
#' Renames columns of a \code{.freqtab()}-style wide data frame.
#' Columns named \code{n_\{fu\}_\{dim\}} become \code{"\{DIM_LABEL\} n"} and
#' \code{freq_\{fu\}_\{dim\}} columns become \code{"\{DIM_LABEL\} \%"}.
#' When multiple follow-up levels exist the fu label is prepended to avoid
#' duplicate column names.
#'
#' @param df A data frame as returned by \code{eq5d_profile_level_summary} etc.
#' @return The same data frame with renamed columns (original order preserved).
#' @keywords internal
#' @noRd
prettify_table_columns <- function(df) {
  col_names <- names(df)
  if (!"level" %in% col_names) return(df)

  rest <- col_names[col_names != "level"]
  if (length(rest) == 0L || !all(grepl("^(n|freq)_", rest))) return(df)

  parsed <- lapply(rest, function(cn) {
    parts <- strsplit(cn, "_")[[1L]]
    if (length(parts) < 3L) return(NULL)
    list(
      metric = parts[1L],
      dim    = parts[length(parts)],
      fu     = paste(parts[2L:(length(parts) - 1L)], collapse = "_")
    )
  })
  if (any(vapply(parsed, is.null, logical(1L)))) return(df)

  fu_vals  <- vapply(parsed, `[[`, character(1L), "fu")
  multi_fu <- length(unique(fu_vals)) > 1L

  new_names <- vapply(parsed, function(p) {
    dim_label <- if (p$dim %in% names(DIM_LABELS)) DIM_LABELS[[p$dim]] else p$dim
    symbol    <- if (p$metric == "freq") "%" else "n"
    if (multi_fu) {
      paste0(p$fu, " \u2014 ", dim_label, " ", symbol)
    } else {
      paste0(dim_label, " ", symbol)
    }
  }, character(1L))

  names(df)[match(rest, names(df))] <- new_names
  df
}

# ── compute_utility_col ───────────────────────────────────────────────────────

#' Compute an EQ-5D utility index column
#'
#' @param df A data frame containing columns \code{mo}, \code{sc}, \code{ua},
#'   \code{pd}, \code{ad}.
#' @param method One of \code{"direct"} (use the instrument's own value set),
#'   \code{"xw"} (crosswalk 5L\eqn{\to}3L), or \code{"xwr"} (reverse crosswalk
#'   3L\eqn{\to}5L).
#' @param country Country/value-set code passed to \code{\link{eq5d}}.
#' @param eq5d_version \code{"3L"} or \code{"5L"} (used only when
#'   \code{method = "direct"}).
#' @return Numeric vector of utility index values, length \code{nrow(df)}.
#' @keywords internal
#' @noRd
compute_utility_col <- function(df, method, country, eq5d_version) {
  version <- switch(method,
    direct = eq5d_version,
    xw     = "XW",
    xwr    = "XWR",
    stop("Unknown utility method: ", method, call. = FALSE)
  )
  x <- df[, c("mo", "sc", "ua", "pd", "ad"), drop = FALSE]
  eq5d(x, country = country, version = version)
}

# ── Word report formatting ────────────────────────────────────────────────────

#' Tidy an analysis table's column headings for a Word document
#'
#' The analysis functions name columns for machines: \code{freq_Post-op_mo},
#' \code{Knee Replacement_Post-op_n}, \code{mo_\% Total}. Word cannot break a
#' line at an underscore, so each of those is one unbreakable token and the
#' column is made wide enough to hold it -- which is how a 21-column table ends
#' up with nothing legible in it. Replacing the underscores lets Word wrap the
#' heading over two or three lines and size the column to its contents instead.
#'
#' @param nm Character vector of column names.
#' @return The names, tidied.
#' @keywords internal
tidy_table_headings <- function(nm) {
  out <- nm
  # n_<fu>_<dim> and freq_<fu>_<dim>: the level and change summaries.
  out <- sub("^n_(.*)_([a-z]{2})$",    "\\2 \\1 n", out)
  out <- sub("^freq_(.*)_([a-z]{2})$", "\\2 \\1 %", out)
  # <group>_<fu>_n and _p: the PCHC tables.
  out <- sub("^(.*)_(.*)_n$", "\\1 \\2 n", out)
  out <- sub("^(.*)_(.*)_p$", "\\1 \\2 %", out)
  # Anything else that still carries an underscore.
  out <- gsub("_", " ", out, fixed = TRUE)
  trimws(gsub("\\s+", " ", out))
}

#' Give the tables in a .docx borders and let Word size their columns
#'
#' Pandoc writes its tables with the reference document's "Table" style, which
#' in the bundled template has no borders at all, and with the column widths
#' fixed from the width of the Markdown source. Both are set right here, on the
#' finished file: the style becomes "Table Grid", which the template already
#' defines, and the layout becomes autofit so Word sizes each column to what is
#' in it. Tables too wide to read at the body font are set smaller.
#'
#' A table of more than \code{landscape_from} columns is given a landscape page
#' of its own. The level-frequency-by-timepoint table is 21 columns wide, which
#' no amount of autofit will make readable across a 6.5-inch page.
#'
#' @param file A .docx to rewrite in place.
#' @param narrow_from Number of columns from which the table font is reduced.
#' @param narrow_half_pt Font size, in half-points, for those tables.
#' @param landscape_from Number of columns from which the table is put on a
#'   landscape page of its own. \code{Inf} keeps every page portrait.
#' Calling this twice on the same file does nothing the second time.
#'
#' @return \code{file}, invisibly.
#' @keywords internal
style_docx_tables <- function(file, narrow_from = 8L, narrow_half_pt = 16L,
                              landscape_from = 12L) {
  # tempfile(), not a name built from the clock: two reports written in the
  # same second would otherwise share a directory, and the first to finish
  # would delete it under the second.
  dir <- tempfile("eq5ddocxfmt_")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  utils::unzip(file, exdir = dir)
  doc <- file.path(dir, "word", "document.xml")
  if (!file.exists(doc)) return(invisible(file))
  xml <- paste(readLines(doc, warn = FALSE, encoding = "UTF-8"), collapse = "")

  # Already done. Running twice would add a second landscape break and a
  # second run of font properties to every cell.
  styled <- regmatches(xml, gregexpr("<w:tbl>.*?</w:tbl>", xml))[[1]]
  if (length(styled) &&
      all(grepl('<w:tblStyle w:val="TableGrid"', styled, fixed = TRUE)))
    return(invisible(file))

  restyle <- function(part) {
    # Borders, from a style the template already carries.
    part <- sub('<w:tblStyle w:val="[^"]*"[^>]*/>',
                '<w:tblStyle w:val="TableGrid" />', part)
    # Let Word size the columns to their contents, rather than keeping the
    # widths Pandoc computed from the width of the Markdown source.
    if (!grepl("<w:tblLayout", part, fixed = TRUE))
      part <- sub("</w:tblPr>", '<w:tblLayout w:type="autofit" /></w:tblPr>', part)
    part <- gsub('<w:gridCol w:w="[0-9]+"[^>]*/>', "<w:gridCol />", part)

    # A wide table needs a smaller font to stand a chance.
    n_cols <- lengths(gregexpr("<w:gridCol", part, fixed = TRUE))
    if (n_cols >= narrow_from) {
      props <- sprintf('<w:sz w:val="%d" /><w:szCs w:val="%d" />',
                       narrow_half_pt, narrow_half_pt)
      # Runs that already carry properties keep them, with the size added;
      # the rest gain a property block.
      part <- gsub("<w:r><w:rPr>", paste0("<w:r><w:rPr>", props), part, fixed = TRUE)
      part <- gsub("<w:r><w:t", paste0("<w:r><w:rPr>", props, "</w:rPr><w:t"),
                   part, fixed = TRUE)
    }
    part
  }

  # The document's own section properties, which the inserted ones copy so
  # that the header, the footer and the margins carry across the break.
  sect <- regmatches(xml, regexpr("<w:sectPr[ >].*?</w:sectPr>", xml))
  turn <- function(sp, landscape) {
    pg <- regmatches(sp, regexpr("<w:pgSz[^>]*/>", sp))
    if (!length(pg)) return(sp)
    w <- as.integer(sub('.*w:w="([0-9]+)".*', "\\1", pg))
    h <- as.integer(sub('.*w:h="([0-9]+)".*', "\\1", pg))
    long <- max(w, h); short <- min(w, h)
    new_pg <- if (landscape)
      sprintf('<w:pgSz w:w="%d" w:h="%d" w:orient="landscape" />', long, short)
    else
      sprintf('<w:pgSz w:w="%d" w:h="%d" />', short, long)
    sub("<w:pgSz[^>]*/>", new_pg, sp)
  }
  wrap_landscape <- function(part) {
    if (!length(sect)) return(part)
    para <- function(sp) paste0("<w:p><w:pPr>", sp, "</w:pPr></w:p>")
    paste0(para(turn(sect, FALSE)), part, para(turn(sect, TRUE)))
  }

  # regmatches<- rather than strsplit(): strsplit() on a zero-width lookaround
  # silently drops a character from each piece.
  m <- gregexpr("<w:tbl>.*?</w:tbl>", xml)
  found <- regmatches(xml, m)[[1]]
  if (length(found)) {
    done <- vapply(found, function(part) {
      n_cols <- lengths(gregexpr("<w:gridCol", part, fixed = TRUE))
      out <- restyle(part)
      if (n_cols >= landscape_from) out <- wrap_landscape(out)
      out
    }, character(1L), USE.NAMES = FALSE)
    regmatches(xml, m) <- list(done)
  }

  writeLines(xml, doc, useBytes = TRUE)

  # Rezip from the unpacked directory, keeping the archive's layout.
  #
  # zip::zip(root =) resolves the file list against a directory without
  # changing the session's working directory. Changing it, even with a
  # restore on exit, is a change to the whole R process: with two Shiny
  # sessions at once, one session's working directory would be in force while
  # another resolved a relative path.
  if (!requireNamespace("zip", quietly = TRUE))
    stop("Package 'zip' is required to write the Word report. ",
         "Install it with: install.packages(\"zip\")", call. = FALSE)

  target <- normalizePath(file, mustWork = FALSE)
  rel <- list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  unlink(target)
  zip::zip(target, files = rel, root = dir, include_directories = FALSE)

  invisible(file)
}

# ── write_results_docx ────────────────────────────────────────────────────────

#' Write the saved Shiny results to a Word document
#'
#' Produces one section per saved result, in the order given, each with the
#' result's title and its table or figure. The document is rendered through
#' Pandoc with \code{inst/shiny/template.docx} as the reference document, so
#' it takes that file's styles, header and footer; the template's own text is
#' not carried over.
#'
#' The order of \code{results} is the order of the report. The Shiny app lets
#' the user reorder and remove saved results on the Results page, and passes
#' whatever is left, so a removed result is simply absent from the list and
#' therefore from the document.
#'
#' @param results A list of saved results, each a list with \code{label},
#'   \code{result_type} ("table", "plot" or "both"), and \code{data} and/or
#'   \code{plot}.
#' @param file Path to write the \code{.docx} to.
#' @param template Path to the reference document. Defaults to the one
#'   bundled with the package.
#' @param title Title for the document.
#' @param format_table Optional function applied to each result's table before
#'   it is written, so that the document can show what the screen shows. The
#'   Shiny app passes its own formatter, which writes proportion columns as
#'   percentages. \code{NULL} prints the numbers rounded to three places.
#' @return \code{file}, invisibly.
#' @keywords internal
write_results_docx <- function(results, file,
                               template = system.file("shiny", "template.docx",
                                                      package = "eq5dsuite"),
                               title = "EQ-5D results",
                               format_table = NULL) {

  if (!requireNamespace("rmarkdown", quietly = TRUE))
    stop("Package 'rmarkdown' is required for the Word report. ",
         "Install it with: install.packages(\"rmarkdown\")", call. = FALSE)
  if (!rmarkdown::pandoc_available())
    stop("Pandoc is required for the Word report, and was not found. ",
         "It ships with RStudio; otherwise see https://pandoc.org/installing.html.",
         call. = FALSE)
  if (!length(results))
    stop("There are no results to put in the report.", call. = FALSE)

  # tempfile() gives a name no other call can collide with; see
  # style_docx_tables().
  dir <- tempfile("eq5ddocx_")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  # The template path goes into YAML, so backslashes and quotes must survive.
  yaml_path <- gsub("\\\\", "/", normalizePath(template, mustWork = TRUE))

  lines <- c(
    "---",
    paste0('title: "', gsub('"', "'", title), '"'),
    paste0('date: "', format(Sys.Date(), "%d %B %Y"), '"'),
    "output:",
    "  word_document:",
    paste0('    reference_docx: "', yaml_path, '"'),
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
    "```",
    ""
  )

  for (i in seq_along(results)) {
    r <- results[[i]]
    # A "#" in a label would start a heading of its own, and a newline would
    # end the heading early.
    heading <- if (is.null(r$label) || !nzchar(r$label)) paste("Result", i)
               else gsub("[\r\n]+", " ", r$label)
    heading <- gsub("^#+\\s*", "", heading)
    lines <- c(lines, paste0("## ", heading), "")

    has_table <- r$result_type %in% c("table", "both") && !is.null(r$data)
    has_plot  <- r$result_type %in% c("plot", "both")  && !is.null(r$plot)

    if (has_table) {
      lines <- c(lines,
        sprintf("```{r tbl-%d}", i),
        sprintf("knitr::kable(.tbl(results[[%d]]$data), row.names = FALSE,", i),
        sprintf("             align = .align(results[[%d]]$data))", i),
        "```", "")
    }
    if (has_plot) {
      lines <- c(lines,
        sprintf("```{r fig-%d, fig.width=6.5, fig.height=4.1, dpi=200}", i),
        sprintf("print(results[[%d]]$plot)", i),
        "```", "")
    }
    if (!has_table && !has_plot)
      lines <- c(lines, "*This result held no table or figure.*", "")
  }

  rmd <- file.path(dir, "report.Rmd")
  writeLines(lines, rmd, useBytes = TRUE)

  # `results` and `.tbl` are read by the chunks above; nothing else is needed.
  env <- new.env(parent = globalenv())
  env$results <- results
  fmt <- if (is.function(format_table)) format_table else
    function(df) {
      for (cn in names(df))
        if (is.double(df[[cn]])) df[[cn]] <- round(df[[cn]], 3L)
      df
    }
  # Numbers right, labels left.
  env$.align <- function(df)
    ifelse(vapply(df, is.numeric, logical(1L)), "r", "l")
  env$.tbl <- function(df) {
    df <- fmt(df)
    names(df) <- tidy_table_headings(names(df))
    df
  }

  out <- rmarkdown::render(rmd, output_file = "report.docx", output_dir = dir,
                           envir = env, quiet = TRUE)
  if (!file.copy(out, file, overwrite = TRUE))
    stop("The Word report could not be written to ", file, call. = FALSE)

  # Pandoc leaves the tables unbordered and their columns fixed at the width
  # of the Markdown source; both are put right on the finished file.
  style_docx_tables(file)

  invisible(file)
}
