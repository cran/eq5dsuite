# Generating a runnable R script from a Shiny session.
#
# The app records what the user did as a list of structured steps -- never as
# assembled text -- and this turns those steps into a script. Every value a
# user supplied goes through deparse(), so a column named `It's a group` or a
# file named `a"b.csv` cannot break the result.
#
# The script calls only the analysis functions. Reading the file, renaming the
# columns, the validation checks, the age-band midpoints and the display
# formatting are written out in full: those are internal to this package, and a
# script that reached into eq5dsuite::: would break the first time one of them
# changed shape. Written out, they can also be read and edited, which is half
# the point of handing someone a script.
#
# The risk of writing them out is that the script and the app drift apart.
# tests/testthat/test-script-generation.R is what stops that: it runs the
# generated script and compares every value column, every result and every
# formatted table against what the app produced, and checks that the inlined
# checks stop and warn exactly where eq5d_validate() does.

# ── Deparsing ─────────────────────────────────────────────────────────────────

#' A string as an R string literal that is valid everywhere
#'
#' The literal is plain ASCII: backslashes, both quote characters and control
#' characters are escaped, and every character outside ASCII is written as
#' \code{\\uXXXX} (or \code{\\UXXXXXXXX} beyond the Basic Multilingual
#' Plane). \code{deparse()} gets the escaping of backslashes and quotes
#' right but leaves non-ASCII characters as they are, and how those survive
#' being written to a file and read back depends on the platform's encoding --
#' on Windows before R 4.2 it is not UTF-8. An ASCII script parses the same
#' everywhere, and \code{parse(text = .r_string(x))} gives back \code{x}.
#'
#' This is the one way the package writes a string into R code; the test
#' suite uses it too, to put a file path into a generated script.
#'
#' @param x A single string; \code{NA} gives \code{NA_character_}.
#' @return A single string.
#' @keywords internal
.r_string <- function(x) {
  stopifnot(is.character(x) || is.factor(x), length(x) == 1L)
  x <- as.character(x)
  if (is.na(x)) return("NA_character_")
  cp <- utf8ToInt(enc2utf8(x))
  if (anyNA(cp)) stop("not valid UTF-8: ", sQuote(x), call. = FALSE)
  out <- vapply(cp, function(c) {
    if (c == 92L) "\\\\"                       # backslash
    else if (c == 34L) "\\\""                   # double quote
    else if (c == 10L) "\\n"
    else if (c == 13L) "\\r"
    else if (c == 9L)  "\\t"
    else if (c < 32L || c == 127L) sprintf("\\x%02x", c)
    else if (c < 128L) intToUtf8(c)
    else if (c <= 0xFFFF) sprintf("\\u%04x", c)
    else sprintf("\\U%08x", c)
  }, character(1L))
  paste0("\"", paste(out, collapse = ""), "\"")
}

# Any character outside ASCII in R source as an escape. Applied to what
# deparse() writes for values other than plain strings, where such a
# character can only stand inside a string literal.
.ascii_escape <- function(code) {
  cp <- utf8ToInt(enc2utf8(code))
  if (all(cp < 128L)) return(code)
  paste(vapply(cp, function(c)
    if (c < 128L) intToUtf8(c)
    else if (c <= 0xFFFF) sprintf("\\u%04x", c)
    else sprintf("\\U%08x", c), character(1L)), collapse = "")
}

# One value, as R source. Strings go through .r_string(), so a quote, a
# backslash or a non-ASCII character in a label, a column name or a file name
# cannot break the script on any platform; other values through deparse(),
# which writes a vector as c(...).
.deparse_arg <- function(x) {
  plain <- is.null(attributes(x)) ||
    identical(names(attributes(x)), "names")
  if (plain && (is.list(x) || (is.atomic(x) && !is.null(names(x))))) {
    # Names are written as string literals -- "Hôpital" = 1 -- because
    # deparse() writes a syntactic name bare, and a bare name cannot carry an
    # escape.
    nms <- if (is.null(names(x))) rep("", length(x)) else names(x)
    items <- vapply(seq_along(x), function(i) {
      v <- .deparse_arg(if (is.list(x)) x[[i]] else unname(x[i]))
      if (nzchar(nms[i])) paste0(.r_string(nms[i]), " = ", v) else v
    }, character(1L))
    return(paste0(if (is.list(x)) "list(" else "c(",
                  paste(items, collapse = ", "), ")"))
  }
  if (is.character(x) && is.null(attributes(x))) {
    if (length(x) == 0L) return("character(0)")
    if (length(x) == 1L) return(.r_string(x))
    return(paste0("c(", paste(vapply(x, .r_string, "", USE.NAMES = FALSE),
                              collapse = ", "), ")"))
  }
  .ascii_escape(paste(deparse(x, width.cutoff = 60L), collapse = ""))
}

#' Write a function call as R source
#'
#' @param fn Function name.
#' @param args Named list of arguments. \code{NULL} entries are kept, since
#'   \code{NULL} is often meaningful to the analysis functions.
#' @param indent Spaces to indent each argument by.
#' @param width Width beyond which the call is broken over several lines.
#' @param close What closes the call. More than one bracket where the call is
#'   wrapped in another, as \code{as.data.frame(readxl::read_excel(...))} is.
#' @return A single string.
#' @keywords internal
.deparse_call <- function(fn, args, indent = 2L, width = 72L, close = ")") {
  if (!length(args)) return(paste0(fn, "(", close))
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  parts <- vapply(seq_along(args), function(i) {
    v <- .deparse_arg(args[[i]])
    # A name that is not a syntactic R name is quoted. Argument names always
    # are; the names of a lookup vector built from the data -- age band
    # labels, group levels -- are not, and `70 to 79 = 70` does not parse.
    nm <- nms[i]
    if (nzchar(nm) && !grepl("^([.][._A-Za-z]|[A-Za-z])[._A-Za-z0-9]*$", nm))
      nm <- .deparse_arg(nm)
    if (nzchar(nm)) paste0(nm, " = ", v) else v
  }, character(1L))

  one_line <- paste0(fn, "(", paste(parts, collapse = ", "), close)
  if (nchar(one_line) <= width) return(one_line)

  pad <- strrep(" ", indent)
  paste0(fn, "(\n", pad, paste(parts, collapse = paste0(",\n", pad)),
         "\n", close)
}

# ── Restricting rows ──────────────────────────────────────────────────────────

#' The R code restricting a data frame to the rows of one group
#'
#' The app applies a result's filter by evaluating exactly this code (see
#' \code{.apply_filter()}), and the script writes it out, so the two select the
#' same rows by construction. \code{which()} leaves out a row whose group is
#' missing; indexing with the comparison itself would return such a row as a
#' row of \code{NA}s.
#'
#' @param filter A list with \code{column} and \code{value}.
#' @param data The name of the data frame to restrict.
#' @return A single string.
#' @keywords internal
.filter_code <- function(filter, data = "analysis_data") {
  paste0(data, "[which(as.character(", data, "[[",
         .deparse_arg(as.character(filter$column)), "]]) == ",
         .deparse_arg(as.character(filter$value)), "), , drop = FALSE]")
}

#' Apply a result's filter in the app
#'
#' @param df The data frame.
#' @param filter A list with \code{column} and \code{value}, or \code{NULL}
#'   for no restriction.
#' @return \code{df}, restricted.
#' @keywords internal
.apply_filter <- function(df, filter) {
  if (is.null(filter)) return(df)
  eval(parse(text = .filter_code(filter, "df"))[[1L]], list(df = df))
}

# ── Object names ──────────────────────────────────────────────────────────────

# A readable object name for a result: the analysis function without the
# package's prefix. Repeats of the same analysis are numbered.
.result_object_names <- function(results) {
  base <- vapply(results, function(r) {
    fn <- r$call$fn
    if (is.null(fn)) "result" else sub("^eq5d_", "", fn)
  }, character(1L))
  out <- base
  for (b in unique(base)) {
    at <- which(base == b)
    if (length(at) > 1L) out[at] <- paste0(b, "_", seq_along(at))
  }
  out
}

# ── The script ────────────────────────────────────────────────────────────────

#' Build an R script reproducing a Shiny session
#'
#' @param steps Ordered list of recorded steps: loading, mapping and value
#'   calculation. Each is a list with a \code{kind}.
#' @param results The saved results, in the order the user put them. Each
#'   carries a \code{call} describing the analysis that produced it.
#' @param title Heading for the script.
#' @return A character vector of lines.
#' @keywords internal
script_from_session <- function(steps = list(), results = list(),
                                title = "EQ-5D analysis") {
  L <- function(...) c(...)
  section <- function(n, heading)
    c("", paste0("# ", n, ". ", heading, " ",
                 strrep("-", max(4L, 74L - nchar(heading) - nchar(n))), ""), "")

  out <- c(.script_header(title), .script_parsers(), "")
  out <- c(out, section(1L, "Load data"), .script_load(steps))
  out <- c(out, section(2L, "Validation"), .script_validate(steps))
  out <- c(out, section(3L, "EQ-5D value calculation"), .script_values(steps))
  out <- c(out, section(4L, "Analysis"), .script_analyses(results))
  out
}

.script_header <- function(title) {
  ver <- tryCatch(as.character(utils::packageVersion("eq5dsuite")),
                  error = function(e) "unknown")
  c("# ---------------------------------------------------------------------------",
    paste0("# ", title),
    "#",
    paste0("# Generated by the eq5dsuite Shiny app on ", format(Sys.Date(), "%Y-%m-%d"), "."),
    paste0("#   eq5dsuite ", ver),
    paste0("#   ", R.version.string),
    "#",
    "# This script reproduces the analysis carried out in the app. It needs only",
    "# eq5dsuite; it does not depend on the app or on anything in it.",
    "# ---------------------------------------------------------------------------",
    "",
    "library(eq5dsuite)",
    "library(ggplot2)",
    "")
}

.step_of <- function(steps, kind) {
  for (s in steps) if (identical(s$kind, kind)) return(s)
  NULL
}
.steps_of <- function(steps, kind)
  Filter(function(s) identical(s$kind, kind), steps)

.script_load <- function(steps) {
  load <- .step_of(steps, "load")
  map  <- .step_of(steps, "map")

  out <- character(0)
  if (is.null(load)) {
    out <- c(out, "# No data were loaded in the app.", "raw_data <- NULL")
  } else if (identical(load$source, "example")) {
    out <- c(out,
      "# The app was run on the example dataset bundled with the package.",
      "raw_data <- eq5dsuite::example_data")
  } else {
    out <- c(out,
      paste0("# Uploaded in the app as ", .deparse_arg(load$file), "."),
      "#",
      "# The reading options below are the ones the app used. Change them if",
      "# your file differs -- for a semicolon-separated file, for instance,",
      '# use sep = ";" and dec = ",".',
      'data_path <- "REPLACE BY ACTUAL PATH"',
      "",
      .script_read(load))
  }

  if (!is.null(map)) out <- c(out, "", .script_map(map$mapping))
  out
}

# Reading the uploaded file. The app's own reader is a switch on the extension
# over read.csv(), readxl::read_excel() and readRDS(); only the one branch the
# user took is written out.
.script_read <- function(load) {
  args <- load$read_args
  if (is.null(args)) args <- list()
  ext <- if (is.null(load$ext)) "" else tolower(as.character(load$ext))

  if (ext %in% c("xlsx", "xls")) {
    return(.deparse_call("raw_data <- as.data.frame(readxl::read_excel",
                         c(list(quote(data_path)), args),
                         close = "))"))
  }
  if (identical(ext, "rds")) {
    return("raw_data <- readRDS(data_path)")
  }
  .deparse_call("raw_data <- read.csv",
                c(list(quote(data_path)), args,
                  list(stringsAsFactors = FALSE, check.names = FALSE)))
}

# Renaming the mapped columns, and the two coercions that go with it. The app
# does this by rule over any mapping; here the mapping is known, so the
# columns can simply be named -- which is also the version a reader can check
# against their own file.
.script_map <- function(mapping) {
  std  <- c("mo", "sc", "ua", "pd", "ad")
  from <- as.character(mapping$names_eq5d)[seq_along(std)]
  to   <- std
  for (opt in list(c("name_fu", "fu"), c("name_groupvar", "groupvar"),
                   c("name_id", "id"), c("name_vas", "vas"),
                   c("name_utility", "utility"))) {
    nm <- mapping[[opt[1L]]]
    if (!is.null(nm) && length(nm) == 1L && !is.na(nm) && nzchar(nm)) {
      from <- c(from, nm)
      to   <- c(to, opt[2L])
    }
  }
  keep <- !is.na(from) & nzchar(from)
  from <- from[keep]; to <- to[keep]

  c("# The columns mapped in the app, renamed to the names the analysis",
    "# functions expect. A name that is already what it should be is listed",
    "# all the same, so the mapping can be read off against your own file.",
    "analysis_data <- raw_data",
    "",
    paste0("mapped <- ", .deparse_arg(from)),
    paste0("as_named <- ", .deparse_arg(to)),
    "if (!all(mapped %in% names(analysis_data)))",
    "  stop(\"Columns not found: \",",
    "       paste(setdiff(mapped, names(analysis_data)), collapse = \", \"))",
    "if (anyDuplicated(mapped))",
    "  stop(\"The same column is mapped twice.\")",
    "names(analysis_data)[match(mapped, names(analysis_data))] <- as_named",
    if (any(c("vas", "utility") %in% to)) c(
      "",
      "# The EQ VAS and an existing EQ-5D value column as numbers, read from",
      "# their labels if they are factors. The dimensions are checked, then",
      "# converted, in the next section.",
      if ("vas" %in% to) "analysis_data$vas <- parse_number(analysis_data$vas)",
      if ("utility" %in% to)
        "analysis_data$utility <- parse_number(analysis_data$utility)"))
}

# The functions the script reads values with: the package's own
# .parse_number() and .parse_levels(), deparsed, so the script reads every
# value exactly as the app does -- labels rather than factor codes, and a
# dimension that is not a whole level of the instrument as NA rather than
# truncated (review Q01, Q08). test-preprocessing-equivalence.R checks they
# agree on a battery of inputs.
.script_parsers <- function() {
  fn_src <- function(name, f) {
    body <- deparse(f, width.cutoff = 70L)
    body <- gsub(".parse_number(", "parse_number(", body, fixed = TRUE)
    c(paste0(name, " <- ", body[1L]), body[-1L])
  }
  c("# How the app reads values. A factor is read through its labels, never",
    "# its level codes; a number is kept exactly. A dimension value is an",
    "# EQ-5D level only if it is a whole number from 1 to the instrument's",
    "# highest level, and NA otherwise: 1.9 is not rounded to either level.",
    fn_src("parse_number", .parse_number),
    "",
    fn_src("parse_levels", .parse_levels))
}

# The checks the app runs on its Validation page, in the order it runs them.
# The app shows all its findings in the browser and lets the user decide; a
# script has to decide for itself, so a missing dimension column -- the one
# finding that makes the analysis impossible -- stops it, and everything else
# warns, exactly as the app carries on past them.
#
# Note what does NOT stop the script: a level outside the instrument's range.
# `example_data` records missing responses as 9, so stopping there would halt
# every script generated from the example dataset, where the app ran happily.
.script_validate <- function(steps) {
  map <- .step_of(steps, "map")
  if (is.null(map))
    return("# No columns were mapped, so there was nothing to check.")
  mapping <- map$mapping
  std <- c("mo", "sc", "ua", "pd", "ad")
  max_level <- .max_level(mapping$eq5d_version)
  has <- function(nm) {
    v <- mapping[[nm]]
    !is.null(v) && length(v) == 1L && !is.na(v) && nzchar(v)
  }

  out <- c(
    "# The checks the app ran on its Validation page. A missing dimension",
    "# column stops the script; anything else is a warning, which is how the",
    "# app treated it.",
    paste0("dims <- ", .deparse_arg(std)),
    "absent <- setdiff(dims, names(analysis_data))",
    "if (length(absent))",
    "  stop(\"EQ-5D columns not found in data: \",",
    "       paste(absent, collapse = \", \"))",
    "",
    "# A value that is a number but not a level of the instrument -- 1.9,",
    "# or 9 used as a missing code -- is reported, then set to NA, as in the",
    "# app. Text that is not a number counts as missing.",
    paste0("max_level <- ", max_level, "L"),
    "invalid <- vapply(analysis_data[dims], function(v)",
    "  any(!is.na(parse_number(v)) & is.na(parse_levels(v, max_level))),",
    "  logical(1L))",
    "if (any(invalid))",
    "  warning(\"Some values in \", paste(dims[invalid], collapse = \", \"),",
    "          \" are not a level of the instrument (a whole number from 1 to \",",
    "          max_level, \") and are set to NA.\", call. = FALSE)",
    "for (d in dims) analysis_data[[d]] <- parse_levels(analysis_data[[d]], max_level)",
    "",
    "complete <- sum(stats::complete.cases(analysis_data[dims]))",
    "message(format(complete, big.mark = \",\"), \" of \",",
    "        format(nrow(analysis_data), big.mark = \",\"),",
    "        \" rows have a complete EQ-5D profile.\")")

  # Repeated patient IDs: expected across timepoints, a problem within one.
  if (has("name_id")) {
    out <- c(out, "",
      if (has("name_fu")) c(
        "# Repeated IDs are expected across timepoints; a repeated",
        "# ID-and-timepoint pair is a duplicate record.",
        "if (anyDuplicated(analysis_data[, c(\"id\", \"fu\")]))",
        "  warning(\"Some ID-timepoint combinations are duplicated. \",",
        "          \"Check for duplicate records.\", call. = FALSE)")
      else c(
        "# Cross-sectional data: one row per patient.",
        "if (anyDuplicated(analysis_data$id))",
        "  warning(\"Repeated patient IDs. If these data are longitudinal, \",",
        "          \"select a timepoint variable.\", call. = FALSE)"))
  }

  # Follow-up values outside the order the user gave become NA downstream.
  if (has("name_fu") && length(mapping$levels_fu)) {
    out <- c(out, "",
      paste0("levels_fu <- ", .deparse_arg(as.character(mapping$levels_fu))),
      "unlisted <- setdiff(unique(as.character(analysis_data$fu)),",
      "                    c(levels_fu, NA))",
      "if (length(unlisted))",
      "  warning(\"Timepoints not in the order given (\",",
      "          paste(unlisted, collapse = \", \"),",
      "          \"); rows holding them become NA.\", call. = FALSE)")
  }

  if (has("name_vas")) {
    out <- c(out, "",
      "if (any(!is.na(analysis_data$vas) &",
      "        (analysis_data$vas < 0 | analysis_data$vas > 100)))",
      "  warning(\"EQ VAS values outside 0-100.\", call. = FALSE)")
  }

  out
}

.script_values <- function(steps) {
  vals <- .steps_of(steps, "value")
  if (!length(vals))
    return(c("# No EQ-5D values were calculated in the app.",
             "# The EQ-5D value analyses need a value column; see ?eq5d."))

  dims <- c("mo", "sc", "ua", "pd", "ad")
  out <- character(0)
  for (i in seq_along(vals)) {
    v <- vals[[i]]
    if (i > 1L) out <- c(out, "")
    if (identical(v$method, "uk")) {
      out <- c(out,
        paste0("# The NICE Decision Support Unit's UK mapping: ", v$from,
               " responses to"),
        paste0("# UK ", v$to, " values. It needs the respondent's age and sex as ",
               "well as"),
        "# the health state.")
      if (isTRUE(v$banded)) {
        out <- c(out,
          "#",
          "# The age column holds bands, so the midpoint of each band is used.",
          "# Age is in completed years, so \"30 to 39\" covers [30, 40) and its",
          "# midpoint is 35, not 34.5. The DSU's bands begin at 35, 45, 55 and",
          "# 65 -- exactly those midpoints -- so a band that straddles a",
          "# boundary falls in the upper one and the result is an",
          "# approximation. Use exact ages where they are available.",
          "#",
          "# Each label is replaced by the age the app used for it: the",
          "# midpoint of a closed band, or the lower bound of a band open at",
          "# the top, where every age in it falls in the same DSU category.",
          "# A label that could identify more than one category has no entry",
          "# and becomes NA, as it did in the app.",
          .deparse_call("age_for_band <- c", as.list(v$age_lookup), width = 0L),
          paste0("age_years <- unname(age_for_band[as.character(analysis_data[[",
                 .deparse_arg(v$age_col), "]])])"))
      } else {
        out <- c(out,
          paste0("age_years <- parse_number(analysis_data[[",
                 .deparse_arg(v$age_col), "]])"))
      }
      out <- c(out,
        paste0("is_male <- as.integer(as.character(analysis_data[[",
               .deparse_arg(v$sex_col), "]]) == ", .deparse_arg(v$male_value), ")"),
        paste0("is_male[is.na(analysis_data[[", .deparse_arg(v$sex_col),
               "]])] <- NA_integer_"),
        "",
        paste0("analysis_data[[", .deparse_arg(v$column), "]] <- ",
               v$fn, "("),
        paste0("  analysis_data[, ", .deparse_arg(dims), "],"),
        "  age  = age_years,",
        "  male = is_male",
        ")")
    } else {
      out <- c(out,
        paste0("# EQ-5D values from the \"", v$method_label, "\" method, value set ",
               .deparse_arg(v$country), "."),
        paste0("analysis_data[[", .deparse_arg(v$column), "]] <- ", v$fn, "("),
        paste0("  analysis_data[, ", .deparse_arg(dims), "],"),
        paste0("  country = ", .deparse_arg(v$country), ","),
        paste0("  version = ", .deparse_arg(v$version)),
        ")")
    }
  }
  out
}

# The display formatting, written out once for the whole script.
#
# The app shows proportions as percentages, counts without decimals and tidied
# headings, and the Word report uses the same rules, so a reader comparing the
# script's output with what was on screen should see the same tables. Which
# columns hold proportions is decided from their names, which is how the
# package decides it too: the analysis functions name them `freq_*`, `*_p`
# beside a `*_n`, or one of a short list of plain-English headings.
#
# The result objects themselves keep full numeric precision; this only
# formats a copy for printing.
.script_format_helper <- function() {
  c("# The app displays proportions as percentages, counts without decimals,",
    "# and tidied column headings. This does the same, so the tables printed",
    "# below match what was on screen. The unformatted results above keep",
    "# their full numeric precision -- use those for further analysis.",
    "format_eq5d_table <- function(df) {",
    "  nm  <- names(df)",
    "  is_percent <- grepl(\"^freq_|% Total$|% Type$\", nm) |",
    "    (grepl(\"_p$\", nm) & sub(\"_p$\", \"_n\", nm) %in% nm) |",
    "    nm %in% c(\"p\", \"cum_p\", \"Percentage\", \"Cumulative percentage\",",
    "              \"%\", \"Cum (%)\")",
    "",
    "  for (i in which(vapply(df, is.numeric, logical(1L)))) {",
    "    v <- df[[i]]",
    "    df[[i]] <- if (is_percent[i]) {",
    "      paste0(formatC(100 * v, format = \"f\", digits = 1), \"%\")",
    "    } else if (is.integer(v) ||",
    "               all(v[is.finite(v)] == round(v[is.finite(v)]))) {",
    "      formatC(v, format = \"d\", big.mark = \",\")",
    "    } else {",
    "      formatC(v, format = \"f\", digits = 2)",
    "    }",
    "    df[[i]][is.na(v)] <- \"\"",
    "  }",
    "",
    "  h <- sub(\"^n_(.*)_([a-z]{2})$\",    \"\\\\2 \\\\1 n\", nm)",
    "  h <- sub(\"^freq_(.*)_([a-z]{2})$\", \"\\\\2 \\\\1 %\", h)",
    "  h <- sub(\"^(.*)_(.*)_n$\",          \"\\\\1 \\\\2 n\", h)",
    "  h <- sub(\"^(.*)_(.*)_p$\",          \"\\\\1 \\\\2 %\", h)",
    "  names(df) <- trimws(gsub(\"\\\\s+\", \" \", gsub(\"_\", \" \", h, fixed = TRUE)))",
    "  df",
    "}")
}

.script_analyses <- function(results) {
  if (!length(results))
    return("# No analyses were saved in the app.")

  objs <- .result_object_names(results)
  # The formatting helper, once, and only where there is a table to format.
  has_table <- any(vapply(results, function(r)
    !identical(r$call$type, "plot") && !is.null(r$call$fn), logical(1L)))
  out <- if (has_table) c(.script_format_helper(), "") else character(0)
  for (i in seq_along(results)) {
    r <- results[[i]]
    cl <- r$call
    if (is.null(cl) || is.null(cl$fn)) next
    if (i > 1L) out <- c(out, "", "")

    out <- c(out, paste0("# 4", letters[i], ". ", r$label))

    # A result whose rows were restricted, or whose data needed a stand-in
    # column, gets a data frame of its own, so that nothing done for one
    # result changes the data the next one is run on.
    data_name <- "analysis_data"
    if (!is.null(cl$filter) || !is.null(cl$prep)) {
      data_name <- paste0(objs[i], "_data")
      out <- c(out,
        if (!is.null(cl$filter)) c(
          "#      Restricted to one group, as in the app. A row whose group is",
          "#      missing belongs to no group and is left out.",
          paste0(data_name, " <- ", .filter_code(cl$filter)))
        else paste0(data_name, " <- analysis_data"))
    }
    if (identical(cl$prep, "groupvar"))
      out <- c(out,
        "#      No group column was mapped; the by-group analyses need one.",
        paste0(data_name, "$groupvar <- \"All\""))
    if (identical(cl$prep, "fu_all"))
      out <- c(out,
        "#      No timepoint was mapped; a single-level one stands in for it.",
        paste0(data_name, "$.fu_all. <- factor(\"All\")"))

    out <- c(out, .deparse_call(paste0(objs[i], " <- ", cl$fn),
                                c(list(df = as.name(data_name)), cl$args)))

    if (identical(cl$type, "plot")) {
      out <- c(out, "",
        "# The plot functions return the plot data and a ggplot object. The app",
        "# shows `p` as it is returned; its theme is applied inside the package.",
        paste0("print(", objs[i], "$p)"))
    } else {
      out <- c(out, "",
        paste0(objs[i], "_formatted <- format_eq5d_table(", objs[i], ")"),
        paste0("print(", objs[i], "_formatted)"))
    }
  }
  out
}
