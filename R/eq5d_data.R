# Reading, mapping, validating and formatting EQ-5D data.
#
# These support the Shiny app, and nothing else in the package calls them.
# They are internal: a user analysing EQ-5D data from the console has no use
# for them, and they would only crowd the analysis functions in `eq5dsuite::`.
# The app reaches them through eq5dsuite:::, which it may because it runs
# inside this package.
#
# The script the app generates cannot, so it writes the same work out in full.
# What keeps the two from drifting is tests/testthat/test-script-generation.R,
# which runs the generated script and compares every value column, every
# result and every formatted table against what the app produced, and checks
# that the script's inlined checks stop and warn where eq5d_validate() does.

# ── eq5d_read_data ────────────────────────────────────────────────────────────

#' Read a data file of EQ-5D responses
#'
#' Reads a CSV, Excel or RDS file, choosing by the file's extension unless
#' \code{type} says otherwise. This is what the Shiny app's upload uses, and
#' what the R script it generates calls, so a script reproduces what the app
#' read.
#'
#' @param path Path to the file.
#' @param type File type: \code{"csv"}, \code{"xlsx"}, \code{"xls"} or
#'   \code{"rds"}. \code{NULL} (default) takes it from the extension of
#'   \code{path}.
#' @param sep Field separator for a CSV. Use \code{";"} for the
#'   semicolon-separated files common where the comma is the decimal mark.
#' @param dec Decimal mark for a CSV.
#' @param fileEncoding Encoding of a CSV, passed to \code{\link{read.csv}}.
#'   \code{""} uses the session's default; \code{"UTF-8"} and
#'   \code{"latin1"} are the usual alternatives.
#' @param sheet Sheet of an Excel file: a number or a name.
#' @param ... Further arguments passed to \code{\link[utils]{read.csv}} or
#'   \code{readxl::read_excel}.
#' @return A data frame. Character columns are not converted to factors and
#'   column names are left as they are in the file.
#' @seealso \code{\link{eq5d_apply_mapping}} for naming the columns an analysis
#'   needs, and \code{\link{eq5d_validate}} for checking them.
#' @keywords internal
eq5d_read_data <- function(path, type = NULL, sep = ",", dec = ".",
                           fileEncoding = "", sheet = 1, ...) {
  if (!length(path) || !nzchar(path))
    stop("`path` must be the path to a file.", call. = FALSE)
  if (is.null(type)) type <- tools::file_ext(path)
  type <- tolower(type)

  switch(type,
    csv = utils::read.csv(path, sep = sep, dec = dec,
                          fileEncoding = fileEncoding,
                          stringsAsFactors = FALSE, check.names = FALSE, ...),
    xlsx = ,
    xls = {
      if (!requireNamespace("readxl", quietly = TRUE))
        stop("Package 'readxl' is required to read Excel files. ",
             "Install it with: install.packages(\"readxl\")", call. = FALSE)
      as.data.frame(readxl::read_excel(path, sheet = sheet, ...))
    },
    rds = readRDS(path),
    stop("Unsupported file type: ", type,
         ". Use a .csv, .xlsx, .xls or .rds file, or name the type with ",
         "`type =`.", call. = FALSE)
  )
}

# ── eq5d_apply_mapping ────────────────────────────────────────────────────────

#' Rename a data frame's columns to the names the analyses expect
#'
#' The analysis functions take their columns by name. This renames the columns
#' a user has identified to the canonical names those functions use -- "mo",
#' "sc", "ua", "pd", "ad" for the dimensions, and "fu", "groupvar", "id",
#' "vas" and "utility" for the optional ones -- and coerces the dimensions to
#' integer and the EQ VAS to numeric.
#'
#' @param df A data frame of EQ-5D responses.
#' @param mapping A named list describing the columns. Recognised entries:
#'   \describe{
#'     \item{\code{names_eq5d}}{character vector of five dimension columns, in
#'       the order mobility, self-care, usual activities, pain/discomfort,
#'       anxiety/depression.}
#'     \item{\code{name_fu}, \code{name_groupvar}, \code{name_id},
#'       \code{name_vas}, \code{name_utility}}{single column names, or
#'       \code{NULL} where there is none.}
#'   }
#'   \code{eq5d_version} (\code{"3L"}, \code{"5L"} or \code{"Y3L"}) sets
#'   the levels a dimension may take, 1 to 3 or 1 to 5: any other value,
#'   including a fractional one, becomes \code{NA}. When it is absent, any
#'   whole number from 1 to 9 is kept.
#'   Other entries, such as \code{levels_fu}, are
#'   ignored here and used by the analysis functions.
#'
#'   The whole mapping is applied at once, so columns may swap names. A column
#'   mapped to more than one role, or a target name that already belongs to a
#'   column nobody mapped, is an error rather than a silently duplicated name.
#'   Factor columns are read through their labels.
#' @return \code{df} with the mapped columns renamed and coerced. Columns not
#'   named in \code{mapping} are left untouched.
#' @seealso \code{\link{eq5d_validate}}, which checks the same mapping.
#' @keywords internal
eq5d_apply_mapping <- function(df, mapping) {
  mapping$eq5d_version <- .mapping_version(mapping$eq5d_version)
  if (!is.data.frame(df)) stop("`df` must be a data frame.", call. = FALSE)
  std_dims <- c("mo", "sc", "ua", "pd", "ad")

  # The whole mapping as one source -> target table, resolved against the
  # column names as they are now.
  #
  # Renaming used to happen one column at a time against already-renamed
  # names, so a swap destroyed a column: mapping sc -> mo and mo -> sc on a
  # standard frame gave sc, sc, ua, pd, ad, with the mobility column gone and
  # a duplicate name in its place. Positions are resolved first and the names
  # assigned together, which is what the generated script has always done.
  from <- character(0L); to <- character(0L)
  named <- function(nm) {
    v <- mapping[[nm]]
    !is.null(v) && length(v) == 1L && !is.na(v) && nzchar(v)
  }
  for (i in seq_along(std_dims)) {
    orig <- mapping$names_eq5d[i]
    if (!is.null(orig) && !is.na(orig) && nzchar(orig)) {
      from <- c(from, orig); to <- c(to, std_dims[i])
    }
  }
  for (opt in list(c("name_fu", "fu"), c("name_groupvar", "groupvar"),
                   c("name_id", "id"), c("name_vas", "vas"),
                   c("name_utility", "utility"))) {
    if (named(opt[1L])) {
      from <- c(from, mapping[[opt[1L]]]); to <- c(to, opt[2L])
    }
  }

  # A column may be mapped once. Mapping it twice is a mistake the user can
  # fix, and silently taking the last one would analyse the wrong variable.
  dup <- unique(from[duplicated(from)])
  if (length(dup))
    stop("Column(s) mapped to more than one role: ",
         paste(dup, collapse = ", "),
         ". Each column may be used once.", call. = FALSE)

  present <- from %in% names(df)
  from <- from[present]; to <- to[present]

  # A target name that already belongs to a column nobody mapped would become
  # a duplicate, and every later lookup by name would find the wrong one.
  untouched <- setdiff(names(df), from)
  clash <- intersect(untouched, to)
  if (length(clash))
    stop("Mapping would give two columns the name(s) ",
         paste(clash, collapse = ", "),
         ". Rename or drop the existing column(s) first.", call. = FALSE)

  names(df)[match(from, names(df))] <- to

  # Coercion after the renaming, so each column is found by its canonical
  # name, through the shared parsers in R/validate_dims.R, which the generated
  # script contains too. A dimension value is checked before it becomes an
  # integer: one that is not a whole level of the instrument is NA, where
  # as.integer() used to truncate 1.9 to level 1 (review Q01). Factors are
  # read through their labels.
  max_level <- .max_level(mapping$eq5d_version)
  for (d in intersect(std_dims, names(df)))
    df[[d]] <- .parse_levels(df[[d]], max_level)
  for (col in intersect(c("vas", "utility"), names(df)))
    df[[col]] <- .parse_number(df[[col]])

  df
}

# ── eq5d_validate ─────────────────────────────────────────────────────────────

#' Check a data frame of EQ-5D responses against its column mapping
#'
#' Runs the checks the Shiny app's Validation page runs: that the dimension
#' columns are there, that their values are levels the instrument allows, how
#' much is missing, whether patient IDs repeat in a way the timepoint column
#' explains, and whether EQ VAS scores are in range.
#'
#' Each finding is reported as it is found -- with \code{message()} for a
#' check that passed, \code{warning()} for one that found something, and for a
#' problem that stops the analysis either \code{stop()} or a warning, according
#' to \code{stop_on_error}. The findings are also returned, so a caller that
#' wants to present them itself can.
#'
#' @param df A data frame, as returned by \code{\link{eq5d_apply_mapping}} or
#'   with the user's own column names.
#' @param mapping A named list of column names; see
#'   \code{\link{eq5d_apply_mapping}}. \code{eq5d_version} decides the levels
#'   the dimensions may take.
#' @param stop_on_error Whether a finding of type \code{"error"} should stop
#'   execution. \code{FALSE} (default) reports it as a warning and carries on,
#'   which is what an interactive caller wants; a script will usually want
#'   \code{TRUE}.
#' @param quiet Whether to suppress the messages, warnings and errors and only
#'   return the findings.
#' @return Invisibly, a data frame with one row per finding and the columns
#'   \code{type} (\code{"ok"}, \code{"warning"} or \code{"error"}) and
#'   \code{message}. The messages are plain text. Attribute \code{"details"}
#'   is a list with one element per finding: \code{NULL}, or a list with
#'   \code{summary} and \code{records} (data frames: the values found and
#'   their counts, and the records affected, by row number and, where those
#'   are selected, patient ID and timepoint), \code{notes}, \code{emphasis}
#'   and \code{denominator} (text). The details describe the data and change
#'   nothing.
#' @seealso \code{\link{eq5d_apply_mapping}}
#' @keywords internal
eq5d_validate <- function(df, mapping, stop_on_error = FALSE, quiet = FALSE) {
  mapping$eq5d_version <- .mapping_version(mapping$eq5d_version)
  if (!is.data.frame(df)) stop("`df` must be a data frame.", call. = FALSE)

  found <- list()
  note <- function(type, ..., details = NULL) {
    found[[length(found) + 1L]] <<- list(type = type, message = paste0(...),
                                         details = details)
  }

  n_rows <- nrow(df)
  note("ok", .fmt_n(n_rows), " rows loaded.")

  eq5d_cols <- mapping$names_eq5d
  gone <- eq5d_cols[!eq5d_cols %in% names(df)]
  if (length(gone)) {
    note("error", "EQ-5D columns not found in data: ",
         paste(gone, collapse = ", "), ".",
         details = .details(
           summary = data.frame(`Selected column` = gone, check.names = FALSE),
           notes = c(paste0("These columns were selected for EQ-5D dimensions ",
                            "but are not in the data. Choose the right columns ",
                            "on the Data page."),
                     paste0("Columns in the data: ",
                            paste(utils::head(names(df), 60L), collapse = ", "),
                            if (ncol(df) > 60L) ", ..." else ""))))
  } else {
    max_level <- if (.max_level(mapping$eq5d_version) == 3L) 3L else 5L
    # The same predicate the analysis functions use, so the page cannot bless
    # a value the analysis will discard. as.integer() ran first here, which
    # truncated 1.9 to 1 and then reported it as in range.
    # .dim_status() in R/validate_dims.R.
    status <- lapply(eq5d_cols, function(col)
      .dim_status(df[[col]], max_level = max_level))
    names(status) <- eq5d_cols
    kinds <- lapply(status, function(st) unique(st[st %in% .DIM_REJECTED]))
    unusable <- vapply(kinds, function(k) length(k) > 0L, logical(1L))

    if (any(unusable)) {
      note("warning", "Some values in [",
           paste(eq5d_cols[unusable], collapse = ", "),
           "] are not a level the instrument allows (",
           paste(sort(unique(unlist(kinds))), collapse = ", "),
           "; levels must be whole numbers from 1 to ", max_level,
           ") and will be set to NA.",
           details = .invalid_level_details(df, mapping, status, max_level))
    } else {
      note("ok", "All EQ-5D values within expected range (1\u2013",
           max_level, ").")
    }

    # Missingness is counted on the usable values, so a fractional or
    # out-of-range level is reported as missing rather than as present.
    usable <- lapply(status, function(st) {
      v <- attr(st, "value"); v[st %in% .DIM_REJECTED] <- NA_real_; v })
    n_complete <- sum(stats::complete.cases(usable))
    n_miss <- n_rows - n_complete
    if (n_miss > 0L) {
      note("warning", .fmt_n(n_miss), " of ", .fmt_n(n_rows), " rows (",
           .pct(n_miss, n_rows), ") have missing EQ-5D values.",
           details = .missing_details(df, mapping, status))
    } else {
      note("ok", "No missing EQ-5D values.")
    }
  }

  if (.has_col(mapping$name_id, df)) {
    ids <- df[[mapping$name_id]]
    n_dup <- sum(duplicated(ids))
    if (n_dup == 0L) {
      note("ok", "No duplicate patient IDs.")
    } else if (.has_col(mapping$name_fu, df)) {
      fu_vals <- df[[mapping$name_fu]]
      # Compared as values, not pasted text, so a missing ID is not the ID
      # "NA" (review A06).
      n_dup_pairs <- sum(duplicated(data.frame(ids, fu_vals)))
      n_tp <- length(unique(fu_vals))
      if (n_dup_pairs == 0L) {
        note("ok", "Repeated patient IDs detected across ", n_tp,
             " timepoints \u2014 this is expected for longitudinal data. ",
             "All ID\u2013timepoint combinations are unique.")
      } else {
        note("warning", n_dup_pairs, " ID\u2013timepoint combinations are ",
             "duplicated. Check for duplicate records: the paired analyses ",
             "will not run until each patient has one record per timepoint.",
             details = .duplicate_details(df, mapping, by_fu = TRUE))
      }
    } else {
      note("warning", n_dup, " repeated patient IDs. For cross-sectional ",
           "data each row should have a unique ID. If this is longitudinal ",
           "data, select a Timepoint variable.",
           details = .duplicate_details(df, mapping, by_fu = FALSE))
    }
  }

  # A patient's group must be the same at every timepoint for the paired
  # analyses by group, which stop otherwise (.check_pairs(); review A04).
  if (.has_col(mapping$name_id, df) && .has_col(mapping$name_fu, df) &&
      .has_col(mapping$name_groupvar, df)) {
    moved <- .group_changes(df, mapping)
    if (length(moved$ids))
      note("warning", .fmt_n(length(moved$ids)), " patient(s) are not in the ",
           "same group at every timepoint. The paired analyses by group ",
           "(the PCHC tables and figures) will not run for these data; the ",
           "other analyses are unaffected.", details = moved$details)
  }

  # Follow-up levels the user did not list.
  if (.has_col(mapping$name_fu, df) && length(mapping$levels_fu)) {
    fu_chr <- as.character(df[[mapping$name_fu]])
    seen <- unique(fu_chr[!is.na(fu_chr)])
    unlisted <- setdiff(seen, as.character(mapping$levels_fu))
    if (length(unlisted)) {
      hit <- which(fu_chr %in% unlisted)
      note("warning", length(unlisted), " value(s) of \"", mapping$name_fu,
           "\" are not in the timepoint order given (",
           paste(utils::head(unlisted, 10L), collapse = ", "),
           "); rows holding them become NA.",
           details = .details(
             summary = .count_table(fu_chr[hit], "Timepoint value"),
             records = .record_keys(df, mapping, hit),
             notes = paste0("These timepoints are not in the order set on ",
                            "the Data page, so analyses by timepoint leave ",
                            "their rows out. Add them to the order there if ",
                            "they should be analysed."),
             denominator = paste0(.fmt_n(length(hit)), " of ",
                                  .fmt_n(n_rows), " rows.")))
    }
  }

  if (.has_col(mapping$name_vas, df)) {
    vas <- .vas_status(df[[mapping$name_vas]])
    n_out <- sum(vas$status == "out of range")
    n_rec <- sum(vas$status %in% c("ok", "out of range"))
    det <- .vas_details(df, mapping, vas)
    if (n_out > 0L) {
      note("warning", .fmt_n(n_out), " of ", .fmt_n(n_rec),
           " recorded VAS values (", .pct(n_out, n_rec),
           ") are outside the expected range (0\u2013100).", details = det)
    } else {
      note("ok", "VAS values within expected range (0\u2013100).",
           details = det)
    }
  }

  out <- data.frame(
    type    = vapply(found, `[[`, character(1L), "type"),
    message = vapply(found, `[[`, character(1L), "message"),
    stringsAsFactors = FALSE
  )
  # What lies behind each finding, for the app's "See details": one element
  # per row, NULL where there is nothing more to show. An attribute rather
  # than a column, so the findings are still a plain two-column table.
  attr(out, "details") <- lapply(found, `[[`, "details")

  if (!quiet) {
    for (i in seq_len(nrow(out))) {
      switch(out$type[i],
        ok      = message(out$message[i]),
        warning = warning(out$message[i], call. = FALSE),
        error   = if (isTRUE(stop_on_error)) stop(out$message[i], call. = FALSE)
                  else warning(out$message[i], call. = FALSE))
    }
  }

  invisible(out)
}

# ── Details behind the findings ───────────────────────────────────────────────
#
# Each is a list: `summary` and `records` (data frames, or NULL), `notes` and
# `emphasis` (plain text, the latter shown in bold) and `denominator` (what a
# percentage is of). They describe the data; none of them changes it. Row
# numbers are rows of the data as given, the first data row being 1.

.details <- function(summary = NULL, records = NULL, notes = NULL,
                     emphasis = NULL, denominator = NULL) {
  list(summary = summary, records = records, notes = notes,
       emphasis = emphasis, denominator = denominator)
}

.fmt_n <- function(n) format(n, big.mark = ",", scientific = FALSE, trim = TRUE)

.pct <- function(n, d) {
  if (d == 0) return("0%")
  paste0(formatC(100 * n / d, format = "f",
                 digits = if (100 * n / d < 1) 1L else 0L), "%")
}

# A value as it appears in the data: text as is, numbers without trailing
# noise, missing as "(missing)".
.as_shown <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  out <- if (is.numeric(x)) format(x, trim = TRUE, digits = 15) else as.character(x)
  out[is.na(x)] <- "(missing)"
  out
}

# Empty: NA, or a string of nothing but spaces.
.is_blank <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  is.na(x) | (is.character(x) & !nzchar(trimws(x)))
}

# The columns that say which record a row is: its row number, and its patient
# ID and timepoint where those are selected.
.record_keys <- function(df, mapping, rows) {
  out <- data.frame(Row = as.integer(rows))
  if (.has_col(mapping$name_id, df))
    out$ID <- .as_shown(df[[mapping$name_id]][rows])
  if (.has_col(mapping$name_fu, df))
    out$Timepoint <- .as_shown(df[[mapping$name_fu]][rows])
  out
}

# Counts of each distinct value, most frequent first.
.count_table <- function(values, what = "Value") {
  if (!length(values)) return(NULL)
  tab <- table(values, useNA = "no")
  out <- data.frame(names(tab), as.integer(tab), stringsAsFactors = FALSE)
  names(out) <- c(what, "Count")
  out[order(-out$Count, out[[1L]]), , drop = FALSE]
}

# Values of the EQ-5D dimensions that are not a level the instrument allows.
.invalid_level_details <- function(df, mapping, status, max_level) {
  cols <- names(status)
  long <- do.call(rbind, lapply(cols, function(col) {
    st <- status[[col]]
    hit <- which(st %in% .DIM_REJECTED)
    if (!length(hit)) return(NULL)
    data.frame(row = hit, Variable = col,
               Value = .as_shown(df[[col]][hit]), Reason = st[hit],
               num = attr(st, "value")[hit], stringsAsFactors = FALSE)
  }))
  summary <- stats::aggregate(list(Count = rep(1L, nrow(long))),
                              by = long[c("Variable", "Value", "Reason")],
                              FUN = sum)
  summary$Variable <- factor(summary$Variable, levels = cols)
  summary <- summary[order(summary$Variable, -summary$Count), , drop = FALSE]
  summary$Variable <- as.character(summary$Variable)
  rownames(summary) <- NULL

  long <- long[order(long$row, match(long$Variable, cols)), , drop = FALSE]
  records <- cbind(.record_keys(df, mapping, long$row),
                   long[c("Variable", "Value", "Reason")])
  rownames(records) <- NULL

  vals <- long$num[is.finite(long$num)]
  three <- .max_level(mapping$eq5d_version) == 3L
  emphasis <- if (three && any(vals %in% c(4, 5)))
    paste0("Please check whether your data use ",
           .eq5d_instrument(toupper(mapping$eq5d_version)), " or EQ-5D-5L.")
  notes <- c(
    paste0("Each level must be a whole number from 1 to ", max_level,
           ". The values below are not, so the analyses treat them as ",
           "missing (NA). Nothing is rounded or recoded, and the instrument ",
           "selected on the Data page is not changed."),
    if (three && any(vals %in% c(4, 5)))
      paste0("Levels 4 and 5 exist only in the EQ-5D-5L. If these data are ",
             "EQ-5D-5L, choose that version on the Data page."),
    if (any(vals == 9))
      paste0("A value of 9 is sometimes used to code a missing response. ",
             "Check your data's coding convention: the app does not assume ",
             "that 9 means missing. Like any value outside 1\u2013", max_level,
             ", it is set to NA and left out of the analyses."))
  .details(summary = summary, records = records, notes = notes,
           emphasis = emphasis,
           denominator = paste0(.fmt_n(nrow(long)), " values in ",
                                .fmt_n(length(unique(long$row))), " of ",
                                .fmt_n(nrow(df)), " rows."))
}

# Which dimensions are unusable in which rows, and why: missing in the data
# (NA or blank), text that is not a number, or a level the instrument does
# not allow (set to NA).
.missing_details <- function(df, mapping, status) {
  cols <- names(status)
  origin <- lapply(cols, function(col) {
    st <- status[[col]]
    blank <- .is_blank(df[[col]])
    ifelse(st %in% .DIM_REJECTED, "invalid",
      ifelse(st == "missing" & blank, "missing",
        ifelse(st == "missing", "text", "ok")))
  })
  names(origin) <- cols
  n <- nrow(df)
  bad <- Reduce(`|`, lapply(origin, function(o) o != "ok"))
  rows <- which(bad)

  summary <- data.frame(
    Variable = cols,
    `Missing in the data` = vapply(origin, function(o) sum(o == "missing"), 0L),
    `Not a number` = vapply(origin, function(o) sum(o == "text"), 0L),
    `Invalid level (set to NA)` = vapply(origin, function(o) sum(o == "invalid"), 0L),
    check.names = FALSE, stringsAsFactors = FALSE)
  summary$`Rows affected` <- rowSums(summary[, 2:4])
  summary$`% of all rows` <- vapply(summary$`Rows affected`, .pct, "", d = n)
  rownames(summary) <- NULL

  which_cols <- function(kind) vapply(rows, function(r) {
    hit <- cols[vapply(origin, function(o) o[r] == kind, NA)]
    paste(hit, collapse = ", ")
  }, character(1L))
  records <- .record_keys(df, mapping, rows)
  records$`Missing in the data` <- which_cols("missing")
  records$`Not a number` <- which_cols("text")
  records$`Invalid level` <- which_cols("invalid")

  .details(
    summary = summary, records = records,
    notes = c(
      paste0("A row is counted when any of its five dimensions cannot be ",
             "used. \"Missing in the data\" is an empty or NA value in the ",
             "data; \"Not a number\" is text such as \"n/a\"; \"Invalid ",
             "level\" is a value the instrument does not allow, which the ",
             "analyses set to NA (see the finding above)."),
      "Rows with a missing dimension are left out of the analyses that need a complete EQ-5D profile."),
    denominator = paste0("Percentages are of all ", .fmt_n(n), " rows."))
}

# Repeated patient IDs, or repeated ID-timepoint pairs.
# Patients whose group differs between timepoints, among the records of the
# selected timepoints (all of them when none are selected). A missing group at
# one timepoint and a group at another counts as a change, as in
# .check_pairs().
.group_changes <- function(df, mapping) {
  ids <- df[[mapping$name_id]]
  fu  <- as.character(df[[mapping$name_fu]])
  g   <- .as_shown(df[[mapping$name_groupvar]])
  use <- !is.na(ids) & !is.na(fu)
  if (length(mapping$levels_fu)) use <- use & fu %in% as.character(mapping$levels_fu)
  key <- as.character(ids[use])
  n_g <- tapply(g[use], key, function(x) length(unique(x)))
  moved <- names(n_g)[n_g > 1L]
  if (!length(moved)) return(list(ids = character(0L), details = NULL))
  rows <- which(use)[key %in% moved]
  summary <- data.frame(
    ID = moved,
    Groups = vapply(moved, function(i)
      paste(unique(g[use][key == i]), collapse = " / "), ""),
    stringsAsFactors = FALSE)
  rownames(summary) <- NULL
  records <- .record_keys(df, mapping, rows)
  records$Group <- g[rows]
  list(ids = moved, details = .details(
    summary = summary, records = records,
    notes = paste0("A paired analysis by group attributes each patient's ",
                   "change to their group, so each patient must stay in one ",
                   "group. Check the group variable for these patients."),
    denominator = paste0(.fmt_n(length(rows)), " of ", .fmt_n(nrow(df)),
                         " rows are involved.")))
}

.duplicate_details <- function(df, mapping, by_fu) {
  ids <- .as_shown(df[[mapping$name_id]])
  # Grouped by value, not by pasted text (review A06).
  raw <- df[[mapping$name_id]]
  key <- match(raw, unique(raw))
  if (by_fu) {
    fu <- df[[mapping$name_fu]]
    key <- paste(key, match(fu, unique(fu)), sep = ".")
  }
  dup_keys <- unique(key[duplicated(key)])
  rows <- which(key %in% dup_keys)
  grp <- split(rows, key[rows])
  summary <- data.frame(
    ID = vapply(grp, function(r) ids[r[1L]], ""),
    stringsAsFactors = FALSE)
  if (by_fu) summary$Timepoint <- vapply(grp, function(r)
    .as_shown(df[[mapping$name_fu]][r[1L]]), "")
  summary$Records <- lengths(grp)
  summary$Rows <- vapply(grp, function(r) paste(r, collapse = ", "), "")
  summary <- summary[order(-summary$Records, summary$ID), , drop = FALSE]
  rownames(summary) <- NULL
  .details(
    summary = summary, records = .record_keys(df, mapping, rows),
    notes = if (by_fu)
      "Each patient should have one record per timepoint. These pairs appear more than once."
    else
      "Each row should be a different patient unless a Timepoint variable is selected.",
    denominator = paste0(.fmt_n(length(rows)), " of ", .fmt_n(nrow(df)),
                         " rows are involved."))
}

# EQ VAS verdicts: "missing", "not a number", "out of range" or "ok".
.vas_status <- function(x) {
  num <- suppressWarnings(as.numeric(as.character(
    if (is.factor(x)) as.character(x) else x)))
  st <- ifelse(.is_blank(x), "missing",
          ifelse(is.na(num), "not a number",
            ifelse(num < 0 | num > 100, "out of range", "ok")))
  list(status = st, value = num)
}

.vas_details <- function(df, mapping, vas) {
  hit <- which(vas$status %in% c("out of range", "not a number"))
  if (!length(hit)) return(NULL)
  raw <- df[[mapping$name_vas]]
  shown <- .as_shown(raw[hit])
  summary <- stats::aggregate(list(Count = rep(1L, length(hit))),
                              by = list(Value = shown,
                                        Problem = vas$status[hit]),
                              FUN = sum)
  summary <- summary[order(-summary$Count), , drop = FALSE]
  rownames(summary) <- NULL
  records <- .record_keys(df, mapping, hit)
  records$Value <- shown
  records$Problem <- vas$status[hit]
  n_rec <- sum(vas$status %in% c("ok", "out of range"))
  .details(
    summary = summary, records = records,
    notes = c(
      paste0("The EQ VAS runs from 0 to 100. The analyses treat values ",
             "outside that range, and entries that are not numbers, as ",
             "missing (NA); nothing is recoded."),
      if (any(vas$value[hit] == 999, na.rm = TRUE))
        paste0("999 is sometimes used to code a missing EQ VAS score. Check ",
               "your data's coding convention: the app does not assume it ",
               "means missing.")),
    denominator = paste0("The percentage in the message is of the ",
                         .fmt_n(n_rec), " recorded numeric VAS values; ",
                         .fmt_n(sum(vas$status == "missing")),
                         " rows have no VAS value."))
}

# Is `name` a column of `df`?
.has_col <- function(name, df) {
  !is.null(name) && length(name) == 1L && !is.na(name) && nzchar(name) &&
    name %in% names(df)
}

# ── eq5d_age_band_midpoint ────────────────────────────────────────────────────

#' The midpoint of each age band
#'
#' Turns a column of age bands -- "30 to 39", "30-39", "65+", "under 20" -- into
#' the age at the middle of each band, for use where an analysis needs a number
#' and the data hold a band. The NICE Decision Support Unit's mapping between
#' the EQ-5D-3L and EQ-5D-5L (\code{\link{eqxwr_UK}},
#' \code{\link{eqxw_UK}}) is the case this exists for.
#'
#' @details
#' Age is recorded in completed years, so the band "30 to 39" covers the
#' continuous interval \eqn{[30, 40)} and its midpoint is 35, not 34.5. The
#' distinction decides the band a respondent is mapped into: the DSU's age
#' bands begin at 35, 45, 55 and 65, which are exactly those midpoints, so a
#' band that straddles one of those boundaries is placed in the upper one.
#'
#' No midpoint is invented for a label that cannot identify a single band. A
#' band open at the top, such as "65+", resolves to its lower bound, because
#' every age in it falls in the same band; one open at the bottom, such as
#' "under 20", or spanning a boundary, such as "50+", returns \code{NA} with a
#' warning naming the label. Supply an exact age in that case.
#'
#' Using a midpoint makes the result an approximation. Where exact ages are
#' available, use them.
#'
#' @param x A vector of age bands, or of ages. A column that is already
#'   numeric, or whose every value reads as a number, is returned as it is:
#'   there is nothing to infer.
#' @return A numeric vector of the same length as \code{x}, with two
#'   attributes: \code{banded}, \code{TRUE} when \code{x} held bands rather
#'   than numbers, and \code{straddles}, a logical vector marking the values
#'   whose band crosses one of the DSU's boundaries.
#' @seealso \code{\link{eqxwr_UK}}, \code{\link{eqxw_UK}}
#' @keywords internal
eq5d_age_band_midpoint <- function(x) {
  # The parsing lives in R/age_bands.R, shared with the NICE mapper and with
  # the script the app generates, so the three cannot disagree.
  out <- .age_band_midpoints(x)
  .warn_unresolved_ages(attr(out, "unresolved"))
  out
}

# ── eq5d_format_table ─────────────────────────────────────────────────────────

#' Format an analysis table for display
#'
#' Presents the output of an analysis function the way the Shiny app does:
#' proportions as percentages, counts with a thousands separator and no
#' decimals, other numbers to a fixed number of places, and column headings
#' with the underscores removed.
#'
#' @details
#' The analysis functions return proportions, whatever the column is called:
#' \code{Percentage} in \code{\link{eq5d_profile_top_states}} holds 0.225, and
#' so does \code{\%} in \code{\link{eq5d_profile_lfs_distribution}}. A column
#' is treated as a proportion when it is named \code{freq_*}; when it is a
#' \code{*_p} column whose matching \code{*_n} is also present, which is the
#' shape the PCHC tables produce; when it ends in "\% Total" or "\% Type"; or
#' when it is one of \code{p}, \code{cum_p}, \code{Percentage},
#' \code{Cumulative percentage}, \code{\%} or \code{Cum (\%)}.
#'
#' Every column becomes character, so the result is for reading, not for
#' further computation. The table passed in is not modified.
#'
#' @param df A data frame returned by an analysis function.
#' @param percent_digits Decimal places for the percentages.
#' @param digits Decimal places for the other numbers.
#' @param tidy_names Whether to tidy the column headings. Underscores stop
#'   Word and HTML from breaking a heading across lines, so a heading like
#'   \code{freq_Post-op_mo} forces a wide column; tidied, it reads
#'   "mo Post-op \%".
#' @return A data frame of character columns.
#' @keywords internal
eq5d_format_table <- function(df, percent_digits = 1L, digits = 2L,
                              tidy_names = TRUE) {
  if (!is.data.frame(df)) stop("`df` must be a data frame.", call. = FALSE)

  pct   <- .proportion_columns(df)
  whole <- .whole_number_columns(df, except = pct)

  for (cn in names(df)) {
    v <- df[[cn]]
    if (!is.numeric(v)) next
    df[[cn]] <- if (cn %in% pct) {
      ifelse(is.na(v), "",
             paste0(formatC(100 * v, format = "f", digits = percent_digits), "%"))
    } else if (cn %in% whole || is.integer(v)) {
      ifelse(is.na(v), "", formatC(v, format = "d", big.mark = ","))
    } else {
      ifelse(is.na(v), "", formatC(v, format = "f", digits = digits))
    }
  }
  if (isTRUE(tidy_names)) names(df) <- tidy_table_headings(names(df))
  df
}

# Which columns of an analysis table hold proportions. See eq5d_format_table().
.proportion_columns <- function(df) {
  nm <- names(df)
  paired_p <- nm[grepl("_p$", nm) & sub("_p$", "_n", nm) %in% nm]
  unique(c(
    nm[grepl("^freq_", nm)],
    paired_p,
    nm[grepl("% Total$|% Type$", nm)],
    intersect(c("p", "cum_p", "Percentage", "Cumulative percentage",
                "%", "Cum (%)"), nm)
  ))
}

# Double columns that only ever hold whole numbers. Counts come back as
# doubles from aggregate() and merge(), and "255.00" where the table means 255
# people is a distraction.
.whole_number_columns <- function(df, except = character(0L)) {
  nm <- setdiff(names(df)[vapply(df, is.double, logical(1L))], except)
  nm[vapply(nm, function(cn) {
    v <- df[[cn]][is.finite(df[[cn]])]
    length(v) > 0L && all(v == round(v))
  }, logical(1L))]
}
