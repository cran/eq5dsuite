# Internal helpers used by the package's Shiny application.
# None of these functions are part of the public API.

# ── EQ-5D full dimension labels ───────────────────────────────────────────────

DIM_LABELS <- c(
  mo = "Mobility",
  sc = "Self care",
  ua = "Usual activities",
  pd = "Pain/Discomfort",
  ad = "Anxiety/Depression"
)

# ── ensure_fu ─────────────────────────────────────────────────────────────────

#' Ensure a follow-up column exists (adding a synthetic one when absent)
#'
#' Package functions such as \code{table_2_1} and \code{table_3_1} internally
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
#' @param df A data frame as returned by \code{table_1_1_1} etc.
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
