# global.R — loaded once by Shiny before ui.R and server.R
# Loads packages, sources modules, and defines shared helpers.

library(shiny)
library(bslib)
library(DT)

# Source all modules
for (.f in list.files("modules", pattern = "\\.R$", full.names = TRUE)) {
  source(.f, local = FALSE)
}
rm(.f)

# ── Shared helper functions ───────────────────────────────────────────────────

#' Read an uploaded file (CSV, XLSX, or RDS)
read_uploaded_file <- function(path, ext) {
  ext <- tolower(ext)
  switch(ext,
    csv  = utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    xlsx = readxl::read_excel(path) |> as.data.frame(),
    rds  = readRDS(path),
    stop("Unsupported file type: ", ext, call. = FALSE)
  )
}

#' Suggest column mapping from a data frame's names
suggest_mapping <- function(col_names) {
  lower <- tolower(col_names)

  match_first <- function(patterns) {
    for (p in patterns) {
      idx <- grep(p, lower)
      if (length(idx) > 0L) return(col_names[idx[1L]])
    }
    NULL
  }

  list(
    mo      = match_first(c("^mo$", "mobil")),
    sc      = match_first(c("^sc$", "self.?care", "selfcare")),
    ua      = match_first(c("^ua$", "usual.?act")),
    pd      = match_first(c("^pd$", "pain")),
    ad      = match_first(c("^ad$", "anxiet")),
    name_fu = match_first(c("^fu$", "^time$", "timepoint", "follow")),
    name_groupvar = match_first(c("^group$", "groupvar", "procedure", "category", "cohort")),
    name_id = match_first(c("^id$", "patient.?id", "subject.?id", "person.?id")),
    name_vas = match_first(c("^vas$", "eq.?vas", "visual"))
  )
}

#' Apply confirmed mapping: rename columns to standard names and cast types
apply_mapping <- function(raw_data, mapping) {
  df <- raw_data

  # Rename EQ-5D dimension columns to standard names
  std_dims <- c("mo", "sc", "ua", "pd", "ad")
  for (i in seq_along(std_dims)) {
    orig <- mapping$names_eq5d[i]
    std  <- std_dims[i]
    if (!is.null(orig) && nzchar(orig) && orig != std && orig %in% names(df)) {
      names(df)[names(df) == orig] <- std
    }
  }

  # Cast dimensions to integer
  for (d in std_dims) {
    if (d %in% names(df)) df[[d]] <- suppressWarnings(as.integer(df[[d]]))
  }

  # Rename optional columns
  rename_col <- function(df, from, to) {
    if (!is.null(from) && nzchar(from) && from != to && from %in% names(df)) {
      names(df)[names(df) == from] <- to
    }
    df
  }

  df <- rename_col(df, mapping$name_fu,       "fu")
  df <- rename_col(df, mapping$name_groupvar,  "groupvar")
  df <- rename_col(df, mapping$name_id,        "id")
  df <- rename_col(df, mapping$name_vas,       "vas")
  df <- rename_col(df, mapping$name_utility,   "utility")

  if ("vas" %in% names(df)) df$vas <- suppressWarnings(as.numeric(df$vas))

  df
}

#' Build a human-readable R function call string for display
format_call <- function(fn_name, args) {
  fmt_val <- function(v) {
    if (is.null(v)) return("NULL")
    if (is.character(v) && length(v) > 1L) {
      paste0('c("', paste(v, collapse = '", "'), '")')
    } else if (is.character(v)) {
      paste0('"', v, '"')
    } else if (is.logical(v)) {
      toupper(as.character(v))
    } else {
      as.character(v)
    }
  }
  pairs <- mapply(
    function(nm, val) paste0("  ", nm, " = ", fmt_val(val)),
    names(args), args,
    SIMPLIFY = FALSE
  )
  paste0(fn_name, "(\n", paste(pairs, collapse = ",\n"), "\n)")
}

#' Save a result to rv$results
save_result <- function(rv, label, fn_call, result_type, data = NULL, plot = NULL) {
  entry <- list(
    id          = paste0("r", format(Sys.time(), "%Y%m%d%H%M%S%OS3")),
    timestamp   = Sys.time(),
    label       = label,
    fn_call     = fn_call,
    result_type = result_type,   # "table", "plot", or "both"
    data        = data,
    plot        = plot
  )
  rv$results <- c(rv$results, list(entry))
  invisible(entry$id)
}

#' Get version-aware utility method choices for the UI
#'
#' Returns a named character vector suitable for selectInput choices,
#' filtered to only include methods valid for the given EQ-5D version:
#' - 3L: direct 3L value sets + reverse crosswalk (XWR)
#' - 5L: direct 5L value sets + crosswalk (XW)
get_utility_method_choices <- function(eq5d_version) {
  if (identical(eq5d_version, "3L")) {
    c(
      "Direct (3L value set)"              = "direct",
      "Reverse crosswalk 3L\u21925L (XWR)" = "xwr"
    )
  } else {
    c(
      "Direct (5L value set)"        = "direct",
      "Crosswalk 5L\u21923L (XW)"   = "xw"
    )
  }
}

#' Get available value-set country codes for a given EQ-5D version
get_country_choices <- function(eq5d_version) {
  tryCatch({
    vs_df <- eqvs_display(version = eq5d_version, return_df = TRUE)
    codes <- vs_df[["VS_code"]]
    names_col <- if ("Name" %in% names(vs_df)) vs_df[["Name"]] else codes
    stats::setNames(codes, paste0(names_col, " (", codes, ")"))
  }, error = function(e) character(0L))
}

# ── Expose internal package helpers in Shiny scope ────────────────────────────
# These are defined in R/shiny_helpers.R as non-exported functions.

DIM_LABELS          <- eq5dsuite:::DIM_LABELS
ensure_fu           <- eq5dsuite:::ensure_fu
compute_utility_col <- eq5dsuite:::compute_utility_col
