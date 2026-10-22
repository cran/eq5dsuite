# Parsing age bands into an age the NICE DSU mapping can use.
#
# Review finding F15. The old parser took any single number in a label as a
# lower bound and added nine, so "under 20" and "<20" both became the band
# 20-29 and returned a midpoint of 25 -- an age above the band the label
# describes, not inside it. A bare number mixed in with bands came back as
# age + 0.5, because sub() returns its input unchanged when the pattern does
# not match and the upper bound parsed as the number itself.
#
# The rule here is the one the maintainer set: do not invent a midpoint. A
# label is resolved only when it identifies a single mapping category; where
# it does not, the value is NA and the caller is told which label was the
# problem and that an exact age resolves it.
#
# Responsibility is narrow: this file turns labels into ages. Dimension
# validation lives in R/validate_dims.R and column mapping in R/eq5d_data.R.

# Age as a number, reading a factor through its labels. as.numeric() on a
# factor returns its level codes, so factor("30") came back as 1 -- below the
# mapping's lower limit, hence NA where numeric 30 worked.
.age_as_numeric <- function(x) .parse_number(x)

# One age label -> list(lo, hi, kind), where kind is one of "number",
# "closed", "upper_open", "lower_open" or "unparsed".
#
# `hi` is the last age in the band, so "30 to 39" is lo = 30, hi = 39.
.age_band_bounds <- function(label) {
  s <- tolower(trimws(label))
  if (is.na(s) || !nzchar(s)) return(list(lo = NA_real_, hi = NA_real_, kind = "unparsed"))

  nums <- suppressWarnings(as.numeric(
    regmatches(s, gregexpr("[0-9]+(\\.[0-9]+)?", s))[[1L]]))
  nums <- nums[!is.na(nums)]

  # A bare number is an age, not a band.
  if (grepl("^[0-9]+(\\.[0-9]+)?$", s))
    return(list(lo = nums[1L], hi = nums[1L], kind = "number"))

  if (length(nums) >= 2L)
    return(list(lo = min(nums[1:2]), hi = max(nums[1:2]), kind = "closed"))

  if (length(nums) == 1L) {
    # One bound only. Which end it is decides everything, so it is read from
    # the label rather than assumed.
    upper_open <- grepl("\\+|and over|and above|or over|or more|older|>=|>", s)
    lower_open <- grepl("under|below|less than|younger|and under|or under|<=|<", s)
    if (upper_open && !lower_open)
      return(list(lo = nums[1L], hi = Inf, kind = "upper_open"))
    if (lower_open && !upper_open)
      return(list(lo = -Inf, hi = nums[1L], kind = "lower_open"))
  }
  list(lo = NA_real_, hi = NA_real_, kind = "unparsed")
}

# Does [lo, hi] fall inside a single NICE age band?
#
# The bands start at .NICE_AGE_BREAKS and the last is open at the top, so an
# interval resolves when no break lies strictly inside it and its lower bound
# is at or above the first break.
.age_band_resolves <- function(lo, hi, breaks = .NICE_AGE_BREAKS) {
  if (is.na(lo) || is.na(hi)) return(FALSE)
  if (lo < breaks[1L]) return(FALSE)          # includes ages the mapping excludes
  inner <- breaks[-1L]
  !any(inner > lo & inner <= hi)
}

# The age to use for each element of an age column.
#
# Returns a numeric vector with the attributes `banded` (TRUE when the column
# held labels rather than numbers), `straddles` (bands crossing a DSU
# boundary, kept for the caller's warning) and `unresolved` (labels that
# could not identify one mapping category).
.age_band_midpoints <- function(x, breaks = .NICE_AGE_BREAKS) {
  n <- length(x)
  num <- .age_as_numeric(x)
  chr <- trimws(as.character(x))

  # A column that is already numeric, or whose every value reads as a number,
  # is returned as it is: there is nothing to infer.
  if (is.numeric(x) || all(is.na(chr) | !is.na(num)))
    return(structure(num, banded = FALSE,
                     straddles = rep(FALSE, n), unresolved = character(0L)))

  out <- rep(NA_real_, n)
  straddles <- rep(FALSE, n)
  unresolved <- character(0L)

  for (i in seq_len(n)) {
    if (is.na(chr[i])) next
    b <- .age_band_bounds(chr[i])

    if (identical(b$kind, "number")) {
      out[i] <- b$lo                           # an exact age, used as given
      next
    }
    if (identical(b$kind, "closed")) {
      # Age is in completed years, so "30 to 39" covers [30, 40).
      out[i] <- (b$lo + b$hi + 1) / 2
      inner <- breaks[-1L]
      straddles[i] <- any(inner > b$lo & inner <= b$hi)
      next
    }
    if (identical(b$kind, "upper_open") && .age_band_resolves(b$lo, b$hi, breaks)) {
      # No upper bound, but every age in the band maps to the same category,
      # so the lower bound identifies it. No midpoint is invented.
      out[i] <- b$lo
      next
    }
    unresolved <- c(unresolved, chr[i])
  }

  structure(out, banded = TRUE, straddles = straddles,
            unresolved = unique(unresolved))
}

# One warning for the labels that could not be resolved.
.warn_unresolved_ages <- function(unresolved, .fname = NULL) {
  if (!length(unresolved)) return(invisible(NULL))
  warning(
    if (is.null(.fname)) "" else paste0("[", .fname, "] "),
    length(unresolved), " age label(s) do not identify a single NICE age ",
    "band and return NA: ",
    paste0("\"", utils::head(unresolved, 5L), "\"", collapse = ", "),
    if (length(unresolved) > 5L) ", ..." else "",
    ". A band open at the bottom (\"under 20\") or spanning a band boundary ",
    "(\"50+\") could be in either category, and no midpoint is invented for ",
    "it. Supply an exact age, or a closed band such as \"30 to 39\".",
    call. = FALSE)
  invisible(NULL)
}
