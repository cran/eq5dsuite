# Shared validation of EQ-5D dimension values.
#
# One predicate, used wherever dimension levels enter the package: the scoring
# functions through toEQ5Dindex(), the analysis functions through
# .prep_eq5d(), and the app's data checks through eq5d_validate(). Before this
# each path coerced with as.integer() and range-checked the result, so 1.9 was
# silently scored as level 1 and a value of 11 could carry into the dimension
# beside it -- c(0, 11, 1, 1, 1) encoded as 11111 and scored as full health.
#
# Responsibility is deliberately narrow: this file decides whether a value is
# a level the instrument allows, and nothing else. Column mapping lives in
# R/eq5d_data.R and age parsing in R/age_bands.R.

# Reading values from data. These two are the only way the package and the
# app turn a column into numbers, and the generated R script contains them
# word for word (.script_parsers() in R/script_gen.R deparses them), so the
# app and the script cannot read a value differently. They use base R only
# and call nothing else in the package.
#
# A number from whatever the column holds. A factor is read through its
# labels, never its level codes: a column whose only label is "3" has level
# code 1, and reading the code turned 33333 into 11111 (review Q08). Numbers
# are returned as they are, not through text, which would round them.
.parse_number <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  if (is.factor(x)) x <- levels(x)[x]
  suppressWarnings(as.numeric(as.character(x)))
}

# An EQ-5D level: a whole number from 1 to `max_level`, as an integer, or NA.
# The check comes before any conversion to integer, so 1.9 is NA rather than
# level 1 (review Q01).
.parse_levels <- function(x, max_level = 9L) {
  v <- .parse_number(x)
  ok <- is.finite(v) & v == trunc(v) & v >= 1 & v <= max_level
  out <- rep(NA_integer_, length(v))
  out[ok] <- as.integer(v[ok])
  out
}

# The highest level of an instrument; 9 where the instrument is not known.
.max_level <- function(eq5d_version) {
  v <- if (is.null(eq5d_version)) "" else toupper(as.character(eq5d_version))
  if (identical(v, "5L")) 5L else if (v %in% c("3L", "Y3L")) 3L else 9L
}

# The instrument named in an app mapping, normalised: NULL stays NULL (the
# instrument is not known), and anything else must be "3L", "5L" or "Y3L" in
# any case. An unsupported version used to be validated as a five-level
# instrument without a word (review A17).
.mapping_version <- function(v) {
  if (is.null(v)) return(NULL)
  .norm_version(v, arg = "eq5d_version")
}

# The level values of a dimension column, as numbers.
.dim_levels <- function(v) .parse_number(v)

# A five-digit health-state code, as an integer, or NA. Read through labels
# like any other value, and whole and finite before it becomes an integer:
# as.integer() truncated 11111.9 to the state 11111, which then scored as
# full health (review Q02). One warning names what was refused. Whether the
# digits are levels of the instrument is the caller's check.
.parse_states <- function(x) {
  v <- .parse_number(x)
  bad <- !is.na(v) & !(is.finite(v) & v == trunc(v))
  if (any(bad))
    warning(sum(bad), " health state code",
            if (sum(bad) == 1L) " is" else "s are",
            " not a whole, finite number and ",
            if (sum(bad) == 1L) "returns" else "return", " NA (",
            paste(utils::head(unique(format(v[bad], digits = 10)), 3L),
                  collapse = ", "),
            "). A state is five digits, such as 11111; a fractional code is ",
            "not truncated to a state.", call. = FALSE)
  out <- rep(NA_integer_, length(v))
  ok <- !is.na(v) & !bad & abs(v) < .Machine$integer.max
  out[ok] <- as.integer(v[ok])
  out
}

# Five dimension columns, each named once. Names are compared without regard
# to case, because they are matched that way; numbers are column positions.
# Five copies of "mo" used to be accepted and mapped mobility five times
# (review Q07).
.check_dim_names <- function(dim.names, .fname = NULL) {
  pre <- if (is.null(.fname)) "" else sprintf("[%s] ", .fname)
  if (length(dim.names) != 5L)
    stop(pre, "`dim.names` must name the five EQ-5D dimension columns; got ",
         length(dim.names), ".", call. = FALSE)
  if (anyNA(dim.names) || (is.character(dim.names) && any(!nzchar(trimws(dim.names)))))
    stop(pre, "`dim.names` has a missing or empty entry.", call. = FALSE)
  key <- if (is.numeric(dim.names)) as.character(dim.names)
         else tolower(trimws(as.character(dim.names)))
  if (anyDuplicated(key))
    stop(pre, "`dim.names` names the same column more than once (",
         paste(unique(dim.names[duplicated(key)]), collapse = ", "),
         "). Each dimension needs its own column.", call. = FALSE)
  invisible(dim.names)
}

# Where the five dimensions are among `available` names: one position per
# entry of `dim.names`, which are checked first. Names are compared without
# regard to case on both sides -- toEQ5Dindex()'s matrix branch lowercased both
# but its named-vector branch only the data, eqxwr() only the data, and eq5d()
# compared exactly, so the same explicit names worked in one scorer and not in
# another (review A12). Numbers are positions. A name matching two columns
# that differ only in case is ambiguous and an error, never a guess.
.resolve_dims <- function(available, dim.names, what = "'x'", .fname = NULL) {
  .check_dim_names(dim.names, .fname)
  pre <- if (is.null(.fname)) "" else sprintf("[%s] ", .fname)
  if (is.numeric(dim.names)) {
    bad <- dim.names[!(dim.names == trunc(dim.names) & dim.names >= 1 &
                       dim.names <= length(available))]
    if (length(bad))
      stop(pre, "Column position(s) ", paste(bad, collapse = ", "),
           " not in ", what, ", which has ", length(available), " column(s).",
           call. = FALSE)
    return(as.integer(dim.names))
  }
  key  <- tolower(trimws(as.character(dim.names)))
  have <- tolower(trimws(as.character(available)))
  hits <- lapply(key, function(k) which(have == k))
  n <- lengths(hits)
  if (any(n == 0L))
    stop(pre, "Required dimension name(s) not found in ", what, ": ",
         paste(dim.names[n == 0L], collapse = ", "), ". Names are matched ",
         "without regard to case; set `dim.names` to the names in your data.",
         call. = FALSE)
  if (any(n > 1L))
    stop(pre, "Dimension name(s) ", paste(dim.names[n > 1L], collapse = ", "),
         " match more than one column of ", what, " (",
         paste(unique(available[unlist(hits[n > 1L])]), collapse = ", "),
         "), which differ only in case. Rename one of them, or give ",
         "`dim.names` as column positions.", call. = FALSE)
  unlist(hits)
}

# Per-element verdict on a dimension column.
#
# `max_level` is the highest level the instrument allows, or NULL where the
# instrument is not known. The 1..9 bound still applies in that case: a
# five-digit state code is only well defined if every dimension is a single
# non-zero digit, which is what makes the carry above impossible.
#
# Returns a character vector of the same length as `v`, one of "ok",
# "missing", "not finite", "fractional" or "out of range", with the parsed
# numbers attached as the "value" attribute.
.dim_status <- function(v, max_level = NULL) {
  num <- .dim_levels(v)
  out <- rep("ok", length(num))

  out[is.na(num)] <- "missing"
  live <- !is.na(num)
  out[live & !is.finite(num)] <- "not finite"
  live <- live & is.finite(num)
  out[live & num != trunc(num)] <- "fractional"
  live <- live & num == trunc(num)
  hi <- if (is.null(max_level)) 9L else max_level
  out[live & (num < 1 | num > hi)] <- "out of range"

  attr(out, "value") <- num
  out
}

# The verdicts that mean a value cannot be used. "missing" is not among them:
# a missing level is ordinary data and is already handled as NA everywhere.
.DIM_REJECTED <- c("not finite", "fractional", "out of range")

# One warning for a whole frame or vector of dimension values, naming what was
# wrong and what the caller can do about it. Returns invisibly, so it can be
# called unconditionally.
.warn_dim_values <- function(status, max_level = NULL, what = "value") {
  bad <- status[status %in% .DIM_REJECTED]
  if (!length(bad)) return(invisible(NULL))
  counts <- table(factor(bad, levels = .DIM_REJECTED))
  counts <- counts[counts > 0L]
  bound <- if (is.null(max_level)) "a single digit from 1 to 9"
           else paste0("a whole number from 1 to ", max_level)
  warning(
    length(bad), " EQ-5D dimension ", what,
    if (length(bad) == 1L) " is" else "s are",
    " not a level the instrument allows and ",
    if (length(bad) == 1L) "has" else "have",
    " been set to NA (",
    paste(paste0(names(counts), ": ", as.integer(counts)), collapse = "; "),
    "). Each level must be ", bound,
    ". Fractional values are not rounded: 1.9 is neither level 1 nor level 2.",
    call. = FALSE)
  invisible(NULL)
}

# Validate a matrix or data frame of dimension columns and return it as
# integers, with every rejected value set to NA and one warning describing
# them all. Valid values are returned unchanged.
#
# `rows` = TRUE propagates a rejection across the whole row, which is what the
# scoring path wants: a state with one unusable dimension is not a state.
.clean_dim_matrix <- function(x, max_level = NULL, rows = FALSE,
                              warn = TRUE, what = "value") {
  cols <- if (is.data.frame(x)) as.list(x) else
          lapply(seq_len(ncol(x)), function(j) x[, j])
  status <- lapply(cols, .dim_status, max_level = max_level)
  vals   <- lapply(status, attr, "value")

  if (isTRUE(warn)) .warn_dim_values(unlist(status, use.names = FALSE),
                                     max_level = max_level, what = what)

  for (j in seq_along(vals))
    vals[[j]][status[[j]] %in% .DIM_REJECTED] <- NA_real_

  m <- matrix(as.integer(unlist(vals, use.names = FALSE)),
              nrow = length(vals[[1L]]), ncol = length(vals),
              dimnames = list(NULL, if (is.data.frame(x)) names(x) else colnames(x)))

  if (isTRUE(rows) && anyNA(m)) m[rowSums(is.na(m)) > 0L, ] <- NA_integer_
  m
}

# Parse 5-digit state codes and set to NA, with one warning, every code that is
# not a state of the instrument: a level above `top` (mo = 4, or a
# missing-value code 9, in the EQ-5D-3L) used to become NA without a word in
# the value functions, while the analysis functions report such levels.
# `instrument` names the instrument in the message. Codes that are already NA,
# or that .parse_states() rejects (with its own warning), are left alone.
.validate_states <- function(x, top, instrument) {
  x <- .parse_states(x)
  pattern <- paste0("^[1-", top, "]{5}$")
  outside <- !is.na(x) & !grepl(pattern, x)
  if (any(outside)) {
    shown <- utils::head(unique(x[outside]), 5L)
    warning(sum(outside), " health state(s) are not valid ", instrument,
            " states and have been set to NA: each of the five digits must be ",
            "from 1 to ", top, " (e.g. ", paste(shown, collapse = ", "), ").",
            call. = FALSE)
    x[outside] <- NA
  }
  x
}
