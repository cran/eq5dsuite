# -----------------------------------------------------------------------------
# Shared internals for the NICE DSU mappings used by eqxw_UK() and eqxwr_UK().
#
# Both functions do the same work in opposite directions: resolve the input to
# health states or to EQ-5D values, band the ages, validate sex, then either
# look the mapped value up directly or, for a value ("score") input, take an
# Epanechnikov kernel-weighted mean of the mapped values within the
# respondent's age band and sex. Everything below is internal.
#
# inst/COPYRIGHTS records where the lookup tables come from, what was kept
# from them, and the citations the NICE DSU asks for.
# -----------------------------------------------------------------------------

# Direction metadata. `source` is the instrument the input is measured on.
.NICE_DIRECTIONS <- list(
  "5Lto3L" = list(source = "5L", target = "3L", max_level = 5L),
  "3Lto5L" = list(source = "3L", target = "5L", max_level = 3L)
)

# Lower bounds of the DSU age bands. Band 1 starts at 16: the DSU's R command
# admits ages from 16 although its table label reads "18-34", and its Stata
# help defines band 1 as "< 35".
.NICE_AGE_BREAKS <- c(16, 35, 45, 55, 65)

# DSU-recommended bandwidths for mapping mean (aggregate) values.
.NICE_BW_THRESHOLD <- 0.6
.NICE_BW_ABOVE     <- 0.1
.NICE_BW_AT_OR_BELOW <- 0.4

# The DSU treats a bandwidth of zero as this, giving an effectively exact match.
.NICE_BW_ZERO <- 1e-6


# Fetch a direction's lookup table from the package environment.
.nice_table <- function(direction, .fname) {
  pkgenv <- getOption("eq.env")
  if (is.null(pkgenv) || is.null(pkgenv$crosswalk_NICE[[direction]]))
    stop("Missing NICE crosswalk data in `options(eq.env)`. Please set the environment.",
         call. = FALSE)
  pkgenv$crosswalk_NICE[[direction]]
}


# Five dimension columns, in order, under the canonical names the encoder
# expects. The columns are assumed to be in the conventional order already:
# the caller has either matched them by name or supplied exactly five.
.as_canonical_dims <- function(x) {
  m <- as.matrix(as.data.frame(x))
  colnames(m) <- c("mo", "sc", "ua", "pd", "ad")
  m
}


# Age in years -> band index 1-5, or NA.
#
# Ages below 16 are outside the DSU's estimation sample and return NA with a
# warning. There is no upper limit. Values 1-5 are ordinary ages here and are
# never silently read as band numbers, which is what the DSU's own R command
# does.
.nice_age_band <- function(age, .fname) {
  # Through the labels: as.numeric() on a factor returns level codes, so
  # factor("30") arrived as 1, below the mapping's lower limit, and returned
  # NA where numeric 30 and character "30" both worked.
  age <- .age_as_numeric(age)
  band <- rep(NA_integer_, length(age))

  # An age must be a finite number. Inf passed the check for "16 or over"
  # and was mapped as if in the oldest band (review Q03). There is still no
  # upper limit: none is defined by the DSU, and none is invented here.
  not_finite <- !is.na(age) & !is.finite(age)
  if (any(not_finite))
    warning(sprintf(
      "[%s] %d age(s) are not a finite number (%s) and return NA.",
      .fname, sum(not_finite),
      paste(utils::head(unique(age[not_finite]), 3L), collapse = ", ")),
      call. = FALSE)

  ok <- is.finite(age) & age >= .NICE_AGE_BREAKS[1L]
  band[ok] <- findInterval(age[ok], .NICE_AGE_BREAKS)

  too_young <- is.finite(age) & age < .NICE_AGE_BREAKS[1L]
  if (any(too_young))
    warning(sprintf(
      "[%s] %d age(s) below %d; the NICE mapping is not defined for them and they return NA.",
      .fname, sum(too_young), .NICE_AGE_BREAKS[1L]), call. = FALSE)

  band
}


# Sex -> 1 (male) / 0 (female), or NA. TRUE/FALSE are accepted.
.nice_male <- function(male, .fname) {
  if (is.logical(male)) return(as.integer(male))

  # A factor must go via its labels: as.numeric() on a factor returns the level
  # index, so factor("m") would otherwise silently become 1, i.e. "male".
  if (is.factor(male)) male <- as.character(male)

  m <- suppressWarnings(as.numeric(male))
  out <- rep(NA_integer_, length(male))
  out[!is.na(m) & m == 1] <- 1L
  out[!is.na(m) & m == 0] <- 0L

  # "Bad" means supplied but unusable. A value that was already NA is missing,
  # not invalid, and passes through silently.
  bad <- !is.na(male) & is.na(out)
  if (any(bad))
    warning(sprintf(
      "[%s] %d value(s) of `male` are neither 0 nor 1 and return NA: %s.",
      .fname, sum(bad),
      paste(utils::head(unique(male[bad]), 5L), collapse = ", ")), call. = FALSE)

  out
}


# Resolve `x`, `age` and `male` to three equal-length vectors.
#
# `x` may be a vector of state indexes or values, or a matrix/data.frame of
# dimension columns. When `x` is a data.frame, `age` and `male` may name
# columns in it or be supplied as vectors.
.nice_inputs <- function(x, age, male, dim.names, max_level, .fname) {

  pick <- function(v, nm) {
    if (is.character(v) && length(v) == 1L && is.data.frame(x) && v %in% names(x))
      return(x[[v]])
    v
  }

  is_2d <- length(dim(x)) == 2L

  if (is_2d && is.numeric(dim.names)) {
    # Column positions.
    bad <- !(dim.names == trunc(dim.names) & dim.names >= 1 & dim.names <= ncol(x))
    if (any(bad))
      stop(sprintf("[%s] `dim.names` position(s) %s are not columns of `x`, which has %d.",
                   .fname, paste(dim.names[bad], collapse = ", "), ncol(x)),
           call. = FALSE)
    age  <- pick(age,  "age")
    male <- pick(male, "male")
    dims <- as.data.frame(x)[, dim.names, drop = FALSE]
    state <- suppressMessages(toEQ5Dindex(.as_canonical_dims(dims), quiet = TRUE))
    score <- NULL
  } else if (is_2d) {
    cn <- tolower(colnames(x))
    has_dims <- !is.null(cn) && all(tolower(dim.names) %in% cn)

    if (has_dims) {
      age  <- pick(age,  "age")
      male <- pick(male, "male")
      dims <- as.data.frame(x)[, match(tolower(dim.names), cn), drop = FALSE]
      # Canonical names before encoding. The columns were selected in the
      # right order but kept the caller's names, and toEQ5Dindex() looks for
      # mo/sc/ua/pd/ad, so the documented dim.names argument could not work:
      # it failed with "Required dimension column(s) not found in 'x'".
      state <- suppressMessages(toEQ5Dindex(.as_canonical_dims(dims), quiet = TRUE))
      score <- NULL
    } else if (ncol(x) == 5L) {
      age  <- pick(age,  "age")
      male <- pick(male, "male")
      # Five columns in the conventional order, whatever they are called.
      state <- suppressMessages(toEQ5Dindex(.as_canonical_dims(x), quiet = TRUE))
      score <- NULL
    } else {
      stop(sprintf(
        "[%s] `x` has %d column(s); expected the five dimension columns %s.",
        .fname, ncol(x), paste(dim.names, collapse = ", ")), call. = FALSE)
    }
  } else {
    v <- x
    chr <- trimws(as.character(v))
    # A health state is a 5-digit code whose digits are all valid levels for
    # the source instrument. Anything else is treated as an EQ-5D value.
    pat <- sprintf("^[1-%d]{5}$", max_level)
    looks_state <- !is.na(chr) & grepl(pat, chr)
    # The mode used to be chosen with all(looks_state | is.na(chr)), so one
    # unusable record -- 99999 in a column of states -- reinterpreted the
    # entire vector as aggregate EQ-5D values and every respondent came back
    # NA, with a message about the bandwidth. A value can never look like a
    # five-digit state (EQ-5D values do not exceed 1), so the presence of any
    # state is enough to settle the mode; records that are not states are
    # then invalid states, reported as such, and the rest keep their values.
    if (any(looks_state) || all(is.na(chr))) {
      state <- rep(NA_integer_, length(chr))
      state[looks_state] <- as.integer(chr[looks_state])
      unusable <- !looks_state & !is.na(chr)
      if (any(unusable))
        warning(sprintf(
          "[%s] %d of %d record(s) are not an EQ-5D-%s health state and return NA: %s. The rest are unaffected.",
          .fname, sum(unusable), length(chr), if (max_level == 3L) "3L" else "5L",
          paste(utils::head(unique(chr[unusable]), 5L), collapse = ", ")),
          call. = FALSE)
      score <- NULL
    } else {
      # Values through their labels: as.numeric() on a factor returned its
      # level codes, so factor(c(.2, .8)) was mapped as 1 and 2 (review Q06).
      # An entry that is not a finite number is NA, and says so.
      state <- NULL
      score <- .parse_number(v)
      unusable <- !is.na(chr) & nzchar(chr) & !is.finite(score)
      if (any(unusable))
        warning(sprintf(
          "[%s] %d value(s) are not a number and return NA: %s.",
          .fname, sum(unusable),
          paste(utils::head(unique(chr[unusable]), 5L), collapse = ", ")),
          call. = FALSE)
      score[!is.finite(score)] <- NA_real_
    }
  }

  n <- if (is.null(state)) length(score) else length(state)
  if (length(age)  == 1L) age  <- rep(age,  n)
  if (length(male) == 1L) male <- rep(male, n)
  if (length(age) != n || length(male) != n)
    stop(sprintf("[%s] `age` and `male` must be length 1 or %d; got %d and %d.",
                 .fname, n, length(age), length(male)), call. = FALSE)

  list(state = state, score = score, age = age, male = male, n = n)
}


# Resolve the bandwidth to one value per row.
#
# Numeric values are recycled. The string "default" selects the DSU's
# recommendation for mean (aggregate) values: 0.1 above 0.6, 0.4 at or below.
# Zero becomes 1e-6, as in both DSU implementations.
.nice_bwidth <- function(bwidth, score, n, .fname) {
  if (is.character(bwidth)) {
    if (!identical(tolower(bwidth[1L]), "default"))
      stop(sprintf("[%s] `bwidth` must be numeric or \"default\"; got \"%s\".",
                   .fname, bwidth[1L]), call. = FALSE)
    bw <- ifelse(!is.na(score) & score > .NICE_BW_THRESHOLD,
                 .NICE_BW_ABOVE, .NICE_BW_AT_OR_BELOW)
  } else {
    bw <- suppressWarnings(as.numeric(bwidth))
    if (length(bw) == 1L) bw <- rep(bw, n)
    if (length(bw) != n)
      stop(sprintf("[%s] `bwidth` must be length 1 or %d; got %d.",
                   .fname, n, length(bw)), call. = FALSE)
    # A bandwidth is a finite, non-negative number. Inf weighted every state
    # equally and returned a plausible-looking mean for any input, an
    # invalid state code included; a negative one left the rows unmapped
    # without saying why (review AI07).
    bad <- !is.finite(bw) | bw < 0
    if (any(bad))
      stop(sprintf(paste0("[%s] `bwidth` must be a finite number of at least ",
                          "0, or \"default\"; got %s."), .fname,
                   paste(utils::head(unique(format(bw[bad])), 3L), collapse = ", ")),
           call. = FALSE)
  }
  bw[!is.na(bw) & bw == 0] <- .NICE_BW_ZERO
  bw
}


# Epanechnikov weights: 1 - (d/bw)^2, truncated to 0 outside the bandwidth.
.nice_epan <- function(values, target, bwidth) {
  d <- (values - target) / bwidth
  ifelse(abs(d) >= 1, 0, 1 - d^2)
}


# The worker shared by eqxw_UK() and eqxwr_UK().
.eqxw_NICE <- function(x, age, male, direction,
                       dim.names = c("mo", "sc", "ua", "pd", "ad"),
                       bwidth = 0, .fname = "eqxw_NICE") {

  .check_dim_names(dim.names, .fname)

  meta <- .NICE_DIRECTIONS[[direction]]
  cw   <- .nice_table(direction, .fname)

  inp  <- .nice_inputs(x, age, male, dim.names, meta$max_level, .fname)
  band <- .nice_age_band(inp$age,  .fname)
  sex  <- .nice_male(inp$male, .fname)

  out <- rep(NA_real_, inp$n)

  if (!is.null(inp$state)) {
    # ---- health state input: direct lookup, order preserved by match() ------
    valid <- !is.na(inp$state) & !is.na(band) & !is.na(sex)
    idx <- match(paste(inp$state[valid], band[valid], sex[valid], sep = "|"),
                 paste(cw$state,          cw$age,     cw$male,    sep = "|"))
    out[valid] <- cw$value_to[idx]

    unmatched <- valid & is.na(out)
    if (any(unmatched))
      warning(sprintf(
        "[%s] %d health state(s) not found in the mapping table and return NA.",
        .fname, sum(unmatched)), call. = FALSE)

  } else {
    # ---- value ("score") input: Epanechnikov kernel within age band and sex --
    bw    <- .nice_bwidth(bwidth, inp$score, inp$n, .fname)
    valid <- !is.na(inp$score) & !is.na(band) & !is.na(sex) & !is.na(bw) & bw > 0

    # One kernel per distinct (band, sex) cell, so the table is subset once
    # per cell rather than once per row.
    cell <- paste(band, sex, sep = "|")
    cw_cell <- split(seq_len(nrow(cw)), paste(cw$age, cw$male, sep = "|"))

    for (k in unique(cell[valid])) {
      rows <- which(valid & cell == k)
      sub  <- cw_cell[[k]]
      if (is.null(sub)) next
      from <- cw$value_from[sub]
      to   <- cw$value_to[sub]
      out[rows] <- vapply(rows, function(i) {
        w <- .nice_epan(from, inp$score[i], bw[i])
        if (all(w == 0)) NA_real_ else sum(w * to) / sum(w)
      }, numeric(1L))
    }

    unmapped <- valid & is.na(out)
    if (any(unmapped))
      message(sprintf(
        "[%s] %d value(s) had no EQ-5D value within the bandwidth and return NA; consider increasing `bwidth`.",
        .fname, sum(unmapped)))
  }

  out
}
