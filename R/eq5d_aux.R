#' Replace NULL names with default values
#'
#' This function takes in a list of parameters, which would be column names of the input data frame, and checks if they are null. Any nulls are replaced with default values, and the updated list of parameters is returned.
#'
#' @param df a data frame; only used/supplied if levels_fu needs to be defined
#' @param ... a list of parameters consisting of any/all of `names_eq5d`, `name_fu`, `levels_fu`, `eq5d_version`, `name_vas`, and `name_utility`.
#' @return a list of parameters with null entries replaced with default values.
#' 
#' @keywords internal
.get_names <- function(df = NULL, ...) {
  
  # read off input parameters and their supplied names
  args <- list(...)
  names_list <- names(args)
  
  # check names_eq5d
  if ("names_eq5d" %in% names_list) {
    names_eq5d <- args$names_eq5d
    if (is.null(names_eq5d)) {
      message("Argument `names_eq5d` not supplied. Default column names will be used: mo, sc, ua, pd, ad")
      names_eq5d <- c("mo", "sc", "ua", "pd", "ad")
    }
    args[["names_eq5d"]] <- names_eq5d
  }
  # check name_fu
  # if name_fu is specified, so must be levels_fu
  if ("name_fu" %in% names_list) {
    name_fu <- args$name_fu
    levels_fu <- args$levels_fu
    if (is.null(name_fu)) {
      message("Argument `name_fu` not supplied. Default column name will be used: fu")
      name_fu <- "fu"
    }
    args[["name_fu"]] <- name_fu
    # check also levels of fu
    if (is.null(levels_fu)) {
      message("No ordering of time supplied. The time variable will be factorised according to the order in the data frame.")
      levels_fu <- unique(df[[name_fu]])
    }
    args[["levels_fu"]] <- levels_fu
  }
  # check eq5d_version
  if ("eq5d_version" %in% names_list) {
    eq5d_version <- args$eq5d_version
    if (is.null(eq5d_version)) {
      # The default matters more than a message suggests. EQ-5D-3L levels are a
      # subset of the EQ-5D-5L levels, so 3L responses valued on a 5L value set
      # look perfectly in range and come back with a different number: state
      # 33333 is -0.594 as 3L and 0.604 as 5L. The bundled example_data is 3L,
      # so the default disagrees with the package's own example dataset.
      hint <- ""
      nm <- args$names_eq5d
      if (!is.null(df) && !is.null(nm) && all(nm %in% names(df))) {
        lv <- suppressWarnings(
          as.integer(as.matrix(as.data.frame(df)[, nm, drop = FALSE])))
        # Only values that are EQ-5D levels at all carry information here.
        # Anything else -- a missing-data code such as the 9 in example_data --
        # is coerced to NA by .prep_eq5d() whichever instrument this is.
        lv <- lv[!is.na(lv) & lv %in% 1:5]
        if (length(lv) && all(lv %in% 1:3))
          hint <- paste0(" All observed levels are between 1 and 3, so this ",
                         "may be EQ-5D-3L data.")
      }
      warning("No EQ-5D version was provided; the EQ-5D-5L is assumed.", hint,
              " Specify the instrument explicitly, for example ",
              "eq5d_version = \"3L\".", call. = FALSE)
      eq5d_version <- "5L"
    }
    # Canonicalise the case here, so that every downstream branch on the
    # version -- the level-range check in .prep_eq5d(), the Level Frequency
    # Score in .get_lfs(), the worst-state label, the "any problems" row label
    # -- sees the same upper-case form. See .norm_version().
    args[["eq5d_version"]] <- .norm_version(eq5d_version, arg = "eq5d_version")
  }
  # check name_vas
  if ("name_vas" %in% names_list) {
    name_vas <- args$name_vas
    if (is.null(name_vas)) {
      message("Argument `name_vas` not supplied. Default column name will be used: vas")
      name_vas <- "vas"
    }
    args[["name_vas"]] <- name_vas
  }
  # check name_utility
  if ("name_utility" %in% names_list) {
    name_utility <- args$name_utility
    if (is.null(name_utility)) {
      message("Argument `name_utility` not supplied. Default column name will be used: utility")
      name_utility <- "utility"
    }
    args[["name_utility"]] <- name_utility
  }
  
  return(args)
}

#' Calculate the Level Frequency Score (LFS)
#'
#' This function calculates the Level Frequency Score (LFS) for a given EQ-5D state and a specified version of EQ-5D.
#' If at least one domain contains a missing entry, the whole LFS is set to be NA.
#'
#' @param s A character vector representing the EQ-5D state, e.g. 11123.
#' @param eq5d_version A character string specifying the version of EQ-5D:
#'   "3L", "5L" or "Y3L". Matching is case-insensitive.
#' @return A character vector representing the calculated LFS.
#' 
#' @keywords internal
.get_lfs <- function(s, eq5d_version) {

  # "5l" must select the five-level branch just as "5L" does. Without this the
  # level-4 and level-5 counts were dropped and a three-digit LFS was returned
  # for a five-level instrument.
  eq5d_version <- .norm_version(eq5d_version, arg = "eq5d_version")

  # count occurrences of each digit; nchar(s) - nchar(gsub(...)) preserves NAs
  cnt <- function(s, ch) nchar(s) - nchar(gsub(ch, "", s, fixed = TRUE))
  # for any eq5d version need to count 1s, 2s and 3s
  lfs <- paste0(cnt(s, "1"), cnt(s, "2"), cnt(s, "3"))
  lfs[is.na(s)] <- NA_character_
  # if 5L, also add count of 4s and 5s
  if (eq5d_version == "5L") {
    lfs <- paste0(lfs, cnt(s, "4"), cnt(s, "5"))
    lfs[is.na(s)] <- NA_character_
  }
  
  return(lfs)
}

#' Add utility values to a data frame
#'
#' This function adds utility values to a data frame based on a specified version of EQ-5D and a country name.
#'
#' @param df A data frame containing the state data. The state must be included in the data frame as a character vector under the column named `state`.
#' @param eq5d_version A character string specifying the version of EQ-5D:
#'   "3L", "5L" or "Y3L". Matching is case-insensitive.
#' @param country A country code or value set code identifying the value set
#'   to use, as listed by \code{eqvs_display()}. Matching is case-insensitive.
#'   For countries with more than one value set (for example Germany, with
#'   \code{"DE_TTO"} and \code{"DE_VAS"}), the value set code must be
#'   given; supplying the country code alone raises an error listing the
#'   available codes.
#' @return A data frame with an additional column named `utility` containing the calculated utility values. If the input country name is not found in the country_codes dataset, a list of available codes is printed, and subsequently an error message is displayed and the function stops.
#' 
#' @keywords internal
.add_utility <- function(df, eq5d_version, country) {
  
  pkgenv <- getOption("eq.env")
  
  # check whether the valuation for this country code exists
  country_code <- .fixCountries(countries = country, EQvariant = eq5d_version)
  if (is.na(country_code)) {
    message('No valid countries listed. These value sets are currently available.')
    eqvs_display(version = eq5d_version)
    stop("No value set was found for country '", country, "'.", call. = FALSE)
  }

  df$utility <- eq5d(x = df$state, country = country_code, version = eq5d_version)

  return(df)
}

#' Data checking/preparation: EQ-5D variables
#'
#' This function prepares a data frame for analysis by extracting, processing, and adding columns for EQ-5D variables, including state, LSS (Level Sum Score), LFS (Level Frequency Score) and utility.
#' 
#' @param df a data frame of EQ-5D scores
#' @param names character vector of length 5 with names of EQ-5D variables in the data frame. The variables should be in an integer format.
#' @param add_state logical indicating whether the EQ-5D state should be added
#' @param add_lss logical indicating whether the LSS (Level Sum Score) should be added
#' @param add_lfs logical indicating whether the LFS (Level Frequency Score) should be added
#' @param add_utility logical indicating whether the utility should be added
#' @param eq5d_version character indicating the version of the EQ-5D
#'   questionnaire to use: "3L", "5L" or "Y3L". Matching is case-insensitive. \code{NULL}
#'   leaves the instrument unspecified, in which case levels 1 to 5 are
#'   accepted.
#' @param country A country code or value set code identifying the value set
#'   to use, as listed by \code{eqvs_display()}. Matching is case-insensitive.
#'   For countries with more than one value set (for example Germany, with
#'   \code{"DE_TTO"} and \code{"DE_VAS"}), the value set code must be
#'   given; supplying the country code alone raises an error listing the
#'   available codes.
#' @return a modified data frame with EQ-5D domain columns renamed to default names, and, if necessary, with added columns for state, LSS, LFS, and/or utility. If any of the checks fail (e.g. EQ-5D columns are not in an integer format), an error message is displayed and the function is stopping.
#' @keywords internal

.prep_eq5d <- function(df, names,
                       add_state = FALSE,
                       add_lss = FALSE,
                       add_lfs = FALSE,
                       add_utility = FALSE,
                       eq5d_version = NULL,
                       country = NULL) {

  # confirm correct length
  if (length(names) != 5)
    stop("Argument dim_names not of length 5.", call. = FALSE)

  # Validate and canonicalise the version before it is used. The range check
  # below used to run first and compare against upper-case literals, so "3l"
  # and "y3l" -- both documented as valid -- fell through to the five-level
  # branch and levels 4 and 5 were accepted for a three-level instrument. The
  # check that rejected an unknown version ran afterwards, so a bad version
  # also produced a coercion warning before erroring.
  if (!is.null(eq5d_version))
    eq5d_version <- .norm_version(eq5d_version, arg = "eq5d_version")

  # confirm numeric format
  df_eq5d <- df[, names, drop = FALSE]

  # A NULL version leaves the instrument unknown; keep the wider range, as
  # before, rather than discarding levels that may well be valid.
  n_levels <- if (!is.null(eq5d_version) && eq5d_version %in% c("3L", "Y3L")) 3L else 5L
  # Validation happens on the values as supplied, not on as.integer() of them:
  # the old order truncated 1.9 to 1, which then passed the range check and
  # was analysed as level 1. .clean_dim_matrix() rejects fractional and
  # non-finite values as well as out-of-range ones, and reports all of them in
  # one warning. See R/validate_dims.R.
  x <- .clean_dim_matrix(df_eq5d, max_level = n_levels, what = "observation")
  # Column-wise, not `df_eq5d[,] <- x`: the latter is a subscript error on a
  # zero-row data frame.
  for (k in seq_along(names)) df_eq5d[[k]] <- x[, k]

  df[, names] <- df_eq5d

  # M-9: with nothing usable left there is no analysis to do, and the
  # functions downstream failed on an empty frame with aggregate()'s "no rows
  # to aggregate" or "incorrect number of dimensions". Say what happened.
  if (nrow(df) > 0L && all(is.na(x)))
    stop("None of the ", nrow(df), " observations hold a usable EQ-5D ",
         "health state: every dimension is missing or outside the range ",
         "allowed by the ",
         if (is.null(eq5d_version)) "EQ-5D" else .eq5d_instrument(eq5d_version),
         ".\n  Check `eq5d_version`, and check that missing values are coded ",
         "as NA or as a value outside 1 to ",
         if (!is.null(eq5d_version) && eq5d_version %in% c("3L", "Y3L")) 3 else 5,
         ".", call. = FALSE)

  # rename EQ-5D columns to standard names
  std_names <- c("mo", "sc", "ua", "pd", "ad")

  # M-17: the rename is positional, so reordering or renaming among the five
  # is fine -- but a column that already carries a standard name and is not
  # one of the five would be duplicated, and `df$mo` would then silently pick
  # whichever came first. The analysis functions subset to the columns they
  # need before calling, so this only fires when one of those columns really
  # is called `mo`, `sc`, `ua`, `pd` or `ad`: a follow-up or ID column, say.
  # There is no way to keep both, since the five standard names are what every
  # function downstream reads.
  clash <- setdiff(intersect(names(df), std_names), names)
  if (length(clash))
    stop("The data frame has ", if (length(clash) > 1L) "columns" else "a column",
         " named ", paste0("\"", clash, "\"", collapse = ", "),
         " that ", if (length(clash) > 1L) "are" else "is",
         " not among the EQ-5D dimension columns given in `names_eq5d`.\n",
         "  The analysis functions use \"mo\", \"sc\", \"ua\", \"pd\" and ",
         "\"ad\" for the five dimensions, so such a column would be ",
         "overwritten.\n  Please rename it before calling.", call. = FALSE)

  names(df)[match(names, names(df))] <- std_names

  # add additional columns if required
  if (add_state)
    df$state <- ifelse(
      is.na(df$mo) | is.na(df$sc) | is.na(df$ua) | is.na(df$pd) | is.na(df$ad),
      NA_character_,
      paste0(df$mo, df$sc, df$ua, df$pd, df$ad))
  if (add_lss)
    df$lss <- df$mo + df$sc + df$ua + df$pd + df$ad
  if (add_lfs)
    df$lfs <- .get_lfs(s = df$state, eq5d_version = eq5d_version)
  if (add_utility)
    df <- .add_utility(df = df, eq5d_version = eq5d_version, country = country)

  return(df)
}

#' Data checking/preparation: follow-up variable
#' 
#' This function prepares the follow-up (FU) variable for analysis by giving it a default name (`fu`) and factorising
#'
#' @param df A data frame.
#' @param name Column name in the data frame that contains follow-up information.
#' @param levels Levels to factorise the FU variable into. Any value not among
#'   them becomes \code{NA} and is excluded from the analysis, with a warning
#'   naming the values responsible.
#' @return A data frame with the follow-up variable renamed as "fu" and factorised.
#' @keywords internal

.prep_fu <- function(df, name = NULL, levels = NULL) {

  names(df)[names(df) == name] <- "fu"

  # factor() turns anything not in `levels` into NA, and the analysis functions
  # then drop those rows. A misspelled or forgotten level used to remove
  # respondents in silence, so say what is being lost and why. Values that were
  # already NA are not the caller's mistake and are not counted.
  unlisted <- !is.na(df$fu) & !as.character(df$fu) %in% as.character(levels)
  if (any(unlisted)) {
    bad <- unique(as.character(df$fu)[unlisted])
    shown <- utils::head(bad, 10L)
    warning(sum(unlisted), " row(s) will be excluded because their follow-up ",
            "value is not one of the levels given in `levels_fu`: ",
            paste0("\"", shown, "\"", collapse = ", "),
            if (length(bad) > length(shown))
              paste0(", and ", length(bad) - length(shown), " more"),
            ".", call. = FALSE)
  }

  df$fu <- factor(df$fu, levels = levels)

  return(df = df)
}

#' Data checking/preparation: VAS variable
#' 
#' The function prepares the data for VAS (Visual Analogue Scale) analyses. 
#' 
#' @param df A data frame.
#' @param name Column name in the data frame that holds the VAS score. The EQ
#'   VAS is recorded on a 0 to 100 integer scale. Values that are not whole
#'   numbers are rounded to the nearest integer, with a warning; values outside
#'   0 to 100, and anything that is not a number, become \code{NA}, also with a
#'   warning.
#' @return A modified data frame with the VAS score renamed to "vas", held as
#'   an integer vector. The function does not stop on invalid input; it coerces
#'   and warns.
#' 
#' @keywords internal
.prep_vas <- function(df, name) {
  
  # extract data
  x <- as.vector(as.data.frame(df)[, name])

  # Work from the numeric values. The previous version did
  # `xorig <- x <- as.integer(x)`, which truncated -- a VAS of 20.5 silently
  # became 20 -- and then compared the truncated vector against itself, so
  # neither the rounding nor the out-of-range coercion could ever be counted.
  # That is why the warning below used to report `TRUE` as its count.
  #
  # A factor is converted through its labels: as.numeric() on a factor returns
  # the level codes, which for a VAS column would be silent nonsense.
  if (is.factor(x)) x <- as.character(x)
  xnum <- suppressWarnings(as.numeric(x))

  # Out of range, or not a number at all, becomes NA. The range is checked on
  # the value as recorded, before rounding: rounding first turned -0.4 and
  # 100.4 into 0 and 100, scores the Validation page had reported as out of
  # range and missing (review A13). It also runs before as.integer() so that
  # a value too large for an integer cannot raise a second, redundant
  # coercion warning from R itself.
  xnum[!is.na(xnum) & !(is.finite(xnum) & xnum >= 0 & xnum <= 100)] <- NA_real_

  # The EQ VAS is recorded as a whole number, so a value within the range that
  # is not one is rounded rather than discarded -- but not silently. round()
  # is R's usual rounding, which takes a half to the nearest even number.
  rounded <- !is.na(xnum) & xnum != round(xnum)
  if (any(rounded))
    warning(sum(rounded), " VAS value(s) were not whole numbers and have been ",
            "rounded to the nearest integer, as the EQ VAS is recorded on a ",
            "0 to 100 integer scale.", call. = FALSE)
  xnum <- round(xnum)
  v <- as.integer(xnum)

  coerced <- sum(is.na(v)) - sum(is.na(x))
  if (coerced > 0)
    warning(coerced, " VAS observation(s) were coerced to NAs as they were ",
            "not interpretable as values in the range allowed by the EQ VAS ",
            "(0 to 100).", call. = FALSE)

  df[, name] <- v

  # all checks passed; rename column
  names(df)[names(df) == name] <- "vas"

  # return value
  return(df)
}

#' Data checking/preparation: EQ-5D value variable
#'
#' Prepares a column of pre-calculated EQ-5D values for the
#' \code{eq5d_utility_*} analyses. Those functions take the values as they
#' are: nothing here recalculates them from the dimensions, and no value set
#' is involved.
#'
#' @param df A data frame.
#' @param name Column name in the data frame that holds the EQ-5D values.
#'   Anything that is not a number becomes \code{NA}, with a warning. A factor
#'   is read through its labels, not its level codes.
#' @return A modified data frame with the EQ-5D value column renamed to
#'   "utility", held as a numeric vector. The function does not stop on
#'   invalid input; it coerces and warns.
#'
#' @details
#' No range check is applied at the lower end: EQ-5D values are negative for
#' states regarded as worse than dead, and how negative depends on the value
#' set. The upper end is fixed, though -- full health is 1 by construction --
#' so values above 1 are reported as a warning. They are kept rather than
#' discarded, because the likeliest cause is the wrong column, which the user
#' should see and fix rather than have silently emptied.
#'
#' @keywords internal
.prep_utility <- function(df, name) {

  # extract data
  x <- as.vector(as.data.frame(df)[, name])

  # A factor is converted through its labels: as.numeric() on a factor returns
  # the level codes, which for a value column would be silent nonsense.
  if (is.factor(x)) x <- as.character(x)
  v <- suppressWarnings(as.numeric(x))

  coerced <- sum(is.na(v)) - sum(is.na(x))
  if (coerced > 0)
    warning(coerced, " EQ-5D value(s) were coerced to NAs as they were not ",
            "interpretable as numbers.", call. = FALSE)

  # Inf and -Inf are not EQ-5D values (NaN is already NA). Kept, -Inf made
  # the mean -Inf and the standard error NaN, and was counted as an
  # observation (review AI06). Finite negative values are valid and kept.
  nonfinite <- is.infinite(v)
  if (any(nonfinite)) {
    warning(sum(nonfinite), " EQ-5D value(s) were not finite numbers (Inf ",
            "or -Inf) and have been set to NA.", call. = FALSE)
    v[nonfinite] <- NA_real_
  }

  too_high <- sum(!is.na(v) & v > 1)
  if (too_high > 0)
    warning(too_high, " EQ-5D value(s) are greater than 1. EQ-5D values are ",
            "at most 1, for full health, so check that `name_utility` names ",
            "the column of EQ-5D values. They have been left unchanged.",
            call. = FALSE)

  df[, name] <- v

  # all checks passed; rename column
  names(df)[names(df) == name] <- "utility"

  # return value
  return(df)
}

#' Check that the columns an analysis function needs are present
#'
#' Every analysis function resolves its column arguments -- filling any left
#' \code{NULL} with a default -- and then checks that the named columns exist.
#' That check used to say only "Provided column names not in dataframe",
#' naming neither the missing column nor the argument it came from. Since the
#' defaults are supplied silently, the commonest cause was a default the caller
#' never chose: a frame with the five EQ-5D columns and nothing else fails
#' because \code{name_fu} defaulted to \code{"fu"}.
#'
#' @param df The data frame the columns must be in.
#' @param ... Column names, each named by the argument that supplied it, for
#'   example \code{.check_columns(df, names_eq5d = names_eq5d,
#'   name_fu = name_fu)}. \code{NULL} arguments are skipped, as are arguments
#'   the caller did not use.
#' @return Invisibly \code{TRUE}. Called for its side effect of stopping when a
#'   column is missing.
#' @keywords internal
.check_columns <- function(df, ...) {
  args <- list(...)
  args <- args[!vapply(args, function(a) is.null(a) || !length(a), logical(1L))]
  have <- colnames(df)

  problems <- character(0)
  for (arg in names(args)) {
    cols <- as.character(args[[arg]])
    gone <- unique(cols[!cols %in% have])
    if (length(gone))
      problems <- c(problems,
                    paste0("  ", paste0("\"", gone, "\"", collapse = ", "),
                           " (from `", arg, "`)"))
  }
  if (!length(problems)) return(invisible(TRUE))

  shown <- utils::head(have, 15L)
  stop("These columns were not found in the data frame:\n",
       paste(problems, collapse = "\n"),
       "\n  The data frame has: ",
       paste0("\"", shown, "\"", collapse = ", "),
       if (length(have) > length(shown))
         paste0(", and ", length(have) - length(shown), " more"),
       ".\n  Column arguments left NULL are filled with defaults; see the ",
       "messages above.",
       call. = FALSE)
}

#' Shannon's indices for one categorical variable
#'
#' Computes Shannon's index H', its maximum, and Shannon's evenness index J',
#' as described in chapter 4 of Devlin et al. (2020).
#'
#' \deqn{H' = -\sum_{i} p_i \log_2 p_i}
#'
#' summed over the categories that occur, where \eqn{p_i} is the proportion of
#' observations in category \eqn{i}. Categories with no observations
#' contribute nothing, since \eqn{0 \log_2 0 = 0}.
#'
#' \eqn{H'_{max} = \log_2 L}, where \eqn{L} is the number of categories the
#' instrument allows -- not the number observed. \eqn{J' = H' / H'_{max}}
#' therefore runs from 0, when every observation falls in one category, to 1,
#' when they are spread evenly over all \eqn{L}.
#'
#' @param x A vector of observed categories. \code{NA}s are dropped.
#' @param n_categories The number \eqn{L} of categories the instrument allows.
#' @return A numeric vector of three elements, \code{H}, \code{Hmax} and
#'   \code{J}. All three are \code{NA} when there is no observation to
#'   summarise.
#' @keywords internal
.shannon <- function(x, n_categories) {
  x <- x[!is.na(x)]
  if (!length(x))
    return(c(H = NA_real_, Hmax = NA_real_, J = NA_real_))

  p <- table(x) / length(x)
  p <- p[p > 0]
  h <- -sum(p * log2(p))
  hmax <- log2(n_categories)
  c(H = h, Hmax = hmax, J = if (hmax > 0) h / hmax else NA_real_)
}

# Check that records can be paired, and stop if they cannot.
#
# The paired analyses compare each respondent's consecutive records. Two
# records of one respondent at the same timepoint were compared with each
# other, as if the second were a later visit, and counted as a further change
# (review A03); a message prefixed "Warning:" was all the caller saw. They now
# stop, naming the respondents: which record is right is the analyst's call.
#
# With `by_group`, a respondent must also be in the same group at every
# timepoint. The records used to be sorted by group before time, so a
# respondent whose group changed had their visits put out of order and
# compared with the wrong one (review A04). A missing group at one timepoint
# and a group at another is a change.
#
# Only records that take part are checked: those with an ID and a timepoint
# in `levels_fu` (fu is NA for the rest after .prep_fu()).
.check_pairs <- function(df, by_group = FALSE) {
  use <- !is.na(df$id) & !is.na(df$fu)
  id <- df$id[use]
  fu <- as.character(df$fu[use])
  shown <- function(ids) {
    u <- unique(ids)
    paste0(paste0("id ", utils::head(u, 5L), collapse = ", "),
           if (length(u) > 5L) paste0(" and ", length(u) - 5L, " more") else "")
  }

  dup <- duplicated(data.frame(id = id, fu = fu, stringsAsFactors = FALSE))
  if (any(dup)) {
    ids <- id[dup]
    stop(length(unique(ids)), " respondent(s) have more than one record at the ",
         "same timepoint (", shown(ids), "). A paired analysis needs one ",
         "record per respondent and timepoint, so it cannot tell which to ",
         "compare. Remove or combine the duplicates first.", call. = FALSE)
  }

  if (isTRUE(by_group) && "groupvar" %in% names(df)) {
    g <- as.character(df$groupvar[use])
    g[is.na(g)] <- "\001NA"
    n_g <- tapply(g, as.character(id), function(x) length(unique(x)))
    moved <- names(n_g)[n_g > 1L]
    if (length(moved))
      stop(length(moved), " respondent(s) are not in the same group at every ",
           "timepoint (", shown(moved), "). A paired analysis by group ",
           "needs each respondent to stay in one group, so that their change ",
           "belongs to it; check the group variable for these respondents.",
           call. = FALSE)
  }
  invisible(TRUE)
}

#' Get the mode of a vector.
#'
#' This function calculates the mode of a numeric or character vector. 
#' If there are multiple modes, the first one is returned. 
#' The code is taken from an \href{https://www.tutorialspoint.com/r/r_mean_median_mode.htm}{R help page}.
#'
#' @param v A numeric or character vector.
#' @return The mode of `v`.
#' 
#' @keywords internal
.getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' Wrapper for the repetitive code in function_table_2_1. Data frame summary
#' 
#' This internal function summarises a data frame by grouping it based on the variables specified in the 'group_by' argument and calculates the frequency of each group. The output is used in Table 2.1
#' 
#' @param df A data frame
#' @param group_by A character vector of variables in `df` to group by. Should contain 'eq5d' and 'fu'.
#' @return A summarised data frame with groups defined by `eq5d` and `fu` variables, the count of observations in each group, and the frequency of each group.
#' @keywords internal

.summary_table_2_1 <- function(df, group_by) {

  # count occurrences per group
  counts <- aggregate(rep(1L, nrow(df)), by = df[group_by], FUN = sum)
  names(counts)[ncol(counts)] <- "n"

  # freq: n / sum(n) within each (eq5d, fu) combination
  key <- paste(counts$eq5d, counts$fu, sep = "\001")
  totals <- tapply(counts$n, key, sum)
  counts$freq <- counts$n / totals[key]

  return(counts)
}

#' Helper function for frequency of levels by dimensions tables
#' 
#' @param df Data frame with the EQ-5D and follow-up columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column. If NULL, no grouping is used, and the table reports for the total population.
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @param add_summary_problems_change If set to false, the resulting dataframe does not include a row on problems change.
#' @param eq5d_version Version of the EQ-5D instrument: "3L", "5L" or
#'   "Y3L". Matching is case-insensitive.
#' @return Summary data frame.

.freqtab<- function(df,
                    names_eq5d = NULL,
                    name_fu = NULL,
                    levels_fu = NULL,
                    eq5d_version = NULL,
                    add_summary_problems_change = TRUE) {
  
  ### data preparation ###
  
  # With no follow-up column the whole sample is one group. Its one level is
  # given too: without it, .get_names() announced "No ordering of time
  # supplied" for analyses that take no time variable at all.
  if(is.null(name_fu)) {
    df$`_all` <- "All"
    name_fu <- "_all"
    levels_fu <- "All"
  }
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, 
                     names_eq5d = names_eq5d, 
                     name_fu = name_fu, levels_fu = levels_fu,
                     eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d, name_fu)
  .check_columns(df, names_eq5d = names_eq5d, name_fu = name_fu)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d, eq5d_version = eq5d_version)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)

  ### analysis ###

  # reshape to long format (replace pivot_longer)
  levels_eq5d <- c("mo", "sc", "ua", "pd", "ad")
  df_long <- do.call(rbind, lapply(levels_eq5d, function(d) {
    data.frame(fu = df$fu, eq5d = d, value = df[[d]], stringsAsFactors = FALSE)
  }))

  # Every cell of dimension x follow-up x level is counted, including those
  # nobody reported. Counting with aggregate() returned only the cells that
  # occurred, so a level or a dimension with no problems came out as NA after
  # widening, and so did the change in the number reporting problems: one
  # respondent going from mo = 2 to mo = 1 gave NA and NA, not 0 and -1.
  #
  # The denominator is the number of non-missing responses on a dimension at
  # a follow-up. Where it is positive every count is a number, 0 when nobody
  # gave that response. Where it is zero -- nobody recorded at that follow-up,
  # or everyone missing on that dimension -- the counts are NA: the data say
  # nothing about how many had a problem, and a 0 there would read as
  # everyone having recovered.
  fu_levels    <- levels(df_long$fu)
  level_values <- seq_len(if (eq5d_version == "5L") 5L else 3L)
  df_cc <- df_long[!is.na(df_long$value) & !is.na(df_long$fu), , drop = FALSE]

  count_by <- function(keep) {
    tab <- table(factor(df_cc$eq5d[keep], levels = levels_eq5d),
                 factor(df_cc$fu[keep], levels = fu_levels))
    tab
  }
  denom <- count_by(rep(TRUE, nrow(df_cc)))
  # Long data frame of one count per (eq5d, fu), NA where there is no
  # denominator.
  as_cells <- function(tab, level) {
    n <- as.integer(tab)
    d <- as.integer(denom)
    n[d == 0] <- NA_integer_
    data.frame(eq5d = rep(levels_eq5d, times = length(fu_levels)),
               fu = rep(fu_levels, each = length(levels_eq5d)),
               n = n, freq = ifelse(d > 0, n / d, NA_real_),
               level = level, stringsAsFactors = FALSE)
  }

  # summary: individual levels
  summary_dim <- do.call(rbind, lapply(level_values, function(lv)
    as_cells(count_by(df_cc$value == lv), as.character(lv))))

  # summary: total -- the denominator itself, 0 where there is none
  summary_total <- as_cells(denom, "Total")
  summary_total$n <- as.integer(denom)
  summary_total$freq <- ifelse(summary_total$n > 0, 1, NA_real_)

  # summary: some problems
  suffix <- paste(level_values[-1L], collapse = "+")
  summary_problems <- as_cells(
    count_by(df_cc$value != 1),
    paste0("Number reporting any problems (levels ", suffix, ")"))

  # change in numbers reporting problems since the previous follow-up, in
  # the order of levels_fu. NA where either count is unavailable; the
  # relative change is NA where the previous count is 0.
  sp_sub <- summary_problems[, c("eq5d", "fu", "n")]
  sp_sub$fu_ord <- match(sp_sub$fu, as.character(levels_fu))
  sp_sub <- sp_sub[order(sp_sub$eq5d, sp_sub$fu_ord), ]
  sp_sub$fu_ord <- NULL
  sp_split <- split(sp_sub, sp_sub$eq5d)
  change_list <- lapply(sp_split, function(g) {
    n_prev <- c(NA_real_, head(g$n, -1))
    g$n <- g$n - n_prev
    g$freq <- ifelse(!is.na(n_prev) & n_prev > 0, g$n / n_prev, NA_real_)
    g
  })
  summary_problems_change <- do.call(rbind, change_list)
  summary_problems_change$level <- "Change in numbers reporting problems"
  rownames(summary_problems_change) <- NULL

  # summary: rankings
  if (add_summary_problems_change) {
    sr <- summary_problems_change[!is.na(summary_problems_change$freq),
                                  c("eq5d", "fu", "freq"), drop = FALSE]
    fu_grps <- unique(as.character(sr$fu))
    rank_list <- lapply(fu_grps, function(f) {
      g <- sr[as.character(sr$fu) == f, , drop = FALSE]
      g$n <- rank(g$freq)
      g
    })
    if (length(rank_list) > 0L) {
      summary_rank <- do.call(rbind, rank_list)
      summary_rank$freq <- NA_real_
      summary_rank$level <- "Rank of dimensions in terms of % changes"
      rownames(summary_rank) <- NULL
    } else {
      summary_rank <- NULL
    }
  } else {
    summary_rank <- NULL
  }

  # summary: missing data
  na_agg <- aggregate(
    cbind(n_miss = is.na(df_long$value), n_total = rep(1L, nrow(df_long))),
    by = list(eq5d = df_long$eq5d, fu = df_long$fu),
    FUN = sum)
  summary_na <- data.frame(
    eq5d = na_agg$eq5d,
    fu = na_agg$fu,
    n = na_agg$n_miss,
    freq = na_agg$n_miss / na_agg$n_total,
    level = "Missing data",
    stringsAsFactors = FALSE)

  # combine all summaries
  all_pieces <- list(summary_dim, summary_total, summary_problems,
                     if (add_summary_problems_change && any(!is.na(summary_problems_change$n))) summary_problems_change,
                     summary_rank, summary_na)
  all_pieces <- Filter(Negate(is.null), all_pieces)

  # ensure all pieces have the same columns: level, eq5d, fu, n, freq
  std_cols <- c("level", "eq5d", "fu", "n", "freq")
  all_pieces <- lapply(all_pieces, function(p) {
    p$eq5d <- as.character(p$eq5d)
    p$fu   <- as.character(p$fu)
    p[, std_cols, drop = FALSE]
  })
  combined <- do.call(rbind, all_pieces)

  # define row order
  row_order_levels <- c(
    sort(unique(summary_dim$level)),
    unique(summary_total$level),
    unique(summary_problems$level),
    if (add_summary_problems_change) unique(summary_problems_change$level),
    if (!is.null(summary_rank)) unique(summary_rank$level),
    unique(summary_na$level))

  # define column order: expand.grid(c("n","freq"), levels_fu, levels_eq5d)
  col_grid <- expand.grid(c("n", "freq"), as.character(levels_fu), levels_eq5d,
                          stringsAsFactors = FALSE)
  col_order <- apply(col_grid, 1, paste, collapse = "_")

  # build wide output: rows = levels, cols = n/freq per (fu, eq5d)
  all_levels <- row_order_levels
  retval <- data.frame(level = all_levels, stringsAsFactors = FALSE)

  for (cn in col_order) {
    parts_cn <- strsplit(cn, "_")[[1]]
    val_type <- parts_cn[1]                              # "n" or "freq"
    eq5d_val <- parts_cn[length(parts_cn)]               # e.g. "mo"
    fu_val   <- paste(parts_cn[2:(length(parts_cn)-1)], collapse = "_")  # fu (may contain _)

    sub <- combined[combined$eq5d == eq5d_val & combined$fu == fu_val, ,
                    drop = FALSE]
    vals <- setNames(sub[[val_type]], sub$level)
    retval[[cn]] <- vals[retval$level]
  }

  rownames(retval) <- NULL

  # return value
  return(retval)
}


# Which rows start a respondent's records, in rows sorted by respondent and
# then follow-up: the first row, any row whose ID differs from the row
# before, and every row with no ID. A record that cannot be attributed to
# anyone cannot be paired with anything -- comparing IDs with `!=` gave NA
# there, and an NA index in an assignment is skipped, so such a record used to
# be paired with the previous respondent's last one.
#
# The PCHC analyses and eq5d_profile_dimension_change_table() both use this,
# so they agree about which records form a pair: consecutive records of one
# respondent, in follow-up order.
.first_of_subject <- function(id) {
  n <- length(id)
  if (n == 0L) return(logical(0))
  prev <- c(NA, id[-n])
  out <- is.na(id) | is.na(prev) | id != prev
  out[1L] <- TRUE
  out
}

#' Wrapper to determine Paretian Classification of Health Change
#' 
#' This internal function determines Paretian Classification of Health Change (PCHC) for each combination of the variables specified in the `group_by` argument. 
#' It is used in the code for eq5d_profile_pchc_table, eq5d_profile_pchc_with_no_problems_table, eq5d_profile_dimension_change_table, and the eq5d_profile_*_by_group_plot functions.
#' An EQ-5D health state is deemed to be `better` than another if it is better on at least one dimension and is no worse on any other dimension.
#' An EQ-5D health state is deemed to be `worse` than another if it is worse in at least one dimension and is no better in any other dimension.
#' @param df A data frame with EQ-5D dimension columns (`mo`, `sc`, `ua`,
#'   `pd`, `ad`), a factorised follow-up column `fu`, and an `id` column
#'   identifying the respondent. The change score is a lag over the rows of
#'   `df`, so `df` must already be sorted with each respondent's records
#'   together and in follow-up order. Rows that start a new respondent are
#'   given a missing change score, so a respondent whose first record is not
#'   the first follow-up is excluded rather than compared against the
#'   preceding respondent. So are rows with no \code{id} and rows whose
#'   \code{fu} is missing.
#' @param level_fu_1 Value of the first (i.e. earliest) follow-up. Would normally be defined as levels_fu[1].
#' @param add_noprobs Logical value indicating whether to include a separate classification for those without problems (default is FALSE)
#' @return A data frame with PCHC value for each combination of the grouping variables. 
#' If 'add_noprobs' is TRUE, a separate classification for those without problems is also included.
#' @keywords internal

.pchc <- function(df, level_fu_1, add_noprobs = FALSE) {

  levels_eq5d <- c("mo", "sc", "ua", "pd", "ad")

  # The change score is a lag over the rows of df, so df must be sorted with
  # each respondent's records together and in follow-up order; every caller
  # does that before calling here.
  #
  # Blanking the first follow-up level is not enough on its own: a respondent
  # whose first record is a later follow-up (no baseline recorded) would
  # otherwise be differenced against the previous respondent's last record,
  # inventing a change for someone who cannot have one. `first_of_subject`
  # marks those rows so they are blanked too.
  if (!"id" %in% names(df))
    stop("[.pchc] `df` must contain an `id` column identifying the respondent.",
         call. = FALSE)

  first_of_subject <- .first_of_subject(df$id)

  # initialise positive, negative & zero difference counts
  df$better <- 0L
  df$worse  <- 0L

  for (dom in levels_eq5d) {
    dom_diff <- paste0(dom, "_diff")

    # lag shift: previous row's value minus current (dplyr::lag equivalent)
    df[[dom_diff]] <- c(NA_real_, head(df[[dom]], -1)) - df[[dom]]
    # baseline rows, any row that starts a new respondent, and any row whose
    # follow-up is not one of levels_fu (NA after .prep_fu()): diff is NA.
    # The last were warned about as excluded, but still paired.
    no_prev <- first_of_subject | is.na(df$fu) |
      as.character(df$fu) == as.character(level_fu_1)
    no_prev[is.na(no_prev)] <- TRUE
    df[[dom_diff]][no_prev] <- NA_real_

    # accumulate improvement/worsening counts (NA propagates for baseline rows)
    df$better <- df$better + (df[[dom_diff]] > 0)
    df$worse  <- df$worse  + (df[[dom_diff]] < 0)
  }

  # classify each row (NA when better/worse are NA, i.e. baseline rows)
  df$state <- ifelse(
    df$better == 0 & df$worse == 0, "No change",
    ifelse(df$better > 0 & df$worse == 0, "Improve",
      ifelse(df$worse > 0 & df$better == 0, "Worsen",
        ifelse(df$better > 0 & df$worse > 0, "Mixed change", NA_character_)
      )
    )
  )

  # separate classification for those without problems if required
  if (add_noprobs) {
    # no change & 11111 at second timepoint means 11111 at first timepoint
    noprobs <- df$mo == 1 & df$sc == 1 & df$ua == 1 & df$pd == 1 & df$ad == 1
    df$noprobs <- noprobs
    df$state_noprobs <- ifelse(!is.na(df$state) & df$state == "No change" & !is.na(noprobs) & noprobs,
                               "No problems", df$state)
  }

  return(df)
}

#' Wrapper to summarise a continuous variable by follow-up (FU) 
#' 
#' This function summarizes a continuous variable for each follow-up (FU) and calculates various statistics such as mean, standard deviation, median, mode, kurtosis, skewness, minimum, maximum, range, and number of observations. It also reports the total sample size and the number (and proportion) of missing values for each FU. 
#' The input `df` must contain an ordered FU variable and the continuous variable of interest. 
#' The name of the continuous variable must be specified using `name_v`. 
#' The wrapper is used in Table 3.1 (for VAS) or Table 4.2 (for EQ-5D utility)
#'
#' @details
#' Skewness and kurtosis are the population (biased) estimators
#' \eqn{m_3 / m_2^{3/2}} and \eqn{m_4 / m_2^2}, as returned by
#' \code{moments::skewness()} and \code{moments::kurtosis()}.
#'
#' The kurtosis is **non-excess**: a normal distribution gives 3, not 0. The
#' row is labelled "Kurtosis (non-excess)" so that the output says which
#' convention it uses. Stata's \code{summarize, detail} reports the same
#' quantity. Excel's \code{KURT()} reports *excess* kurtosis with a
#' sample-bias correction, so it is roughly 3 lower for the same data.
#'
#' @param df A data frame containing the FU and continuous variable of interest. The dataset must contain an ordered `fu` variable.
#' @param name_v A character string with the name of the continuous variable in `df` to be summarised.
#' @return Data frame with one row for each statistic and one column for each FU. 
#' @importFrom stats median quantile sd
#' @importFrom moments kurtosis
#' @keywords internal

.summary_cts_by_fu <- function(df, name_v) {

  ### prepare dataset ###

  names(df)[names(df) == name_v] <- "v"

  fu_levels <- if (is.factor(df$fu)) levels(df$fu) else sort(unique(df$fu))

  # A level listed in levels_fu but absent from the data used to be summarised
  # anyway: mean(numeric(0)) is NaN, and min()/max() are Inf and -Inf with a
  # warning of their own, so the column read NaN / Inf / -Inf and a Range of
  # -Inf. Report the empty levels once and give them NA.
  # in_level() excludes rows whose follow-up is NA -- a value not listed in
  # levels_fu, which .prep_fu() has already warned about. Without that, the
  # logical index carries NAs, and the subset picks up NA elements that inflate
  # Observations and turn every statistic NA.
  in_level <- function(f) !is.na(df$fu) & df$fu == f
  empty <- vapply(fu_levels, function(f) !any(in_level(f) & !is.na(df$v)),
                  logical(1L))
  if (any(empty))
    warning("No non-missing ", name_v, " values for follow-up level",
            if (sum(empty) > 1L) "s" else "", " ",
            paste0("\"", fu_levels[empty], "\"", collapse = ", "),
            ". Their summary statistics are NA.", call. = FALSE)

  # summarise non-NA values per fu level
  stats_list <- lapply(fu_levels, function(f) {
    v <- df$v[in_level(f) & !is.na(df$v)]
    if (!length(v))
      return(data.frame(
        fu = f,
        Mean = NA_real_, `Standard error` = NA_real_, Median = NA_real_,
        Mode = NA_real_, `Standard deviation` = NA_real_,
        `Kurtosis (non-excess)` = NA_real_,
        Skewness = NA_real_, Minimum = NA_real_, Maximum = NA_real_,
        Range = NA_real_, Observations = 0L,
        check.names = FALSE, stringsAsFactors = FALSE))
    data.frame(
      fu = f,
      Mean = mean(v),
      `Standard error` = sd(v) / sqrt(length(v)),
      Median = median(v),
      Mode = .getmode(v),
      `Standard deviation` = sd(v),
      `Kurtosis (non-excess)` = kurtosis(v),
      Skewness = skewness(v),
      Minimum = min(v),
      Maximum = max(v),
      Range = max(v) - min(v),
      Observations = length(v),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  summary <- do.call(rbind, stats_list)

  # summarise total and NA values per fu level
  total_na_list <- lapply(fu_levels, function(f) {
    v <- df$v[in_level(f)]
    miss_n <- sum(is.na(v))
    tot <- length(v)
    data.frame(
      fu = f,
      `Missing (n)` = miss_n,
      `Total sample` = tot,
      # 0 / 0 is NaN; a level with no rows has no missing percentage.
      # A percentage, 0 to 100, as its name says; it was a proportion
      # (0.33 for one missing of three, review Q10).
      `Missing (%)` = if (tot == 0L) NA_real_ else 100 * miss_n / tot,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  summary_total_na <- do.call(rbind, total_na_list)

  # combine
  combined <- merge(summary, summary_total_na, by = "fu")

  # merge() sorts its result by the `by` column, which discards the requested
  # order of the follow-up levels: with levels_fu = c("Pre-op", "Post-op") the
  # rows came back alphabetically, as Post-op then Pre-op. Those rows become
  # the columns of the returned table two lines down, so this is what decides
  # whether the caller reads baseline or follow-up first -- and nothing in the
  # output says which is which. Reindex against fu_levels, which is the order
  # the caller asked for.
  combined <- combined[match(as.character(fu_levels), as.character(combined$fu)), ,
                       drop = FALSE]

  # transpose: rows become stat names, columns become fu levels
  stat_cols <- setdiff(names(combined), "fu")
  mat <- as.matrix(combined[, stat_cols, drop = FALSE])
  rownames(mat) <- as.character(combined$fu)
  t_mat <- t(mat)

  retval <- as.data.frame(t_mat, stringsAsFactors = FALSE)
  for (cn in names(retval)) retval[[cn]] <- as.numeric(retval[[cn]])
  retval <- data.frame(name = rownames(retval), retval, check.names = FALSE,
                       stringsAsFactors = FALSE, row.names = NULL)

  return(retval)
}

# Stop, saying why, when an analysis has no records left to work on. Analyses
# that need a complete profile filter to complete ones first, and with none
# left they failed inside aggregate() with "no rows to aggregate", or with
# "incorrect number of dimensions" (review A09). Data with some dimensions
# missing in every row still has usable dimensions, so this is checked per
# analysis, after its own filtering, never for the data as a whole.
.stop_if_no_records <- function(n, fn, paired = FALSE, with_value = FALSE) {
  if (n > 0L) return(invisible(TRUE))
  what <- if (paired)
    "no respondent has complete, valid EQ-5D profiles at two of the timepoints in `levels_fu`"
  else if (with_value)
    "no record has both a complete, valid EQ-5D profile and an EQ-5D value"
  else
    "no record has a complete, valid EQ-5D profile"
  stop(fn, "(): ", what, ", so there is nothing to summarise. A profile ",
       "needs all five dimensions, each a level the instrument allows; ",
       "eq5d_profile_level_summary() reports the dimensions that are there.",
       call. = FALSE)
}

# Keep the first two timepoints of `fu`. The PCHC figures by group show one
# change per respondent and have no timepoint axis; with three timepoints
# they pooled the a->b and b->c changes into one bar while documenting a
# comparison of two (review AI06). A third or later timepoint is set to NA,
# with a message, and fewer than two is an error.
.first_two_timepoints <- function(df, fn) {
  lv <- levels(df$fu)
  if (length(lv) < 2L)
    stop(fn, "(): needs two timepoints in `levels_fu`, the first compared ",
         "with the second; found ", length(lv), ".", call. = FALSE)
  if (length(lv) > 2L) {
    message(fn, "(): comparing \"", lv[1L], "\" and \"", lv[2L], "\" only, ",
            "the first two timepoints in `levels_fu`; the figure shows one ",
            "change per respondent.")
    df$fu <- factor(as.character(df$fu), levels = lv[1:2])
  }
  df
}

# Rows of each group, for the summary helpers below.
#
# Groups are told apart by their values, not by pasting the values into one
# string: paste() turned a missing group into the text "NA", so a real group
# called "NA" and the missing one were summarised together, and a separator
# inside a value could join two different groups (review A06). match() keeps
# NA and "NA" apart. The groups come out in the order the pasted text sorted
# them before, so outputs that had no such clash are unchanged.
.group_rows <- function(df, group_by) {
  codes <- lapply(df[group_by], function(x) match(x, unique(x)))
  key <- do.call(paste, c(codes, list(sep = ".")))
  rows <- split(seq_len(nrow(df)), factor(key, levels = unique(key)))
  if (!length(rows)) return(rows)
  legacy <- vapply(rows, function(r)
    do.call(paste, c(lapply(df[r[1L], group_by, drop = FALSE], as.character),
                     list(sep = "\001"))), character(1L))
  unname(rows[order(legacy)])
}

# Display labels for the values of a grouping column. A missing group is
# labelled "NA", as the by-group tables have labelled it -- unless a real
# group is called "NA", when the missing one is "NA (missing)": the two used to
# be summarised together (review A06). Any other coincidence is made distinct.
.group_labels <- function(x, missing = "NA") {
  u <- unique(x)
  lab <- as.character(u)
  if (anyNA(u))
    lab[is.na(u)] <- if (missing %in% lab[!is.na(u)])
      paste0(missing, " (missing)") else missing
  lab <- make.unique(lab, sep = " ")
  lab[match(x, u)]
}

# The label of a total, made distinct from the groups' labels. A group that
# happened to be called "All groups" or "Total" took the total's place, and
# the column that looked like the total held that group (review A07).
.total_label <- function(label, taken) {
  taken <- as.character(taken)
  if (!label %in% taken) return(label)
  out <- paste0(label, " (total)")
  while (out %in% taken) out <- paste0(out, " ")
  out
}

#' Summary wrapper for Table 4.3
#' 
#' This internal function creates a summary of the data frame for Table 4.3. 
#' It groups the data by the variables specified in `group_by` and calculates various summary statistics.
#' 
#' @param df A data frame.
#' @param group_by A character vector of names of variables by which to group the data.
#' @return A data frame with the summary statistics.
#' @keywords internal

.summary_table_4_3 <- function(df, group_by) {

  rows <- .group_rows(df, group_by)

  result_list <- lapply(rows, function(r) {
    u <- df$utility[r]
    group_vals <- df[r[1L], group_by, drop = FALSE]
    row.names(group_vals) <- NULL
    cbind(group_vals, data.frame(
      Mean = mean(u, na.rm = TRUE),
      `Standard error` = sd(u, na.rm = TRUE) / sqrt(sum(!is.na(u))),
      Median = median(u, na.rm = TRUE),
      `25th` = quantile(u, probs = 0.25, na.rm = TRUE, names = FALSE),
      `75th` = quantile(u, probs = 0.75, na.rm = TRUE, names = FALSE),
      N = sum(!is.na(u)),
      Missing = sum(is.na(u)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  })

  retval <- do.call(rbind, result_list)
  rownames(retval) <- NULL
  return(retval)
}

#' Summary wrapper for Table 4.4
#' 
#' This internal function creates a summary of the data frame for Table 4.4. 
#' It groups the data by the variables specified in `group_by` and calculates various summary statistics.
#' 
#' @param df A data frame.
#' @param group_by A character vector of names of variables by which to group the data.
#' @return A data frame with the summary statistics.
#' @keywords internal

.summary_table_4_4 <- function(df, group_by) {

  rows <- .group_rows(df, group_by)

  result_list <- lapply(rows, function(r) {
    u <- df$utility[r]
    group_vals <- df[r[1L], group_by, drop = FALSE]
    row.names(group_vals) <- NULL
    cbind(group_vals, data.frame(
      Mean = mean(u, na.rm = TRUE),
      `Standard error` = sd(u, na.rm = TRUE) / sqrt(sum(!is.na(u))),
      `25th Percentile` = quantile(u, probs = 0.25, na.rm = TRUE, names = FALSE),
      `50th Percentile (median)` = median(u, na.rm = TRUE),
      `75th Percentile` = quantile(u, probs = 0.75, na.rm = TRUE, names = FALSE),
      n = sum(!is.na(u)),
      Missing = sum(is.na(u)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  })

  retval <- do.call(rbind, result_list)
  rownames(retval) <- NULL
  return(retval)
}

#' Wrapper to calculate summary mean with 95\% confidence interval
#' 
#' This internal function calculates summary mean and 95\% confidence interval of the utility variable, which can also be grouped.
#' The function is used in Figures 4.2-4.4.
#'
#' @param df A data frame containing a `utility` column.
#' @param group_by A character vector of column names to group by.
#' @return A data frame with the mean, lower bound, and upper bound of the 95% confidence interval of `utility` grouped by the `group_by` variables.
#'
#' @keywords internal
.summary_mean_ci <- function(df, group_by) {

  df <- df[!is.na(df$utility), , drop = FALSE]

  if (is.null(group_by) || length(group_by) == 0L) {
    u <- df$utility
    m <- mean(u)
    se <- sd(u) / sqrt(length(u))
    retval <- data.frame(
      mean = m,
      ci_lb = m - 1.96 * se,
      ci_ub = m + 1.96 * se,
      stringsAsFactors = FALSE
    )
    return(retval)
  }

  rows <- .group_rows(df, group_by)

  result_list <- lapply(rows, function(r) {
    u <- df$utility[r]
    group_vals <- df[r[1L], group_by, drop = FALSE]
    row.names(group_vals) <- NULL
    m <- mean(u)
    se <- sd(u) / sqrt(length(u))
    cbind(group_vals, data.frame(
      mean = m,
      ci_lb = m - 1.96 * se,
      ci_ub = m + 1.96 * se,
      stringsAsFactors = FALSE
    ))
  })

  retval <- do.call(rbind, result_list)
  rownames(retval) <- NULL
  return(retval)
}

#' Generate colours for PCHC figures
#'
#' This internal function generates a vector of colours based on the specified base colour. 
#' Currently only green and orange colours are implemented. 
#' The wrapper is used in Figures 2.2-2.4.
#'
#' @param col A character string specifying the base colour. Only "green" or "orange" is accepted.
#' @param n A positive integer specifying the number of colours to generate.
#' @return A vector of colours generated based on the specified base colour and number of colours.
#' @importFrom grDevices colorRampPalette
#'
#' @keywords internal
.gen_colours <- function(col, n) {
  retval <- if (col == "green")
    colorRampPalette(c("#99FF99", "#006600"))(n) else 
      if (col == "orange")
        colorRampPalette(c("#FFCC99", "#663300"))(n)
  
  return(retval)
}

#' Modify ggplot2 theme
#'
#' @param p ggplot2 plot
#' @return ggplot2 plot with modified theme
#' @keywords internal
.modify_ggplot_theme <- function(p) {
  # set ggplot2 theme
  p <- p + theme_bw() + theme(
    # remove vertical gridlines
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # remove horisontal minor gridlines
    panel.grid.minor.y = element_blank(),
    # remove x-axis ticks
    axis.ticks.x = element_blank(),
    # centre plot title
    plot.title = element_text(hjust = 0.5),
    # remove legend title
    legend.title = element_blank(),
    # move legend to bottom
    legend.position = "bottom")

return(p)

}

#' Wrapper to generate Paretian Classification of Health Change plot by dimension
#'
#' This internal function plots Paretian Classification of Health Change (PCHC) by dimension. 
#' The input is a data frame containing the information to plot, and the plot will contain bars representing 
#' the proportion of the total data that falls into each dimension, stacked by covariate.
#' The wrapper is used in Figures 2.2-2.4.
#'
#' @param plot_data A data frame containing information to plot, with columns for name (the dimensions to plot), p (the proportion of the total data falling into each dimension), and fu (the follow-up).
#' @param ylab The label for the y-axis.
#' @param title The plot title.
#' @param cols A vector of colours to use for the bars.
#' @param text_rotate A logical indicating whether to rotate the text labels for the bars.
#' @return A ggplot object containing the PCHC plot.
#' @keywords internal

.pchc_plot_by_dim <- function(plot_data, ylab, title, cols, text_rotate = FALSE) {
  
  p <- ggplot(plot_data, aes(x = .data$name, y = p, fill = .data$groupvar)) + 
    # bar chart
    geom_bar(stat = "identity", position = "dodge") + 
    # manipuilate x-axis
    scale_x_discrete(name = "") + 
    # manipulate y-axis
    scale_y_continuous(name = ylab,
                       expand = expansion(mult = c(0, 0.2)),
                       labels = scales::percent_format()) +
    # title
    ggtitle(title) +
    # manipulate legend
    scale_fill_manual(values = cols)
  
  # add percentages
  if (text_rotate) { 
    p <- p + geom_text(aes(label = scales::percent(p, accuracy = 0.1)), 
                       position = position_dodge(width = 0.9),
                       hjust = -0.1, angle = 90)
  } else {
   p <- p + geom_text(aes(label = scales::percent(p, accuracy = 0.1)), 
                      position = position_dodge(width = 0.9),
                      vjust = -0.5)
  }
  return(p)
}
