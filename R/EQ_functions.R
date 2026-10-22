#' Convert EQ-5D dimension scores to a five-digit profile index
#' @param x A data.frame, matrix, or named numeric vector of EQ-5D dimension
#'   scores. Each dimension must be a whole number from 1 to 9; a value that is
#'   fractional, non-finite or outside that range is not encoded and returns
#'   \code{NA} for its row, with a warning. Fractional values are not rounded.
#'   Factor columns are read through their labels.
#' @param dim.names Character vector of length 5 giving the dimension names, in
#'   the conventional MO, SC, UA, PD, AD order.
#' @param na.rm Logical. If \code{FALSE} (default), any \code{NA} in a row
#'   produces \code{NA} in the output. If \code{TRUE}, \code{NA} dimensions are
#'   treated as 0 — use with care, as this silently changes the index value.
#' @param quiet Logical. Suppress informational messages about assumed column /
#'   name order. Default \code{FALSE} so existing scripts see the same messages;
#'   set \code{TRUE} inside pipelines.
#' @return An integer vector with one element per row (data.frame/matrix input)
#'   or a single integer (vector input).  Works inside \code{dplyr::mutate()}
#'   without \code{rowwise()}.
#' @examples
#' # Named vector — scalar usage unchanged
#' toEQ5Dindex(c(mo=1, sc=2, ua=3, pd=1, ad=2))
#' @export

toEQ5Dindex <- function(
    x,
    dim.names = c("mo", "sc", "ua", "pd", "ad"),
    na.rm     = FALSE,
    quiet     = FALSE
) {
  
  # ── 1. Validate dim.names ─────────────────────────────────────────────────
  .check_dim_names(dim.names)
  
  msg <- function(...) if (!quiet) message(...)
  
  # ── 2. Weights: 10^4, 10^3, 10^2, 10^1, 10^0 ────────────────────────────
  weights <- 10L ^ (4L:0L)
  
  # ── 3. Matrix / data.frame branch (vectorised) ───────────────────────────
  if (length(dim(x)) == 2L) {
    
    # Assign default names if none present
    if (is.null(colnames(x)) && !is.numeric(dim.names)) {
      if (ncol(x) == 5L) {
        msg("No column names found; assuming conventional order: ",
            paste(toupper(dim.names), collapse = ", "), ".")
        colnames(x) <- dim.names
      } else {
        stop("'x' has no column names and ", ncol(x),
             " columns (expected 5). ",
             "Provide a named data.frame/matrix or set 'dim.names'.")
      }
    }
    
    # The columns, matched as every scorer matches them (.resolve_dims()).
    cn  <- if (is.null(colnames(x))) character(ncol(x)) else colnames(x)
    pos <- .resolve_dims(cn, dim.names)
    
    # Validate before encoding, not after. mode(m) <- "integer" truncated
    # 1.9 to level 1, and a value of 11 carried into the dimension beside it:
    # c(0, 11, 1, 1, 1) encoded as 11111 and scored as full health. See
    # .clean_dim_matrix() in R/validate_dims.R. The instrument is not known
    # here, so the bound is the digit bound the encoding itself requires.
    m <- .clean_dim_matrix(x[, pos, drop = FALSE], max_level = NULL)
    
    # NA handling
    if (!na.rm && anyNA(m)) {
      # Propagate NA row-wise without changing non-NA rows
      row_has_na <- rowSums(is.na(m)) > 0L
      result <- integer(nrow(m))
      result[!row_has_na] <- as.integer(
        m[!row_has_na, , drop = FALSE] %*% weights
      )
      result[row_has_na] <- NA_integer_
      return(result)
    }
    
    if (na.rm) m[is.na(m)] <- 0L
    return(as.integer(m %*% weights))
  }
  
  # ── 4. Named-vector branch (scalar, backward-compatible) ─────────────────
  if (length(x) < 5L)
    stop("'x' has ", length(x), " element(s); at least 5 required.")
  
  if (is.null(names(x))) {
    if (length(x) == 5L) {
      msg("No names found in vector; assuming conventional order: ",
          paste(toupper(dim.names), collapse = ", "), ".")
      names(x) <- dim.names
    } else {
      stop("'x' has no names and length != 5. ",
           "Provide a named vector or a data.frame/matrix.")
    }
  }
  
  vals <- x[.resolve_dims(names(x), dim.names)]
  # The same validation as the matrix branch, so a named vector and a
  # one-row data frame of the same values cannot disagree.
  st <- .dim_status(vals, max_level = NULL)
  .warn_dim_values(st, max_level = NULL)
  vals <- attr(st, "value")
  vals[st %in% .DIM_REJECTED] <- NA_real_

  if (!na.rm && anyNA(vals))
    return(NA_integer_)
  
  if (na.rm) vals[is.na(vals)] <- 0L
  as.integer(as.integer(vals) %*% weights)
}

#' @title toEQ5Ddims
#' @description Generate dimension vectors based on state index
#' @param x A vector of 5-digit EQ-5D state indexes.
#' @param dim.names A vector of dimension names to be used as names for output columns.
#' @return A data.frame with 5 integer columns, one for each EQ-5D dimension,
#'   with names from the dim.names argument; the same layout as the output of
#'   \code{make_all_EQ_states()}. A code that is not a valid 5-digit state gives
#'   \code{NA} in every column, with a warning.
#' @examples 
#' toEQ5Ddims(c(12345, 54321, 12321))
#' @export
toEQ5Ddims <- function(x, dim.names = c("mo", "sc", "ua", "pd", "ad")) {
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  # Whole, finite codes only; a fractional one is NA, not truncated.
  x <- .parse_states(x)
  # Remove items outside of bounds
  x[!regexpr("^[1-5]{5}$", x)==1] <- NA
  if(sum(is.na(x))) warning(paste0("Provided vector contained ", sum(is.na(x)), " items not conforming to 5-digit numbers with exclusively digits in the 1-5 range."))
  out <- as.data.frame(outer(X = x, Y = structure(.Data = 10^(4:0), names = dim.names), FUN = "%/%") %% 10)
  # Integer columns, as make_all_EQ_states() returns: the levels are whole
  # numbers, and the two are documented as having the same layout.
  out[] <- lapply(out, as.integer)
  out
}

#' @title make_all_EQ_states
#' @description Make a data.frame with all health states defined by dimensions
#' @param version "3L", "5L" or "Y3L", to signify whether 243 or 3125 states should be generated. The EQ-5D-Y-3L has the same 243 states as the EQ-5D-3L. Matching is case-insensitive.
#' @param dim.names A vector of dimension names to be used as names for output columns.
#' @param append_index Boolean to indicate whether a column of 5-digit EQ-5D health state indexes should be added to output.
#' @return A data.frame with 5 columns and 243 (-3L, -Y-3L) or 3125 (-5L) health states
#' @examples 
#' make_all_EQ_states('3L')
#' @export
make_all_EQ_states <- function(version = "5L", dim.names = c("mo", "sc", "ua", "pd", "ad"), append_index = FALSE) {
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  if(!is.vector(version)) stop("version argument is not a vector of length 1")
  if(length(version)>1) {
      message("version argument provided length of more than 1, first element is used.")
      version <- version[[1]]
    }
  # The version was validated with toupper() but the branch below compared the
  # raw string, so make_all_EQ_states("5l") returned the 243 three-level states.
  # The EQ-5D-Y-3L has the same 243 states as the EQ-5D-3L.
  version <- .norm_version(version, allowed = c("3L", "5L", "Y3L"))
  xout <- do.call(expand.grid, structure(rep(list(1:ifelse(version == "5L", 5, 3)), 5), names = dim.names[5:1]))[,5:1]
  if(append_index) xout$state <- toEQ5Dindex(xout, dim.names)
  xout
}

#' @title make_all_EQ_indexes
#' @description Make a vector containing all 5-digit EQ-5D indexes for -3L or -5L version.
#' @param version "3L", "5L" or "Y3L", to signify whether 243 or 3125 states should be generated. The EQ-5D-Y-3L has the same 243 states as the EQ-5D-3L. Matching is case-insensitive.
#' @param dim.names A vector of dimension names to be used as names for output columns.
#' @return A vector with 5-digit state indexes for all 243 (-3L, -Y-3L) or 3125 (-5L) EQ-5D health states
#' @examples 
#' make_all_EQ_indexes('3L')
#' @export
make_all_EQ_indexes <- function(version = "5L", dim.names = c("mo", "sc", "ua", "pd", "ad")) {
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  states <- do.call(make_all_EQ_states, list(version = version, dim.names = dim.names))
  toEQ5Dindex(states, dim.names = dim.names)
  #toEQ5Dindex(do.call(make_all_EQ_states, as.list(match.call()[-1])))
}

#' @title EQ_dummies
#' @description Make a data.frame of all EQ-5D dummies relevant for e.g. regression modelling. 
#' @param df data.frame containing EQ-5D health states.
#' @param version EQ-5D instrument version: "3L", "5L" or "Y3L". Matching is case-insensitive.
#' @param dim.names A vector of dimension names to be used as names for output columns.
#' @param drop_level_1 If set to FALSE, dummies for level 1 will be included. Defaults to TRUE.
#' @param add_intercept If set to TRUE, a column containing 1s will be appended. Defaults to FALSE.
#' @param incremental If set to TRUE, incremental dummies will be produced (e.g. MO = 3 will give mo2 = 1, mo3 = 1). Defaults to FALSE.
#' @param append Optional string to be appended to column names.
#' @param prepend Optional string to be prepended to column names.
#' @param return_df If set to TRUE, data.frame is returned, otherwise matrix. Defaults to TRUE.
#' @return A data.frame of dummy variables, one row per row of \code{df}.
#'   Levels are read as the value functions read them: text and factors
#'   through their labels. A level the instrument does not have (fractional,
#'   0, negative, non-finite or above the highest level) is not a level, so
#'   that dimension's dummies are \code{NA}, with a warning; a missing level
#'   gives \code{NA} dummies without one.
#' @examples 
#' make_dummies(make_all_EQ_states('3L'), '3L')
#' 
#' make_dummies(df = make_all_EQ_states('3L'), 
#'              version =  '3L', 
#'              incremental = TRUE, 
#'              add_intercept = TRUE, 
#'              prepend = "d_")
#' @export
make_dummies <- function(df, 
                         version      = "5L", 
                         dim.names    = c("mo", "sc", "ua", "pd", "ad"), 
                         drop_level_1  = TRUE, 
                         add_intercept = FALSE, 
                         incremental   = FALSE, 
                         prepend       = NULL, 
                         append        = NULL, 
                         return_df     = TRUE) {
  
  if (!length(dim.names) == 5) 
    stop("Argument dim.names not of length 5.")
  .check_dim_names(dim.names, "make_dummies")
  if (!length(dim(df) == 2)) 
    stop("Need to provide matrix or data.frame of 2 dimensions.")
  # A matrix is documented as acceptable input, but the column extraction
  # below uses df[[col]], which is data-frame extraction: on a matrix it
  # raised "subscript out of bounds". Normalise once, here, so the rest of
  # the function has one kind of object to work with.
  if (is.matrix(df)) df <- as.data.frame(df, stringsAsFactors = FALSE)
  # The version was not checked at all, and the branch below compares against
  # "5L", so any other spelling -- including "5l" -- built three-level dummies.
  # On five-level data that indexed past the end of the design matrix and
  # returned rows of NA.
  version <- .norm_version(version)
  
  # Case-insensitive column matching without renaming
  col_lower    <- tolower(colnames(df))
  dim_lower    <- tolower(dim.names)
  matched_cols <- col_lower %in% dim_lower
  
  if (!all(dim_lower %in% col_lower)) {
    if (NCOL(df) == 5) {
      # Assume conventional order — map actual column names
      # to dim.names without renaming
      message(
        "Column names do not match dim.names. ",
        "Assuming dimensions are in conventional order: ",
        paste(dim.names, collapse = ", "), "."
      )
      # Create a named mapping: dim.name -> actual column name
      col_map <- setNames(colnames(df), dim.names)
    } else {
      stop(
        "Provided dimension names (",
        paste(dim.names, collapse = ", "),
        ") not found in column names (",
        paste(colnames(df), collapse = ", "),
        "). Please rename your columns or update dim.names."
      )
    }
  } else {
    # Match dim.names to actual column names case-insensitively
    col_map <- setNames(
      colnames(df)[match(dim_lower, col_lower)],
      dim.names
    )
  }
  
  vers       <- ifelse(version == "5L", 5, 3)
  startlevel <- ifelse(drop_level_1, 2, 1)
  
  if (incremental) {
    tmp <- 1 * lower.tri(diag(vers), TRUE)[, startlevel:vers, drop = FALSE]
  } else {
    tmp <- diag(vers)[, startlevel:vers, drop = FALSE]
  }
  
  # Use actual column names via col_map. drop = FALSE matters: with a single
  # row of input, tmp[i, ] returned the row as a plain vector, cbind() then
  # laid the five dimensions out as five columns instead of one row of
  # dummies, and naming those columns failed with "length of 'dimnames' [2]
  # not equal to array extent".
  #
  # The levels are read with the package's shared parser before they index
  # anything. Used directly as subscripts, a factor indexed by its level code
  # (factor("3") is code 1, so level 3 came out as level 1) and 1.9 was
  # truncated to 1, both silently; 0 or a negative number dropped or excluded
  # rows. A level the instrument does not have is now NA, so its dimension's
  # dummies are NA, with one warning -- never a valid level, and never the
  # reference category's zeros (review A01).
  lev <- .clean_dim_matrix(df[, unname(col_map), drop = FALSE],
                           max_level = vers, what = "value")
  tmpout <- do.call(cbind, lapply(seq_along(col_map), function(j) {
    tmp[lev[, j], , drop = FALSE]
  }))
  
  colnames(tmpout) <- as.vector(
    t(outer(paste0(prepend, dim.names),
            paste0(startlevel:vers, append),
            FUN = paste0))
  )
  
  if (add_intercept) tmpout <- cbind(intercept = 1, tmpout)
  if (return_df)     tmpout <- as.data.frame(tmpout)
  
  tmpout
}


#' @title eqvs_add
#' @description Add user-defined EQ-5D value set and corresponding crosswalk option.
#' @details
#' Every check below is made before anything is changed, so a value set that
#' fails one is not added, in part or in full, and nothing is saved.
#' \itemize{
#'   \item \code{df}, if a file name, must name an existing csv file, and must
#'     have exactly two columns.
#'   \item It must have exactly one row per health state of the instrument
#'     (3,125 for the EQ-5D-5L, 243 for the EQ-5D-3L and EQ-5D-Y-3L), and every
#'     state must be present exactly once. The rows may be in any order, and the
#'     states may be given as numbers, text or factors; they must be whole
#'     numbers, so \code{11111.9} is refused rather than read as \code{11111}.
#'   \item The value set code (\code{VSCode}, or the name of the second column)
#'     must not be a built-in value set code, compared without regard to case
#'     and across all three instruments; this is an error. If it is the code of
#'     one of your own value sets for the same instrument, again without regard
#'     to case, the function warns and returns \code{0} without adding it.
#'   \item Every value must be numeric and finite: \code{NA}, \code{NaN},
#'     text, \code{Inf} and \code{-Inf} are errors.
#'   \item With \code{saveOption = 3}, \code{savePath} must name an existing
#'     directory. With \code{saveOption = 2} or \code{3}, if the cache cannot
#'     be saved, the value set is removed again.
#' }
#' The values themselves are not checked further: nothing requires full health
#' (\code{11111}) to be 1, or the values to fall as the levels worsen.
#' @param df A data.frame or file name pointing to csv file. The contents of the data.frame or csv file should be exactly two columns: state, containing a list of all 3125 (for 5L) or 243 (for 3L and Y3L) EQ-5D health state vectors, and a column of corresponding utility values, with a suitable name. The EQ-5D-Y-3L uses the same 243 health states as the EQ-5D-3L.
#' @param version Version of the EQ-5D instrument. Can take values 5L (default), 3L or Y3L. Matching is case-insensitive.
#' @param country Optional string. If not NULL, will be used as a country description for the user-defined value set.
#' @param countryCode Optional string. If not NULL, will be used as the two-digit code for the value set. Must be different from any existing national value set code.
#' @param VSCode Optional string. If not NULL, will be used as the code for the
#'   value set; otherwise the name of the second column of \code{df} is used.
#'   It must differ from every built-in value set code, compared without regard
#'   to case and across all three instruments, and from the codes of your own
#'   value sets for this instrument. \code{eqvs_display()} lists the codes
#'   already in use.
#' @param description Optional string. If not NULL, will be used as a descriptive text for the user-defined value set. 
#' @param saveOption Integer indicating how the cache data should be saved.
#'   1: Do not save (default).
#'   2: Save in this package's user cache directory, as returned by
#'   \code{tools::R_user_dir("eq5dsuite", "cache")}. It is read back
#'   automatically when the package is next loaded. Nothing is written inside
#'   the installed package, which CRAN does not permit.
#'   3: Save in the directory given by \code{savePath}, to be read back with
#'   \code{eqvs_load()}.
#' @param savePath A path where the cache data should be saved when `saveOption` is 3. Please use `eqvs_load` to load it in your next session.
#' @return Called for its side effect. When the value set is added,
#'   \code{NULL}, invisibly, with a message. When \code{VSCode} is already
#'   used by one of your own value sets for this instrument, \code{0}, with a
#'   warning, and nothing is added.
#' @examples
#' # Make a nonsense value set. The values are a plain sequence rather than
#' # runif(), which would move the random seed of whoever runs the example.
#' new_df <- data.frame(state = make_all_EQ_indexes(),
#'                      TEST = seq(1, -0.5, length.out = 3125))
#' # Add as value set for Fantasia
#' eqvs_add(
#'    new_df,
#'    version = "5L",
#'    country = 'Fantasia',
#'    countryCode = "MyCountry",
#'    VSCode = "FAN",
#'    saveOption = 1
#' )
#' eq5d5l(55555,country = "FAN")
#' # Remove it again, so the example leaves no value set behind.
#' eqvs_drop(country = "FAN", version = "5L", saveOption = 1, ask = FALSE)
#' @importFrom utils read.csv
#' @export

eqvs_add <- function(df, version = "5L", country = NULL, countryCode = NULL, VSCode = NULL, description = NULL, saveOption = 1, savePath = NULL) {
  # Ensure saveOption is either 1, 2, or 3
  if (!saveOption %in% c(1, 2, 3)) {
    stop("Invalid 'saveOption'. It must be 1, 2, or 3.")
  }
  
  pkgenv <- getOption("eq.env")
  # The version is pasted into the package environment keys below, and decides
  # how many rows the value set must have, so it has to be canonical first.
  version <- .norm_version(version)
  if(inherits(df, 'character')) {
    if(!file.exists(df)) stop('File named ', df, ' does not appear to exist. Exiting.')
    df <- utils::read.csv(file = df, stringsAsFactors = F)
  } 
  if(!NCOL(df) == 2) stop('df should have exactly two columns.')
  # The state codes are checked as numbers before they become integers.
  # class<- 'integer' truncated them, so 11111.9 became the state 11111 and a
  # set of codes that were not states was accepted (review A11). Text and
  # factors are read through their labels.
  keys <- .parse_number(df[, 1])
  badk <- is.na(keys) | !is.finite(keys) | keys != trunc(keys)
  if (any(badk))
    stop("The first column of df must hold whole health state codes, such as ",
         "11111; ", sum(badk), " do not (e.g. ",
         paste(utils::head(unique(as.character(df[badk, 1])), 3L), collapse = ", "),
         "). Fractional codes are not truncated to a state.", call. = FALSE)
  df[, 1] <- as.integer(keys)
  if (anyDuplicated(df[, 1]))
    stop("Each health state must appear exactly once in the first column of ",
         "df; ", length(unique(df[duplicated(df[, 1]), 1])), " appear more ",
         "than once (e.g. ",
         paste(utils::head(unique(df[duplicated(df[, 1]), 1]), 3L), collapse = ", "),
         ").", call. = FALSE)
  
  # read off version-dependent parameters
  j <- if (version == "5L") 5 else 3
  # Not paste0("states_", version): that produced "states_Y3L", a key nothing
  # creates, so the health-state check below rejected every EQ-5D-Y-3L value
  # set. The Y3L states are the 3L states; see .states_key().
  states_str <- .states_key(version)
  eq5d_str <- .eq5d_instrument(version)
  uservsets_str <- paste0("uservsets", version)
  user_defined_str <- paste0("user_defined_", version)
  
  # check correct number of rows
  n <- j^5
  if(!NROW(df) == n) stop(paste0("df should have exactly ", n, " rows."))
  # check all states present
  if(!all(df[,1] %in% pkgenv[[states_str]]$state)) stop(paste0("First column of df should contain all ", eq5d_str, " health state indexes exactly once."))
  df <- df[match(pkgenv[[states_str]]$state, df[,1]),]
  # check country
  if(!is.null(country)) {
    if(length(country)>1) {
      warning('Length of country argument > 1, first item used.')
      country <- country[1]
    }
  }
  if(!is.null(description)) {
    if(length(description)>1) {
      warning('Length of description argument > 1, first item used.')
      description <- description[1]
    }
  }
  thisName <- ifelse(is.null(VSCode), colnames(df)[2], VSCode)

  # Reject a code that a built-in value set already uses. .fixPkgEnv() combines
  # the built-in and user-defined tables with merge(), which treats a column
  # name they share as an extra join key: the combined table collapses to the
  # rows where the two sets happen to hold identical values -- none, in
  # practice -- and every lookup for that instrument fails. A code differing
  # only in case is just as damaging, because .fixCountries() matches
  # case-insensitively and then reports the two sets as ambiguous.
  #
  # All instruments are checked, not just this one: reusing a code that is
  # built in elsewhere is confusing, and a set can be added for one instrument
  # later.
  builtin <- .builtin_vs_codes()
  clash <- builtin[toupper(builtin) == toupper(thisName)]
  if (length(clash)) {
    used_for <- .builtin_vs_versions(clash[1])
    stop("Value set code '", thisName, "' is already used by the built-in ",
         "value set '", clash[1], "'",
         if (length(used_for))
           paste0(" (", paste(used_for, collapse = ", "), ")") else "",
         ".\n  Built-in codes cannot be reused, even in a different case or ",
         "for a different instrument.\n  Please choose a different code and ",
         "pass it as `VSCode`. eqvs_display(version = \"", version,
         "\") lists the codes already in use.",
         call. = FALSE)
  }

  # Case-insensitively, as the lookup is. Checking it case-sensitively here
  # while .fixCountries() matches case-insensitively let REVIEW_A and
  # review_a both be added, after which either lookup reported them as
  # ambiguous and neither could be used -- and the error told the user to
  # supply an exact code, which they already had.
  existing <- colnames(pkgenv[[uservsets_str]])
  dup <- existing[toupper(existing) == toupper(thisName)]
  if (length(dup)) {
    warning("Value set code '", thisName, "' is already used by the ",
            "user-defined ", eq5d_str, " value set '", dup[1L], "'. ",
            "Codes differing only in case cannot both be used, because ",
            "value sets are looked up case-insensitively. Drop the existing ",
            "set with eqvs_drop() first, or choose a different code.",
            call. = FALSE)
    return(0)
  }
  
  if(any(is.na(df[,2]*-1.1))) stop("Non-numeric values in second column of df.")
  # Inf and -Inf passed the check above. A value set holding one scored that
  # state as infinite for the rest of the session, and when saved it was
  # dropped on the next load, because the cache check refuses non-finite
  # values -- so it is refused here, before anything is changed.
  if (!all(is.finite(as.numeric(df[,2])))) {
    nf <- which(!is.finite(as.numeric(df[,2])))
    stop("Values in the second column of df are not finite (Inf or -Inf) for ",
         length(nf), " health state(s), e.g. ",
         paste(utils::head(df[nf, 1], 3L), collapse = ", "),
         ". Every value must be a finite number.", call. = FALSE)
  }

  # ── F13: everything that can fail is checked before anything is changed ──
  #
  # The save path used to be validated after the value table and the metadata
  # had already been written into the package environment, so a bad path left
  # the registry advertising a value set with no values behind it and the next
  # lookup failed with "replacement has length zero".
  path <- NULL
  if (saveOption == 2) {
    path <- pkgenv$cache_path
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    if (!dir.exists(path))
      stop("The cache directory '", path, "' could not be created.",
           call. = FALSE)
  } else if (saveOption == 3) {
    if (is.null(savePath) || !nzchar(savePath))
      stop("Option 3 requires a valid 'savePath'.")
    if (!dir.exists(savePath))
      stop("The specified 'savePath' does not exist.")
    path <- savePath
  }

  # No problems. Snapshot first, so a failed persist can put the session
  # back: "the add failed" should mean the same in memory as on disk.
  .vs_snapshot <- .vs_state_snapshot(pkgenv)
  tmp <- pkgenv[[uservsets_str]]
  tmp[, thisName] <- df[, 2]
  assign(x = uservsets_str, value = tmp, envir = pkgenv)
  
  # Build the metadata row from the shared schema (derived from the built-in
  # country_codes table) so that user-defined and built-in value set tables
  # always have the same columns, in the same order, with the same types.
  # Columns with no value for a user-defined set are left as typed NA.
  tmp <- .new_vs_meta_row(
    Version      = version,
    Name         = country,
    Name_short   = country,
    Country_code = if (is.null(countryCode)) colnames(df)[2] else countryCode,
    VS_code      = if (is.null(VSCode)) colnames(df)[2] else VSCode,
    doi          = description
  )

  if(user_defined_str %in% names(pkgenv))
    tmp <- rbind(.migrate_vs_meta(pkgenv[[user_defined_str]]), tmp)

  rownames(tmp) <- NULL
  assign(x = user_defined_str, value = tmp, envir = pkgenv)
  
  # The path was validated before anything was changed, above.
  if(saveOption == 1){
    .fixPkgEnv(saveCache = FALSE)
  }
  if (saveOption == 2 || saveOption == 3) {
    filePath <- file.path(path, .cache_basename)
    if (.fixPkgEnv(saveCache = TRUE, filePath = filePath)) {
      message(paste0('Cache data saved to ', filePath))
    } else {
      .vs_state_restore(pkgenv, .vs_snapshot)
      stop("The value set ", country, " could not be saved to '", filePath,
           "', so it has not been added. Nothing has been changed.",
           call. = FALSE)
    }
  }

  message(paste("The value set", country, "was added."))
}

#' @title eqvs_load
#' @description Load cache data from a specified path.
#' @details
#' \code{loadPath} is the *directory* a cache was written to with
#' \code{eqvs_add(saveOption = 3, savePath = ...)} or
#' \code{eqvs_drop(saveOption = 3, savePath = ...)}, not a single value set.
#' Everything saved there is restored at once.
#'
#' A cache written with \code{saveOption = 2} goes to the package's user cache
#' directory and is read back automatically when the package loads, so it needs
#' no call to \code{eqvs_load()}.
#' @param loadPath The path from which to load the cache data.
#' @return TRUE if loading is successful, FALSE otherwise.
#' @seealso \code{\link{eqvs_add}}, \code{\link{eqvs_drop}}
#' @examples
#' # Save a custom value set to a directory of your choosing, then read it
#' # back. tempdir() is used here so the example leaves nothing behind.
#' path <- file.path(tempdir(), "eq5d-value-sets")
#' dir.create(path, showWarnings = FALSE)
#'
#' my_vs <- data.frame(state = make_all_EQ_indexes("3L"),
#'                     MY_VS = round(seq(1, -0.5, length.out = 243), 4))
#' eqvs_add(my_vs, version = "3L", country = "My Country",
#'          countryCode = "MC", VSCode = "MY_VS",
#'          saveOption = 3, savePath = path)
#'
#' # In a later session, restore it from that directory.
#' eqvs_load(loadPath = path)
#' eq5d3l(c(11111, 33333), country = "MY_VS")
#'
#' # Tidy up.
#' eqvs_drop(country = "MY_VS", version = "3L", ask = FALSE)
#' unlink(path, recursive = TRUE)
#' @export
eqvs_load <- function(loadPath) {
  pkgenv <- getOption("eq.env")
  if (is.null(loadPath) || !nzchar(loadPath)) {
    stop("A valid 'loadPath' is required.")
  }
  # Accepts a cache written by any supported schema version, including the
  # legacy 'cache.Rdta.' basename. See R/cache_schema.R.
  cacheFile <- .find_cache_file(loadPath)
  if (is.null(cacheFile)) {
    stop("Cache file not found at specified 'loadPath'.")
  }
  status <- .apply_cache(pkgenv, loadPath)
  if (identical(status, "reject")) {
    return(FALSE)
  }
  # Rebuild the combined and crosswalk value set tables so the newly loaded
  # user-defined sets are usable straight away.
  .fixPkgEnv(saveCache = FALSE)
  message(paste0('Cache data loaded from ', cacheFile, '.'))
  return(TRUE)
}


#' @title eqvs_drop
#' @description Drop user-defined EQ-5D value set to reverse crosswalk options.
#' @param version Version of the EQ-5D instrument. Can take values 5L (default), 3L or Y3L. Matching is case-insensitive.
#' @param country A country code or value set code identifying the user-defined
#'   value set to remove, as listed by \code{eqvs_display()}. If more than one
#'   user-defined value set shares the country code, the value set code must be
#'   given; otherwise the function stops and lists the matching codes.
#' @param saveOption Integer indicating how the cache data should be saved.
#'   1: Do not save (default).
#'   2: Save in this package's user cache directory, as returned by
#'   \code{tools::R_user_dir("eq5dsuite", "cache")}. It is read back
#'   automatically when the package is next loaded. Nothing is written inside
#'   the installed package, which CRAN does not permit.
#'   3: Save in the directory given by \code{savePath}, to be read back with
#'   \code{eqvs_load()}.
#' @param savePath A path where the cache data should be saved when `saveOption` is 3. Please use `eqvs_load` to load it in your next session.
#' @param ask Logical. Whether to ask for confirmation before removing the
#'   value set. Confirmation is only requested when \code{ask} is \code{TRUE}
#'   \emph{and} the session is interactive; with \code{ask = FALSE}, or in a
#'   non-interactive session, the value set is removed without prompting.
#'   Defaults to \code{TRUE}.
#' @return Invisibly \code{TRUE} if a value set was removed, and \code{FALSE}
#'   otherwise (for example if no matching value set exists, or the user
#'   declined the confirmation).
#' @examples
#' \donttest{
#'   # Make a nonsense value set, without moving the random seed.
#'   new_df <- data.frame(state = make_all_EQ_indexes(),
#'                        TEST = seq(1, -0.5, length.out = 3125))
#'   # Add as value set for Fantasia
#'   eqvs_add(
#'    new_df,
#'    version = "5L",
#'    country = 'Fantasia',
#'    countryCode = "MyCountry",
#'    VSCode = "FAN",
#'    saveOption = 1
#'   )
#'   # Test the new value set
#'   eq5d5l(55555,country = "FAN")
#'   # Drop value set for Fantasia, without asking for confirmation
#'   eqvs_drop(country = 'FAN', saveOption = 1, ask = FALSE)
#' }
#' @export
eqvs_drop <- function(country = NULL, version = "5L", saveOption = 1, savePath = NULL, ask = TRUE) {
  
  # Ensure saveOption is valid
  if (!saveOption %in% c(1, 2, 3)) {
    stop("Invalid 'saveOption'. It must be 1, 2, or 3.")
  }
  
  pkgenv <- getOption("eq.env")
  version <- .norm_version(version)
  
  user_defined_str <- paste0("user_defined_", version)
  uservsets_str <- paste0("uservsets", version)
  
  if (!user_defined_str %in% names(pkgenv)) {
    message(paste0("No user-defined value sets exist for ", version, ". Exiting."))
    return(invisible(FALSE))
  }

  udc <- pkgenv[[user_defined_str]]

  # Ensure `udc` exists and is not empty
  if (is.null(udc) || nrow(udc) == 0) {
    message("No user-defined value sets found. Exiting.")
    return(invisible(FALSE))
  }

  # Handle case where multiple value sets exist for the same country.
  # An exact value set code always wins over a country code match, mirroring
  # .fixCountries().
  exact_vs <- which(toupper(udc$VS_code) == toupper(country))
  matched_rows <- if (length(exact_vs) == 1L) exact_vs else
    which(toupper(udc$Country_code) == toupper(country) | toupper(udc$VS_code) == toupper(country))

  if (length(matched_rows) == 0) {
    message("No matching user-defined value set found for country: ", country, ". Exiting.")
    return(invisible(FALSE))
  }

  if (length(matched_rows) > 1) {
    # This used to prompt with readline() in a repeat loop, which never
    # terminates in a non-interactive session. Fail with an actionable error
    # instead, in every kind of session. Mirrors .fixCountries().
    stop("Multiple value sets are available for country code '", country,
         "': ", paste(udc$VS_code[matched_rows], collapse = ", "),
         ". Please specify one of these value set codes. See ",
         "eqvs_display(version = \"", version, "\") for the full list.",
         call. = FALSE)
  }

  country <- udc$VS_code[matched_rows]

  # Ask for confirmation, but only where a user can actually answer. With
  # ask = FALSE, or in a non-interactive session, proceed without prompting --
  # the same pattern as drop_value_set() and update_value_sets().
  confirmed <- TRUE
  if (ask && interactive()) {
    yesno <- readline(prompt = paste0('Are you sure you want to delete value set "', country, '" for ', version, '? ([Y]es/[N]o) : '))
    confirmed <- tolower(trimws(yesno)) %in% c("yes", "y")
  }

  if (confirmed) {

    # F13: validate the save path before anything is removed. The removal
    # used to happen first, so a bad path dropped the value set from the
    # running session and then errored, leaving no way to tell that the
    # session no longer matched the cache on disk.
    path <- NULL
    if (saveOption == 2) {
      path <- pkgenv$cache_path
      if (!dir.exists(path)) dir.create(path, recursive = TRUE)
      if (!dir.exists(path))
        stop("The cache directory '", path, "' could not be created.",
             call. = FALSE)
    } else if (saveOption == 3) {
      if (is.null(savePath) || !nzchar(savePath))
        stop("Option 3 requires a valid 'savePath'.")
      if (!dir.exists(savePath))
        stop("The specified 'savePath' does not exist.")
      path <- savePath
    }

    .vs_snapshot <- .vs_state_snapshot(pkgenv)

    message('Removing ', country, ' from user-defined value sets.')
    
    # Remove from user-defined dataset
    udc <- udc[udc$VS_code != country, ]
    if (nrow(udc) > 0) {
      assign(x = user_defined_str, value = udc, envir = pkgenv)
    } else {
      rm(list = user_defined_str, envir = pkgenv)
    }
    
    # Remove from value set matrix
    tmp <- pkgenv[[uservsets_str]]
    if (country %in% colnames(tmp)) {
      tmp <- tmp[, !colnames(tmp) %in% country, drop = FALSE]
      assign(x = uservsets_str, value = tmp, envir = pkgenv)
    }
    
    # The path was validated before anything was removed, above.
    if (saveOption == 1) {
      .fixPkgEnv(saveCache = FALSE)
    }
    
    if (saveOption == 2 || saveOption == 3) {
      filePath <- file.path(path, .cache_basename)
      if (.fixPkgEnv(saveCache = TRUE, filePath = filePath)) {
        message(paste0('Cache data saved to ', filePath))
      } else {
        .vs_state_restore(pkgenv, .vs_snapshot)
        stop("The value set ", country, " could not be removed from the cache ",
             "at '", filePath, "', so it has not been removed. Nothing has ",
             "been changed.", call. = FALSE)
      }
    }
    message(paste("The value set", country, "was deleted."))
    return(invisible(TRUE))
  }

  message("OK. Exiting without deletion.")
  invisible(FALSE)
}


#' @title eqvs_display
#' @description Display available value sets, which can also be used as
#'   (reverse) crosswalks. Built-in value sets are shown first, followed
#'   by any user-defined value sets added via \code{eqvs_add()}.
#' @param version Version of the EQ-5D instrument. One of \code{"3L"},
#'   \code{"5L"} (default), or \code{"Y3L"}. Matching is case-insensitive.
#' @param return_df Logical. If \code{FALSE} (default), the summary table is
#'   printed to the console. If \code{TRUE}, nothing is printed and the value
#'   set information is returned as a data.frame instead, with all columns
#'   (including \code{citation} if present), so that it can be used in further
#'   code without cluttering scripts, reports or the console.
#' @param show_citation Logical. If \code{TRUE}, prints the full AMA
#'   citation for each value set after the summary table. Defaults to
#'   \code{FALSE}. Applies only when \code{return_df = FALSE}: the returned
#'   data.frame always includes the \code{citation} column, so this argument
#'   is ignored when \code{return_df = TRUE}.
#' @return When \code{return_df = FALSE}, \code{NULL} invisibly, called for the
#'   printed output. When \code{return_df = TRUE}, a data.frame, returned
#'   visibly so that it still prints when the call is made at the console.
#' @examples
#' # Print the available EQ-5D-5L value sets.
#' eqvs_display(version = "5L")
#'
#' # Get the same information as a data.frame, printing nothing.
#' vs <- eqvs_display(version = "5L", return_df = TRUE)
#' head(vs)
#' @export
eqvs_display <- function(version       = "5L",
                          return_df     = FALSE,
                          show_citation = FALSE) {

  # Helper: print a compact table (no citation column)
  print_vs_table <- function(df) {
    display_cols <- c("Version", "Name_short", "Country_code", "VS_code", "doi")
    display_cols <- display_cols[display_cols %in% colnames(df)]
    display_df   <- df[, display_cols, drop = FALSE]
    if ("Name_short" %in% colnames(display_df))
      display_df$Name_short <- strtrim(display_df$Name_short, 20)
    if ("doi" %in% colnames(display_df))
      display_df$doi <- strtrim(display_df$doi, 35)
    print(display_df, row.names = FALSE)
  }

  # Helper: align columns of two data frames before rbind
  align_df_cols <- function(df1, df2) {
    for (col in setdiff(colnames(df1), colnames(df2)))
      df2[[col]] <- NA
    for (col in setdiff(colnames(df2), colnames(df1)))
      df1[[col]] <- NA
    df2 <- df2[, colnames(df1), drop = FALSE]
    rbind(df1, df2)
  }

  pkgenv <- getOption("eq.env")

  # Without this check an unknown version silently yields a NULL table, which
  # printed as a bare "NULL" and, with return_df = TRUE, returned a 1x1 matrix
  # holding nothing.
  version <- .norm_version(version)

  user_defined_str <- paste0("user_defined_", version)

  builtin <- pkgenv$country_codes[[version]]
  ud      <- pkgenv[[user_defined_str]]

  # --- Data frame form: return without printing anything ---------------------
  # Returned visibly, so an interactive call still shows the table through R's
  # normal printing, while `vs <- eqvs_display(...)` stays silent.
  if (return_df) {
    if (NROW(ud) > 0) {
      return(align_df_cols(
        cbind(Type = "Value set",    builtin),
        cbind(Type = "User-defined", ud)
      ))
    }
    return(cbind(Type = "Value set", builtin))
  }

  # --- Print built-in sets ---
  message("Available national value sets for ", version, " version:")
  print_vs_table(builtin)

  # --- Print user-defined sets ---
  if (NROW(ud) > 0) {
    message("User-defined value sets:")
    print_vs_table(ud)
  } else {
    message("No user-defined value sets available.")
  }

  # --- Optionally print citations ---
  if (show_citation) {
    combined_for_cit <- if (NROW(ud) > 0) align_df_cols(builtin, ud) else builtin
    if ("citation" %in% colnames(combined_for_cit)) {
      message("\nCitations:")
      for (i in seq_len(nrow(combined_for_cit))) {
        cit <- combined_for_cit$citation[i]
        vs  <- combined_for_cit$VS_code[i]
        if (!is.na(cit) && nchar(trimws(cit)) > 0)
          message("[", vs, "] ", cit)
      }
    }
  }

  invisible(NULL)
}


#' @title eq5d
#' @description Get EQ-5D values for the -3L, -5L, crosswalk (-3L value set applied to -5L health states), reverse crosswalk (-5L value set applied to -3L health states), and -Y-3L
#' @param x A vector of 5-digit EQ-5D-3L state indexes or a matrix/data.frame with columns corresponding to EQ-5D state dimensions
#' @param version String indicating which version to use. Options are '5L'  (default), '3L', 'xw', 'xwr', and 'Y3L'. Matching is case-insensitive.
#' @param country A country code or value set code identifying the value set
#'   to use, as listed by \code{eqvs_display()}. Matching is case-insensitive.
#'   For countries with more than one value set (for example Germany, with
#'   \code{"DE_TTO"} and \code{"DE_VAS"}), the value set code must be
#'   given; supplying the country code alone raises an error listing the
#'   available codes.
#' @param dim.names A vector of dimension names to identify dimension columns.
#' @return A numeric vector of values, or a data.frame with one column for
#'   each value set requested. The result is an unnamed numeric vector, one element per
#'   element or row of \code{x} and in the same order.
#' @section Missing and invalid responses:
#' Each health state is checked before it is scored, and a state that cannot
#' be scored gives \code{NA} for that element or row; the others are scored
#' as usual.
#' \itemize{
#'   \item A missing response (\code{NA} in any dimension, or an \code{NA}
#'     state index) gives \code{NA} without a warning.
#'   \item An invalid response gives \code{NA} with a warning, which gives
#'     the number of such states and examples. Invalid means a level the
#'     instrument does not have (for example \code{4} in the EQ-5D-3L, or a
#'     missing-value code such as \code{9}), a level or index that is not a
#'     whole, finite number, or an index that is not five digits long.
#'     Each problem is reported once, however many value sets are requested.
#'   \item State indexes and levels given as text or as factors are read
#'     through their labels, so \code{"21111"} and \code{factor(2)} are
#'     scored as \code{21111} and \code{2}.
#' }
#' Recode missing-value codes to \code{NA} first if the warning is not wanted.
#' @examples 
#' # US -3L value set
#' eq5d(c(11111, 12321, 32123, 33333), 'US', '3L') 
#' # Danish and US -5L value sets applied to -3L descriptives, i.e. reverse crosswalk
#' eq5d(make_all_EQ_states('3L'), c('DK', 'US'), 'XWR') 
#' # US -5L value set
#' eq5d(c(11111, 12321, 32153, 55555), 'US', '5L') 
#' @export
eq5d <- function(x, country = NULL, version = '5L', dim.names = c("mo", "sc", "ua", "pd", "ad")) {
  pkgenv <- getOption("eq.env")
  version <- toupper(version)

  valid_versions <- c('3L', '5L', 'Y3L', 'XW', 'RXW', 'XWR', 'CW', 'CWR', 'RCW')
  if (!version %in% valid_versions) stop("No valid argument for EQ-5D version.")
  
  # Normalize version mapping
  version <- c('3L', '5L', 'Y3L', 'XW', 'XWR', 'XWR', 'XW', 'XWR', 'XWR')[match(version, valid_versions)]
  vers <- c('3L', '5L', 'Y3L', '3L', '5L')[match(version, c('3L', '5L', 'Y3L', 'XW', 'XWR'))]
  
  .check_dim_names(dim.names)

  if (is.matrix(x) || is.data.frame(x)) {
    if (is.null(colnames(x))) {
      message("No column names detected.")
      if (NCOL(x) == 5) {
        message("Assuming dimensions in order: MO, SC, UA, PD, AD.")
        colnames(x) <- dim.names
      }
    }
    # toEQ5Dindex() finds the columns (.resolve_dims()), as every scorer
    # does; an exact-match check here refused names it would have accepted.
    x <- toEQ5Dindex(x = x, dim.names = dim.names, quiet = TRUE)
  }

  # Validate the state codes once, here, before the value sets are split up
  # below: done after the split, every problem was reported once per value
  # set. A code outside the instrument -- mo = 4, or a missing-value code 9,
  # in the EQ-5D-3L -- used to become NA without a word; it is now reported,
  # as the analysis functions report such levels.
  x <- .validate_states(
    x, top = if (version %in% c('3L', 'Y3L', 'XWR')) 3L else 5L,
    instrument = c('3L' = "EQ-5D-3L", '5L' = "EQ-5D-5L", 'Y3L' = "EQ-5D-Y-3L",
                   'XW' = "EQ-5D-5L", 'XWR' = "EQ-5D-3L")[[version]])

  country <- .fixCountries(country, EQvariant = vers)
  
  # Handle cases where the country input is invalid
  if (any(is.na(country))) {
    invalid_countries <- names(country)[is.na(country)]
    warning("The following countries were not found and will be ignored: ", paste(invalid_countries, collapse = ", "))
    country <- country[!is.na(country)]
  }
  
  if (length(country) == 0) {
    message("No valid countries listed. Available value sets are:")
    eqvs_display(version = vers)
    stop("No valid countries listed.")
  }
  
  # If multiple countries, apply the function iteratively
  if (length(country) > 1) {
    names(country) <- country
    return(do.call(cbind, lapply(country, function(count) eq5d(x, count, version, dim.names))))
  }
  
  # Match states. The codes were validated above; NA stays NA.
  
  # Select the appropriate value set and state mapping
  vset <- switch(version,
                 '3L' = pkgenv$vsets3L_combined,
                 '5L' = pkgenv$vsets5L_combined,
                 'XW' = pkgenv$xwsets,
                 'XWR' = pkgenv$xwrsets,
                 'Y3L' = pkgenv$vsetsY3L_combined)
  svec <- switch(version,
                 '3L' = pkgenv$states_3L,
                 '5L' = pkgenv$states_5L,
                 'XW' = pkgenv$states_5L,
                 'XWR' = pkgenv$states_3L,
                 'Y3L' = pkgenv$states_3L)
  
  # Compute EQ-5D values
  xout <- rep(NA_real_, length(x))
  xout[!is.na(x)] <- vset[match(x[!is.na(x)], svec$state), country]
  # Deliberately unnamed. This used to attach the input state codes as names,
  # which then travelled into every data.frame column, plot label and
  # comparison built on the result, and made identical() fail against a plain
  # numeric vector. Nothing in the package looked the names up. Callers who
  # want them can use setNames().
  xout
}

#' @title eq5d3l
#' @description
#' Get EQ-5D-3L index values from individual responses to the five
#' dimensions of the EQ-5D-3L.
#' @param x A vector of 5-digit EQ-5D-3L state indexes, or a matrix/data.frame
#'   with columns corresponding to the EQ-5D-3L dimensions.
#' @param country A country code or value set code identifying the value set
#'   to use, as listed by \code{eqvs_display()}. Matching is case-insensitive.
#'   For countries with more than one value set (for example Germany, with
#'   \code{"DE_TTO"} and \code{"DE_VAS"}), the value set code must be
#'   given; supplying the country code alone raises an error listing the
#'   available codes.
#' @param dim.names A character vector specifying the names of the EQ-5D-3L
#'   dimensions. Default is `c("mo", "sc", "ua", "pd", "ad")`.
#' @return A numeric vector of EQ-5D-3L values, or a data.frame with one column
#'   for each requested value set. The result is an unnamed numeric vector, one element per
#'   element or row of \code{x} and in the same order.
#' @inheritSection eq5d Missing and invalid responses
#' @examples
#' # Example 1: utility values from EQ-5D-3L profile codes
#' eq5d3l(c(11111, 12321, 32123, 33333), country = "US")
#'
#' # Example 2: request multiple value sets
#' eq5d3l(make_all_EQ_states("3L"), country = c("DK", "CA"))
#'
#' # Example 3: use a data.frame with dimension columns
#' df3l <- data.frame(
#'   mo = c(1, 2, 3),
#'   sc = c(1, 2, 2),
#'   ua = c(1, 3, 1),
#'   pd = c(2, 2, 3),
#'   ad = c(1, 1, 2)
#' )
#' eq5d3l(df3l, country = "US")
#'
#' # Example 4: use custom dimension column names
#' df3l_named <- data.frame(
#'   mobility = c(1, 2, 3),
#'   self_care = c(1, 2, 2),
#'   usual_activities = c(1, 3, 1),
#'   pain_discomfort = c(2, 2, 3),
#'   anxiety_depression = c(1, 1, 2)
#' )
#' eq5d3l(
#'   df3l_named,
#'   country = "US",
#'   dim.names = c(
#'     "mobility", "self_care", "usual_activities",
#'     "pain_discomfort", "anxiety_depression"
#'   )
#' )
#' @export
eq5d3l <- function(x, country = NULL, dim.names = c("mo", "sc", "ua", "pd", "ad")){
  eq5d(x = x, country = country, version = "3L", dim.names = dim.names)
}

#' @title eq5d5l
#' @description
#' Get EQ-5D-5L index values from individual responses to the five
#' dimensions of the EQ-5D-5L.
#'
#' @param x A vector of 5-digit EQ-5D-5L state indexes, or a matrix/data.frame
#'   with columns corresponding to the EQ-5D-5L dimensions.
#' @param country A country code or value set code identifying the value set
#'   to use, as listed by \code{eqvs_display()}. Matching is case-insensitive.
#'   For countries with more than one value set (for example Germany, with
#'   \code{"DE_TTO"} and \code{"DE_VAS"}), the value set code must be
#'   given; supplying the country code alone raises an error listing the
#'   available codes.
#' @param dim.names A character vector specifying the names of the EQ-5D-5L
#'   dimensions. Default is `c("mo", "sc", "ua", "pd", "ad")`.
#'
#' @return A numeric vector of EQ-5D-5L values, or a data.frame with one column
#'   for each requested value set. The result is an unnamed numeric vector, one element per
#'   element or row of \code{x} and in the same order.
#'
#' @inheritSection eq5d Missing and invalid responses
#' @examples
#' # Example 1: utility values from EQ-5D-5L profile codes
#' eq5d5l(c(11111, 12321, 32423, 55555), country = "IT")
#'
#' # Example 2: request multiple value sets
#' eq5d5l(make_all_EQ_states("5L"), country = c("ES", "DE"))
#'
#' # Example 3: use a data.frame with dimension columns
#' df5l <- data.frame(
#'   mo = c(1, 2, 5),
#'   sc = c(1, 2, 4),
#'   ua = c(1, 3, 3),
#'   pd = c(2, 4, 2),
#'   ad = c(1, 5, 1)
#' )
#' eq5d5l(df5l, country = "ES")
#'
#' # Example 4: use custom dimension column names from a real-world style dataset
#' df5l_named <- data.frame(
#'   mobility = c(1, 5, 3),
#'   self_care = c(2, 4, 2),
#'   usual_activities = c(3, 3, 1),
#'   pain_discomfort = c(4, 2, 2),
#'   anxiety_depression = c(5, 1, 3)
#' )
#' eq5d5l(
#'   df5l_named,
#'   country = "ES",
#'   dim.names = c(
#'     "mobility", "self_care", "usual_activities",
#'     "pain_discomfort", "anxiety_depression"
#'   )
#' )
#'
#' @export
eq5d5l <- function(x, country = NULL, dim.names = c("mo", "sc", "ua", "pd", "ad")){
  eq5d(x = x, country = country, version = "5L", dim.names = dim.names)
}

#' @title eq5dy3l
#' @description
#' Get EQ-5D-Y-3L index values from individual responses to the five
#' dimensions of the EQ-5D-Y-3L.
#' @param x A vector of 5-digit EQ-5D-Y-3L state indexes, or a matrix/data.frame
#'   with columns corresponding to the EQ-5D-Y-3L dimensions.
#' @param country A country code or value set code identifying the value set
#'   to use, as listed by \code{eqvs_display()}. Matching is case-insensitive.
#'   For countries with more than one value set (for example Germany, with
#'   \code{"DE_TTO"} and \code{"DE_VAS"}), the value set code must be
#'   given; supplying the country code alone raises an error listing the
#'   available codes.
#' @param dim.names A character vector specifying the names of the EQ-5D-Y-3L
#'   dimensions. Default is `c("mo", "sc", "ua", "pd", "ad")`.
#' @return A numeric vector of EQ-5D-Y-3L values, or a data.frame with one
#'   column for each requested value set. The result is an unnamed numeric vector, one element per
#'   element or row of \code{x} and in the same order.
#' @inheritSection eq5d Missing and invalid responses
#' @examples
#' # Example 1: utility values from EQ-5D-Y-3L profile codes
#' eq5dy3l(x = c(11111, 12321, 33333), country = "SI")
#' 
#' # Example 2: request multiple value sets
#' eq5dy3l(make_all_EQ_states("3L"), country = c("ES", "DE"))
#'
#' # Example 3: use a data.frame with dimension columns
#' dfy3l <- data.frame(
#'   mo = c(1, 2, 3),
#'   sc = c(1, 1, 2),
#'   ua = c(1, 2, 3),
#'   pd = c(2, 2, 3),
#'   ad = c(1, 3, 2)
#' )
#' eq5dy3l(dfy3l, country = "SI")
#'
#' # Example 4: use custom dimension column names
#' dfy3l_named <- data.frame(
#'   mobility = c(1, 2, 3),
#'   self_care = c(1, 1, 2),
#'   usual_activities = c(1, 2, 3),
#'   pain_discomfort = c(2, 2, 3),
#'   anxiety_depression = c(1, 3, 2)
#' )
#' eq5dy3l(
#'   dfy3l_named,
#'   country = "SI",
#'   dim.names = c(
#'     "mobility", "self_care", "usual_activities",
#'     "pain_discomfort", "anxiety_depression"
#'   )
#' )
#' @export
eq5dy3l <- function(x, country = NULL, dim.names = c("mo", "sc", "ua", "pd", "ad")){
  eq5d(x = x, country = country, version = "Y3L", dim.names = dim.names)
}
