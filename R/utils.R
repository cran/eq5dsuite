# Wrapper for file.copy that throws if any files fail to copy. Use name
# file.copy2 to make clear that it's a wrapper for a base function, and not for
# fs::file_copy.
file.copy2 <- function(from, to, ...) {
  res <- file.copy(from, to, ...)
  if (!all(res)) {
    stop("Error copying files: ", paste0(from[!res], collapse = ", "))
  }
}

dir.create2 <- function(path, ...) {
  res <- dir.create(path, ...)
  if (!res) {
    stop("Error creating directory: ", path)
  }
}

# Normalise an EQ-5D version argument.
#
# Version arguments are documented as accepting either case, and several
# functions validated them with toupper() but then branched on an upper-case
# literal, so a lower-case version reached the wrong branch: "3l" admitted
# levels 4 and 5 for a three-level instrument, and .get_lfs("12345", "5l")
# returned a three-digit Level Frequency Score. This is the single place where
# a version argument is checked, so that a canonical value reaches every
# branch that depends on it.
#
# `allowed` is the set of canonical versions the calling function accepts;
# `arg` names the argument in the error message.
.norm_version <- function(version, allowed = .cache_versions,
                          arg = "version") {
  if (!is.character(version) || length(version) != 1L || is.na(version))
    stop("Argument '", arg, "' must be a single string, one of: ",
         paste(allowed, collapse = ", "), ".", call. = FALSE)
  out <- toupper(trimws(version))
  if (!out %in% allowed)
    stop("Unknown EQ-5D version '", version, "'. Expected one of: ",
         paste(allowed, collapse = ", "), ".", call. = FALSE)
  out
}

.fixCountries <- function(countries, EQvariant = '5L') {
  pkgenv <- getOption("eq.env")

  EQvariant <- .norm_version(EQvariant, arg = "EQvariant")

  # Fail loudly and specifically on a malformed country table. Previously any
  # structural problem here produced NA, which callers reported as "No valid
  # countries listed" -- pointing the user at their `country` argument rather
  # than at the real fault.
  cc <- pkgenv$country_codes[[EQvariant]]
  problem <- .validate_vs_meta(
    cc, paste0("The built-in value set table for EQ-5D-", EQvariant))
  if (length(problem))
    stop(problem,
         "\n  This indicates a corrupted eq5dsuite installation or package ",
         "environment. Try restarting R and reinstalling eq5dsuite.",
         call. = FALSE)

  ud <- pkgenv[[paste0("user_defined_", EQvariant)]]
  if (!is.null(ud)) {
    problem <- .validate_vs_meta(
      ud, paste0("The user-defined value set table for EQ-5D-", EQvariant))
    if (length(problem)) {
      warning(problem,
              "\n  These user-defined value sets will be ignored for now. ",
              "Re-add them with eqvs_add().", call. = FALSE)
      ud <- NULL
    }
  }

  if (!is.null(ud) && nrow(ud) > 0) {
    # Both tables share the same schema, so no column reconciliation is needed.
    cntrs <- rbind(cc, ud)
    rownames(cntrs) <- NULL
  } else {
    cntrs <- cc
  }

  match_one <- function(country) {
    # An exact value set code always wins over a country code match. Without
    # this, adding a user-defined set that reuses a built-in country code (say
    # countryCode = "GB") would make the built-in "GB" value set unreachable,
    # because every way of naming it would look ambiguous.
    exact_vs <- which(toupper(cntrs$VS_code) == toupper(country))
    if (length(exact_vs) == 1L) {
      return(cntrs$VS_code[exact_vs])
    }

    which(toupper(cntrs$Country_code) == toupper(country) |
            toupper(cntrs$VS_code) == toupper(country), arr.ind = TRUE)
  }

  result <- sapply(countries, function(country) {
    matched_rows <- match_one(country)
    if (is.character(matched_rows)) return(matched_rows)

    # Deprecated alias: the United Kingdom used to be coded "UK"; it is now
    # "GB", the ISO 3166-1 alpha-2 code. Only fall back to "GB" when "UK"
    # matched nothing, so a user-defined value set still coded "UK" keeps
    # working and is never silently redirected.
    if (length(matched_rows) == 0L &&
        is.character(country) && toupper(country) == "UK") {
      gb <- match_one("GB")
      if (length(gb) > 0L) {
        rlang::inform(
          paste0(
            "eq5dsuite: the value set code \"UK\" is deprecated; ",
            "use \"GB\" instead.\n",
            "  The United Kingdom value sets now use the ISO 3166-1 alpha-2 ",
            "code \"GB\".\n",
            "  \"UK\" still works for now and selects the same value set."
          ),
          .frequency    = "once",
          .frequency_id = "eq5dsuite_uk_to_gb"
        )
        if (is.character(gb)) return(gb)
        matched_rows <- gb
      }
    }

    if (length(matched_rows) == 0) {
      return(NA)
    }
    if (length(matched_rows) == 1) {
      return(cntrs$VS_code[matched_rows])
    }

    # More than one value set shares this country code. This used to prompt
    # with readline() in a repeat loop, which never terminates in a
    # non-interactive session and makes the result depend on console input
    # rather than on the call. Fail with an actionable error instead, in every
    # kind of session.
    stop("Multiple value sets are available for country code '", country,
         "': ", paste(cntrs$VS_code[matched_rows], collapse = ", "),
         ". Please specify one of these value set codes. See ",
         "eqvs_display(version = \"", EQvariant, "\") for the full list.",
         call. = FALSE)
  }, USE.NAMES = TRUE)
  
  return(result)
}


find_cache_dir <- function(pkg) {
  # In R 4.0 and above, CRAN wants us to use the new tools::R_user_dir().
  # If not present, fall back to rappdirs::user_cache_dir().
  R_user_dir <- getNamespace('tools')$R_user_dir
  if (!is.null(R_user_dir)) {
    R_user_dir(pkg, which = "cache")
  } else {
    rappdirs::user_cache_dir(pkg, "R")
  }
}

.prettyPrint <- function(df, justify = 'r') {
  cnames <- colnames(df)
  if(length(justify)<length(cnames)) justify <- rep(justify, length(cnames))
  n      <- as.matrix(nchar(cnames))
  
  d <- as.matrix(as.data.frame(apply(df, 2, format, simplify = F)))
  d[which(trimws(d) == "NA", arr.ind = T)] <- ""
  n <- apply(cbind(n, nchar(d[1,])), 1, max)
  
  fmts <- vapply(1:length(justify), FUN.VALUE = 'aha', function(i) paste0("%",ifelse(tolower(justify[[i]]) == 'l','-', ''), n[[i]], "s"))
  for(i in 1:length(cnames)) {
    cnames[i] <- sprintf(fmts[i], cnames[i])
    d[,i] <- sprintf(fmts[i], trimws(d[,i]))
  }
  d <- rbind(cnames, d)
  
  for(i in 1:NROW(d)) cat(d[i,], '\r\n')
}