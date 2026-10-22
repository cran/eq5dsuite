# ---------------------------------------------------------------------------
# Value set cache: schema definition, validation, migration and I/O.
#
# Two unrelated things in this package are called "migration". Keep them apart:
#
#   * Value set MIGRATIONS (R/migrations.R, apply_pending_migrations()) rename
#     published value set codes (e.g. "NL" -> "NL_2006") when a country
#     publishes a second value set. They are driven by a migrations.csv
#     fetched over the network from the value sets repository, are applied
#     from update_value_sets(), and record their state in
#     applied_migrations.rds.
#
#   * Cache SCHEMA migration (this file) upgrades the on-disk layout of the
#     user's own cache file when the package changes the shape of the tables
#     it stores. It is purely local, must work offline, and runs synchronously
#     inside .onLoad(). It deliberately does not go through
#     apply_pending_migrations(), because loading the package must never
#     depend on network access.
# ---------------------------------------------------------------------------

# Current on-disk cache schema.
#
#   1  Legacy, unstamped. Written by eq5dsuite <= 2.0.0. Contains a full dump
#      of the package environment (including copies of built-in data), and a
#      user_defined_* table with no `citation` column.
#   2  Stamped with `cache_schema_version`. Contains only the user's own
#      objects (uservsets*, user_defined_*); user_defined_* matches the
#      built-in country_codes schema exactly.
.cache_schema_version <- 2L

# Basename written by the current version, and the legacy basename carrying a
# stray trailing dot that eqvs_add()/eqvs_drop() used to write (and that
# .onLoad() never read back).
#
# On Windows the two are one file: the Win32 API drops a trailing dot, so
# writing or reading "cache.Rdta." uses "cache.Rdta". A legacy cache there is
# therefore simply found under the current name. That is why nothing here may
# ever delete or rename the legacy name: on Windows it would be the current
# cache.
.cache_basename        <- "cache.Rdta"
.cache_basename_legacy <- "cache.Rdta."

# The EQ-5D instrument versions that can carry user-defined value sets.
.cache_versions <- c("3L", "5L", "Y3L")

# The instrument's name, for messages and for the folders in the value set
# repository. Note "Y-3L", not "Y3L": the version code and the instrument name
# are spelled differently.
.eq5d_instrument <- function(version) {
  switch(version,
    "3L"  = "EQ-5D-3L",
    "5L"  = "EQ-5D-5L",
    "Y3L" = "EQ-5D-Y-3L",
    stop("Unknown version: ", version)
  )
}

# Every value set code that ships with the package, across all instruments.
#
# Taken from both the metadata table and the value set tables themselves: a
# code present in .cntrcodes but missing from the matching .vsets* table is
# filtered out of `country_codes` by .fixPkgEnv(), and it should still be
# refused to a user, so that correcting the omission later cannot collide with
# a set they have already added.
.builtin_vs_codes <- function() {
  codes <- .cntrcodes$VS_code
  for (v in .cache_versions)
    codes <- c(codes, setdiff(colnames(get(paste0(".vsets", v))), "state"))
  sort(unique(codes))
}

# The instruments a built-in value set code is used for, as instrument names.
.builtin_vs_versions <- function(code) {
  hit <- toupper(.cntrcodes$VS_code) == toupper(code)
  versions <- intersect(.cache_versions, unique(.cntrcodes$Version[hit]))
  for (v in .cache_versions)
    if (!v %in% versions &&
        any(toupper(setdiff(colnames(get(paste0(".vsets", v))), "state")) ==
            toupper(code)))
      versions <- c(versions, v)
  vapply(intersect(.cache_versions, versions), .eq5d_instrument, character(1),
         USE.NAMES = FALSE)
}

# Value set codes that a built-in set already uses.
#
# Such a code cannot be carried by a user-defined value set: .fixPkgEnv()
# combines the built-in and user-defined tables, and a shared column name
# becomes part of the join key, so the combined table collapses and every
# lookup for that instrument fails. A code differing only in case is equally
# unusable, because .fixCountries() matches case-insensitively and then reports
# the two sets as ambiguous.
#
# eqvs_add() refuses such a code outright. This is for the codes that reach the
# package by another route: a cache written before that check existed, or one
# holding a code that has since become built in -- the realistic case, since a
# value set that is user-defined today may ship with the package tomorrow.
.colliding_vs_codes <- function(codes) {
  if (!length(codes)) return(character(0))
  codes[toupper(codes) %in% toupper(.builtin_vs_codes())]
}

# The key under which the package environment holds the health states for an
# instrument. The EQ-5D-Y-3L has the same 243 health states as the EQ-5D-3L,
# and the package stores only one copy of them: .fixPkgEnv() seeds
# `uservsetsY3L` from `states_3L`, and eq5d() looks Y3L states up in
# `states_3L`. There is no `states_Y3L`.
.states_key <- function(version) {
  if (version == "5L") "states_5L" else "states_3L"
}

#' Column names and types of the value set metadata tables
#'
#' Derived from the built-in \code{.cntrcodes} table, which is the single
#' source of truth for the schema shared by \code{country_codes} (built in
#' \code{.fixPkgEnv()}) and the \code{user_defined_*} tables (built in
#' \code{eqvs_add()}). Deriving rather than hard-coding means the two cannot
#' drift apart if a column is added to \code{.cntrcodes} later.
#'
#' @return A named character vector mapping column name to storage type.
#' @keywords internal
#' @noRd
.vs_meta_schema <- function() {
  vapply(.cntrcodes, function(x) class(x)[1L], character(1L))
}

#' Coerce a vector to a schema type
#' @keywords internal
#' @noRd
.coerce_col <- function(x, type) {
  switch(type,
    character = as.character(x),
    integer   = as.integer(x),
    numeric   = as.numeric(x),
    double    = as.numeric(x),
    logical   = as.logical(x),
    x
  )
}

#' Build a one-row value set metadata record matching the shared schema
#'
#' Named arguments are matched against the schema; every column that is not
#' supplied (or is supplied as \code{NULL}) is filled with a correctly typed
#' \code{NA}. Guarantees that the row has exactly the columns of
#' \code{country_codes}, in the same order and with the same types.
#'
#' @param ... Named values, one per schema column.
#' @return A one-row data.frame.
#' @keywords internal
#' @noRd
.new_vs_meta_row <- function(...) {
  schema <- .vs_meta_schema()
  vals   <- list(...)

  if (length(vals) && is.null(names(vals)))
    stop("All arguments to .new_vs_meta_row() must be named.", call. = FALSE)
  vals <- vals[!vapply(vals, is.null, logical(1L))]

  unknown <- setdiff(names(vals), names(schema))
  if (length(unknown))
    stop("Unknown value set metadata column(s): ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)

  row <- lapply(names(schema), function(nm) {
    v <- if (nm %in% names(vals)) vals[[nm]] else NA
    if (length(v) == 0L) v <- NA
    .coerce_col(v[[1L]], schema[[nm]])
  })
  names(row) <- names(schema)

  as.data.frame(row, stringsAsFactors = FALSE)
}

#' Validate a value set metadata table against the shared schema
#'
#' @param df The table to check.
#' @param what A short description used to build the message.
#' @return \code{character(0)} if the table is valid, otherwise a single
#'   descriptive string naming exactly what is wrong.
#' @keywords internal
#' @noRd
.validate_vs_meta <- function(df, what = "The value set metadata table") {
  schema <- .vs_meta_schema()

  if (is.null(df))
    return(paste0(what, " is missing."))
  if (!is.data.frame(df))
    return(paste0(what, " is not a data.frame (found ",
                  paste(class(df), collapse = "/"), ")."))

  problems <- character(0)

  missing_cols <- setdiff(names(schema), colnames(df))
  if (length(missing_cols))
    problems <- c(problems, paste0("is missing the column(s) ",
                                   paste(missing_cols, collapse = ", ")))

  extra_cols <- setdiff(colnames(df), names(schema))
  if (length(extra_cols))
    problems <- c(problems, paste0("has unexpected column(s) ",
                                   paste(extra_cols, collapse = ", ")))

  if (!length(missing_cols) && !length(extra_cols) &&
      !identical(colnames(df), unname(names(schema))))
    problems <- c(problems, paste0("has its columns in the wrong order (expected ",
                                   paste(names(schema), collapse = ", "), ")"))

  shared <- intersect(names(schema), colnames(df))
  bad_type <- shared[vapply(shared, function(nm)
    !identical(class(df[[nm]])[1L], unname(schema[[nm]])), logical(1L))]
  if (length(bad_type))
    problems <- c(problems, paste0(
      "has the wrong type for ",
      paste0(bad_type, " (expected ", schema[bad_type],
             ", found ", vapply(bad_type, function(nm) class(df[[nm]])[1L],
                                character(1L)), ")",
             collapse = ", ")))

  if (!length(problems)) return(character(0))
  paste0(what, " ", paste(problems, collapse = "; "), ".")
}

#' Bring a value set metadata table up to the current schema
#'
#' Adds missing columns as typed \code{NA}, drops unexpected columns, restores
#' the canonical column order and coerces types.
#'
#' @param df The table to migrate.
#' @return The migrated table, or \code{NULL} if it cannot be migrated.
#' @keywords internal
#' @noRd
.migrate_vs_meta <- function(df) {
  schema <- .vs_meta_schema()
  if (is.null(df) || !is.data.frame(df)) return(NULL)
  if (!"VS_code" %in% colnames(df)) return(NULL)

  for (nm in setdiff(names(schema), colnames(df)))
    df[[nm]] <- .coerce_col(rep(NA, nrow(df)), schema[[nm]])

  df <- df[, names(schema), drop = FALSE]
  for (nm in names(schema))
    df[[nm]] <- .coerce_col(df[[nm]], schema[[nm]])

  rownames(df) <- NULL
  df
}

#' Validate a user value set matrix (uservsets3L / uservsets5L / uservsetsY3L)
#'
#' @param x The table to check.
#' @param version One of "3L", "5L", "Y3L".
#' @return \code{character(0)} if valid, otherwise a descriptive string.
#' @keywords internal
#' @noRd
.validate_uservsets <- function(x, version) {
  what <- paste0("The user-defined value set table for EQ-5D-", version)

  if (is.null(x))        return(paste0(what, " is missing."))
  if (!is.data.frame(x)) return(paste0(what, " is not a data.frame (found ",
                                       paste(class(x), collapse = "/"), ")."))
  if (!NCOL(x) || !identical(colnames(x)[1L], "state"))
    return(paste0(what, " does not start with a 'state' column."))

  expected <- if (version == "5L") 3125L else 243L
  if (!identical(nrow(x), as.integer(expected)))
    return(paste0(what, " has ", nrow(x), " rows (expected ", expected, ")."))

  # The state universe, not just its size.
  #
  # Row count was the only structural check, so a cache whose rows had been
  # reordered, or whose state keys had all been replaced by one value, passed
  # and was applied. Reordering reversed custom crosswalk values, because the
  # crosswalk multiplies by position; replacing the keys corrupted built-in
  # scoring, because the combined table is built by joining on `state` and a
  # degenerate key set collapses it.
  canonical <- make_all_EQ_indexes(version = if (version == "5L") "5L" else "3L")
  # Whole numbers before integers: as.integer() truncated 11111.5 to the
  # state 11111 (review A11).
  keys <- .parse_number(x$state)
  keys[!is.finite(keys) | keys != trunc(keys)] <- NA
  if (anyNA(keys))
    return(paste0(what, " has ", sum(is.na(keys)),
                  " state key(s) that are not health state codes."))
  if (anyDuplicated(keys))
    return(paste0(what, " repeats ", length(unique(keys[duplicated(keys)])),
                  " state key(s); each state must appear exactly once."))
  if (length(setdiff(keys, canonical)))
    return(paste0(what, " holds ", length(setdiff(keys, canonical)),
                  " state key(s) that are not EQ-5D-", version, " states."))
  if (length(setdiff(canonical, keys)))
    return(paste0(what, " is missing ", length(setdiff(canonical, keys)),
                  " of the ", expected, " EQ-5D-", version, " states."))

  if (NCOL(x) > 1L) {
    value_cols <- colnames(x)[-1L]
    not_num <- value_cols[!vapply(x[value_cols], is.numeric, logical(1L))]
    if (length(not_num))
      return(paste0(what, " has non-numeric value column(s): ",
                    paste(not_num, collapse = ", "), "."))
    if (anyDuplicated(value_cols))
      return(paste0(what, " has duplicated value set code(s): ",
                    paste(unique(value_cols[duplicated(value_cols)]),
                          collapse = ", "), "."))
    # A non-finite utility propagates through every score and every crosswalk
    # as Inf or NaN rather than as a missing value.
    bad_vals <- value_cols[vapply(x[value_cols], function(v)
      any(!is.na(v) & !is.finite(v)), logical(1L))]
    if (length(bad_vals))
      return(paste0(what, " has non-finite value(s) in column(s): ",
                    paste(bad_vals, collapse = ", "), "."))
  }

  character(0)
}

# Put an accepted user value set table into canonical state order.
#
# The crosswalk multiplies a value set by a probability matrix and so depends
# on row order, and the direct lookup is positional. Both are safe once the
# table is ordered by the canonical state vector, whatever order the cache
# happened to hold.
.canonicalise_uservsets <- function(x, version) {
  if (is.null(x) || !is.data.frame(x) || !nrow(x)) return(x)
  canonical <- make_all_EQ_indexes(version = if (version == "5L") "5L" else "3L")
  ord <- match(canonical, .parse_number(x$state))
  if (anyNA(ord)) return(x)        # validation will already have rejected it
  out <- x[ord, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Check that uservsets* and user_defined_* agree on which codes exist
#' @keywords internal
#' @noRd
.validate_cache_consistency <- function(uservsets, user_defined, version) {
  codes_matrix <- if (NCOL(uservsets) > 1L) colnames(uservsets)[-1L] else character(0)
  codes_meta   <- if (!is.null(user_defined) && nrow(user_defined) > 0L)
    as.character(user_defined$VS_code) else character(0)

  only_matrix <- setdiff(codes_matrix, codes_meta)
  only_meta   <- setdiff(codes_meta, codes_matrix)

  if (!length(only_matrix) && !length(only_meta)) return(character(0))

  parts <- character(0)
  if (length(only_matrix))
    parts <- c(parts, paste0("value(s) with no metadata row: ",
                             paste(only_matrix, collapse = ", ")))
  if (length(only_meta))
    parts <- c(parts, paste0("metadata row(s) with no values: ",
                             paste(only_meta, collapse = ", ")))

  paste0("The cached EQ-5D-", version, " value sets are inconsistent: ",
         paste(parts, collapse = "; "), ".")
}

#' Locate the cache file in a directory
#'
#' Prefers the current basename, and falls back to the legacy basename with a
#' trailing dot so that value sets saved by older versions are not lost. The
#' legacy file is only ever read, never renamed or removed.
#'
#' @param dir Directory to look in.
#' @return Path to the cache file, or \code{NULL} if there isn't one.
#' @keywords internal
#' @noRd
.find_cache_file <- function(dir) {
  if (is.null(dir) || !length(dir) || !nzchar(dir[1L])) return(NULL)
  if (!dir.exists(dir[1L])) return(NULL)

  current <- file.path(dir[1L], .cache_basename)
  if (file.exists(current)) return(current)

  legacy <- file.path(dir[1L], .cache_basename_legacy)
  if (file.exists(legacy)) return(legacy)

  NULL
}

#' Read, validate and if necessary migrate a cache file
#'
#' The file is loaded into a temporary environment, never directly into the
#' package environment, so that a corrupt cache cannot leave the package in a
#' half-initialised state. Only the user's own objects are extracted: built-in
#' data is always rebuilt from the installed package by \code{.fixPkgEnv()},
#' so a stale cache can no longer shadow it.
#'
#' This function never writes to disk.
#'
#' @param cache_file Path to the cache file.
#' @return A list with elements \code{status} ("ok", "migrated" or "reject"),
#'   \code{objects} (named list to copy into the package environment, or
#'   \code{NULL}), \code{message} and \code{schema}.
#' @keywords internal
#' @noRd
.read_cache <- function(cache_file) {
  reject <- function(msg, schema = "unknown") {
    list(status = "reject", objects = NULL, schema = schema,
         message = paste0(
           "eq5dsuite: the value set cache at '", cache_file,
           "' could not be used and has been ignored.\n  ", msg,
           "\n  The file has been left unchanged. The built-in value sets are ",
           "available as usual; any custom value sets can be re-added with ",
           "eqvs_add()."))
  }

  tmp <- new.env(parent = emptyenv())
  err <- tryCatch({
    suppressWarnings(load(cache_file, envir = tmp))
    NULL
  }, error = function(e) conditionMessage(e))

  if (!is.null(err))
    return(reject(paste0("The file could not be read: ", err, ".")))

  # ---- schema version -----------------------------------------------------
  if (exists("cache_schema_version", envir = tmp, inherits = FALSE)) {
    schema <- get("cache_schema_version", envir = tmp, inherits = FALSE)
    # Checked as a number before it becomes an integer: as.integer(Inf) is NA,
    # and the comparison below then failed with an error that stopped the
    # package from loading; 2.9 was read as 2, and 0 or -1 as an old schema
    # (review A10).
    if (!is.numeric(schema) || length(schema) != 1L || !is.finite(schema) ||
        schema != trunc(schema) || schema < 1)
      return(reject("Its schema version is not a whole number of at least 1."))
    if (schema > .cache_schema_version)
      return(reject(paste0(
        "It was written with cache schema version ", format(schema),
        ", but this version of eq5dsuite understands at most version ",
        .cache_schema_version,
        ". This usually means it was written by a newer eq5dsuite; ",
        "upgrading the package should make it readable again."), schema))
    schema <- as.integer(schema)
  } else {
    # Unstamped: written before schema versioning was introduced.
    schema <- 1L
  }

  # ---- extract and migrate the user's objects -----------------------------
  objects <- list()
  changed <- schema < .cache_schema_version
  migrated_meta <- character(0)
  collisions <- character(0)

  for (version in .cache_versions) {
    uservsets_str    <- paste0("uservsets", version)
    user_defined_str <- paste0("user_defined_", version)

    has_matrix <- exists(uservsets_str, envir = tmp, inherits = FALSE)
    has_meta   <- exists(user_defined_str, envir = tmp, inherits = FALSE)

    if (!has_matrix && !has_meta) next

    if (!has_matrix)
      return(reject(paste0(
        "It holds metadata for EQ-5D-", version,
        " value sets but not the values themselves."), schema))

    uservsets <- get(uservsets_str, envir = tmp, inherits = FALSE)
    problem   <- .validate_uservsets(uservsets, version)
    if (length(problem)) return(reject(problem, schema))
    # Accepted, so put it in canonical state order: the crosswalk multiplies
    # by position and the direct lookup is positional, so a cache written in
    # another order would otherwise give different values for the same set.
    uservsets <- .canonicalise_uservsets(uservsets, version)
    assign(uservsets_str, uservsets, envir = tmp)

    user_defined <- NULL
    if (has_meta) {
      user_defined <- get(user_defined_str, envir = tmp, inherits = FALSE)
      if (!is.null(user_defined)) {
        if (length(.validate_vs_meta(user_defined))) {
          fixed <- .migrate_vs_meta(user_defined)
          if (is.null(fixed))
            return(reject(.validate_vs_meta(
              user_defined,
              paste0("Its metadata table for EQ-5D-", version,
                     " value sets")), schema))
          user_defined  <- fixed
          changed       <- TRUE
          migrated_meta <- c(migrated_meta, version)
        }
      }
    }

    problem <- .validate_cache_consistency(uservsets, user_defined, version)
    if (length(problem)) return(reject(problem, schema))

    # Drop any value set whose code a built-in set already uses, rather than
    # rejecting the whole cache: the user's other value sets are fine and
    # should survive. Done after the consistency check, so a cache that really
    # is inconsistent is still reported as such, and from both tables at once,
    # so they stay in step. The file on disk is not touched.
    codes <- if (NCOL(uservsets) > 1L) colnames(uservsets)[-1L] else character(0)
    clash <- .colliding_vs_codes(codes)
    if (length(clash)) {
      collisions <- c(collisions,
                      paste0(.eq5d_instrument(version), ": ",
                             paste(clash, collapse = ", ")))
      uservsets <- uservsets[, !colnames(uservsets) %in% clash, drop = FALSE]
      if (!is.null(user_defined) && nrow(user_defined) > 0L)
        user_defined <- user_defined[!user_defined$VS_code %in% clash, ,
                                     drop = FALSE]
    }

    objects[[uservsets_str]] <- uservsets
    if (!is.null(user_defined) && nrow(user_defined) > 0L)
      objects[[user_defined_str]] <- user_defined
  }

  if (!changed)
    return(list(status = "ok", objects = objects, schema = schema,
                message = character(0), collisions = collisions))

  detail <- if (length(migrated_meta))
    paste0(" Missing value set details (such as the citation) were filled in ",
           "as NA for EQ-5D-", paste(migrated_meta, collapse = ", "), ".")
  else ""

  list(
    status     = "migrated",
    objects    = objects,
    schema     = schema,
    collisions = collisions,
    message = paste0(
      "eq5dsuite: the value set cache at '", cache_file,
      "' was written with cache schema version ", schema,
      " and has been migrated to version ", .cache_schema_version,
      " for this session.", detail,
      "\n  Your custom value sets have been kept. The upgraded cache will be ",
      "written to disk the next time you save one with eqvs_add() or ",
      "eqvs_drop()."))
}

#' Copy a cache into the package environment
#'
#' Applies the result of \code{.read_cache()}, emitting a startup message for a
#' migrated cache or a warning for one that had to be ignored. Records a
#' rejected file so that \code{.save_cache()} can back it up before a later
#' save overwrites it.
#'
#' This function never writes to disk.
#'
#' @param pkgenv The package environment.
#' @param cache_path Directory holding the cache.
#' @return Invisibly, the status string, or \code{NULL} if there was no cache.
#' @keywords internal
#' @noRd
.apply_cache <- function(pkgenv, cache_path) {
  cache_file <- .find_cache_file(cache_path)
  if (is.null(cache_file)) return(invisible(NULL))

  # .read_cache() reports every problem it knows of as a rejection. Anything
  # it did not foresee is one too: an error here would stop the package from
  # loading, which is worse than ignoring the cache (review A10).
  res <- tryCatch(.read_cache(cache_file), error = function(e) list(
    status = "reject", objects = NULL, schema = "unknown",
    message = paste0("eq5dsuite: the value set cache at '", cache_file,
                     "' could not be used and has been ignored.\n  It could ",
                     "not be checked: ", conditionMessage(e), "\n  The file ",
                     "has been left unchanged.")))

  if (identical(res$status, "reject")) {
    # Remember the file so a later save backs it up instead of silently
    # destroying value sets we could not read.
    assign(".cache_rejected", list(file = cache_file, schema = res$schema),
           envir = pkgenv)
    warning(res$message, call. = FALSE)
    return(invisible(res$status))
  }

  for (nm in names(res$objects))
    assign(nm, res$objects[[nm]], envir = pkgenv)

  if (length(res$collisions))
    warning("eq5dsuite: the value set cache at '", cache_file,
            "' holds value set(s) whose code a built-in value set already ",
            "uses. They have been ignored:\n  ",
            paste(res$collisions, collapse = "\n  "),
            "\n  Keeping them would break every lookup for that instrument. ",
            "This usually means a value set you added yourself now ships with ",
            "the package.\n  If you still need your own version, re-add it ",
            "under a different code with eqvs_add(). The cache file is ",
            "unchanged until the next eqvs_add() or eqvs_drop() rewrites it.",
            call. = FALSE)

  if (identical(res$status, "migrated")) {
    assign(".cache_migrated_from", res$schema, envir = pkgenv)
    packageStartupMessage(res$message)
  }

  invisible(res$status)
}

#' Objects belonging to the user that are worth caching
#' @keywords internal
#' @noRd
# The user's value set state, for restoring after a failed operation.
#
# eqvs_add() and eqvs_drop() change the runtime tables and then try to
# persist them. A failed write used to leave the session changed and the file
# on disk not, with no way to tell. Both now roll back, so "the operation
# failed" means the same thing in memory as on disk.
.vs_state_snapshot <- function(pkgenv) {
  nms <- .cache_user_objects(pkgenv)
  stats::setNames(
    lapply(nms, function(n) get(n, envir = pkgenv, inherits = FALSE)), nms)
}

.vs_state_restore <- function(pkgenv, snap) {
  for (n in setdiff(.cache_user_objects(pkgenv), names(snap)))
    rm(list = n, envir = pkgenv)
  for (n in names(snap)) assign(n, snap[[n]], envir = pkgenv)
  suppressMessages(.fixPkgEnv(saveCache = FALSE))
  invisible(NULL)
}

.cache_user_objects <- function(pkgenv) {
  nms <- c(paste0("uservsets", .cache_versions),
           paste0("user_defined_", .cache_versions))
  nms[vapply(nms, function(n) exists(n, envir = pkgenv, inherits = FALSE),
             logical(1L))]
}

#' Write the user's value sets to a cache file
#'
#' Saves only the user's own objects plus the schema stamp; built-in data is
#' deliberately not cached, so that it is always rebuilt from the installed
#' package. If the target is a cache that had to be ignored at load time, it is
#' backed up first so the unreadable file is not lost.
#'
#' All I/O is wrapped so that a read-only location produces a warning rather
#' than an error.
#'
#' @param pkgenv The package environment.
#' @param filePath Full path of the cache file to write.
#' @return Invisibly \code{TRUE} on success, \code{FALSE} otherwise.
#' @keywords internal
#' @noRd
.save_cache <- function(pkgenv, filePath) {
  ok <- tryCatch({
    target_dir <- dirname(filePath)
    if (!dir.exists(target_dir))
      dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(target_dir))
      stop("the directory '", target_dir, "' does not exist and could not be created")

    # A cache that was rejected on load is copied aside before it is
    # replaced. If that copy is needed and does not succeed, nothing is
    # written: the save used to go ahead, replacing the only copy of value
    # sets that could not be read (review Q05).
    if (.backup_required(pkgenv, filePath) &&
        !isTRUE(.backup_rejected_cache(pkgenv, filePath)))
      stop("the unreadable cache at '", filePath, "' could not be backed up, ",
           "so it has not been replaced")

    tmpenv <- new.env(parent = emptyenv())
    for (nm in .cache_user_objects(pkgenv))
      assign(nm, get(nm, envir = pkgenv, inherits = FALSE), envir = tmpenv)
    assign("cache_schema_version", .cache_schema_version, envir = tmpenv)

    # Written beside the target and moved onto it, so a write that fails
    # part way through leaves the previous usable cache in place. save()
    # straight to filePath truncated the old file first, and an interrupted
    # or failing write destroyed value sets that were still fine.
    tmpfile <- tempfile(paste0(basename(filePath), "."), tmpdir = target_dir)
    on.exit(if (file.exists(tmpfile)) unlink(tmpfile), add = TRUE)
    save(list = ls(tmpenv, all.names = TRUE), envir = tmpenv, file = tmpfile)
    # Readable before it replaces anything: a file that cannot be loaded back
    # is worse than no new file at all.
    if (!file.exists(tmpfile))
      stop("the cache could not be written to a temporary file in '",
           target_dir, "'")
    probe <- new.env(parent = emptyenv())
    load(tmpfile, envir = probe)
    if (!length(ls(probe, all.names = TRUE)))
      stop("the cache written to a temporary file held nothing")
    # And readable by the package itself. A file that loads but that
    # .read_cache() rejects would be ignored at the next start, taking every
    # value set in it with it, including ones that were fine (review A02).
    checked <- .read_cache(tmpfile)
    if (identical(checked$status, "reject"))
      stop("the new cache would not be usable when the package is next ",
           "loaded, so the previous one has been kept: ",
           sub("^.*has been ignored\\.\n  ", "",
               sub("\n  The file has been left unchanged.*$", "", checked$message)))
    if (!file.rename(tmpfile, filePath)) {
      # Same-directory rename, so this is a permissions or filesystem
      # failure rather than a cross-device move.
      stop("the new cache could not be moved into place at '", filePath, "'")
    }

    # The cache on disk now matches the current schema.
    if (exists(".cache_migrated_from", envir = pkgenv, inherits = FALSE))
      rm(".cache_migrated_from", envir = pkgenv)

    TRUE
  }, error = function(e) {
    warning("eq5dsuite: could not write the value set cache to '", filePath,
            "': ", conditionMessage(e),
            "\n  Your value sets are available for this session but have not ",
            "been saved.", call. = FALSE)
    FALSE
  })

  invisible(isTRUE(ok))
}

#' Back up a cache file that was ignored at load time
#'
#' Called only from \code{.save_cache()}, i.e. at the moment a save would
#' overwrite the unreadable file.
#' @keywords internal
#' @noRd
.backup_rejected_cache <- function(pkgenv, filePath) {
  if (!.backup_required(pkgenv, filePath)) return(invisible(FALSE))
  rejected <- get(".cache_rejected", envir = pkgenv, inherits = FALSE)

  # A name not already taken, so an earlier backup is never overwritten.
  stem <- paste0(filePath, ".bak-", rejected$schema, "-",
                 format(Sys.Date(), "%Y%m%d"))
  backup <- stem
  k <- 1L
  while (file.exists(backup)) {
    k <- k + 1L
    backup <- paste0(stem, "-", k)
  }
  ok <- isTRUE(.copy_backup(rejected$file, backup)) && file.exists(backup) &&
    identical(unname(file.size(backup)), unname(file.size(rejected$file)))
  if (ok) {
    message("eq5dsuite: the unreadable cache was backed up to '", backup, "'.")
    rm(".cache_rejected", envir = pkgenv)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

# Whether the file about to be replaced is a cache that was rejected on load
# and has not yet been backed up.
.backup_required <- function(pkgenv, filePath) {
  if (!exists(".cache_rejected", envir = pkgenv, inherits = FALSE)) return(FALSE)
  rejected <- get(".cache_rejected", envir = pkgenv, inherits = FALSE)
  if (!file.exists(filePath) || !file.exists(rejected$file)) return(FALSE)
  identical(normalizePath(filePath, mustWork = FALSE),
            normalizePath(rejected$file, mustWork = FALSE))
}

# The copy itself, never over an existing file. Separate so a test can make
# it fail.
.copy_backup <- function(from, to) file.copy(from, to, overwrite = FALSE)
