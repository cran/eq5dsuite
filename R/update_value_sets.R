# Base URL for the value sets repository
.vs_base_url <- paste0(
  "https://raw.githubusercontent.com/",
  "MathsInHealth/eq5dsuite-value-sets/main/"
)

# Map instrument version to folder name. The folders are named after the
# instrument, so this is the instrument name; see .eq5d_instrument().
.vs_folder <- function(version) .eq5d_instrument(version)

# Read a CSV file from the value sets repository. Every download goes through
# here, with time limits, so a stalled connection cannot hang the session;
# read.csv() opens the connection and closes it again (review AI02).
.remote_csv <- function(url) {
  h <- curl::new_handle(connecttimeout = 15, timeout = 60)
  utils::read.csv(curl::curl(url, handle = h), stringsAsFactors = FALSE)
}

#' Fetch the index of available value sets from GitHub
#'
#' @param version Character. One of "3L", "5L", or "Y3L".
#' @return A data frame with columns Version, Name, Name_short,
#'   Country_code, VS_code, doi. Returns NULL if fetch fails.
#' @keywords internal
fetch_available_value_sets <- function(version) {
  url <- paste0(
    .vs_base_url,
    .vs_folder(version),
    "/value_sets.csv"
  )
  tryCatch(.remote_csv(url),
  error = function(e) {
    message("eq5dsuite: Could not fetch value set index for ",
            version, ": ", conditionMessage(e))
    NULL
  })
}

# Whether there is a connection at all. A function of the package's own, so
# the tests can replace it without touching curl.
.has_internet <- function() curl::has_internet()

# Read one instrument's index, and say whether it can be relied on.
#
# A download that fails, or an index without the columns the installer uses,
# or without a usable VS_code for every row, is a failed check -- never an
# empty list of value sets. An index with the right columns and no rows is a
# valid answer: nothing is published.
#
# Returns list(ok, data, reason).
.check_vs_index <- function(version) {
  idx <- tryCatch(fetch_available_value_sets(version),
                  error = function(e) structure(conditionMessage(e),
                                                class = "vs_fetch_error"))
  if (inherits(idx, "vs_fetch_error"))
    return(list(ok = FALSE, data = NULL,
                reason = paste0("the index could not be downloaded (",
                                unclass(idx), ")")))
  if (is.null(idx))
    return(list(ok = FALSE, data = NULL,
                reason = "the index could not be downloaded"))
  if (!is.data.frame(idx))
    return(list(ok = FALSE, data = NULL,
                reason = "malformed index: not a table"))
  need <- c("Name", "Country_code", "VS_code", "doi")
  miss <- setdiff(need, names(idx))
  if (length(miss))
    return(list(ok = FALSE, data = NULL,
                reason = paste0("malformed index: no ",
                                paste(miss, collapse = ", "), " column")))
  code <- as.character(idx$VS_code)
  if (anyNA(code) || any(!nzchar(trimws(code))))
    return(list(ok = FALSE, data = NULL,
                reason = "malformed index: a value set without a VS_code"))
  if (anyDuplicated(toupper(code)))
    return(list(ok = FALSE, data = NULL,
                reason = "malformed index: a VS_code listed twice"))
  list(ok = TRUE, data = idx, reason = NULL)
}

#' Get the VS_codes currently installed for a given instrument
#'
#' Returns all installed VS_codes for the specified instrument
#' version, including both built-in value sets shipped with the
#' package and any user-added value sets installed via
#' \code{eqvs_add()} or \code{update_value_sets()}.
#'
#' Reads directly from the runtime package environment
#' (\code{getOption("eq.env")}) to include user-added value sets
#' without triggering the display side-effects of
#' \code{eqvs_display()}.
#'
#' @param version Character. One of "3L", "5L", or "Y3L".
#' @return A character vector of VS_codes.
#' @keywords internal
get_installed_vs_codes <- function(version) {
  tryCatch({
    pkgenv <- getOption("eq.env")
    version_upper    <- toupper(version)
    user_defined_str <- paste0("user_defined_", version_upper)

    built_in     <- pkgenv$country_codes[[version_upper]]
    user_defined <- pkgenv[[user_defined_str]]

    built_in_codes <- if (!is.null(built_in) && nrow(built_in) > 0)
      built_in$VS_code else character(0)
    user_codes <- if (NROW(user_defined) > 0)
      user_defined$VS_code else character(0)

    c(built_in_codes, user_codes)
  },
  error = function(e) {
    # Fall back to namespace if pkgenv is unavailable
    cc <- get(".cntrcodes", envir = asNamespace("eq5dsuite"))
    cc$VS_code[cc$Version == version]
  })
}

#' Download and install a single value set
#'
#' @param vs_code Character. The VS_code to download.
#' @param version Character. One of "3L", "5L", or "Y3L".
#' @param meta_row A single-row data frame from the index CSV.
#' @return Logical. TRUE if successful, FALSE otherwise.
#' @keywords internal
install_value_set <- function(vs_code, version, meta_row) {
  url <- paste0(
    .vs_base_url,
    .vs_folder(version), "/",
    vs_code, ".csv"
  )
  vs_data <- tryCatch(.remote_csv(url),
  error = function(e) {
    message("eq5dsuite: Could not download ", vs_code,
            ": ", conditionMessage(e))
    NULL
  })
  
  if (is.null(vs_data)) return(FALSE)
  
  if (!all(c("state", "value") %in% colnames(vs_data))) {
    message("eq5dsuite: Invalid format for ", vs_code,
            " \u2014 expected columns 'state' and 'value'")
    return(FALSE)
  }
  
  expected_rows <- switch(version,
                          "3L"  = 243,
                          "5L"  = 3125,
                          "Y3L" = 243
  )
  
  if (nrow(vs_data) != expected_rows) {
    message("eq5dsuite: ", vs_code, " has ", nrow(vs_data),
            " rows (expected ", expected_rows, ")")
    return(FALSE)
  }
  
  tryCatch({
    res <- eqvs_add(
      vs_data,
      version     = version,
      country     = meta_row$Name,
      countryCode = meta_row$Country_code,
      VSCode      = vs_code,
      description = paste0("doi:", meta_row$doi),
      saveOption  = 2
    )
    # eqvs_add() returns 0, with a warning, when the code is already one of
    # the user's own value sets, and adds nothing. That was counted as an
    # installation (review AI02).
    if (identical(res, 0)) {
      message("eq5dsuite: Could not install ", vs_code, ": the code is ",
              "already used by one of your own value sets.")
      return(FALSE)
    }
    TRUE
  },
  error = function(e) {
    message("eq5dsuite: Could not install ", vs_code,
            ": ", conditionMessage(e))
    FALSE
  })
}

#' Drop a value set from the installed library
#'
#' Removes a value set from the installed library using
#' \code{eqvs_drop()}. Only user-added value sets can be
#' removed — built-in value sets are protected.
#'
#' @param vs_code Character. The VS_code to remove.
#' @param version Character. One of "3L", "5L", or "Y3L".
#' @param ask Logical. Whether to ask for confirmation.
#'   Defaults to TRUE.
#' @return Logical. TRUE if successful, FALSE otherwise.
#' @keywords internal
drop_value_set <- function(vs_code, version, ask = TRUE) {

  installed <- get_installed_vs_codes(version)
  if (!vs_code %in% installed) {
    message("eq5dsuite: ", vs_code, " is not installed.")
    return(FALSE)
  }

  if (ask && interactive()) {
    message("Remove ", vs_code, " (EQ-5D-", version, ")?")
    response <- readline(
      "Enter [y] to confirm or [n] to cancel: "
    )
    if (tolower(trimws(response)) != "y") {
      message("eq5dsuite: Removal of ", vs_code, " cancelled.")
      return(FALSE)
    }
  }

  tryCatch({
    # ask = FALSE: confirmation, if any, has already been obtained above.
    removed <- suppressMessages(
      eqvs_drop(
        country    = vs_code,
        version    = version,
        saveOption = 2,
        ask        = FALSE
      )
    )
    if (isTRUE(removed)) {
      message("\u2705 ", vs_code, " removed successfully.")
    } else {
      message("\u274c ", vs_code, " could not be removed.")
    }
    isTRUE(removed)
  },
  error = function(e) {
    message("\u274c Could not remove ", vs_code,
            ": ", conditionMessage(e))
    FALSE
  })
}

#' Rename a value set
#'
#' Renames a \emph{user-defined} value set, preserving its values and its
#' metadata: only the identifier changes. The two tables are rebuilt as
#' locals, checked, and committed together, so a rename that cannot complete
#' leaves the session and the cache exactly as they were.
#'
#' A \strong{built-in} code is not renamed at runtime. It is part of the
#' installed package's data, and changing it in one session would mean two
#' installations of the same version disagreeing about what a code means. A
#' built-in code change ships in a release; where backward compatibility is
#' needed the old code is kept as a deprecated alias in
#' \code{.fixCountries()}, as \code{"UK"} is for \code{"GB"}. Asked to
#' rename a built-in, this function explains that and returns \code{FALSE}
#' without changing anything.
#'
#' @param old_vs_code Character. The current VS_code.
#' @param new_vs_code Character. The new VS_code to use.
#' @param version Character. One of "3L", "5L", or "Y3L".
#' @param ask Logical. Whether to ask for confirmation.
#'   Defaults to TRUE.
#' @return Logical. TRUE if successful, FALSE otherwise.
#' @keywords internal
rename_value_set <- function(old_vs_code,
                             new_vs_code,
                             version,
                             ask = TRUE) {

  pkgenv <- getOption("eq.env")
  version_upper    <- toupper(version)
  uservsets_str    <- paste0("uservsets", version_upper)
  user_defined_str <- paste0("user_defined_", version_upper)

  installed <- get_installed_vs_codes(version)

  if (!old_vs_code %in% installed) {
    message("eq5dsuite: ", old_vs_code, " is not installed.")
    return(FALSE)
  }

  # Case-insensitively, as a lookup is: renaming to a code that differs from
  # an existing one only in case would make both unusable.
  if (any(toupper(installed) == toupper(new_vs_code))) {
    message("eq5dsuite: ", new_vs_code,
            " already exists (codes differing only in case count as the ",
            "same). Drop it first.")
    return(FALSE)
  }

  # Which kind of value set is this?
  #
  # The two halves used to be incompatible: the values were read from the
  # namespace's built-in tables, so a user-defined set's data could never be
  # found, while the removal went through eqvs_drop(), which only removes
  # user-defined sets. No value set could satisfy both, so every rename
  # returned FALSE.
  user_codes <- if (NROW(pkgenv[[user_defined_str]]) > 0L)
    pkgenv[[user_defined_str]]$VS_code else character(0)
  is_user <- old_vs_code %in% user_codes

  if (!is_user) {
    # A built-in code is part of the installed package's data. It is not
    # renamed at runtime: the change ships in a release, so that every
    # installation of a given version agrees on what a code means. Where
    # backward compatibility is needed, .fixCountries() accepts the old code
    # as a deprecated alias, as it does for "UK" -> "GB".
    message("eq5dsuite: ", old_vs_code, " is a built-in value set for EQ-5D-",
            version, ". Built-in codes are changed by a package release, not ",
            "at runtime, so that every installation of a version agrees on ",
            "what a code means. Nothing has been changed.")
    return(FALSE)
  }

  if (ask && interactive()) {
    message("Rename ", old_vs_code, " to ", new_vs_code,
            " (EQ-5D-", version, ")?")
    response <- readline(
      "Enter [y] to confirm or [n] to cancel: "
    )
    if (tolower(trimws(response)) != "y") {
      message("eq5dsuite: Rename cancelled.")
      return(FALSE)
    }
  }

  # Transactional: both tables are built as locals, checked, and only then
  # committed together. On any failure the snapshot is put back, so a failed
  # rename leaves the session exactly as it was.
  before_vs   <- pkgenv[[uservsets_str]]
  before_meta <- pkgenv[[user_defined_str]]

  if (is.null(before_vs) || !old_vs_code %in% colnames(before_vs)) {
    message("eq5dsuite: Could not extract data for ", old_vs_code)
    return(FALSE)
  }

  after_vs <- before_vs
  colnames(after_vs)[colnames(after_vs) == old_vs_code] <- new_vs_code

  after_meta <- before_meta
  row <- after_meta$VS_code == old_vs_code
  after_meta$VS_code[row]    <- new_vs_code
  after_meta$Name[row]       <- gsub(old_vs_code, new_vs_code,
                                     after_meta$Name[row], fixed = TRUE)
  after_meta$Name_short[row] <- gsub(old_vs_code, new_vs_code,
                                     after_meta$Name_short[row], fixed = TRUE)

  clash <- .colliding_vs_codes(new_vs_code)
  if (length(clash)) {
    message("eq5dsuite: ", new_vs_code,
            " is already used by a built-in value set. Nothing has been ",
            "changed.")
    return(FALSE)
  }

  ok <- tryCatch({
    assign(uservsets_str,    after_vs,   envir = pkgenv)
    assign(user_defined_str, after_meta, envir = pkgenv)
    filePath <- file.path(pkgenv$cache_path, .cache_basename)
    saved <- .fixPkgEnv(saveCache = TRUE, filePath = filePath)
    if (!isTRUE(saved))
      stop("the value set cache could not be written to ", filePath,
           call. = FALSE)
    TRUE
  }, error = function(e) {
    # Put the session back as it was, including the derived tables.
    assign(uservsets_str,    before_vs,   envir = pkgenv)
    assign(user_defined_str, before_meta, envir = pkgenv)
    suppressMessages(.fixPkgEnv(saveCache = FALSE))
    message("\u274c Could not rename ", old_vs_code, " to ", new_vs_code,
            ": ", conditionMessage(e), ". Nothing has been changed.")
    FALSE
  })

  if (isTRUE(ok))
    message("\u2705 ", old_vs_code, " renamed to ", new_vs_code)
  ok
}

#' Check for conflicts between user-defined and built-in value sets
#'
#' After applying migrations or installing new value sets, checks
#' whether any built-in value sets share a VS_code with a migration
#' that has been applied. If conflicts are found, warns the user
#' that the built-in value set still exists and will be removed
#' in the next package release.
#'
#' @param version Character. One of "3L", "5L", or "Y3L".
#' @return Invisibly returns a character vector of conflicting
#'   VS_codes, or character(0) if none found.
#' @keywords internal
check_builtin_conflicts <- function(version) {

  # Get built-in VS_codes from package namespace
  cc <- get(".cntrcodes", envir = asNamespace("eq5dsuite"))
  builtin_codes <- cc$VS_code[cc$Version == version]

  # Get all installed codes including user-defined
  all_codes <- get_installed_vs_codes(version)

  # User-defined codes are those not in the built-in list
  user_codes <- setdiff(all_codes, builtin_codes)

  # Fetch migrations to find shadowed built-in codes
  # A built-in is shadowed when:
  # - old_VS_code still exists as a built-in
  # - new_VS_code exists as a user-defined set
  migrations <- tryCatch(
    fetch_migrations(),
    error = function(e) NULL
  )

  shadowed <- character(0)

  if (!is.null(migrations)) {
    version_migrations <- migrations[
      migrations$version == version, ,
      drop = FALSE
    ]
    for (i in seq_len(nrow(version_migrations))) {
      old_code <- trimws(version_migrations$old_VS_code[i])
      new_code <- trimws(version_migrations$new_VS_code[i])
      if (old_code %in% builtin_codes &&
          new_code %in% user_codes) {
        shadowed <- c(shadowed, old_code)
      }
    }
  }

  if (length(shadowed) > 0) {
    message(
      "\n\u26a0\ufe0f  Warning: The following EQ-5D-", version,
      " value set(s) have been superseded by a renamed version:\n",
      paste0("  - ", shadowed, collapse = "\n"), "\n",
      "  The old codes above are still present in the package ",
      "but will be removed in the next release.\n",
      "  To avoid this warning, switch to the new VS_code ",
      "when calculating values."
    )
  }

  invisible(shadowed)
}

#' Update eq5dsuite value sets
#'
#' Connects to the eq5dsuite value sets repository and checks
#' whether new value sets are available for any of the three
#' EQ-5D instruments. Optionally drop or rename existing value
#' sets before checking for updates.
#'
#' This function requires an internet connection for checking
#' and installing new value sets. Drop and rename operations
#' work offline.
#'
#' @param versions Character vector. Which instruments to check: any of
#'   \code{"3L"}, \code{"5L"} and \code{"Y3L"}, in any case. The date of the
#'   last check, which decides when you are reminded to check again, moves only
#'   when all three are checked.
#'   Defaults to c("3L", "5L", "Y3L").
#' @param ask Logical. Whether to ask for confirmation before
#'   installing, dropping, or renaming. Defaults to TRUE.
#' @param drop Character vector of VS_codes to remove, in the
#'   format "VS_code:version" e.g. c("NL:3L", "US:5L").
#'   Defaults to NULL (no removals).
#' @param rename Named character vector of renames in the format
#'   c("old_VS_code:version" = "new_VS_code") e.g.
#'   c("NL:3L" = "NL_2006"). Defaults to NULL (no renames).
#' @details
#' An instrument counts as checked only when its index was downloaded and is
#' valid. One that could not be downloaded, or whose index is malformed, is
#' reported as failed, with the reason, and is not reported as up to date.
#' The date of the last check, which the reminder on loading the package
#' uses, is moved on only when every requested instrument was checked and
#' every new value set installed; otherwise the reminder stays due.
#' @return Invisibly returns a list with elements
#'   \code{checked} (the instruments whose index was read and valid),
#'   \code{failed} (a named character vector: for each instrument that
#'   could not be checked, the reason), \code{new}, \code{installed},
#'   \code{install_failed} (new value sets that could not be installed),
#'   \code{migration_conflicts} (renames not applied because both codes are
#'   installed; both value sets are kept), \code{migrations_unrecorded}
#'   (renames made, and kept, whose record could not be saved),
#'   \code{dropped}, and \code{renamed}. A list of renames that could not be
#'   downloaded, or is malformed, or renames that could not be recorded,
#'   appear in \code{failed} as \code{"migrations"}; that, or a conflict,
#'   leaves the check incomplete, so the date of the last check is not moved
#'   on. A rename made but not recorded is recorded by the next run, without
#'   being made again.
#' @export
#' @examples
#' \dontrun{
#' # Check for new value sets interactively
#' update_value_sets()
#'
#' # Check specific instrument only
#' update_value_sets(versions = "5L")
#'
#' # Drop a value set
#' update_value_sets(drop = "NL:3L")
#'
#' # Rename a value set
#' update_value_sets(rename = c("NL:3L" = "NL_2006"))
#'
#' # Drop and rename in one call
#' update_value_sets(
#'   drop   = "DE:3L",
#'   rename = c("NL:3L" = "NL_2006", "SI:3L" = "SI_TTO")
#' )
#'
#' # Non-interactive update
#' update_value_sets(ask = FALSE)
#' }
update_value_sets <- function(versions = c("3L", "5L", "Y3L"),
                              ask      = TRUE,
                              drop     = NULL,
                              rename   = NULL) {

  # Value sets live in an environment shared by the whole R process, and the
  # cache they are written to is shared by every session on the server. An app
  # on a public server must not change either, so this refuses there. See
  # DEPLOY.md.
  if (.online_now())
    stop("update_value_sets() is disabled while the app is running online. ",
         "Value sets are shared by every session on the server, so they are ",
         "updated by whoever maintains the deployment, not from the app.",
         call. = FALSE)

  # Normalised here, as everywhere else: "5l" was not recognised, and an
  # unknown version was only noticed deep in the installer (review AI02).
  if (!is.character(versions) || !length(versions))
    stop("Argument 'versions' must name at least one instrument: ",
         paste(.cache_versions, collapse = ", "), ".", call. = FALSE)
  versions <- unique(vapply(versions, .norm_version, character(1L),
                            arg = "versions", USE.NAMES = FALSE))

  dropped  <- character(0)
  renamed  <- character(0)

  # --------------------------------------------------------
  # 0. Apply any pending migrations from the repository
  # --------------------------------------------------------
  # Its outcome counts towards whether the check completed: a renames list
  # that could not be downloaded used to pass for an empty one, and the
  # last-checked date moved on (review Q11).
  migrations <- NULL
  if (.has_internet()) {
    migrations <- apply_pending_migrations(ask = ask)
  }
  mig_conflicts <- if (is.list(migrations)) migrations$conflicts else character(0)
  # Renames made whose record could not be saved (verification V02): kept,
  # but the migration step is not complete.
  mig_unrecorded <- if (is.list(migrations) &&
                        identical(migrations$status, "unrecorded"))
    migrations$applied else character(0)

  # --------------------------------------------------------
  # 1. Handle drops first
  # --------------------------------------------------------
  if (!is.null(drop)) {
    message("eq5dsuite: Processing removals...\n")
    for (entry in drop) {
      parts <- strsplit(entry, ":")[[1]]
      if (length(parts) != 2) {
        message("\u26a0\ufe0f Invalid drop format: '", entry,
                "' \u2014 expected 'VS_code:version'")
        next
      }
      vs_code <- trimws(parts[1])
      version <- trimws(parts[2])
      success <- drop_value_set(vs_code, version, ask = ask)
      if (success) dropped <- c(dropped, entry)
    }
  }

  # --------------------------------------------------------
  # 2. Handle renames
  # --------------------------------------------------------
  if (!is.null(rename)) {
    message("eq5dsuite: Processing renames...\n")
    for (old_entry in names(rename)) {
      new_vs_code <- rename[[old_entry]]
      parts <- strsplit(old_entry, ":")[[1]]
      if (length(parts) != 2) {
        message("\u26a0\ufe0f Invalid rename format: '", old_entry,
                "' \u2014 expected 'VS_code:version'")
        next
      }
      old_vs_code <- trimws(parts[1])
      version     <- trimws(parts[2])
      success <- rename_value_set(
        old_vs_code, new_vs_code, version, ask = ask
      )
      if (success) renamed <- c(renamed, old_entry)
    }
  }

  # --------------------------------------------------------
  # 3. Check for new value sets
  # --------------------------------------------------------
  # An instrument is "checked" only when its index was read and is valid;
  # anything else is "failed", with the reason. A failed download used to be
  # skipped, which left nothing new and so reported everything up to date --
  # and moved the date of the last check on, silencing the reminder.
  result <- function(checked, failed, new = character(0),
                     installed = character(0),
                     install_failed = character(0))
    invisible(list(checked = checked, failed = failed, new = new,
                   installed = installed, install_failed = install_failed,
                   migration_conflicts = mig_conflicts,
                   migrations_unrecorded = mig_unrecorded,
                   dropped = dropped, renamed = renamed))
  # What could not be checked, in words.
  what <- function(n) ifelse(n == "migrations",
                             if (length(mig_unrecorded))
                               "the value set renames (made, but not recorded)"
                             else "the value set renames",
                             paste0("EQ-5D-", n))
  not_recorded <- paste0(
    "eq5dsuite: The date of the last check was not updated, so you will ",
    "be reminded again. Run update_value_sets() later to try again.")

  # The reminder date is one date for every instrument, so it moves only
  # when every instrument was checked: a check of the EQ-5D-5L alone used to
  # silence the reminder for the EQ-5D-3L and EQ-5D-Y-3L too (review AI02).
  all_checked_versions <- function() all(.cache_versions %in% checked)
  partial_note <- paste0(
    "eq5dsuite: Only ", paste0("EQ-5D-", versions, collapse = ", "),
    if (length(versions) == 1L) " was" else " were",
    " checked, so the date of the last check was not updated. Run ",
    "update_value_sets() without `versions` to check every instrument.")

  if (!.has_internet()) {
    message("eq5dsuite: No internet connection available. ",
            "No value sets were checked.")
    return(result(character(0),
                  stats::setNames(rep("no internet connection",
                                      length(versions)), versions)))
  }

  all_new       <- list()
  all_installed <- character(0)
  checked       <- character(0)
  failed        <- stats::setNames(character(0), character(0))
  if (is.list(migrations) && !migrations$status %in% c("ok", "conflict"))
    failed[["migrations"]] <- if (!is.null(migrations$reason)) migrations$reason
      else paste0("renames not applied: ", paste(migrations$failed, collapse = ", "))

  for (version in versions) {
    message("eq5dsuite: Checking EQ-5D-", version,
            " value sets...")

    idx <- .check_vs_index(version)
    if (!isTRUE(idx$ok)) {
      failed[[version]] <- idx$reason
      message("eq5dsuite: EQ-5D-", version, " could not be checked: ",
              idx$reason, ".")
      next
    }
    checked   <- c(checked, version)
    available <- idx$data

    installed_codes <- get_installed_vs_codes(version)
    # Compared without regard to case, as codes are looked up: an installed
    # "GB" listed as "gb" is not new (review AI02).
    new_codes <- unique(available$VS_code[
      !toupper(available$VS_code) %in% toupper(installed_codes)])

    if (length(new_codes) == 0) {
      message("eq5dsuite: EQ-5D-", version,
              " value sets are up to date.")
      next
    }

    message("\neq5dsuite: New EQ-5D-", version,
            " value sets available:")
    new_rows <- available[available$VS_code %in% new_codes, ]
    for (i in seq_len(nrow(new_rows))) {
      message("  - ", new_rows$Name[i],
              " (", new_rows$VS_code[i], ")",
              " doi:", new_rows$doi[i])
    }
    all_new[[version]] <- new_rows
  }

  all_checked <- all_checked_versions()

  conflict_note <- if (length(mig_conflicts))
    paste0(" ", length(mig_conflicts), " value set rename(s) could not be ",
           "applied because both codes are installed: ",
           paste(mig_conflicts, collapse = ", "), ".")

  if (length(all_new) == 0) {
    if (length(failed) == 0L && !length(mig_conflicts)) {
      message("\neq5dsuite: All value sets are up to date.",
              if (!all_checked) paste0("\n", partial_note))
      if (all_checked) set_last_checked()
    } else if (length(failed) == 0L) {
      message("\neq5dsuite: No new value sets.", conflict_note, "\n",
              not_recorded)
    } else {
      message("\neq5dsuite: Could not check ",
              paste(what(names(failed)), collapse = ", "), ".",
              if (length(checked))
                paste0(" No new value sets for ",
                       paste0("EQ-5D-", checked, collapse = ", "), "."),
              "\n", not_recorded)
    }
    return(result(checked, failed))
  }

  # Ask for confirmation
  if (ask && interactive()) {
    total_new <- sum(sapply(all_new, nrow))
    message("\n", total_new,
            " new value set(s) available. Install now?")
    response <- readline(
      "Enter [y] to install or [n] to cancel: "
    )
    if (tolower(trimws(response)) != "y") {
      message("eq5dsuite: Update cancelled.")
      return(result(checked, failed,
                    new = unname(unlist(lapply(all_new, `[[`, "VS_code")))))
    }
  }

  # Install new value sets
  install_failed <- character(0)
  for (version in names(all_new)) {
    new_rows <- all_new[[version]]
    for (i in seq_len(nrow(new_rows))) {
      vs_code  <- new_rows$VS_code[i]
      meta_row <- new_rows[i, ]
      message("Installing ", vs_code, "...")
      success <- install_value_set(vs_code, version, meta_row)
      if (success) {
        all_installed <- c(all_installed, vs_code)
        message("\u2705 ", vs_code, " installed successfully")
      } else {
        message("\u274c ", vs_code, " could not be installed")
        install_failed <- c(install_failed, vs_code)
      }
    }
  }

  # Check for conflicts between built-in and user-defined
  for (v in checked) {
    check_builtin_conflicts(v)
  }

  complete <- length(failed) == 0L && length(install_failed) == 0L &&
    !length(mig_conflicts)
  if (complete && all_checked) set_last_checked()
  if (complete && !all_checked) message(partial_note)
  message("\neq5dsuite: ",
          if (complete) "Update complete. " else "Update incomplete. ",
          length(all_installed), " value set(s) installed.",
          if (length(install_failed))
            paste0(" Could not install: ",
                   paste(install_failed, collapse = ", "), "."),
          if (length(failed))
            paste0(" Could not check: ",
                   paste(what(names(failed)), collapse = ", "), "."),
          conflict_note,
          if (!complete) paste0("\n", not_recorded))

  result(checked, failed,
         new = unname(unlist(lapply(all_new, `[[`, "VS_code"))),
         installed = all_installed, install_failed = install_failed)
}
