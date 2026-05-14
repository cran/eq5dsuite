# Base URL for the value sets repository
.vs_base_url <- paste0(
  "https://raw.githubusercontent.com/",
  "MathsInHealth/eq5dsuite-value-sets/main/"
)

# Map instrument version to folder name
.vs_folder <- function(version) {
  switch(version,
    "3L"  = "EQ-5D-3L",
    "5L"  = "EQ-5D-5L",
    "Y3L" = "EQ-5D-Y-3L",
    stop("Unknown version: ", version)
  )
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
  tryCatch({
    utils::read.csv(
      curl::curl(url),
      stringsAsFactors = FALSE
    )
  },
  error = function(e) {
    message("eq5dsuite: Could not fetch value set index for ",
            version, ": ", conditionMessage(e))
    NULL
  })
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
  vs_data <- tryCatch({
    utils::read.csv(
      curl::curl(url),
      stringsAsFactors = FALSE
    )
  },
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
    eqvs_add(
      vs_data,
      version     = version,
      country     = meta_row$Name,
      countryCode = meta_row$Country_code,
      VSCode      = vs_code,
      description = paste0("doi:", meta_row$doi),
      saveOption  = 2
    )
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
    suppressMessages(
      eqvs_drop(
        country    = vs_code,
        version    = version,
        saveOption = 2
      )
    )
    message("\u2705 ", vs_code, " removed successfully.")
    TRUE
  },
  error = function(e) {
    message("\u274c Could not remove ", vs_code,
            ": ", conditionMessage(e))
    FALSE
  })
}

#' Rename a value set
#'
#' Renames a value set by dropping the old VS_code and
#' reinstalling under the new VS_code. The value data is
#' preserved — only the identifier changes.
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

  installed <- get_installed_vs_codes(version)

  if (!old_vs_code %in% installed) {
    message("eq5dsuite: ", old_vs_code, " is not installed.")
    return(FALSE)
  }

  if (new_vs_code %in% installed) {
    message("eq5dsuite: ", new_vs_code,
            " already exists. Drop it first.")
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

  vsets_obj <- switch(version,
    "3L"  = ".vsets3L",
    "5L"  = ".vsets5L",
    "Y3L" = ".vsetsY3L"
  )

  vsets <- tryCatch(
    get(vsets_obj, envir = asNamespace("eq5dsuite")),
    error = function(e) NULL
  )

  if (is.null(vsets) || !old_vs_code %in% colnames(vsets)) {
    message("eq5dsuite: Could not extract data for ",
            old_vs_code)
    return(FALSE)
  }

  cc <- get(".cntrcodes", envir = asNamespace("eq5dsuite"))
  old_meta <- cc[cc$VS_code == old_vs_code &
                 cc$Version  == version, ]

  if (nrow(old_meta) == 0) {
    message("eq5dsuite: No metadata found for ", old_vs_code)
    return(FALSE)
  }

  vs_data <- data.frame(
    state = vsets$state,
    value = vsets[[old_vs_code]],
    stringsAsFactors = FALSE
  )
  colnames(vs_data)[2] <- new_vs_code

  message("Removing ", old_vs_code, "...")
  dropped <- drop_value_set(old_vs_code, version, ask = FALSE)
  if (!dropped) return(FALSE)

  message("Installing as ", new_vs_code, "...")
  new_meta            <- old_meta
  new_meta$VS_code    <- new_vs_code
  new_meta$Name       <- gsub(old_vs_code, new_vs_code,
                               old_meta$Name)
  new_meta$Name_short <- gsub(old_vs_code, new_vs_code,
                               old_meta$Name_short)

  tryCatch({
    eqvs_add(
      vs_data,
      version     = version,
      country     = new_meta$Name,
      countryCode = new_meta$Country_code,
      VSCode      = new_vs_code,
      description = paste0("doi:", new_meta$doi),
      saveOption  = 2
    )
    TRUE
  },
  error = function(e) {
    message("\u274c Could not install ", new_vs_code,
            ": ", conditionMessage(e))
    FALSE
  })
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
#' @param versions Character vector. Which instruments to check.
#'   Defaults to c("3L", "5L", "Y3L").
#' @param ask Logical. Whether to ask for confirmation before
#'   installing, dropping, or renaming. Defaults to TRUE.
#' @param drop Character vector of VS_codes to remove, in the
#'   format "VS_code:version" e.g. c("NL:3L", "US:5L").
#'   Defaults to NULL (no removals).
#' @param rename Named character vector of renames in the format
#'   c("old_VS_code:version" = "new_VS_code") e.g.
#'   c("NL:3L" = "NL_2006"). Defaults to NULL (no renames).
#' @return Invisibly returns a list with elements
#'   \code{checked}, \code{new}, \code{installed},
#'   \code{dropped}, and \code{renamed}.
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

  dropped  <- character(0)
  renamed  <- character(0)

  # --------------------------------------------------------
  # 0. Apply any pending migrations from the repository
  # --------------------------------------------------------
  if (curl::has_internet()) {
    apply_pending_migrations(ask = ask)
  }

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
  if (!curl::has_internet()) {
    message("eq5dsuite: No internet connection available.")
    return(invisible(list(
      checked   = versions,
      new       = character(0),
      installed = character(0),
      dropped   = dropped,
      renamed   = renamed
    )))
  }

  all_new       <- list()
  all_installed <- character(0)

  for (version in versions) {
    message("eq5dsuite: Checking EQ-5D-", version,
            " value sets...")

    available <- fetch_available_value_sets(version)
    if (is.null(available)) next

    installed_codes <- get_installed_vs_codes(version)
    new_codes       <- setdiff(available$VS_code,
                               installed_codes)

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

  if (length(all_new) == 0) {
    message("\neq5dsuite: All value sets are up to date.")
    set_last_checked()
    return(invisible(list(
      checked   = versions,
      new       = character(0),
      installed = character(0),
      dropped   = dropped,
      renamed   = renamed
    )))
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
      return(invisible(list(
        checked   = versions,
        new       = unlist(lapply(all_new, `[[`, "VS_code")),
        installed = character(0),
        dropped   = dropped,
        renamed   = renamed
      )))
    }
  }

  # Install new value sets
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
      }
    }
  }

  set_last_checked()

  # Check for conflicts between built-in and user-defined
  for (v in versions) {
    check_builtin_conflicts(v)
  }

  message("\neq5dsuite: Update complete. ",
          length(all_installed), " value set(s) installed.")

  invisible(list(
    checked   = versions,
    new       = unlist(lapply(all_new, `[[`, "VS_code")),
    installed = all_installed,
    dropped   = dropped,
    renamed   = renamed
  ))
}
