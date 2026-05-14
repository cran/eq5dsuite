#' Fetch the migrations index from the repository
#'
#' Downloads the migrations.csv file from the eq5dsuite-value-sets
#' repository, which records all historical value set renames.
#' Returns NULL silently if the fetch fails.
#'
#' @return A data frame with columns version, old_VS_code,
#'   new_VS_code, reason, date. Returns NULL if fetch fails.
#' @keywords internal
fetch_migrations <- function() {
  url <- paste0(.vs_base_url, "migrations.csv")
  tryCatch({
    utils::read.csv(
      curl::curl(url),
      stringsAsFactors = FALSE
    )
  },
  error = function(e) {
    message("eq5dsuite: Could not fetch migrations: ",
            conditionMessage(e))
    NULL
  })
}

#' Get the list of already-applied migrations
#'
#' Reads the locally cached list of migration IDs that have
#' already been applied. Returns an empty character vector if
#' no migrations have been applied yet.
#'
#' @return A character vector of migration IDs.
#' @keywords internal
get_applied_migrations <- function() {
  path <- file.path(get_cache_dir(), "applied_migrations.rds")
  if (file.exists(path)) {
    tryCatch(
      readRDS(path),
      error = function(e) character(0)
    )
  } else {
    character(0)
  }
}

#' Save the list of applied migrations to cache
#'
#' @param migrations Character vector of migration IDs to save.
#' @return Invisibly returns the saved migrations.
#' @keywords internal
set_applied_migrations <- function(migrations) {
  path <- file.path(get_cache_dir(), "applied_migrations.rds")
  tryCatch({
    saveRDS(migrations, path)
    invisible(migrations)
  },
  error = function(e) {
    warning("eq5dsuite: Could not save applied migrations: ",
            conditionMessage(e))
    invisible(NULL)
  })
}

#' Apply pending value set migrations
#'
#' Fetches the migrations index from the repository and applies
#' any renames that have not yet been applied locally. This
#' ensures that value set codes remain consistent when a country
#' publishes a second value set and the original code needs to
#' be disambiguated with a year suffix.
#'
#' Migrations are applied before checking for new value sets in
#' \code{update_value_sets()}, ensuring the local state is
#' consistent before any new installations.
#'
#' @param ask Logical. Whether to ask for confirmation before
#'   applying migrations. Defaults to TRUE.
#' @return Invisibly returns a character vector of migration IDs
#'   that were applied in this session.
#' @keywords internal
apply_pending_migrations <- function(ask = TRUE) {

  # Fetch migrations from repository
  migrations <- fetch_migrations()
  if (is.null(migrations)) return(invisible(character(0)))

  # Validate structure (date column not required — not used in logic)
  required_cols <- c("version", "old_VS_code",
                     "new_VS_code", "reason")
  if (!all(required_cols %in% colnames(migrations))) {
    message("eq5dsuite: migrations.csv has unexpected format.")
    return(invisible(character(0)))
  }

  # Create unique migration IDs
  migrations$id <- paste0(
    migrations$version, ":",
    migrations$old_VS_code, "->",
    migrations$new_VS_code
  )

  # Find pending migrations
  applied <- get_applied_migrations()
  pending <- migrations[!migrations$id %in% applied, ]

  if (nrow(pending) == 0) {
    return(invisible(character(0)))
  }

  # Report pending migrations
  message("eq5dsuite: ", nrow(pending),
          " value set rename(s) to apply:")
  for (i in seq_len(nrow(pending))) {
    message("  - Renaming EQ-5D-", pending$version[i],
            " ", pending$old_VS_code[i],
            " -> ", pending$new_VS_code[i],
            ": ", pending$reason[i])
  }

  # Ask for confirmation
  if (ask && interactive()) {
    response <- readline(
      "Apply these renames now? [y/n]: "
    )
    if (tolower(trimws(response)) != "y") {
      message("eq5dsuite: Renames postponed. Run ",
              "update_value_sets() again to apply later.")
      return(invisible(character(0)))
    }
  }

  # Apply each pending migration
  newly_applied <- character(0)

  for (i in seq_len(nrow(pending))) {
    row <- pending[i, ]

    # Check if old code is installed
    installed <- get_installed_vs_codes(row$version)

    if (!row$old_VS_code %in% installed) {
      # Old code not installed — mark as applied and skip
      message("eq5dsuite: ", row$old_VS_code,
              " not installed \u2014 skipping rename.")
      newly_applied <- c(newly_applied, row$id)
      next
    }

    # Check if new code already exists
    if (row$new_VS_code %in% installed) {
      # Already renamed — mark as applied and skip
      message("eq5dsuite: ", row$new_VS_code,
              " already exists \u2014 skipping rename.")
      newly_applied <- c(newly_applied, row$id)
      next
    }

    # Apply the rename
    success <- rename_value_set(
      old_vs_code = row$old_VS_code,
      new_vs_code = row$new_VS_code,
      version     = row$version,
      ask         = FALSE
    )

    if (success) {
      newly_applied <- c(newly_applied, row$id)
      message("\u2705 ", row$old_VS_code, " renamed to ",
              row$new_VS_code)
    } else {
      message("\u274c Could not rename ", row$old_VS_code,
              " to ", row$new_VS_code)
    }
  }

  # Save updated list of applied migrations
  set_applied_migrations(c(applied, newly_applied))

  invisible(newly_applied)
}
