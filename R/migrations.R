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
  tryCatch(.remote_csv(url),
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
  path <- file.path(get_cache_dir(create = FALSE), "applied_migrations.rds")
  if (file.exists(path) && !dir.exists(path)) {
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
#' @return Invisibly, \code{TRUE} if the list was saved and reads back as
#'   saved, \code{FALSE} (with a warning) otherwise.
#' @keywords internal
set_applied_migrations <- function(migrations) {
  path <- file.path(get_cache_dir(), "applied_migrations.rds")
  # Saved means it reads back: the caller reports a rename as recorded only
  # on TRUE (verification V02).
  ok <- tryCatch({
    saveRDS(migrations, path)
    identical(readRDS(path), migrations)
  },
  error = function(e) {
    warning("eq5dsuite: Could not save applied migrations: ",
            conditionMessage(e), call. = FALSE)
    NA
  })
  if (isFALSE(ok))
    warning("eq5dsuite: Could not save applied migrations: ", path,
            " does not read back as written.", call. = FALSE)
  invisible(isTRUE(ok))
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
#' A migration is recorded as applied only when its outcome is verified: the
#' old code is gone and the new one is there, or there was nothing to rename.
#' When both codes are installed -- the target already exists -- nothing is
#' renamed or deleted, the migration is a conflict, and it stays pending: two
#' different value sets cannot be merged without a policy that says how.
#' Codes are compared without regard to case, as lookups are.
#'
#' A rename that was made but whose record (\code{applied_migrations.rds})
#' could not be saved is kept, and the status is \code{"unrecorded"}. The
#' migration stays pending; on the next run its old code is gone, so it is
#' recorded without renaming anything again.
#'
#' @param ask Logical. Whether to ask for confirmation before
#'   applying migrations. Defaults to TRUE.
#' @return Invisibly, a list: \code{status} (\code{"ok"}, \code{"conflict"},
#'   \code{"failed"}, \code{"unrecorded"} or \code{"postponed"}),
#'   \code{applied} (migration ids whose outcome holds now), \code{recorded}
#'   (those of them saved as applied), \code{conflicts} and \code{failed}
#'   (ids left pending),
#'   and \code{reason} (why the whole step failed, or \code{NULL}). A list
#'   that could not be downloaded, or is malformed, is \code{"failed"}; an
#'   empty list is \code{"ok"}.
#' @keywords internal
apply_pending_migrations <- function(ask = TRUE) {
  result <- function(status, applied = character(0), conflicts = character(0),
                     failed = character(0), reason = NULL,
                     recorded = applied)
    invisible(list(status = status, applied = applied, recorded = recorded,
                   conflicts = conflicts, failed = failed, reason = reason))

  # A download that failed is not an empty list (review Q11).
  migrations <- fetch_migrations()
  if (is.null(migrations))
    return(result("failed",
                  reason = "the list of value set renames could not be downloaded"))

  required_cols <- c("version", "old_VS_code", "new_VS_code", "reason")
  if (!is.data.frame(migrations) ||
      !all(required_cols %in% colnames(migrations))) {
    message("eq5dsuite: migrations.csv has unexpected format.")
    return(result("failed",
                  reason = "malformed list of value set renames (migrations.csv)"))
  }
  if (nrow(migrations) == 0L) return(result("ok"))

  migrations$id <- paste0(
    migrations$version, ":",
    migrations$old_VS_code, "->",
    migrations$new_VS_code
  )

  applied <- get_applied_migrations()
  pending <- migrations[!migrations$id %in% applied, , drop = FALSE]
  if (nrow(pending) == 0) return(result("ok"))

  message("eq5dsuite: ", nrow(pending),
          " value set rename(s) to apply:")
  for (i in seq_len(nrow(pending))) {
    message("  - Renaming EQ-5D-", pending$version[i],
            " ", pending$old_VS_code[i],
            " -> ", pending$new_VS_code[i],
            ": ", pending$reason[i])
  }

  if (ask && interactive()) {
    response <- readline(
      "Apply these renames now? [y/n]: "
    )
    if (tolower(trimws(response)) != "y") {
      message("eq5dsuite: Renames postponed. Run ",
              "update_value_sets() again to apply later.")
      return(result("postponed", failed = pending$id))
    }
  }

  newly_applied <- character(0)
  conflicts     <- character(0)
  failed        <- character(0)
  has <- function(code, version)
    toupper(code) %in% toupper(get_installed_vs_codes(version))

  for (i in seq_len(nrow(pending))) {
    row <- pending[i, ]
    old_in <- has(row$old_VS_code, row$version)
    new_in <- has(row$new_VS_code, row$version)

    if (!old_in) {
      # Nothing to rename: either never installed, or already renamed -- also
      # by an earlier run whose record could not be saved (V02), which is
      # recorded now. What is checked is the rename's outcome, the old code
      # gone; the target being installed is never taken as proof, since
      # with the old code still there it is a conflict (review Q04, below).
      newly_applied <- c(newly_applied, row$id)
      next
    }

    if (new_in) {
      # Both installed. The target existing is not evidence the rename was
      # done, and the two sets may differ; neither is touched (review Q04).
      message("eq5dsuite: ", row$new_VS_code, " and ", row$old_VS_code,
              " are both installed, so ", row$old_VS_code,
              " was not renamed. Both are kept; resolve this with ",
              "eqvs_drop() or update_value_sets(rename = ...), and the ",
              "rename will be offered again.")
      conflicts <- c(conflicts, row$id)
      next
    }

    installed <- get_installed_vs_codes(row$version)
    old_code <- installed[match(toupper(row$old_VS_code), toupper(installed))]
    success <- isTRUE(rename_value_set(
      old_vs_code = old_code,
      new_vs_code = row$new_VS_code,
      version     = row$version,
      ask         = FALSE
    ))

    # Recorded only if the outcome is there.
    if (success && !has(row$old_VS_code, row$version) &&
        has(row$new_VS_code, row$version)) {
      newly_applied <- c(newly_applied, row$id)
      message("\u2705 ", row$old_VS_code, " renamed to ", row$new_VS_code)
    } else {
      failed <- c(failed, row$id)
      message("\u274c Could not rename ", row$old_VS_code,
              " to ", row$new_VS_code)
    }
  }

  # A rename that was made but could not be recorded is kept -- it was
  # saved, and undoing it could fail too -- and reported as unrecorded. The
  # migration stays pending, so the next run finds the old code gone and
  # records it then (verification V02).
  if (length(newly_applied) &&
      !isTRUE(set_applied_migrations(unique(c(applied, newly_applied))))) {
    message("eq5dsuite: The value set renames were made, but the record of ",
            "them could not be saved; update_value_sets() will record them ",
            "next time.")
    return(result("unrecorded", applied = newly_applied, recorded = character(0),
                  conflicts = conflicts, failed = failed,
                  reason = paste0("renames made but could not be recorded ",
                                  "(applied_migrations.rds): ",
                                  paste(newly_applied, collapse = ", "),
                                  if (length(failed))
                                    paste0("; renames not applied: ",
                                           paste(failed, collapse = ", ")))))
  }

  result(if (length(failed)) "failed" else if (length(conflicts)) "conflict"
         else "ok",
         applied = newly_applied, conflicts = conflicts, failed = failed)
}
