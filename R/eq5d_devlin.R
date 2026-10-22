#' eq5d_profile_level_summary: Frequency of levels by dimensions, cross-sectional
#'
#' @param df Data frame with the EQ-5D and follow-up columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @return Summary data frame.
#' @examples
#' eq5d_profile_level_summary(
#'  df = example_data[example_data$time == "Pre-op",],
#'  names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'  eq5d_version = "3L"
#' )
#' @export

eq5d_profile_level_summary <- function(df, 
                      names_eq5d = NULL,
                      eq5d_version = NULL) {
  do.call(.freqtab, list(
    df = df,
    names_eq5d = names_eq5d,
    eq5d_version = eq5d_version,
    add_summary_problems_change = FALSE
  ))
}


#' eq5d_profile_level_summary_by_group: Frequency of levels by dimensions, separated by category
#' 
#' @param df Data frame with the EQ-5D and follow-up columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_cat Character string for the category  column. If NULL, no grouping is used, and the table reports for the total population, i.e. equal to table 1.1.1.
#' @param levels_cat Character vector containing the order of the values in the category column, if the wish is to have these presented in a particular order. 
#' If NULL (default value), unless the variable is a factor, the levels will be ordered in the order of appearance in df.
#' @param eq5d_version Version of the EQ-5D instrument
#' @return Summary data frame.
#' @examples
#' eq5d_profile_level_summary_by_group(
#'  df = example_data[example_data$time == "Pre-op",],
#'  names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'  name_cat = "procedure",
#'  levels_cat = c("Hip Replacement", "Knee Replacement"),
#'  eq5d_version = "3L"
#' )
#' @export
#' 
eq5d_profile_level_summary_by_group <- function(df, 
                       names_eq5d = NULL,
                       name_cat = NULL,
                       levels_cat = NULL,
                       eq5d_version = NULL) {
  # Groups are not in any order, so there is no change between them to
  # report. The change and rank rows used to be computed and then removed by
  # position, which removed the wrong rows whenever either was absent.
  .freqtab(df, names_eq5d, name_cat, levels_cat, eq5d_version,
           add_summary_problems_change = FALSE)
}

#' eq5d_profile_change_summary: Frequency of levels by dimensions, by follow-up
#' 
#' @param df Data frame with the EQ-5D and follow-up columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column. If NULL, the function will check if there is a column named "follow-up" or "fu", in which case the first of those will be used.
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @param eq5d_version Version of the EQ-5D instrument
#' @return Summary data frame.
#' @details
#' Counts are made against the number of non-missing responses on each
#' dimension at each follow-up. Where that number is positive, every level of
#' the instrument has a row and every count is a number: a level, or a
#' problem, that nobody reported is 0. Where it is zero -- nobody recorded at
#' that follow-up, or everyone missing on that dimension -- the counts are
#' \code{NA}, because the data say nothing about them.
#'
#' The change in the number reporting problems is between consecutive levels
#' of \code{levels_fu}, and is \code{NA} where either count is. The relative
#' change is \code{NA} where the earlier count is 0. These are counts at each
#' follow-up, not changes within respondents; for those, see
#' \code{\link{eq5d_profile_pchc_table}} and
#' \code{\link{eq5d_profile_dimension_change_table}}.
#' @examples
#' eq5d_profile_change_summary(
#'   df = example_data,
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'   name_fu = "time",
#'   levels_fu = c("Pre-op" , "Post-op"),
#'   eq5d_version = "3L"
#' )
#' @export

eq5d_profile_change_summary <- function(df, 
                       names_eq5d = NULL,
                       name_fu = NULL,
                       levels_fu = NULL,
                       eq5d_version = NULL) {
  # The arguments are passed on as values. They used to be forwarded through
  # match.call() -- the caller's unevaluated expressions -- and evaluated
  # here, so a call from inside another function with a local variable as an
  # argument failed, or picked up something else of the same name.
  if(is.null(name_fu)){
    # The name found, not "follow_up", which was then not found (review A15).
    if("follow-up" %in% colnames(df)) name_fu <- "follow-up"
    if("fu" %in% colnames(df)) name_fu <- "fu"
  }
  .freqtab(df = df, names_eq5d = names_eq5d, name_fu = name_fu,
           levels_fu = levels_fu, eq5d_version = eq5d_version)
}

#' eq5d_profile_top_states: Prevalence of the 10 most frequently observed self-reported health states
#' 
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @param n Number of most frequently observed states to display (default 10)
#' @return Summary data frame
#' @export
#' @examples
#' eq5d_profile_top_states(
#'   df = example_data[example_data$time == "Pre-op",],
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'   eq5d_version = "3L",
#'   n = 10
#' )

eq5d_profile_top_states <- function(df, 
                      names_eq5d = NULL,
                      eq5d_version = NULL,
                      n = 10) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, names_eq5d = names_eq5d, 
                     eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- names_eq5d
  .check_columns(df, names_eq5d = names_eq5d)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d, eq5d_version = eq5d_version, add_state = TRUE)
  df <- df[, "state", drop = FALSE]

  ### analysis ####

  # all states (non-NA only), aggregated and sorted
  df_cc <- df[!is.na(df$state), , drop = FALSE]
  .stop_if_no_records(nrow(df_cc), "eq5d_profile_top_states")
  states_agg <- aggregate(rep(1L, nrow(df_cc)), by = list(state = df_cc$state), FUN = sum)
  names(states_agg)[2] <- "n"
  states_agg <- states_agg[order(-states_agg$n), , drop = FALSE]
  states_agg$p <- states_agg$n / sum(states_agg$n)
  states_agg$cum_p <- cumsum(states_agg$p)
  rownames(states_agg) <- NULL

  # most frequent n non-NA states
  states_top <- states_agg[seq_len(min(n, nrow(states_agg))), , drop = FALSE]

  # worst state
  state_worst <- if (eq5d_version == "5L") "55555" else "33333"

  if (!state_worst %in% states_top[, 1]) {
    idx <- match(state_worst, states_agg$state)
    if (!is.na(idx)) {
      states_worst <- states_agg[idx, , drop = FALSE]
      states_worst$cum_p <- 1
    } else {
      states_worst <- states_agg[0, , drop = FALSE]
    }
    states_worst <- states_worst[c(1, 1), , drop = FALSE]
    states_worst[1, ] <- NA
    states_worst[1, 1] <- '...'
  } else {
    states_worst <- states_agg[0, , drop = FALSE]
  }

  # missing data
  n_miss <- sum(is.na(df$state))
  state_na <- data.frame(state = "Missing", n = n_miss,
                         p = n_miss / nrow(df), cum_p = NA_real_,
                         stringsAsFactors = FALSE)

  # combine and tidy up
  retval <- rbind(states_top, states_worst, state_na)
  names(retval) <- c("Health state", "Frequency", "Percentage", "Cumulative percentage")
  rownames(retval) <- NULL

  # return value
  return(retval)
}

#' eq5d_profile_pchc_table: Changes in health according to the PCHC (Paretian Classification of Health Change)
#' 
#' @param df Data frame with the EQ-5D, grouping, id and follow-up columns
#' @param name_id Character string for the patient id column
#' @param name_groupvar Character string for the grouping column. If NULL (default), the analysis is performed on the full population.
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column.
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @return Summary data frame
#' @export
#' @section Pairing records:
#' Each respondent's records are compared in timepoint order, using only the
#' timepoints in \code{levels_fu}. The function stops, naming the respondents,
#' if a respondent has more than one record at the same timepoint: it cannot
#' tell which to compare, and does not choose. Where the analysis is split by
#' \code{name_groupvar}, it also stops if a respondent is not in the same
#' group at every timepoint (a missing group at one timepoint counts as a
#' change), because the change is attributed to the respondent's group.
#' @examples
#' eq5d_profile_pchc_table(
#'   df = example_data,
#'   name_id = "id",
#'   name_groupvar = "procedure",
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'   name_fu = "time",
#'   levels_fu = c("Pre-op" , "Post-op")
#' )

eq5d_profile_pchc_table <- function(df,
                      name_id,
                      name_groupvar = NULL,
                      names_eq5d = NULL,
                      name_fu = NULL,
                      levels_fu = NULL) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, 
                     names_eq5d = names_eq5d, 
                     name_fu = name_fu, levels_fu = levels_fu)
  names_eq5d <- temp$names_eq5d
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  # check existence of columns 
  names_all <- c(name_groupvar, name_id, names_eq5d, name_fu)
  .check_columns(df, name_groupvar = name_groupvar,
                 name_id = name_id,
                 names_eq5d = names_eq5d,
                 name_fu = name_fu)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  names(df)[names(df) == name_id] <- "id"
  if (is.null(name_groupvar)) {
    df$groupvar <- "All"
  } else {
    names(df)[names(df) == name_groupvar] <- "groupvar"
  }
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  # sort by id - groupvar - time

  # One record per respondent and timepoint, each respondent in one group
  # (.check_pairs()); then sorted by respondent and time only, so the pairs
  # follow time whatever the group (review A03, A04).
  .check_pairs(df, by_group = !is.null(name_groupvar))
  df <- df[order(df$id, df$fu), , drop = FALSE]

  ### analysis ###

  # calculate change
  df <- .pchc(df = df, level_fu_1 = levels_fu[1])
  df <- df[!is.na(df$state), , drop = FALSE]
  .stop_if_no_records(nrow(df), "eq5d_profile_pchc_table", paired = TRUE)
  df$state <- factor(df$state, levels = c("No change", "Improve", "Worsen", "Mixed change"))

  # summarise by groupvar, fu & state
  summary_dim <- aggregate(rep(1L, nrow(df)),
                           by = list(groupvar = df$groupvar, fu = df$fu, state = df$state),
                           FUN = sum)
  names(summary_dim)[names(summary_dim) == "x"] <- "n"
  key_gf <- paste(summary_dim$groupvar, summary_dim$fu, sep = "\001")
  summary_dim$p <- summary_dim$n / tapply(summary_dim$n, key_gf, sum)[key_gf]

  # summarise totals
  agg_tot <- aggregate(cbind(n = summary_dim$n, p = summary_dim$p),
                       by = list(groupvar = summary_dim$groupvar, fu = summary_dim$fu),
                       FUN = sum)
  agg_tot$state <- "Grand Total"
  summary_total <- agg_tot

  # combine & tidy up: pivot n/p to wide columns {groupvar}_{fu}_n / {groupvar}_{fu}_p
  combined <- rbind(summary_dim, summary_total)
  state_order <- c(levels(df$state), "Grand Total")
  all_states <- state_order[state_order %in% unique(as.character(combined$state))]
  retval <- data.frame(state = all_states, stringsAsFactors = FALSE)
  grp_vals <- unique(as.character(combined$groupvar))
  fu_vals  <- as.character(levels_fu[-1])
  for (gv in grp_vals) {
    for (f in fu_vals) {
      sub <- combined[combined$groupvar == gv & as.character(combined$fu) == f, , drop = FALSE]
      vals_n <- setNames(sub$n, as.character(sub$state))
      vals_p <- setNames(sub$p, as.character(sub$state))
      retval[[paste(gv, f, "n", sep = "_")]] <- ifelse(is.na(vals_n[retval$state]), 0, vals_n[retval$state])
      retval[[paste(gv, f, "p", sep = "_")]] <- ifelse(is.na(vals_p[retval$state]), 0, vals_p[retval$state])
    }
  }
  rownames(retval) <- NULL

  # return value
  return(retval)
}

#' eq5d_profile_pchc_with_no_problems_table: Changes in health according to the PCHC, taking account of those with no problems
#' 
#' @param df Data frame with the EQ-5D, grouping, id and follow-up columns
#' @param name_id Character string for the patient id column
#' @param name_groupvar Character string for the grouping column. If NULL (default), the analysis is performed on the full population.
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column.
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @return Summary data frame. For each group and later timepoint, \code{_n}
#'   is a count and \code{_p} a proportion: of everyone for "No problems" and
#'   "Total with problems", and of those with problems for the four change
#'   categories. Where nobody in a group had problems at a timepoint, those
#'   four proportions are \code{NA}, as they have no denominator.
#' @export
#' @inheritSection eq5d_profile_pchc_table Pairing records
#' @examples
#' eq5d_profile_pchc_with_no_problems_table(
#'   df = example_data,
#'   name_id = "id",
#'   name_groupvar = "procedure",
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'   name_fu = "time",
#'   levels_fu = c("Pre-op" , "Post-op")
#' )

eq5d_profile_pchc_with_no_problems_table <- function(df,
                      name_id,
                      name_groupvar = NULL,
                      names_eq5d = NULL,
                      name_fu = NULL,
                      levels_fu = NULL) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, 
                     names_eq5d = names_eq5d, 
                     name_fu = name_fu, levels_fu = levels_fu)
  names_eq5d <- temp$names_eq5d
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  # check existence of columns 
  names_all <- c(name_groupvar, name_id, names_eq5d, name_fu)
  .check_columns(df, name_groupvar = name_groupvar,
                 name_id = name_id,
                 names_eq5d = names_eq5d,
                 name_fu = name_fu)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  names(df)[names(df) == name_id] <- "id"
  if (is.null(name_groupvar)) {
    df$groupvar <- "All"
  } else {
    names(df)[names(df) == name_groupvar] <- "groupvar"
  }
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  # sort by id - groupvar - time
  # One record per respondent and timepoint, each respondent in one group
  # (.check_pairs()); then sorted by respondent and time only, so the pairs
  # follow time whatever the group (review A03, A04).
  .check_pairs(df, by_group = !is.null(name_groupvar))
  df <- df[order(df$id, df$fu), , drop = FALSE]
  
  ### analysis ###
  
  # calculate change
  df <- .pchc(df = df, level_fu_1 = levels_fu[1], add_noprobs = TRUE)
  df <- df[!is.na(df$state), , drop = FALSE]
  .stop_if_no_records(nrow(df), "eq5d_profile_pchc_with_no_problems_table", paired = TRUE)

  # separate out those with problems & calculate percentages
  df_status <- df
  df_status$state_noprobs <- ifelse(df_status$state_noprobs == "No problems",
                                    "No problems", "Total with problems")
  summary_by_probs_status <- aggregate(rep(1L, nrow(df_status)),
    by = list(groupvar = df_status$groupvar, fu = df_status$fu,
              state_noprobs = df_status$state_noprobs), FUN = sum)
  names(summary_by_probs_status)[names(summary_by_probs_status) == "x"] <- "n"
  key_bp <- paste(summary_by_probs_status$groupvar, summary_by_probs_status$fu, sep = "\001")
  summary_by_probs_status$p <- summary_by_probs_status$n /
    tapply(summary_by_probs_status$n, key_bp, sum)[key_bp]

  # summarise classes within those with problems. Nobody may have had
  # problems -- an all-healthy cohort -- and aggregate() fails on no rows
  # ("no rows to aggregate"), so that case is an empty summary (review A08).
  df_probs <- df[df$state_noprobs != "No problems", , drop = FALSE]
  if (nrow(df_probs)) {
    summary_with_probs <- aggregate(rep(1L, nrow(df_probs)),
      by = list(groupvar = df_probs$groupvar, fu = df_probs$fu,
                state_noprobs = df_probs$state_noprobs), FUN = sum)
    names(summary_with_probs)[names(summary_with_probs) == "x"] <- "n"
    key_wp <- paste(summary_with_probs$groupvar, summary_with_probs$fu, sep = "\001")
    summary_with_probs$p <- summary_with_probs$n /
      tapply(summary_with_probs$n, key_wp, sum)[key_wp]
  } else {
    summary_with_probs <- summary_by_probs_status[0L, , drop = FALSE]
  }

  # combine & tidy up
  combined <- rbind(summary_with_probs, summary_by_probs_status)
  state_order <- c("No change", "Improve", "Worsen", "Mixed change",
                   "Total with problems", "No problems")
  combined$state_noprobs <- factor(combined$state_noprobs, levels = state_order)
  combined <- combined[order(combined$state_noprobs), , drop = FALSE]
  all_states <- levels(combined$state_noprobs)[levels(combined$state_noprobs) %in%
                                                unique(as.character(combined$state_noprobs))]
  retval <- data.frame(state_noprobs = all_states, stringsAsFactors = FALSE)
  grp_vals <- unique(as.character(combined$groupvar))
  fu_vals  <- as.character(levels_fu[-1])
  for (gv in grp_vals) {
    for (f in fu_vals) {
      sub <- combined[combined$groupvar == gv & as.character(combined$fu) == f, , drop = FALSE]
      vals_n <- setNames(sub$n, as.character(sub$state_noprobs))
      vals_p <- setNames(sub$p, as.character(sub$state_noprobs))
      p <- ifelse(is.na(vals_p[retval$state_noprobs]), 0, vals_p[retval$state_noprobs])
      # The change categories are proportions of those with problems. Where
      # nobody in this group at this timepoint had problems there is no
      # denominator, and 0 would read as an observed 0% (review A08).
      with_probs <- vals_n["Total with problems"]
      if (is.na(with_probs) || with_probs == 0)
        p[retval$state_noprobs %in% c("No change", "Improve", "Worsen",
                                      "Mixed change")] <- NA_real_
      retval[[paste(gv, f, "n", sep = "_")]] <- as.numeric(ifelse(is.na(vals_n[retval$state_noprobs]), 0, vals_n[retval$state_noprobs]))
      retval[[paste(gv, f, "p", sep = "_")]] <- unname(p)
    }
  }
  rownames(retval) <- NULL

  # return value
  return(retval)
}

#' eq5d_profile_dimension_change_table: Changes in levels in each dimension, percentages of total and of type of change
#' 
#' @param df Data frame with the EQ-5D, id and follow-up columns
#' @param name_id Character string for the patient id column
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @return Summary data frame
#' @export
#' @inheritSection eq5d_profile_pchc_table Pairing records
#' @examples
#' eq5d_profile_dimension_change_table(
#'   df = example_data,
#'   name_id = "id",
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'   name_fu = "time",
#'   levels_fu = c("Pre-op" , "Post-op")
#' )

eq5d_profile_dimension_change_table <- function(df, 
                      name_id,
                      names_eq5d = NULL,
                      name_fu = NULL,
                      levels_fu = NULL) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, 
                     names_eq5d = names_eq5d, 
                     name_fu = name_fu, levels_fu = levels_fu)
  names_eq5d <- temp$names_eq5d
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  # check existence of columns 
  names_all <- c(name_id, names_eq5d, name_fu)
  .check_columns(df, name_id = name_id,
                 names_eq5d = names_eq5d,
                 name_fu = name_fu)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  names(df)[names(df) == name_id] <- "id"
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  # Rows whose follow-up is not one of levels_fu are NA here, and
  # .prep_fu() has said they are excluded. They used to be paired all the
  # same, sorted after the respondent's other records.
  df <- df[!is.na(df$fu), , drop = FALSE]
  # sort by id - time

  # One record per respondent and timepoint (.check_pairs(); review A03).
  .check_pairs(df)
  df <- df[order(df$id, df$fu), , drop = FALSE]
  
  ### analysis ###
  
  # calculate change
  levels_eq5d <- c("mo", "sc", "ua", "pd", "ad")
  level_fu_1 <- levels_fu[1]

  # reshape to long format
  df_long <- do.call(rbind, lapply(levels_eq5d, function(d) {
    data.frame(domain = d, id = df$id, fu = df$fu, value = df[[d]],
               stringsAsFactors = FALSE)
  }))
  # sort by domain, id, fu
  df_long <- df_long[order(df_long$domain, df_long$id, df_long$fu), , drop = FALSE]
  rownames(df_long) <- NULL

  # Each record is compared with the one before it, but only within one
  # dimension and one respondent: the first record of each, and every record
  # at the first timepoint, has no change. This is the pairing the PCHC
  # analyses use (see .first_of_subject()). The lag used to run across the
  # whole table, so a respondent without a baseline was compared with the
  # previous respondent's last record.
  n_long <- nrow(df_long)
  new_domain <- if (n_long == 0L) logical(0) else
    c(TRUE, df_long$domain[-1] != df_long$domain[-n_long])
  no_prev <- new_domain | .first_of_subject(df_long$id) |
    (!is.na(df_long$fu) &
       as.character(df_long$fu) == as.character(level_fu_1))
  prev_val <- c(NA_real_, head(df_long$value, -1))
  prev_val[no_prev] <- NA_real_
  df_long$diff <- prev_val - df_long$value
  df_long$level_change <- ifelse(is.na(df_long$diff), NA_character_,
                                 paste0(prev_val, "-", df_long$value))

  # remove NAs (baseline rows) and classify difference
  df_diff <- df_long[!is.na(df_long$diff), , drop = FALSE]
  df_diff$diff <- ifelse(df_diff$diff > 0, "Better",
                   ifelse(df_diff$diff < 0, "Worse", "No change"))
  df_diff$domain <- factor(df_diff$domain, levels = levels_eq5d)
  df_diff <- df_diff[order(df_diff$domain), , drop = FALSE]

  # % total: n per (domain, diff, level_change), p = n / sum(n) within domain
  p_agg <- aggregate(rep(1L, nrow(df_diff)),
                     by = list(domain = df_diff$domain, diff = df_diff$diff,
                                level_change = df_diff$level_change), FUN = sum)
  names(p_agg)[names(p_agg) == "x"] <- "n"
  p_total <- p_agg
  p_total$p <- p_total$n / tapply(p_total$n, p_total$domain, sum)[p_total$domain]
  p_total$type <- "% Total"

  # % type: p = n / sum(n) within (domain, diff)
  p_type <- p_agg
  key_dt <- paste(p_type$domain, p_type$diff, sep = "\001")
  p_type$p <- p_type$n / tapply(p_type$n, key_dt, sum)[key_dt]
  p_type$type <- "% Type"

  # combine and build wide output
  combined_p <- rbind(p_total[, c("domain", "type", "diff", "level_change", "p")],
                      p_type[,  c("domain", "type", "diff", "level_change", "p")])

  # ordered (diff, level_change) row keys
  all_rows <- unique(combined_p[, c("diff", "level_change")])
  all_rows$diff <- factor(all_rows$diff, levels = c("No change", "Better", "Worse"))
  all_rows <- all_rows[order(all_rows$diff, all_rows$level_change), , drop = FALSE]
  rownames(all_rows) <- NULL
  retval <- all_rows

  # column order: {domain}_{type} as produced by expand_grid(levels_eq5d, c("% Total","% Type"))
  col_names <- as.vector(outer(levels_eq5d, c("% Total", "% Type"), paste, sep = "_"))
  for (cn in col_names) {
    sep_idx <- regexpr("_% ", cn)
    dom  <- substr(cn, 1, sep_idx - 1)
    type <- substr(cn, sep_idx + 1, nchar(cn))
    sub  <- combined_p[as.character(combined_p$domain) == dom & combined_p$type == type, , drop = FALSE]
    vals <- setNames(sub$p, paste(sub$diff, sub$level_change, sep = "\001"))
    row_key <- paste(as.character(retval$diff), retval$level_change, sep = "\001")
    retval[[cn]] <- ifelse(is.na(vals[row_key]), 0, vals[row_key])
  }
  retval <- retval[, c("diff", "level_change", col_names), drop = FALSE]
  rownames(retval) <- NULL

  # return value
  return(retval)
}

#' eq5d_profile_lss_utility_summary: Summary statistics for the EQ-5D values by all the different LSSs (Level Sum Scores)
#' 
#' @param df Data frame with the EQ-5D dimension and value columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_utility Character string naming the column of pre-calculated
#'   EQ-5D values. If \code{NULL} (default), the column \code{"utility"} is
#'   used. The values are analysed as they stand; see
#'   \code{\link{eq5d}} for calculating them from the dimensions.
#' @param eq5d_version Version of the EQ-5D instrument
#' @return Summary data frame
#' @export
#' @examples
#' df <- data.frame(make_all_EQ_states(version = "5L"))
#' df$value <- eq5d5l(df[, c("mo", "sc", "ua", "pd", "ad")], country = "US")
#' eq5d_profile_lss_utility_summary(
#'   df, 
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'   name_utility = "value",
#'   eq5d_version = "5L"
#' )

eq5d_profile_lss_utility_summary <- function(df, 
                      names_eq5d = NULL,
                      name_utility = NULL,
                      eq5d_version = NULL){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, names_eq5d = names_eq5d,
                     name_utility = name_utility, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  name_utility <- temp$name_utility
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d, name_utility)
  .check_columns(df, names_eq5d = names_eq5d, name_utility = name_utility)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation. The Level Sum Score comes from the
  # dimensions; the values are taken as supplied.
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_lss = TRUE, eq5d_version = eq5d_version)
  df <- .prep_utility(df = df, name = name_utility)
  df <- df[, c("lss", "utility"), drop = FALSE]

  ### analysis ###

  # summarise non-NA values
  df_cc <- df[!is.na(df$lss), , drop = FALSE]
  grp_key <- df_cc$lss
  parts <- split(df_cc$utility, grp_key)
  summary_list <- lapply(sort(unique(grp_key)), function(k) {
    u <- parts[[as.character(k)]]
    data.frame(lss = k, Number = length(u), Mean = mean(u),
               `Standard Deviation` = sd(u), Median = median(u),
               Minimum = min(u), Maximum = max(u), Range = max(u) - min(u),
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  summary <- do.call(rbind, summary_list)

  # missing data
  summary_na <- data.frame(lss = "Missing", Number = sum(is.na(df$lss)),
                            Mean = NA_real_, `Standard Deviation` = NA_real_,
                            Median = NA_real_, Minimum = NA_real_,
                            Maximum = NA_real_, Range = NA_real_,
                            check.names = FALSE, stringsAsFactors = FALSE)

  # combine and tidy up
  summary$lss <- as.character(summary$lss)
  retval <- rbind(summary, summary_na)
  names(retval)[names(retval) == "lss"] <- "LSS"
  rownames(retval) <- NULL

  # return value
  return(retval)
}

#' eq5d_profile_lfs_distribution: Distribution of the EQ-5D states by LFS (Level Frequency Score)
#' 
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @return Summary data frame
#' @export
#' @examples
#' eq5d_profile_lfs_distribution(
#'   example_data, 
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'   eq5d_version = "3L"
#' )

eq5d_profile_lfs_distribution <- function(df, 
                      names_eq5d = NULL, 
                      eq5d_version = NULL){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, names_eq5d = names_eq5d, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d)
  .check_columns(df, names_eq5d = names_eq5d)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_lfs = TRUE, eq5d_version = eq5d_version)
  df <- df[, "lfs", drop = FALSE]

  ### analysis ###

  # summarise non-NA values
  df_cc <- df[!is.na(df$lfs), , drop = FALSE]
  .stop_if_no_records(nrow(df_cc), "eq5d_profile_lfs_distribution")
  counts <- aggregate(rep(1L, nrow(df_cc)), by = list(lfs = df_cc$lfs), FUN = sum)
  names(counts)[2] <- "n"
  counts <- counts[order(-counts$n), , drop = FALSE]
  counts$p <- counts$n / sum(counts$n)
  counts$cum_p <- cumsum(counts$p)
  names(counts) <- c("LFS", "Frequency", "%", "Cum (%)")

  # missing data
  summary_na <- data.frame(LFS = "Missing", Frequency = sum(is.na(df$lfs)),
                            `%` = NA_real_, `Cum (%)` = NA_real_,
                            check.names = FALSE, stringsAsFactors = FALSE)

  # combine
  retval <- rbind(counts, summary_na)
  rownames(retval) <- NULL

  # return value
  return(retval)
}

#' eq5d_profile_lfs_mean_utility: Number of observations in the LFS (Level Frequency Score) according to the EQ-5D values
#' 
#' @param df Data frame with the EQ-5D dimension and value columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_utility Character string naming the column of pre-calculated
#'   EQ-5D values. If \code{NULL} (default), the column \code{"utility"} is
#'   used. The values are analysed as they stand; see
#'   \code{\link{eq5d}} for calculating them from the dimensions.
#' @param eq5d_version Version of the EQ-5D instrument
#' @return Summary data frame
#' @export
#' @examples
#' example_data$value <- eq5d3l(example_data[, c("mo", "sc", "ua", "pd", "ad")],
#'                              country = "GB")
#' eq5d_profile_lfs_mean_utility(
#'   example_data, 
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'   name_utility = "value",
#'   eq5d_version = "3L"
#' )

eq5d_profile_lfs_mean_utility <- function(df, 
                      names_eq5d = NULL,
                      name_utility = NULL,
                      eq5d_version = NULL){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, names_eq5d = names_eq5d,
                     name_utility = name_utility, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  name_utility <- temp$name_utility
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d, name_utility)
  .check_columns(df, names_eq5d = names_eq5d, name_utility = name_utility)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation. The Level Frequency Score comes from
  # the dimensions; the values are taken as supplied.
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_lfs = TRUE, eq5d_version = eq5d_version)
  df <- .prep_utility(df = df, name = name_utility)
  df <- df[, c("lfs", "utility"), drop = FALSE]

  ### analysis ###

  # exclude NAs and count per (utility, lfs) combination
  df_cc <- df[!is.na(df$lfs), , drop = FALSE]
  .stop_if_no_records(nrow(df_cc), "eq5d_profile_lfs_mean_utility")
  counts <- aggregate(rep(1L, nrow(df_cc)),
                      by = list(utility = df_cc$utility, lfs = df_cc$lfs), FUN = sum)
  names(counts)[3] <- "n"

  # all lfs levels (sorted)
  all_lfs <- sort(unique(as.character(df_cc$lfs)))
  all_utils <- sort(unique(df_cc$utility))

  # build wide matrix: rows = utility, cols = lfs
  retval <- data.frame(utility = all_utils, stringsAsFactors = FALSE)
  for (lf in all_lfs) {
    sub <- counts[as.character(counts$lfs) == lf, , drop = FALSE]
    vals <- setNames(sub$n, sub$utility)
    retval[[lf]] <- ifelse(is.na(vals[as.character(retval$utility)]), 0L,
                           vals[as.character(retval$utility)])
  }
  # add row totals
  retval$Total <- rowSums(retval[, all_lfs, drop = FALSE])
  # tidy up
  names(retval)[names(retval) == "utility"] <- "EQ-5D value"
  rownames(retval) <- NULL

  # return value
  return(retval)
}

#' eq5d_profile_lfs_utility_summary: Summary statistics of EQ-5D values by LFS (Level Frequency Score)
#' 
#' @param df Data frame with the EQ-5D dimension and value columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_utility Character string naming the column of pre-calculated
#'   EQ-5D values. If \code{NULL} (default), the column \code{"utility"} is
#'   used. The values are analysed as they stand; see
#'   \code{\link{eq5d}} for calculating them from the dimensions.
#' @param eq5d_version Version of the EQ-5D instrument
#' @return Summary data frame
#' @export
#' @examples
#' example_data$value <- eq5d3l(example_data[, c("mo", "sc", "ua", "pd", "ad")],
#'                              country = "GB")
#' eq5d_profile_lfs_utility_summary(
#'   example_data, 
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'   name_utility = "value",
#'   eq5d_version = "3L"
#' )

eq5d_profile_lfs_utility_summary <- function(df, 
                       names_eq5d = NULL,
                       name_utility = NULL,
                       eq5d_version = NULL){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, names_eq5d = names_eq5d,
                     name_utility = name_utility, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  name_utility <- temp$name_utility
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d, name_utility)
  .check_columns(df, names_eq5d = names_eq5d, name_utility = name_utility)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation. The Level Frequency Score comes from
  # the dimensions; the values are taken as supplied.
  df <- .prep_eq5d(df = df, names = names_eq5d, 
                   add_state = TRUE, 
                   add_lfs = TRUE, eq5d_version = eq5d_version)
  df <- .prep_utility(df = df, name = name_utility)
  
  ### analysis ###
  
  # summarise non-NA values
  df_cc <- df[!is.na(df$utility), , drop = FALSE]
  parts <- split(df_cc$utility, df_cc$lfs)
  result_list <- lapply(sort(names(parts)), function(k) {
    u <- parts[[k]]
    data.frame(LFS = k, Frequency = length(u), Mean = mean(u), SD = sd(u),
               Median = median(u), Minimum = min(u), Maximum = max(u),
               Range = max(u) - min(u), stringsAsFactors = FALSE)
  })
  retval <- do.call(rbind, result_list)
  rownames(retval) <- NULL

  # return value
  return(retval)
}

#' eq5d_profile_shannon: Shannon's informativity indices
#'
#' Reports Shannon's index H', its maximum H'max, and Shannon's evenness index
#' J', for each EQ-5D dimension and for the health state as a whole. These are
#' the informativity measures of chapter 4 of Devlin et al. (2020): how much of
#' the classification system a sample actually uses.
#'
#' @details
#' For a variable with categories \eqn{i} holding proportions \eqn{p_i} of the
#' observations,
#'
#' \deqn{H' = -\sum_{i} p_i \log_2 p_i,\quad H'_{max} = \log_2 L,\quad
#'       J' = H' / H'_{max}}
#'
#' where \eqn{L} is the number of categories the instrument allows. For a
#' dimension that is 3 (EQ-5D-3L, EQ-5D-Y-3L) or 5 (EQ-5D-5L); for the health
#' state it is \eqn{3^5 = 243} or \eqn{5^5 = 3125}. \eqn{L} is what the
#' instrument allows, not what the sample happens to contain, so J' is
#' comparable between samples.
#'
#' H' is in bits and rises with both the number of categories used and how
#' evenly they are used. J' is H' as a fraction of its maximum, from 0 when
#' every observation falls in one category to 1 when they are spread evenly
#' over all \eqn{L}. Reporting both is the recommendation: H' alone cannot be
#' compared between instruments with different numbers of categories.
#'
#' Each dimension is computed over its own non-missing responses. The health
#' state row uses complete profiles only, since a profile with any dimension
#' missing is not a health state.
#'
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument: "3L", "5L" or "Y3L".
#'   Matching is case-insensitive. It decides \eqn{L}, so it changes H'max and
#'   J'.
#' @param name_fu Character string for the follow-up column. If NULL (default),
#'   the indices are reported for the sample as a whole.
#' @param levels_fu Character vector containing the order of the values in the
#'   follow-up column. If NULL (default value), the levels will be ordered in
#'   the order of appearance in df.
#' @return A data frame with one row per EQ-5D dimension and a final row for
#'   the health state, and three columns per follow-up level: \code{H_<level>},
#'   \code{Hmax_<level>} and \code{J_<level>}. Without \code{name_fu} there is
#'   a single set of columns, suffixed \code{All}.
#' @references
#' Devlin N, Parkin D, Janssen B (2020). Methods for Analysing and Reporting
#' EQ-5D Data. Springer. \doi{10.1007/978-3-030-47622-9}
#' @export
#' @examples
#' eq5d_profile_shannon(
#'   example_data,
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'   eq5d_version = "3L"
#' )
#'
#' # By time point
#' eq5d_profile_shannon(
#'   example_data,
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'   eq5d_version = "3L",
#'   name_fu = "time",
#'   levels_fu = c("Pre-op", "Post-op")
#' )

eq5d_profile_shannon <- function(df,
                      names_eq5d = NULL,
                      eq5d_version = NULL,
                      name_fu = NULL,
                      levels_fu = NULL) {

  ### data preparation ###

  # With no follow-up column the whole sample is one group, as in .freqtab().
  no_fu <- is.null(name_fu)
  if (no_fu) {
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
  df <- .prep_eq5d(df = df, names = names_eq5d, add_state = TRUE,
                   eq5d_version = eq5d_version)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)

  ### analysis ###

  levels_eq5d <- c("mo", "sc", "ua", "pd", "ad")
  n_levels <- if (eq5d_version %in% c("3L", "Y3L")) 3L else 5L
  # The health state has one category per possible profile.
  n_states <- n_levels^5L

  empty <- vapply(levels_fu, function(f) !any(!is.na(df$fu) & df$fu == f),
                  logical(1L))
  if (any(empty))
    warning("No observations for follow-up level",
            if (sum(empty) > 1L) "s" else "", " ",
            paste0("\"", levels_fu[empty], "\"", collapse = ", "),
            ". Their indices are NA.", call. = FALSE)

  retval <- data.frame(dimension = c(levels_eq5d, "Health state"),
                       stringsAsFactors = FALSE)

  for (f in levels_fu) {
    rows <- !is.na(df$fu) & df$fu == f
    idx <- rbind(
      do.call(rbind, lapply(levels_eq5d,
                            function(d) .shannon(df[[d]][rows], n_levels))),
      .shannon(df$state[rows], n_states))
    retval[[paste0("H_", f)]]    <- unname(idx[, "H"])
    retval[[paste0("Hmax_", f)]] <- unname(idx[, "Hmax"])
    retval[[paste0("J_", f)]]    <- unname(idx[, "J"])
  }

  rownames(retval) <- NULL

  # return value
  return(retval)
}

#' eq5d_profile_density_curve: Generate a Health State Density Curve (HSDC) for EQ-5D Data
#'
#' Calculates and plots the Health State Density Curve (HSDC) and the Health
#' State Density Index (HSDI) of Zamora et al. (2018), which describe how
#' evenly the observations are spread over the health state profiles that
#' occur in the data. Profiles are formed from the five dimensions; a row
#' whose profile is not valid for the instrument is excluded with a warning.
#'
#' @details
#' \strong{The curve.} The \eqn{S} distinct profiles observed are ranked from the
#' most to the least frequent. For \eqn{i = 1, \dots, S}{i = 1, ..., S}, \eqn{x_i} is the
#' cumulative proportion of observations in the first \eqn{i} profiles and
#' \eqn{y_i = i / S} the cumulative proportion of profiles. The HSDC plots
#' \eqn{y} against \eqn{x}, from \eqn{(0, 0)} to \eqn{(1, 1)}. The diagonal
#' is perfect evenness, every profile equally frequent; the further the curve
#' lies below it, the more the observations are concentrated in a few
#' profiles.
#'
#' \strong{The index.} Following Zamora et al. (2018, equation 1),
#' \deqn{HSDI = \sum_{i=1}^{S} (x_i - x_{i-1})(y_i + y_{i-1}),}{HSDI = sum over i of (x[i] - x[i-1]) (y[i] + y[i-1]),}
#' with \eqn{x_0 = y_0 = 0}: twice the area under the curve. It is analogous
#' to the Gini coefficient, but runs the other way: the HSDI is
#' \eqn{1 - G}, where \eqn{G} is twice the area between the curve and the
#' diagonal.
#'
#' \strong{Direction.} Higher is more even. The HSDI is 1 when every observed
#' profile is equally frequent, including the case of a single profile, and
#' falls as the observations concentrate in fewer of the observed profiles.
#' Profiles of equal frequency may be ranked in either order without changing
#' it.
#'
#' \strong{Attainable bounds.} With \eqn{N} observations over \eqn{S} observed
#' profiles, the HSDI lies in
#' \deqn{1/S + (S - 1)/N \le HSDI \le 1.}{1/S + (S - 1)/N <= HSDI <= 1.}
#' The lower bound is reached when one profile holds \eqn{N - S + 1}
#' observations and each of the others one. The HSDI is therefore never
#' below \eqn{1/S}, and never 0: the 0 of "total inequality" in Zamora et al.
#' is a limit, approached only as \eqn{S} and \eqn{N/S} both grow. With two
#' profiles it cannot fall below 0.5.
#'
#' \strong{What it is relative to.} Only the profiles that occur in these data
#' count: \eqn{y_i} is \eqn{i / S} for the \eqn{S} profiles observed, not
#' for the 243 (EQ-5D-3L, EQ-5D-Y-3L) or 3,125 (EQ-5D-5L) the instrument
#' allows, and a profile nobody reported does not enter. The index describes
#' how evenly observations are spread over the profiles that appear. Because
#' its attainable range depends on \eqn{S} and \eqn{N}, compare it between
#' samples with care. Repeating a whole distribution over more profiles
#' leaves it exactly unchanged (Zamora et al.'s independence of \eqn{S}),
#' but samples whose distributions differ in shape are not on a common scale.
#'
#' \strong{Worked values.} Two profiles observed equally often: 1. Frequencies
#' 9 and 1: \eqn{0.9 \times 0.5 + 0.1 \times 1.5 = 0.6}{0.9 x 0.5 + 0.1 x 1.5 = 0.6}. Frequencies 999
#' and 1: 0.501, the paper's "0.5" for this case. Frequencies 2, 1 and 1:
#' \eqn{(0.5 \times 1 + 0.25 \times 3 + 0.25 \times 5) / 3 = 5/6}{(0.5 x 1 + 0.25 x 3 + 0.25 x 5) / 3 = 5/6},
#' the lower bound for \eqn{N = 4}, \eqn{S = 3}.
#'
#' @references
#' Zamora B, Parkin D, Feng Y, Bateman A, Herdman M, Devlin N (2018). New
#' methods for analysing the distribution of EQ-5D observations. OHE
#' Research Paper 18/03. London: Office of Health Economics.
#' \url{https://www.ohe.org/publications/new-methods-analysing-distribution-eq-5d-observations/}
#'
#' Devlin N, Parkin D, Janssen B (2020). Methods for Analysing and Reporting
#' EQ-5D Data. Cham: Springer. \doi{10.1007/978-3-030-47622-9}
#'
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @return A list containing:
#'   \item{plot_data}{A data frame with the cumulative distribution of profiles}
#'   \item{hsdi}{The Health State Density Index, unrounded. The plot subtitle
#'     shows it rounded to three decimal places.}
#'   \item{p}{A ggplot2 object showing the Health State Density Curve}
#' @export
#' @examples
#' figure <- eq5d_profile_density_curve(
#'             df = example_data, 
#'             names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'             eq5d_version = "3L"
#'           )
#' figure$plot_data
#' figure$hsdi
#' figure$p
#' @importFrom utils head

eq5d_profile_density_curve <- function(df, names_eq5d, eq5d_version) {
  
  # Retrieve validated names & version from helper function
  temp <- .get_names(df = df, names_eq5d = names_eq5d, eq5d_version = eq5d_version)
  names_eq5d   <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  
  # Check existence of columns
  names_all <- c(names_eq5d)
  .check_columns(df, names_eq5d = names_eq5d)
  
  # Subset to relevant columns
  df <- df[, names_all, drop = FALSE]

  # Prepare data (adds 'state' column via .prep_eq5d)
  df <- .prep_eq5d(
    df           = df,
    names        = names_eq5d,
    add_state    = TRUE,
    add_lfs      = FALSE,
    eq5d_version = eq5d_version
  )

  # Check for NA in 'state', show warning and filter
  n_na <- sum(is.na(df$state))
  if (n_na > 0) {
    warning(n_na, " excluded observations with invalid EQ-5D profiles.")
    df <- df[!is.na(df$state), , drop = FALSE]
  }

  # Calculate frequencies & cumulative proportions
  .stop_if_no_records(nrow(df), "eq5d_profile_density_curve")
  freq_agg <- aggregate(rep(1L, nrow(df)), by = list(state = df$state), FUN = sum)
  names(freq_agg)[2] <- "Frequency"
  freq_agg <- freq_agg[order(-freq_agg$Frequency), , drop = FALSE]
  rownames(freq_agg) <- NULL
  freq_agg$CumPropObservations <- cumsum(freq_agg$Frequency) / sum(freq_agg$Frequency)
  freq_agg$CumPropStates       <- seq_len(nrow(freq_agg)) / nrow(freq_agg)
  profile_freq <- freq_agg
  
  # Compute the Health State Density Index (HSDI)
  hsdi_area <- sum(
    diff(c(0, profile_freq$CumPropObservations)) * 
      (head(c(0, profile_freq$CumPropStates), -1) + profile_freq$CumPropStates) / 2
  )
  # Rounded only where it is displayed. The value returned below is the one
  # that was computed: a user wanting the index for a table had to scrape it
  # out of the plot subtitle, and got the rounded figure at that.
  hsdi <- 2 * hsdi_area
  
  # Plot Health State Density Curve. The curve starts at the origin, as the
  # HSDI above does; plotting only the observed points left out its first
  # segment, and with a single state there was no line at all (review A14).
  # plot_data below is unchanged.
  curve <- rbind(data.frame(CumPropObservations = 0, CumPropStates = 0),
                 profile_freq[, c("CumPropObservations", "CumPropStates")])
  p <- ggplot2::ggplot(curve, ggplot2::aes(
    x = .data$CumPropObservations, 
    y = .data$CumPropStates
  )) +
    ggplot2::geom_line(color = "blue", linewidth = 1.2) +
    ggplot2::geom_abline(
      intercept = 0, 
      slope = 1, 
      linetype = "solid", 
      linewidth = 1.2, 
      color = "orange"
    ) +
    ggplot2::labs(
      title    = paste0("Health State Density Curve (HSDC)"),
      subtitle = paste0("HSDI = ", round(hsdi, 3)),
      x        = "Cumulative proportion of observations",
      y        = "Cumulative proportion of profiles"
    ) +
    # Force the aspect ratio to 1:1
    ggplot2::coord_fixed(ratio = 1) +
    # Limit x,y range from 0 to 1
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0))

  # Through the shared theme, like every other plot function, rather than
  # ggplot2::theme_minimal() applied here: the HSDC plot used to be the one
  # that looked different from the rest.
  # `hsdi` is new in 2.1.0 and sits between the two elements that were always
  # here, so anything reading result$plot_data or result$p is unaffected.
  return(list(plot_data = profile_freq, hsdi = hsdi,
              p = .modify_ggplot_theme(p = p)))
}

#' eq5d_vas_summary: EQ VAS Score by timepoints
#' 
#' @param df Data frame with the VAS and the follow-up columns
#' @param name_vas Character string for the VAS column
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' @return Summary data frame with one row per statistic and one column per
#'   follow-up level, in the order given by \code{levels_fu}.
#'   \code{"Missing (\%)"} is a percentage, from 0 to 100, of the rows at
#'   that follow-up (the \code{"Total sample"} row) with no usable
#'   value; it is \code{NA} for a follow-up with no rows. Before 2.1.0
#'   it was a proportion.
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
#' @export
#' @examples
#' eq5d_vas_summary(
#'   example_data,
#'   name_vas = 'vas', 
#'   name_fu = 'time', 
#'   levels_fu = c('Pre-op', 'Post-op')
#' )

eq5d_vas_summary <- function(df, 
                      name_vas = NULL,
                      name_fu = NULL,
                      levels_fu = NULL){
  
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, name_fu = name_fu, levels_fu = levels_fu, name_vas = name_vas)
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  name_vas <- temp$name_vas
  # check existence of columns 
  names_all <- c(name_vas, name_fu)
  .check_columns(df, name_vas = name_vas, name_fu = name_fu)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  df <- .prep_vas(df = df, name = name_vas)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  
  ### analysis ###
  
  # values calculated in the .summary_cts_by_fu wrapper
  retval <- .summary_cts_by_fu(df = df, name_v = "vas")
  
  # return value
  return(retval)
}

#' eq5d_vas_distribution_table: EQ VAS Scores frequency of mid-points
#' 
#' @param df Data frame with the VAS column
#' @param name_vas Character string for the VAS column
#' @param add_na_total Logical, whether to add summary of the missing, and across the Total, data
#' @return Summary data frame
#' @export
#' @examples
#' eq5d_vas_distribution_table(
#'   example_data,
#'   name_vas = 'vas', 
#'   add_na_total =  TRUE
#'   )

eq5d_vas_distribution_table <- function(df, 
                      name_vas = NULL, 
                      add_na_total = TRUE){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(name_vas = name_vas)
  name_vas <- temp$name_vas
  # check existence of columns 
  names_all <- c(name_vas)
  .check_columns(df, name_vas = name_vas)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  df <- .prep_vas(df = df, name = name_vas)
  
  ### analysis ###
  
  # define ranges and midpoints
  l_vals <- c(0:2, seq(from = 3, to = 93, by = 5), 98:100)
  u_vals <- c(0:2, seq(from = 7, to = 97, by = 5), 98:100)
  mid_vals <- c(0:2, seq(from = 5, to = 95, by = 5), 98:100)
  retval <- data.frame(l = l_vals, u = u_vals, Midpoint = mid_vals, Frequency = NA_integer_)

  # populate the table
  for (i in seq_len(nrow(retval))) {
    retval[i, "Frequency"] <- sum(!is.na(df$vas) & df$vas >= retval$l[i] & df$vas <= retval$u[i])
  }

  # add range column and tidy up
  retval$Range <- ifelse(retval$l == retval$u, as.character(retval$l),
                         paste(retval$l, retval$u, sep = "-"))
  retval <- retval[, c("Range", "Midpoint", "Frequency")]

  # add totals & missing data if needed
  if (add_na_total) {
    n_total <- nrow(df)
    n_na    <- sum(is.na(df$vas))
    retval  <- rbind(retval,
                     data.frame(Range = c("Total observed", "Missing", "Total sample"),
                                Midpoint = rep(NA_real_, 3),
                                Frequency = c(n_total - n_na, n_na, n_total)))
  }
  
  # return value
  return(retval)
}

#' eq5d_utility_summary: EQ-5D values: by timepoints
#' 
#' @param df Data frame with the EQ-5D value and follow-up columns
#' @param name_utility Character string naming the column of pre-calculated
#'   EQ-5D values. If \code{NULL} (default), the column \code{"utility"} is
#'   used. The values are analysed as they stand; see
#'   \code{\link{eq5d}} for calculating them from the dimensions.
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @return Summary data frame with one row per statistic and one column per
#'   follow-up level, in the order given by \code{levels_fu}.
#'   \code{"Missing (\%)"} is a percentage, from 0 to 100, of the rows at
#'   that follow-up (the \code{"Total sample"} row) with no usable
#'   value; it is \code{NA} for a follow-up with no rows. Before 2.1.0
#'   it was a proportion.
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
#' @export
#' @examples
#' example_data$value <- eq5d3l(example_data[, c("mo", "sc", "ua", "pd", "ad")],
#'                              country = "GB")
#' eq5d_utility_summary(
#'   example_data,
#'   name_utility = "value",
#'   name_fu = "time",
#'   levels_fu = c('Pre-op', 'Post-op')
#' )

eq5d_utility_summary <- function(df, 
                      name_utility = NULL,
                      name_fu = NULL,
                      levels_fu = NULL){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, 
                     name_utility = name_utility,
                     name_fu = name_fu, levels_fu = levels_fu)
  name_utility <- temp$name_utility
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  # check existence of columns 
  names_all <- c(name_utility, name_fu)
  .check_columns(df, name_utility = name_utility, name_fu = name_fu)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation. .prep_utility() runs last so that the
  # other renames are done before the "utility" name is claimed.
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  df <- .prep_utility(df = df, name = name_utility)
  df <- df[, c("fu", "utility"), drop = FALSE]

  ### analysis ###

  # summarise
  retval <- .summary_cts_by_fu(df = df, name_v = "utility")
  
  # return value
  return(retval)
}

#' eq5d_utility_summary_by_group:EQ-5D values: by groupvar
#' 
#' @param df Data frame with the EQ-5D value and grouping columns
#' @param name_utility Character string naming the column of pre-calculated
#'   EQ-5D values. If \code{NULL} (default), the column \code{"utility"} is
#'   used. The values are analysed as they stand; see
#'   \code{\link{eq5d}} for calculating them from the dimensions.
#' @param name_groupvar Character string for the grouping column. If NULL (default), the analysis is performed on the full population.
#' @return Summary data frame
#' @export
#' @examples
#' example_data$value <- eq5d3l(example_data[, c("mo", "sc", "ua", "pd", "ad")],
#'                              country = "GB")
#' eq5d_utility_summary_by_group(
#'   example_data,
#'   name_utility = "value",
#'   name_groupvar = "procedure"
#' )

eq5d_utility_summary_by_group <- function(df,
                      name_utility = NULL,
                      name_groupvar = NULL){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, name_utility = name_utility)
  name_utility <- temp$name_utility
  # check existence of columns 
  names_all <- c(name_utility, name_groupvar)
  .check_columns(df, name_utility = name_utility, name_groupvar = name_groupvar)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  if (is.null(name_groupvar)) {
    df$groupvar <- "All"
  } else {
    names(df)[names(df) == name_groupvar] <- "groupvar"
  }
  # Each group becomes a column below, labelled by .group_labels(): a missing
  # group value is kept as its own group, labelled "NA" (or "NA (missing)"
  # when a real group is called "NA", which it used to be merged with). Left
  # as NA it could not name a column, and the pivot failed with "missing value
  # where TRUE/FALSE needed"; as a factor, the pivot would index by the
  # level's integer code rather than its label.
  df$groupvar <- as.character(df$groupvar)

  df <- .prep_utility(df = df, name = name_utility)
  df <- df[, c("groupvar", "utility"), drop = FALSE]

  ### analysis ###

  # summary by category
  summary_by <- .summary_table_4_3(df = df, group_by = "groupvar")
  # summary_total (all groups combined)
  u_all <- df$utility
  summary_total <- data.frame(
    groupvar = "All groups",
    Mean = mean(u_all, na.rm = TRUE),
    `Standard error` = sd(u_all, na.rm = TRUE) / sqrt(sum(!is.na(u_all))),
    Median = median(u_all, na.rm = TRUE),
    `25th` = quantile(u_all, probs = 0.25, na.rm = TRUE, names = FALSE),
    `75th` = quantile(u_all, probs = 0.75, na.rm = TRUE, names = FALSE),
    N = sum(!is.na(u_all)),
    Missing = sum(is.na(u_all)),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  # One column per group, then the total, placed by position rather than
  # looked up by name: a missing group is labelled "(missing)" and kept apart
  # from a group called "NA", and the total's label is made distinct from
  # every group's (review A06, A07).
  stat_cols <- setdiff(names(summary_by), "groupvar")
  labs <- .group_labels(summary_by$groupvar)
  retval <- data.frame(name = stat_cols, stringsAsFactors = FALSE)
  for (i in seq_along(labs))
    retval[[labs[i]]] <- as.numeric(summary_by[i, stat_cols])
  retval[[.total_label("All groups", labs)]] <- as.numeric(summary_total[1, stat_cols])
  rownames(retval) <- NULL

  # return value
  return(retval)
}

#' eq5d_utility_norms_comparison:EQ-5D values: by age and groupvar
#' 
#' @param df Data frame with the EQ-5D value, age, follow-up and grouping columns
#' @param name_utility Character string naming the column of pre-calculated
#'   EQ-5D values. If \code{NULL} (default), the column \code{"utility"} is
#'   used. The values are analysed as they stand; see
#'   \code{\link{eq5d}} for calculating them from the dimensions.
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column.
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' The first level is the baseline, and only records at the baseline are
#' summarised.
#' @param name_groupvar Character string for the grouping column. If NULL (default), the analysis is performed on the full population.
#' @param name_age Character string for the age column
#' @details The comparison with population norms describes the sample at
#'   baseline: only records at the first timepoint in \code{levels_fu} are
#'   used, so each respondent counts once, and a message says which timepoint
#'   that is. Records at other timepoints, or with a missing timepoint, are
#'   not included.
#' @return Summary data frame
#' @export
#' @examples
#' example_data$ageband <- factor(
#'   example_data$ageband,
#'   levels = c("20 to 29", "30 to 39", "40 to 49", "50 to 59", "60 to 69", "70 to 79", "80 to 89")
#' )
#' example_data <- example_data[example_data$gender %in% c("Male", "Female"),]
#' example_data$value <- eq5d3l(example_data[, c("mo", "sc", "ua", "pd", "ad")],
#'                              country = "GB")
#' eq5d_utility_norms_comparison(
#'   example_data,
#'   name_utility = "value",
#'   name_fu = "time",
#'   levels_fu = c('Pre-op', 'Post-op'),
#'   name_groupvar = "gender",
#'   name_age = "ageband"
#' )

eq5d_utility_norms_comparison <- function(df,
                      name_utility = NULL,
                      name_fu = NULL,
                      levels_fu = NULL,
                      name_groupvar = NULL,
                      name_age){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df,
                     name_utility = name_utility,
                     name_fu = name_fu, levels_fu = levels_fu)
  name_utility <- temp$name_utility
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  # check existence of columns 
  names_all <- c(name_utility, name_fu, name_groupvar, name_age)
  .check_columns(df, name_utility = name_utility,
                 name_fu = name_fu,
                 name_groupvar = name_groupvar,
                 name_age = name_age)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  if (is.null(name_groupvar)) {
    df$groupvar <- "All"
  } else {
    names(df)[names(df) == name_groupvar] <- "groupvar"
  }
  names(df)[names(df) == name_age]      <- "age"
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  df <- .prep_utility(df = df, name = name_utility)
  df <- df[, c("groupvar", "age", "fu", "utility"), drop = FALSE]

  # The baseline only: the first timepoint in levels_fu. The summaries below
  # group by group and age alone, so every timepoint used to be pooled --
  # including those .prep_fu() had just warned would be excluded -- and a
  # respondent seen twice was counted twice (review A05).
  baseline <- levels(df$fu)[1L]
  if (length(levels(df$fu)) > 1L)
    message("eq5d_utility_norms_comparison(): using the baseline timepoint \"",
            baseline, "\" only; later timepoints are not included, so each ",
            "respondent is counted once.")
  df <- df[!is.na(df$fu) & df$fu == baseline, , drop = FALSE]

  # split age into categories
  if (is.numeric(df$age)) {
    age_breaks <- c(0, 18, seq(from = 25, to = 75, by = 10), Inf)
    df$age_cat <- cut(df$age, breaks = age_breaks, right = FALSE)
    df$age_cat <- factor(df$age_cat,
      levels = c("[0,18)", "[18,25)", "[25,35)", "[35,45)", "[45,55)", "[55,65)", "[65,75)", "[75,Inf)"),
      labels = c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"))
    df$age <- NULL
  } else if (is.factor(df$age)) {
    names(df)[names(df) == "age"] <- "age_cat"
  } else {
    stop("The 'age' column must be either numeric or factor.", call. = FALSE)
  }

  # A record without an age (band) belongs in no column, and the table had
  # no place for it: a missing band stopped the function. Such records are
  # left out, and the message says how many.
  no_age <- is.na(df$age_cat)
  if (any(no_age)) {
    message("eq5d_utility_norms_comparison(): ", sum(no_age), " record(s) ",
            "without an age or age band are not included.")
    df <- df[!no_age, , drop = FALSE]
  }
  # Groups are shown by label, a missing group as "(missing)"; the totals'
  # labels are made distinct from the groups' and the age bands' (review
  # A06, A07).
  df$groupvar <- .group_labels(df$groupvar)
  tot_g <- .total_label("Total", unique(df$groupvar))
  tot_a <- .total_label("Total", if (is.factor(df$age_cat)) levels(df$age_cat)
                                 else unique(df$age_cat))

  ### analysis ###

  # helper for overall summary without grouping
  .total_summary_4_4 <- function(u) {
    data.frame(
      Mean = mean(u, na.rm = TRUE),
      `Standard error` = sd(u, na.rm = TRUE) / sqrt(sum(!is.na(u))),
      `25th Percentile` = quantile(u, probs = 0.25, na.rm = TRUE, names = FALSE),
      `50th Percentile (median)` = median(u, na.rm = TRUE),
      `75th Percentile` = quantile(u, probs = 0.75, na.rm = TRUE, names = FALSE),
      n = sum(!is.na(u)),
      Missing = sum(is.na(u)),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }

  # summary by category
  summary_by <- .summary_table_4_4(df = df, group_by = c("groupvar", "age_cat"))
  # summary of totals by groupvar (age_cat = "Total")
  summary_total_age <- .summary_table_4_4(df = df, group_by = "groupvar")
  summary_total_age$age_cat <- tot_a
  # summary of totals by age_cat (groupvar = "Total")
  summary_total_by <- .summary_table_4_4(df = df, group_by = "age_cat")
  summary_total_by$groupvar <- tot_g
  # overall total
  st <- .total_summary_4_4(df$utility)
  st$groupvar <- tot_g
  st$age_cat  <- tot_a
  summary_total_by_age <- st

  # combine all
  combined <- rbind(summary_by, summary_total_age, summary_total_by, summary_total_by_age)

  # pivot: rows = (groupvar, stat_name), cols = age_cat values
  stat_cols <- setdiff(names(combined), c("groupvar", "age_cat"))
  all_gc <- unique(combined[, c("groupvar", "age_cat"), drop = FALSE])
  grp_vals <- unique(combined$groupvar)
  # Age bands in the order of the age factor's levels (for numeric ages, the
  # bands cut() made), then the total; bands nobody is in are left out. They
  # were in order of first appearance, so a band seen only in the second
  # group came after all the others (a 20-29 column after 80-89).
  band_order <- if (is.factor(df$age_cat)) levels(df$age_cat) else sort(unique(df$age_cat))
  present <- unique(as.character(combined$age_cat))
  age_vals <- c(intersect(band_order, present), setdiff(present, band_order))

  # build result: for each (groupvar, stat), create row; cols = age_cat
  result_list <- lapply(grp_vals, function(gv) {
    sub <- combined[combined$groupvar == gv, , drop = FALSE]
    do.call(rbind, lapply(stat_cols, function(sc) {
      row <- data.frame(groupvar = gv, name = sc, stringsAsFactors = FALSE)
      for (ac in age_vals) {
        s2 <- sub[sub$age_cat == ac, sc, drop = TRUE]
        row[[ac]] <- if (length(s2) == 1) s2 else NA_real_
      }
      row
    }))
  })
  retval <- do.call(rbind, result_list)
  rownames(retval) <- NULL

  # return value
  return(retval)
}


#' eq5d_profile_pchc_by_group_plot: Paretian Classification of Health Change (PCHC) by Group
#' This function computes PCHC categories between two time points for each subject, 
#' stratifies them by a grouping variable, and produces
#' a single bar chart with side-by-side bars showing the distribution of PCHC categories.
#' @param df Data frame containing EQ-5D dimensions, a grouping variable, patient ID, and follow-up columns
#' @param name_id Character string for the patient ID column
#' @param name_groupvar Character string for the grouping column (e.g., procedure). If NULL (default), the analysis is performed on the full population.
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector of length 2 indicating the order of follow-up time points (e.g., c("Pre-op", "Post-op"))
#' @return A list with two elements:
#'   \item{plot_data}{A tibble of PCHC percentages by group}
#'   \item{p}{A ggplot2 object showing a bar chart with side-by-side bars for each PCHC category}
#' @export
#' @inheritSection eq5d_profile_pchc_table Pairing records
#' @examples
#' result <- eq5d_profile_pchc_by_group_plot(
#'   df = example_data,
#'   name_id = "id",
#'   name_groupvar = "procedure",
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'   name_fu = "time",
#'   levels_fu = c("Pre-op", "Post-op")
#' )
#' result$p        # shows the plot
#' result$plot_data  # shows the summary table

eq5d_profile_pchc_by_group_plot <- function(df,
                         name_id,
                         name_groupvar = NULL,
                         names_eq5d = NULL,
                         name_fu = NULL,
                         levels_fu = NULL) {
  ### 1) Data Preparation ###

  # Replace NULL names with defaults (helper function that sets names_eq5d, name_fu, etc.)
  temp <- .get_names(df = df,
                     names_eq5d = names_eq5d,
                     name_fu = name_fu,
                     levels_fu = levels_fu)
  names_eq5d <- temp$names_eq5d
  name_fu    <- temp$name_fu
  levels_fu  <- temp$levels_fu

  # Check columns exist
  names_all <- c(name_id, name_groupvar, names_eq5d, name_fu)
  .check_columns(df, name_id = name_id,
                 name_groupvar = name_groupvar,
                 names_eq5d = names_eq5d,
                 name_fu = name_fu)

  # Keep only relevant columns
  df <- df[, names_all, drop = FALSE]

  # Rename for internal use
  names(df)[names(df) == name_id] <- "id"
  if (is.null(name_groupvar)) {
    df$groupvar <- "All"
  } else {
    names(df)[names(df) == name_groupvar] <- "groupvar"
  }

  # Prepare EQ-5D & Follow-up columns
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  # One change per respondent, from the first timepoint to the second: the
  # figure has no timepoint axis (review AI06).
  df <- .first_two_timepoints(df, "eq5d_profile_pchc_by_group_plot")
  levels_fu <- levels(df$fu)

  # One record per respondent and timepoint, each respondent in one group
  # (.check_pairs()); then sorted by respondent and time only, so the pairs
  # follow time whatever the group (review A03, A04).
  .check_pairs(df, by_group = !is.null(name_groupvar))
  df <- df[order(df$id, df$fu), , drop = FALSE]

  ### 2) Calculate PCHC (Paretian Classification) ###
  # .pchc() compares the first level in levels_fu (e.g. "Pre-op") vs. the second (e.g. "Post-op")
  df <- .pchc(df = df, level_fu_1 = levels_fu[1], add_noprobs = TRUE)
  df <- df[!is.na(df$state), , drop = FALSE]
  .stop_if_no_records(nrow(df), "eq5d_profile_pchc_by_group_plot", paired = TRUE)
  # The factor levels for PCHC categories can be: "No problems", "No change", "Improve", "Worsen", "Mixed change"
  df$state_noprobs <- factor(
    df$state_noprobs,
    levels = c("No problems", "No change", "Improve", "Worsen", "Mixed change")
  )

  ### 3) Summarize for Single Bar Chart ###
  # We want one bar per groupvar for each state_noprobs
  plot_data <- aggregate(rep(1L, nrow(df)),
                         by = list(groupvar = df$groupvar, state_noprobs = df$state_noprobs),
                         FUN = sum)
  names(plot_data)[names(plot_data) == "x"] <- "n"
  key_gv <- plot_data$groupvar
  plot_data$p <- plot_data$n / tapply(plot_data$n, key_gv, sum)[key_gv]

  ### 4) Build the Plot ###
  p <- ggplot2::ggplot(plot_data,
                       ggplot2::aes(x = .data$state_noprobs,
                                    y = .data$p,
                                    fill = .data$groupvar)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.9)) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(.data$p, accuracy = 0.1)),
      position = ggplot2::position_dodge(width = 0.9),
      vjust = -0.5
    ) +
    ggplot2::scale_x_discrete(name = "Pareto classification") +
    ggplot2::scale_y_continuous(
      name    = "Percentage of respondents",
      labels  = scales::percent_format(accuracy = 1),
      expand  = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ggplot2::labs(fill = "Group") +
    ggplot2::ggtitle("Paretian Classification of Health Change (PCHC) by group over time") +
    ggplot2::scale_fill_brewer(palette = "Blues") +
    ggplot2::theme_minimal(base_size = 14)

  ### 5) Reshape Plot Data for Return ###
  # Convert from long to wide so each group appears as a column
  plot_data_wide <- stats::reshape(
    plot_data[, c("state_noprobs", "groupvar", "p")],
    idvar     = "state_noprobs",
    timevar   = "groupvar",
    direction = "wide"
  )
  names(plot_data_wide) <- sub("^p\\.", "", names(plot_data_wide))
  plot_data_wide[is.na(plot_data_wide)] <- 0
  names(plot_data_wide)[names(plot_data_wide) == "state_noprobs"] <- "Change category"

  ### 6) Return ###
  # Return list with summarized data & the ggplot object (with optional theme modifications)
  return(list(
    plot_data = plot_data_wide,
    p = .modify_ggplot_theme(p = p)  # if .modify_ggplot_theme is available
  ))
}

#' eq5d_profile_better_dimensions_by_group_plot: Percentage of Respondents Who Improved in Each EQ-5D Dimension, by Group
#' This function calculates how many respondents improved in each dimension between
#' two time points and summarizes the results for each group. It then produces
#' a dimension-focused chart illustrating improvement percentages by dimension.
#' @param df Data frame containing EQ-5D columns, a grouping variable, an ID column, and a follow-up column
#' @param name_id Character string for the patient ID column
#' @param name_groupvar Character string for the grouping column (e.g. procedure). If NULL (default), the analysis is performed on the full population.
#' @param names_eq5d Character vector of EQ-5D dimension names
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector of length 2, specifying the order of the follow-up levels (e.g. c("Pre-op","Post-op"))
#' @return A list containing:
#'   \item{plot_data}{A data frame of improvements by group and dimension}
#'   \item{p}{A ggplot2 object produced by `.pchc_plot_by_dim()`}
#' @export
#'
#' @inheritSection eq5d_profile_pchc_table Pairing records
#' @examples
#' result <- eq5d_profile_better_dimensions_by_group_plot(
#'   df = example_data,
#'   name_id = "id",
#'   name_groupvar = "procedure",
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'   name_fu = "time",
#'   levels_fu = c("Pre-op","Post-op")
#' )
#' result$p
#' result$plot_data
#'
eq5d_profile_better_dimensions_by_group_plot <- function(df,
                         name_id,
                         name_groupvar = NULL,
                         names_eq5d = NULL,
                         name_fu = NULL,
                         levels_fu = NULL) {
  ### 1) Data Preparation ###
  
  # Replace NULL names with defaults
  temp <- .get_names(df = df, 
                     names_eq5d = names_eq5d, 
                     name_fu = name_fu, 
                     levels_fu = levels_fu)
  names_eq5d <- temp$names_eq5d
  name_fu    <- temp$name_fu
  levels_fu  <- temp$levels_fu
  
  # Check columns exist
  names_all <- c(name_id, name_groupvar, names_eq5d, name_fu)
  .check_columns(df, name_id = name_id,
                 name_groupvar = name_groupvar,
                 names_eq5d = names_eq5d,
                 name_fu = name_fu)
  
  # Keep only relevant columns
  df <- df[, names_all, drop = FALSE]

  # Rename internal columns
  names(df)[names(df) == name_id] <- "id"
  if (is.null(name_groupvar)) {
    df$groupvar <- "All"
  } else {
    names(df)[names(df) == name_groupvar] <- "groupvar"
  }

  # Prepare EQ-5D & Follow-up columns
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  # One change per respondent, from the first timepoint to the second: the
  # figure has no timepoint axis (review AI06).
  df <- .first_two_timepoints(df, "eq5d_profile_better_dimensions_by_group_plot")
  levels_fu <- levels(df$fu)

  # One record per respondent and timepoint, each respondent in one group
  # (.check_pairs()); then sorted by respondent and time only, so the pairs
  # follow time whatever the group (review A03, A04).
  .check_pairs(df, by_group = !is.null(name_groupvar))
  df <- df[order(df$id, df$fu), , drop = FALSE]

  ### 2) Analysis ###
  # .pchc() calculates difference columns like mo_diff, sc_diff, etc.
  df <- .pchc(df = df, level_fu_1 = levels_fu[1])
  df <- df[!is.na(df$state), , drop = FALSE]

  # Focus on those whose overall 'state' == "Improve"
  # then check dimension-specific improvements (e.g. mo_diff > 0)
  dimension_names <- c("mo_diff", "sc_diff", "ua_diff", "pd_diff", "ad_diff")

  df_imp <- df[df$state == "Improve", c("groupvar", "fu", dimension_names), drop = FALSE]
  # pivot longer manually
  # No respondent in this category is ordinary data, not an error, but there
  # is no chart to draw from it. Said plainly: `name = dn` is length 1, so an
  # empty subset used to fail inside data.frame() with "arguments imply
  # differing number of rows: 0, 1".
  if (nrow(df_imp) == 0L)
    stop("No respondent was classified as improving, so there is nothing to plot. ",
         "eq5d_profile_pchc_table() shows the classification counts.",
         call. = FALSE)

  df_long <- do.call(rbind, lapply(dimension_names, function(dn) {
    data.frame(groupvar = df_imp$groupvar,
               fu       = df_imp$fu,
               name     = rep(dn, nrow(df_imp)),
               value    = df_imp[[dn]],
               stringsAsFactors = FALSE)
  }))
  # group_by(groupvar, fu, name) summarise
  plot_data <- do.call(rbind, lapply(
    split(df_long, list(df_long$groupvar, df_long$fu, df_long$name), drop = TRUE),
    function(sub) {
      data.frame(groupvar = sub$groupvar[1],
                 fu       = sub$fu[1],
                 name     = sub$name[1],
                 n        = sum(sub$value > 0),
                 n_total  = nrow(sub),
                 stringsAsFactors = FALSE)
    }
  ))
  rownames(plot_data) <- NULL
  plot_data$p <- plot_data$n / plot_data$n_total
  # Convert dimension code to a nice label
  plot_data$name <- factor(
    plot_data$name,
    levels = dimension_names,
    labels = c("Mobility", "Self-care", "Usual activities", "Pain & Discomfort", "Anxiety & Depression")
  )
  
  ### 3) Plot ###
  # We assume .pchc_plot_by_dim() is a helper function that
  # takes a data frame w/ columns: groupvar, fu, name, p
  # and returns a ggplot object. Alternatively, you can build your own chart here.
  
  p <- .pchc_plot_by_dim(
    plot_data = plot_data,
    ylab      = "Percentage of respondents who improved overall",
    title     = "Percentage of respondents who improved overall \nby the dimensions in which they improved (%)",
    cols      = .gen_colours("green", length(unique(plot_data$groupvar)))
  )
  
  ### 4) Tidy Up & Return ###
  # Reshape from long to wide for final table
  # e.g. columns for each fu within each groupvar
  # If you want separate columns by groupvar as well, you can unify them in a single pivot.
  
  # Pivot only by fu, keeping groupvar as id col
  plot_data_wide <- stats::reshape(
    plot_data[, c("groupvar", "name", "fu", "p")],
    idvar     = c("groupvar", "name"),
    timevar   = "fu",
    direction = "wide"
  )
  names(plot_data_wide) <- sub("^p\\.", "", names(plot_data_wide))
  plot_data_wide[is.na(plot_data_wide)] <- 0
  names(plot_data_wide)[names(plot_data_wide) == "groupvar"] <- "Group"
  names(plot_data_wide)[names(plot_data_wide) == "name"]     <- "Values"

  return(list(
    plot_data = plot_data_wide,
    p         = .modify_ggplot_theme(p = p) # if you have a custom theme function
  ))
}


#’ Figure 1.2.3: Percentage of Respondents Who Worsened in Each EQ-5D Dimension, by Group
#'
#' This function identifies respondents with a "Worsen" PCHC state (i.e., overall
#' health state got worse between levels_fu[1] and levels_fu[2]), checks
#' dimension-specific changes (e.g., mo_diff < 0), and summarizes by a grouping variable
#' (e.g., procedure) and time points. It returns a data table and a ggplot object.
#'
#' @param df A data frame containing EQ-5D columns, a grouping variable, an ID column, and a follow-up column
#' @param name_id A character string for the patient ID column
#' @param name_groupvar A character string for the grouping column (e.g., procedure). If NULL (default), the analysis is performed on the full population.
#' @param names_eq5d A character vector of EQ-5D dimension names
#' @param name_fu A character string for the follow-up column
#' @param levels_fu A character vector of length 2, specifying the order of the follow-up levels (e.g., c("Pre-op","Post-op"))
#'
#' @return A list containing:
#'   \item{plot_data}{A data frame of "Worsen" percentages by group and dimension}
#'   \item{p}{A ggplot2 object produced by `.pchc_plot_by_dim()`}
#' @export
#'
#' @inheritSection eq5d_profile_pchc_table Pairing records
#' @examples
#' result <- eq5d_profile_worse_dimensions_by_group_plot(
#'   df = example_data,
#'   name_id = "id",
#'   name_groupvar = "procedure",
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'   name_fu = "time",
#'   levels_fu = c("Pre-op","Post-op")
#' )
#' result$p        # shows the plot
#' result$plot_data  # shows the summary table
#'
eq5d_profile_worse_dimensions_by_group_plot <- function(df,
                         name_id,
                         name_groupvar = NULL,
                         names_eq5d = NULL,
                         name_fu = NULL,
                         levels_fu = NULL) {
  
  ### 1) Data Preparation ###
  
  # Replace NULL names with defaults (helper function that sets names_eq5d, name_fu, etc.)
  temp <- .get_names(df = df,
                     names_eq5d = names_eq5d,
                     name_fu = name_fu,
                     levels_fu = levels_fu)
  
  names_eq5d <- temp$names_eq5d
  name_fu    <- temp$name_fu
  levels_fu  <- temp$levels_fu
  
  # Check columns exist
  names_all <- c(name_id, name_groupvar, names_eq5d, name_fu)
  .check_columns(df, name_id = name_id,
                 name_groupvar = name_groupvar,
                 names_eq5d = names_eq5d,
                 name_fu = name_fu)
  
  # Keep only relevant columns
  df <- df[, names_all, drop = FALSE]

  # Rename internal columns
  names(df)[names(df) == name_id] <- "id"
  if (is.null(name_groupvar)) {
    df$groupvar <- "All"
  } else {
    names(df)[names(df) == name_groupvar] <- "groupvar"
  }

  # Prepare EQ-5D & Follow-up columns
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  # One change per respondent, from the first timepoint to the second: the
  # figure has no timepoint axis (review AI06).
  df <- .first_two_timepoints(df, "eq5d_profile_worse_dimensions_by_group_plot")
  levels_fu <- levels(df$fu)

  # One record per respondent and timepoint, each respondent in one group
  # (.check_pairs()); then sorted by respondent and time only, so the pairs
  # follow time whatever the group (review A03, A04).
  .check_pairs(df, by_group = !is.null(name_groupvar))
  df <- df[order(df$id, df$fu), , drop = FALSE]

  ### 2) Analysis: PCHC ###

  # .pchc() calculates difference columns like mo_diff, sc_diff, etc.
  # level_fu_1 is the 'baseline' time point.
  df <- .pchc(df = df, level_fu_1 = levels_fu[1])
  df <- df[!is.na(df$state), , drop = FALSE]

  # We focus on those whose overall state == "Worsen"
  # then check dimension-specific changes, e.g., mo_diff < 0
  dimension_names <- c("mo_diff", "sc_diff", "ua_diff", "pd_diff", "ad_diff")

  df_wor <- df[df$state == "Worsen", c("groupvar", "fu", dimension_names), drop = FALSE]
  # pivot longer manually
  # No respondent in this category is ordinary data, not an error, but there
  # is no chart to draw from it. Said plainly: `name = dn` is length 1, so an
  # empty subset used to fail inside data.frame() with "arguments imply
  # differing number of rows: 0, 1".
  if (nrow(df_wor) == 0L)
    stop("No respondent was classified as worsening, so there is nothing to plot. ",
         "eq5d_profile_pchc_table() shows the classification counts.",
         call. = FALSE)

  df_long <- do.call(rbind, lapply(dimension_names, function(dn) {
    data.frame(groupvar = df_wor$groupvar,
               fu       = df_wor$fu,
               name     = rep(dn, nrow(df_wor)),
               value    = df_wor[[dn]],
               stringsAsFactors = FALSE)
  }))
  # group_by(groupvar, fu, name) summarise
  plot_data <- do.call(rbind, lapply(
    split(df_long, list(df_long$groupvar, df_long$fu, df_long$name), drop = TRUE),
    function(sub) {
      data.frame(groupvar = sub$groupvar[1],
                 fu       = sub$fu[1],
                 name     = sub$name[1],
                 n        = sum(sub$value < 0),
                 n_total  = nrow(sub),
                 stringsAsFactors = FALSE)
    }
  ))
  rownames(plot_data) <- NULL
  plot_data$p <- plot_data$n / plot_data$n_total
  # Convert dimension code to a nice label
  plot_data$name <- factor(
    plot_data$name,
    levels = dimension_names,
    labels = c("Mobility", "Self-care", "Usual activities", "Pain & Discomfort", "Anxiety & Depression")
  )
  
  ### 3) Plot ###
  
  # We assume .pchc_plot_by_dim() is a helper function that
  # creates a bar chart from plot_data with columns:
  # groupvar, fu, name, and p
  # The color palette is "orange" for Worsen by dimension
  p <- .pchc_plot_by_dim(
    plot_data = plot_data,
    ylab      = "Percentage of respondents who worsened overall",
    title     = "Percentage of respondents who worsened overall \nby the dimensions in which they got worse (%)",
    cols      = .gen_colours("orange", length(unique(plot_data$groupvar)))
  )
  
  ### 4) Tidy Up & Return ###

  # Reshape from long to wide for final table
  plot_data_wide <- stats::reshape(
    plot_data[, c("groupvar", "name", "fu", "p")],
    idvar     = c("groupvar", "name"),
    timevar   = "fu",
    direction = "wide"
  )
  names(plot_data_wide) <- sub("^p\\.", "", names(plot_data_wide))
  plot_data_wide[is.na(plot_data_wide)] <- 0
  names(plot_data_wide)[names(plot_data_wide) == "groupvar"] <- "Group"
  names(plot_data_wide)[names(plot_data_wide) == "name"]     <- "Values"

  return(list(
    plot_data = plot_data_wide,
    p         = .modify_ggplot_theme(p = p) # if you have a custom theme function
  ))
}


#' eq5d_profile_mixed_dimensions_by_group_plot: Percentage of Respondents Who Had a Mixed Change Overall,
#' by Dimension Improved or Worsened, Grouped by Procedure (or Other Grouping)
#'
#' This function focuses on patients classified as having "Mixed change" overall
#' (i.e., some dimensions improved, others worsened). It then examines which dimensions
#' improved vs. worsened for each subject. Results are summarized by a grouping variable
#' (e.g., procedure) and time points. The final output is a table plus a ggplot object.
#'
#' @param df Data frame containing columns for EQ-5D dimensions, a grouping variable,
#' a patient ID, and a follow-up variable
#' @param name_id Character string indicating the patient ID column
#' @param name_groupvar Character string for the grouping column (e.g. "procedure"). If NULL (default), the analysis is performed on the full population.
#' @param names_eq5d Character vector naming the EQ-5D dimensions (e.g. c("mo","sc","ua","pd","ad"))
#' @param name_fu Character string for the follow-up column (e.g. "time")
#' @param levels_fu Character vector of length 2 specifying the time order (e.g. c("Pre-op","Post-op"))
#'
#' @return A list with two elements:
#'   \item{plot_data}{A wide-format data frame of dimension-specific improvements/worsenings for "Mixed change"}
#'   \item{p}{A ggplot2 object showing a dimension-level bar chart from .pchc_plot_by_dim}
#'
#' @export
#'
#' @inheritSection eq5d_profile_pchc_table Pairing records
#' @examples
#' result <- eq5d_profile_mixed_dimensions_by_group_plot(
#'   df = example_data,
#'   name_id = "id",
#'   name_groupvar = "procedure",
#'   names_eq5d = c("mo","sc","ua","pd","ad"),
#'   name_fu = "time",
#'   levels_fu = c("Pre-op","Post-op")
#' )
#' result$plot_data
#' result$p
#'
eq5d_profile_mixed_dimensions_by_group_plot <- function(df,
                         name_id,
                         name_groupvar = NULL,
                         names_eq5d = NULL,
                         name_fu = NULL,
                         levels_fu = NULL) {
  
  ### 1) Data Preparation ###
  
  # Replace NULL names with defaults
  temp <- .get_names(df = df,
                     names_eq5d = names_eq5d,
                     name_fu = name_fu,
                     levels_fu = levels_fu)
  names_eq5d <- temp$names_eq5d
  name_fu    <- temp$name_fu
  levels_fu  <- temp$levels_fu
  
  # Check columns exist
  names_all <- c(name_id, name_groupvar, names_eq5d, name_fu)
  .check_columns(df, name_id = name_id,
                 name_groupvar = name_groupvar,
                 names_eq5d = names_eq5d,
                 name_fu = name_fu)
  
  # Keep only relevant columns
  df <- df[, names_all, drop = FALSE]

  # Rename for internal use
  names(df)[names(df) == name_id] <- "id"
  if (is.null(name_groupvar)) {
    df$groupvar <- "All"
  } else {
    names(df)[names(df) == name_groupvar] <- "groupvar"
  }

  # Prepare EQ-5D & Follow-up columns
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  # One change per respondent, from the first timepoint to the second: the
  # figure has no timepoint axis (review AI06).
  df <- .first_two_timepoints(df, "eq5d_profile_mixed_dimensions_by_group_plot")
  levels_fu <- levels(df$fu)

  # One record per respondent and timepoint, each respondent in one group
  # (.check_pairs()); then sorted by respondent and time only, so the pairs
  # follow time whatever the group (review A03, A04).
  .check_pairs(df, by_group = !is.null(name_groupvar))
  df <- df[order(df$id, df$fu), , drop = FALSE]

  ### 2) Analysis: Identify Mixed Change ###

  # .pchc() adds difference columns like mo_diff, sc_diff, etc.
  # level_fu_1 is the 'baseline' time point
  df <- .pchc(df = df, level_fu_1 = levels_fu[1])
  df <- df[!is.na(df$state), , drop = FALSE]

  # We'll focus on those with "Mixed change" overall
  dimension_names <- c("mo_diff", "sc_diff", "ua_diff", "pd_diff", "ad_diff")

  # We also define the fu values and directions for labeling
  n_time <- length(levels_fu)
  # For example: "Improve: Pre-op", "Improve: Post-op", "Worsen: Pre-op", "Worsen: Post-op"
  levels_fu_change <- c(
    paste0("Improve: ", levels_fu),
    paste0("Worsen: ", levels_fu)
  )

  # 2a) Summarize dimension-level improvements & worsenings among "Mixed change" patients
  df_mix <- df[df$state == "Mixed change", c("groupvar", "fu", dimension_names), drop = FALSE]
  # pivot longer manually
  # No respondent in this category is ordinary data, not an error, but there
  # is no chart to draw from it. Said plainly: `name = dn` is length 1, so an
  # empty subset used to fail inside data.frame() with "arguments imply
  # differing number of rows: 0, 1".
  if (nrow(df_mix) == 0L)
    stop("No respondent was classified as a mixed change, so there is nothing to plot. ",
         "eq5d_profile_pchc_table() shows the classification counts.",
         call. = FALSE)

  df_long <- do.call(rbind, lapply(dimension_names, function(dn) {
    data.frame(groupvar = df_mix$groupvar,
               fu       = df_mix$fu,
               name     = rep(dn, nrow(df_mix)),
               value    = df_mix[[dn]],
               stringsAsFactors = FALSE)
  }))
  # group_by(groupvar, fu, name) summarise
  agg <- do.call(rbind, lapply(
    split(df_long, list(df_long$groupvar, df_long$fu, df_long$name), drop = TRUE),
    function(sub) {
      data.frame(groupvar  = sub$groupvar[1],
                 fu        = as.character(sub$fu[1]),
                 name      = sub$name[1],
                 n_improve = sum(sub$value > 0),
                 n_worsen  = sum(sub$value < 0),
                 n_total   = nrow(sub),
                 stringsAsFactors = FALSE)
    }
  ))
  rownames(agg) <- NULL
  agg$Improve <- agg$n_improve / agg$n_total
  agg$Worsen  <- agg$n_worsen  / agg$n_total
  # Convert e.g. "mo_diff" -> "Mobility"
  agg$name <- factor(
    agg$name,
    levels = dimension_names,
    labels = c("Mobility", "Self-care", "Usual activities", "Pain & Discomfort", "Anxiety & Depression")
  )
  # pivot Improve & Worsen into single column
  plot_data_imp <- data.frame(groupvar = agg$groupvar, fu = agg$fu, name = agg$name,
                               change = "Improve", p = agg$Improve, stringsAsFactors = FALSE)
  plot_data_wor <- data.frame(groupvar = agg$groupvar, fu = agg$fu, name = agg$name,
                               change = "Worsen",  p = agg$Worsen,  stringsAsFactors = FALSE)
  plot_data <- rbind(plot_data_imp, plot_data_wor)
  # Build combined factor "Improve: Pre-op" / "Worsen: Pre-op" etc.
  plot_data$fu <- factor(paste0(plot_data$change, ": ", plot_data$fu), levels = levels_fu_change)
  
  ### 3) Plot ###
  
  plot_data2 <- plot_data
  plot_data2$groupvar <- paste(plot_data$groupvar, plot_data$fu)
  
  plot_data2$name   <- factor(plot_data2$name,   levels = c("Mobility", "Self-care", "Usual activities", "Pain & Discomfort", "Anxiety & Depression"))
  plot_data2$change <- factor(plot_data2$change, levels = c("Improve", "Worsen"))
  plot_data2 <- plot_data2[order(plot_data2$name, plot_data2$change), , drop = FALSE]
  
  plot_data2$groupvar <- factor(plot_data2$groupvar, levels = unique(plot_data2$groupvar))
  
  p <- .pchc_plot_by_dim(
    plot_data = plot_data2,
    ylab      = "Percentage of respondents who had mixed change",
    title     = "Percentage of respondents who had a mixed change overall \nby the dimensions in which they improved and worsened (%)",
    cols      = c(
      .gen_colours("green", n_time*2),  # for "Improve: ...",
      .gen_colours("orange", n_time*2)  # for "Worsen: ..."
    ),
    text_rotate = TRUE
  )
  
  ### 4) Reshape & Return ###

  # Pivot wide by the combined fu factor
  plot_data_wide <- stats::reshape(
    plot_data[, c("groupvar", "name", "fu", "p")],
    idvar     = c("groupvar", "name"),
    timevar   = "fu",
    direction = "wide"
  )
  names(plot_data_wide) <- sub("^p\\.", "", names(plot_data_wide))
  plot_data_wide[is.na(plot_data_wide)] <- 0
  names(plot_data_wide)[names(plot_data_wide) == "groupvar"] <- "Group"
  names(plot_data_wide)[names(plot_data_wide) == "name"]     <- "Values"

  return(list(
    plot_data = plot_data_wide,
    p         = .modify_ggplot_theme(p = p)
  ))
}

#' eq5d_profile_health_profile_grid: Health Profile Grid (HPG) for Two Time Points
#'
#' This function creates a Health Profile Grid (HPG) for EQ-5D data, plotting each
#' individual's change in health states (ranked from best to worst) between two time
#' points. A diagonal reference line indicates no change; points above the line reflect
#' improvement, and points below indicate deterioration.
#'
#' @details
#' The ranking is over \strong{every health state the instrument allows}, not
#' only those the data contain: all 243 states for the EQ-5D-3L and the
#' EQ-5D-Y-3L, and all 3,125 for the EQ-5D-5L. Each is valued with the value
#' set named by \code{country} and \code{eq5d_version}, and ordered best to
#' worst, so the best possible state is rank 1 and the worst is rank 243 or
#' 3,125. A rank therefore means the same thing in every dataset valued with
#' the same value set, and the axes span the whole classification system
#' rather than the part of it a particular sample happens to occupy.
#'
#' This is why the function takes a value set rather than a column of
#' pre-calculated values: the states that do \emph{not} occur in the data
#' still have to be valued in order to place the states that do.
#'
#' @param df A data frame containing EQ-5D columns, an ID column and a follow-up column
#' @param names_eq5d A character vector of EQ-5D dimension names
#' @param name_fu A character string for the follow-up column
#' @param levels_fu A character vector of length 2, specifying the order of the follow-up levels (e.g., c("Pre-op","Post-op"))
#' @param name_id A character string for the patient ID column
#' @param eq5d_version Version of the EQ-5D instrument: "3L", "5L" or "Y3L".
#'   It decides how many states are ranked.
#' @param country Value set code used to rank the states; see
#'   \code{\link{eqvs_display}} for the codes available.
#' @return A list with components:
#'   \item{plot_data}{The plot data with ranks and classification.}
#'   \item{p}{A \code{ggplot2} object displaying the HPG scatter plot.}
#' @export
#' @inheritSection eq5d_profile_pchc_table Pairing records
#' @examples
#' tmp <- eq5d_profile_health_profile_grid(
#'            df = example_data,
#'            names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'            name_fu = "time",
#'            levels_fu = c("Pre-op", "Post-op"),
#'            name_id = "id",
#'            eq5d_version = "3L",
#'            country = "GB"
#'        )

eq5d_profile_health_profile_grid <- function(df,
                         names_eq5d,
                         name_fu,
                         levels_fu = NULL,
                         name_id,
                         eq5d_version,
                         country) {
  ### 1) Data Preparation ###
  # Replace NULL names with defaults (helper function that sets names_eq5d, name_fu, etc.)
  temp <- .get_names(df = df,
                     names_eq5d = names_eq5d,
                     name_fu = name_fu,
                     levels_fu = levels_fu,
                     eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  name_fu    <- temp$name_fu
  levels_fu  <- temp$levels_fu
  eq5d_version <- temp$eq5d_version

  # The grid plots one timepoint against another, so it needs exactly two.
  if (length(levels_fu) != 2L)
    stop("The Health Profile Grid compares exactly two timepoints; ",
         "`levels_fu` has ", length(levels_fu), ": ",
         paste0("\"", levels_fu, "\"", collapse = ", "),
         ". Give the two to compare, baseline first.", call. = FALSE)
  
  # Check columns exist
  names_all <- c(name_id,  names_eq5d, name_fu)
  .check_columns(df, name_id = name_id,
                 names_eq5d = names_eq5d,
                 name_fu = name_fu)
  
  # Keep only relevant columns
  df <- df[, names_all, drop = FALSE]

  # Rename for internal use
  names(df)[names(df) == name_id] <- "id"

  # Prepare EQ-5D & Follow-up columns. The instrument is passed so that a
  # level it does not have becomes NA, and the dimensions are mo..ad from here
  # on whatever the caller called them. Rows at a timepoint other than the
  # two compared are dropped, as .prep_fu() says they are.
  df <- .prep_eq5d(df = df, names = names_eq5d, eq5d_version = eq5d_version)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  df <- df[!is.na(df$fu), , drop = FALSE]

  # Every state the instrument allows, valued and ordered best to worst. The
  # states absent from the data are ranked too: a state's rank is its
  # position in the whole classification system, so it means the same thing
  # in every sample valued with this value set.
  vs <- data.frame(profile = make_all_EQ_indexes(version = eq5d_version))
  vs$utility <- eq5d(vs$profile, country = country, version = eq5d_version)
  if (all(is.na(vs$utility)))
    stop("No state could be valued with country = \"", country,
         "\" and version = \"", eq5d_version,
         "\", so the states cannot be ranked. See eqvs_display() for the ",
         "codes available.", call. = FALSE)

  # One record per respondent and timepoint (.check_pairs(); review A03).
  .check_pairs(df)
  df <- df[order(df$id, df$fu), , drop = FALSE]

  ### 2) Calculate PCHC (Paretian Classification) ###
  # .pchc() compares the first level in levels_fu (e.g. "Pre-op") vs. the second (e.g. "Post-op")
  df <- .pchc(df = df, level_fu_1 = levels_fu[1], add_noprobs = TRUE)
  df <- df[!is.na(df$state), , drop = FALSE]
  if (nrow(df) == 0L)
    stop("No respondent has a valid EQ-5D profile at both \"", levels_fu[1],
         "\" and \"", levels_fu[2], "\", so there is nothing to plot.",
         call. = FALSE)
  # The factor levels for PCHC categories can be: "No problems", "No change", "Improve", "Worsen", "Mixed change"
  df$state_noprobs <- factor(
    df$state_noprobs,
    levels = c("No problems", "No change", "Improve", "Worsen", "Mixed change")
  )

  ### 3) Summarize for HPG ###
  # Rank the observed states, best first
  vs <- vs[order(-vs$utility), , drop = FALSE]
  vs$rank <- seq_len(nrow(vs))
  
  # Build profiles. .prep_eq5d() renamed the dimensions; the caller's names
  # are gone, and looking them up failed with "replacement has 0 rows".
  eqdims <- c("mo", "sc", "ua", "pd", "ad")
  for (d in eqdims) {
    df[[paste0(d, "_t2")]] <- df[[d]]
    df[[paste0(d, "_t1")]] <- df[[d]] + df[[paste0(d, "_diff")]]
  }
  # Build profile codes row-by-row using apply
  t1_cols <- paste0(eqdims, "_t1")
  t2_cols <- paste0(eqdims, "_t2")
  df$profile_t1 <- as.integer(apply(df[, t1_cols, drop = FALSE], 1,
                                     function(r) paste0(r, collapse = "")))
  df$profile_t2 <- as.integer(apply(df[, t2_cols, drop = FALSE], 1,
                                     function(r) paste0(r, collapse = "")))

  # Get rank via match
  df$rank_t1 <- vs$rank[match(df$profile_t1, vs$profile)]
  df$rank_t2 <- vs$rank[match(df$profile_t2, vs$profile)]
  
  # Cteate plot
  max_rank <- nrow(vs)
  # `rank_t1` is the first follow-up level and `rank_t2` the second, so the
  # x axis holds the second and the y axis the first. The labels used to be
  # the other way round, which made the figure say that the x axis was
  # baseline when it was follow-up. Mapping and labels agree now, and
  # improvement stays above the diagonal: an individual whose state improves
  # has a worse (higher) baseline rank on y than follow-up rank on x.
  p <- ggplot(df, aes(x = .data$rank_t2, y = .data$rank_t1, color = .data$state, shape = .data$state)) +
    geom_point(size = 3) +
    geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "black") +
    scale_x_continuous(limits = c(1, max_rank), expand = c(0,0)) +
    scale_y_continuous(limits = c(1, max_rank), expand = c(0,0)) +
    labs(
      title    = "Health Profile Grid (HPG)",
      x        = paste0(levels_fu[2], " rank"),
      y        = paste0(levels_fu[1], " rank"),
      color    = "Classification",  # Legend title for color
      shape    = "Classification"   # Legend title for shape
    ) +
    scale_shape_manual(values = c(
      "Improve"    = 3,  
      "No change"  = 16,  
      "Worsen"     = 4,   
      "Mixed change" = 17  
    )) +
    scale_color_manual(values = c(
      "Improve"      = "#69AB3E",
      "No change"    = "#FDC020",
      "Worsen"       = "#BF2518",
      "Mixed change" = "#5782BF"
    )) +
    
    theme_minimal(base_size = 14) + 
    theme(
      panel.grid = element_blank(),
      axis.line  = element_line(color = "black"),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.box       = "horizontal"
    ) +
    coord_fixed(ratio = 1)
  
  return(list(plot_data = df, p = p))
}



#' eq5d_profile_lss_utility_plot: EQ-5D values plotted against LSS
#' 
#' @param df Data frame with the EQ-5D dimension and value columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_utility Character string naming the column of pre-calculated
#'   EQ-5D values. If \code{NULL} (default), the column \code{"utility"} is
#'   used. The values are analysed as they stand; see
#'   \code{\link{eq5d}} for calculating them from the dimensions.
#' @param eq5d_version Version of the EQ-5D instrument
#' @return Summary plot and data used for plotting
#' @export
#' @examples
#' df <- data.frame(make_all_EQ_states(version = "5L"))
#' df$value <- eq5d5l(df[, c("mo", "sc", "ua", "pd", "ad")], country = "US")
#' tmp <- eq5d_profile_lss_utility_plot(
#'  df, 
#'  names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'  name_utility = "value",
#'  eq5d_version = "5L"
#' )
#' tmp$p
#' tmp$plot_data

eq5d_profile_lss_utility_plot <- function(df,
                       names_eq5d = NULL,
                       name_utility = NULL,
                       eq5d_version = NULL){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, names_eq5d = names_eq5d,
                     name_utility = name_utility, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  name_utility <- temp$name_utility
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d, name_utility)
  .check_columns(df, names_eq5d = names_eq5d, name_utility = name_utility)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation. The score comes from the dimensions;
  # the values are taken as supplied.
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_lss = TRUE, eq5d_version = eq5d_version)
  df <- .prep_utility(df = df, name = name_utility)
  df <- df[, c("lss", "utility"), drop = FALSE]

  ### analysis ###

  # prepare data for plotting: remove NAs, group by lss, summarise
  df2 <- df[!is.na(df$lss) & !is.na(df$utility), , drop = FALSE]
  .stop_if_no_records(nrow(df2), "eq5d_profile_lss_utility_plot", with_value = TRUE)
  plot_data <- do.call(rbind, lapply(split(df2$utility, df2$lss), function(vals) {
    data.frame(median = median(vals), min = min(vals), max = max(vals))
  }))
  plot_data$lss <- as.integer(rownames(plot_data))
  rownames(plot_data) <- NULL
  plot_data <- plot_data[order(plot_data$lss), , drop = FALSE]

  # graphical parameters
  x_breaks <- 1:100
  y_breaks <- seq(from = -1, to = 1, by = 0.2)

  # The three summaries used to be drawn as three identical horizontal
  # segments at the same x. Where they coincide -- LSS 5 and 15 for the
  # EQ-5D-3L, LSS 5 and 25 for the 5L, each reachable by exactly one health
  # state -- they were drawn on top of one another and only the last was
  # visible, so the median vanished at the two most interpretable points on
  # the plot.
  #
  # The lowest and highest values are now short horizontal ticks joined by a
  # vertical line, and the median is a point drawn after them, so it is always
  # on top. Shape as well as colour separates the median from the two range
  # marks, so the distinction survives in greyscale.
  lvl <- c("Median", "Lowest", "Highest")

  tick_data <- do.call(rbind, list(
    data.frame(lss = plot_data$lss, name = "Lowest",  value = plot_data$min),
    data.frame(lss = plot_data$lss, name = "Highest", value = plot_data$max)
  ))
  tick_data$name <- factor(tick_data$name, levels = lvl)

  median_data <- data.frame(lss = plot_data$lss,
                            name = factor("Median", levels = lvl),
                            value = plot_data$median)

  # plot
  p <- ggplot() +
    # connect lowest and highest with a vertical line
    geom_segment(data = plot_data,
                 aes(x = .data$lss, xend = .data$lss,
                     y = .data$min, yend = .data$max),
                 colour = "grey50") +
    # lowest and highest, as short horizontal ticks
    geom_segment(data = tick_data,
                 aes(x = .data$lss - 0.2, xend = .data$lss + 0.2,
                     y = .data$value, yend = .data$value,
                     colour = .data$name)) +
    # median last, so that it is visible even where all three coincide
    geom_point(data = median_data,
               aes(x = .data$lss, y = .data$value,
                   colour = .data$name, shape = .data$name),
               size = 2) +
    scale_colour_manual(values = c(Median = "#BF2518", Lowest = "#5782BF",
                                   Highest = "#69AB3E"),
                        limits = lvl, drop = FALSE) +
    # 45 is a horizontal dash, matching the ticks drawn for those two series
    scale_shape_manual(values = c(Median = 16, Lowest = 45, Highest = 45),
                       limits = lvl, drop = FALSE) +
    # plot title
    ggtitle("EQ-5D values plotted against the LSS (Level Sum Score)") +
    # manipulate x-axis
    scale_x_continuous(name = "Level Sum Score",
                       breaks = x_breaks,
                       expand = expansion(add = c(0.5, 0.5))) +
    # manipulate y-axis
    scale_y_continuous(name = "EQ-5D value",
                       breaks = y_breaks)

  # tidy up summary
  names(plot_data)[names(plot_data) == "lss"]    <- "LSS"
  names(plot_data)[names(plot_data) == "median"] <- "Median"
  names(plot_data)[names(plot_data) == "min"]    <- "Minimum"
  names(plot_data)[names(plot_data) == "max"]    <- "Maximum"
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' eq5d_profile_lfs_utility_plot: EQ-5D values plotted against LFS
#' 
#' @param df Data frame with the EQ-5D dimension and value columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_utility Character string naming the column of pre-calculated
#'   EQ-5D values. If \code{NULL} (default), the column \code{"utility"} is
#'   used. The values are analysed as they stand; see
#'   \code{\link{eq5d}} for calculating them from the dimensions.
#' @param eq5d_version Version of the EQ-5D instrument
#' @return Summary plot and data used for plotting
#' @export
#' @examples
#' example_data$value <- eq5d3l(example_data[, c("mo", "sc", "ua", "pd", "ad")],
#'                              country = "GB")
#' tmp <- eq5d_profile_lfs_utility_plot(
#'  example_data, 
#'  names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'  name_utility = "value",
#'  eq5d_version = "3L"
#' )
#' tmp$p
#' tmp$plot_data

eq5d_profile_lfs_utility_plot <- function(df,
                        names_eq5d = NULL,
                        name_utility = NULL,
                        eq5d_version = NULL){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, names_eq5d = names_eq5d,
                     name_utility = name_utility, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  name_utility <- temp$name_utility
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d, name_utility)
  .check_columns(df, names_eq5d = names_eq5d, name_utility = name_utility)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation. The score comes from the dimensions;
  # the values are taken as supplied.
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_lfs = TRUE, eq5d_version = eq5d_version)
  df <- .prep_utility(df = df, name = name_utility)
  df <- df[, c("lfs", "utility"), drop = FALSE]

  ### analysis ###

  # prepare data for plotting: remove NAs, group by lfs, summarise
  df2 <- df[!is.na(df$lfs) & !is.na(df$utility), , drop = FALSE]
  .stop_if_no_records(nrow(df2), "eq5d_profile_lfs_utility_plot", with_value = TRUE)
  plot_data <- do.call(rbind, lapply(split(df2$utility, df2$lfs), function(vals) {
    data.frame(median = median(vals), min = min(vals), max = max(vals))
  }))
  plot_data$lfs <- rownames(plot_data)
  rownames(plot_data) <- NULL
  # order by descending median
  plot_data <- plot_data[order(-plot_data$median), , drop = FALSE]

  # impose order on the x-axis
  lfs_levels <- plot_data$lfs
  plot_data$lfs_f <- factor(plot_data$lfs, levels = lfs_levels)
  
  # axis breaks
  n <- length(lfs_levels)
  i <- seq(from = 1, to = n, by = 3)
  x_breaks <- (1:n)[i]
  x_labels <- lfs_levels[i]
  y_breaks <- seq(from = -1, to = 1, by = 0.2)
  
  # Same construction, and the same defect, as eq5d_profile_lss_utility_plot():
  # an LFS group reachable by a single health state has median, lowest and
  # highest all equal -- 500, 050 and 005 for the EQ-5D-3L -- and the three
  # identical segments hid one another, taking the median with them. Ticks for
  # the range, a point for the median, drawn last.
  lvl <- c("Median", "Lowest", "Highest")

  tick_data <- do.call(rbind, list(
    data.frame(lfs_f = plot_data$lfs_f, name = "Lowest",  value = plot_data$min),
    data.frame(lfs_f = plot_data$lfs_f, name = "Highest", value = plot_data$max)
  ))
  tick_data$name <- factor(tick_data$name, levels = lvl)

  median_data <- data.frame(lfs_f = plot_data$lfs_f,
                            name = factor("Median", levels = lvl),
                            value = plot_data$median)

  # plot
  p <- ggplot() +
    # connect lowest and highest with a vertical line
    geom_segment(data = plot_data,
                 aes(x = as.numeric(.data$lfs_f), xend = as.numeric(.data$lfs_f),
                     y = .data$min, yend = .data$max),
                 colour = "grey50") +
    # lowest and highest, as short horizontal ticks
    geom_segment(data = tick_data,
                 aes(x = as.numeric(.data$lfs_f) - 0.5,
                     xend = as.numeric(.data$lfs_f) + 0.5,
                     y = .data$value, yend = .data$value,
                     colour = .data$name)) +
    # median last, so that it is visible even where all three coincide
    geom_point(data = median_data,
               aes(x = as.numeric(.data$lfs_f), y = .data$value,
                   colour = .data$name, shape = .data$name),
               size = 2) +
    scale_colour_manual(values = c(Median = "#BF2518", Lowest = "#5782BF",
                                   Highest = "#69AB3E"),
                        limits = lvl, drop = FALSE) +
    # 45 is a horizontal dash, matching the ticks drawn for those two series
    scale_shape_manual(values = c(Median = 16, Lowest = 45, Highest = 45),
                       limits = lvl, drop = FALSE) +
    # plot title
    ggtitle("EQ-5D values plotted against the LFS (Level Frequency Score)") +
    # manipulate x-axis
    scale_x_continuous(name = "Level Frequency Score", 
                       breaks = x_breaks,
                       labels = x_labels,
                       expand = expansion(add = c(0.5, 0.5))) +
    # manipulate y-axis
    scale_y_continuous(name = "EQ-5D value",
                       breaks = y_breaks)
  p <- .modify_ggplot_theme(p = p) + 
    # tidy up the graph
    theme(
      # rotate labels in the x-axis
      axis.text.x = element_text(angle = 90))
  
  # tidy up summary
  plot_data <- plot_data[, setdiff(names(plot_data), "lfs_f"), drop = FALSE]
  names(plot_data)[names(plot_data) == "lfs"]    <- "LFS"
  names(plot_data)[names(plot_data) == "median"] <- "Median"
  names(plot_data)[names(plot_data) == "min"]    <- "Minimum"
  names(plot_data)[names(plot_data) == "max"]    <- "Maximum"
  
  return(list(plot_data = plot_data, p = p))
}

#' eq5d_vas_histogram: EQ VAS scores
#' 
#' @param df Data frame with the VAS column
#' @param name_vas Character string for the VAS column
#' @return Summary plot and data used for plotting
#' @export
#' @examples
#' tmp <- eq5d_vas_histogram(example_data, name_vas = 'vas')
#' tmp$p
#' tmp$plot_data

eq5d_vas_histogram <- function(df, name_vas = NULL){
  
  ### data preparation ###
  # replace NULL names with defaults
  temp <- .get_names(name_vas = name_vas)
  name_vas <- temp$name_vas
  # check existence of columns 
  names_all <- c(name_vas)
  .check_columns(df, name_vas = name_vas)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  df <- .prep_vas(df = df, name = name_vas)
  
  ### analysis ###
  
  # plot
  p <- ggplot(df, aes(x = .data$vas)) +
    geom_bar(width = 0.1, fill = "blue") + 
    scale_x_continuous(name = "EQ VAS score", 
                       breaks = seq(from = 0, to = 100, by = 5),
                       limits = c(-3, 103),
                       expand = c(0, 0)) +
    scale_y_continuous(name = "Frequency",
                       expand = expansion(mult = c(0, 0.05))) + 
    ggtitle("EQ VAS Scores") + 
    # tidy up the graph
    theme(
      # reinstate x-axis ticks
      axis.ticks.x = element_line(colour = "grey50"))
  
  # output plotting data: count for each value between 0 & 100
  vas_factor <- factor(df$vas, levels = 1:100)
  tbl <- tabulate(vas_factor, nbins = 100L)
  plot_data <- data.frame(vas = factor(1:100, levels = 1:100), count = tbl,
                          stringsAsFactors = FALSE)
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' eq5d_vas_grouped_distribution_plot: Mid-point EQ VAS scores
#' 
#' @param df Data frame with the VAS column
#' @param name_vas Character string for the VAS column
#' @return Summary plot and data used for plotting
#' @export
#' @examples
#' tmp <- eq5d_vas_grouped_distribution_plot(example_data, name_vas = 'vas')
#' tmp$p
#' tmp$plot_data

eq5d_vas_grouped_distribution_plot <- function(df, name_vas = NULL){
  
  # produce output to be plotted (Table 3.2)
  # do not add totals
  plot_data <- eq5d_vas_distribution_table(df = df, name_vas = name_vas, add_na_total = FALSE)
  # remove Range column, not needed
  plot_data <- plot_data[, setdiff(names(plot_data), "Range"), drop = FALSE]
  
  # plot
  p <- ggplot(plot_data, aes(x = .data$Midpoint, y = .data$Frequency)) +
    geom_bar(width = 0.7, fill = "blue", stat = "identity") + coord_flip() +
    scale_x_continuous(name = "EQ VAS score (Midpoint)", 
                       breaks = seq(from = 0, to = 100, by = 10),
                       limits = c(-3, 103),
                       expand = c(0, 0)) +
    scale_y_continuous(name = "Frequency",
                       expand = expansion(mult = c(0, 0.05))) + 
    ggtitle("EQ VAS Scores (Midpoint)") +
    # tidy up the graph
    theme(
      # reinstate x-axis ticks
      axis.ticks.x = element_line(colour = "grey50"),
      # remove y-axis ticks
      axis.ticks.y = element_blank())
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' eq5d_utility_over_time_plot: EQ-5D values by timepoints: mean values and 95\% confidence intervals
#' 
#' @param df Data frame with the EQ-5D value and follow-up columns
#' @param name_utility Character string naming the column of pre-calculated
#'   EQ-5D values. If \code{NULL} (default), the column \code{"utility"} is
#'   used. The values are analysed as they stand; see
#'   \code{\link{eq5d}} for calculating them from the dimensions.
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @return Summary plot and data used for plotting
#' @section Confidence intervals:
#' The 95\% confidence interval of a mean is the mean plus or minus 1.96
#' standard errors, the standard error being the standard deviation divided
#' by the square root of the number of values: a normal approximation, which
#' is less accurate for small groups.
#' @export
#' @examples
#' example_data$value <- eq5d3l(example_data[, c("mo", "sc", "ua", "pd", "ad")],
#'                              country = "GB")
#' tmp <- eq5d_utility_over_time_plot(
#'  example_data,
#'  name_utility = "value",
#'  name_fu = "time",
#'  levels_fu = c('Pre-op', 'Post-op')
#' )
#' tmp$p
#' tmp$plot_data

eq5d_utility_over_time_plot <- function(df,
                       name_utility = NULL,
                       name_fu = NULL,
                       levels_fu = NULL) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, name_utility = name_utility,
                     name_fu = name_fu, levels_fu = levels_fu)
  name_utility <- temp$name_utility
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  # check existence of columns 
  names_all <- c(name_utility, name_fu)
  .check_columns(df, name_utility = name_utility, name_fu = name_fu)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  df <- .prep_utility(df = df, name = name_utility)
  df <- df[, c("fu", "utility"), drop = FALSE]
  
  ### analysis ###
  
  # prepare data for plotting
  plot_data <- .summary_mean_ci(df = df, group_by = "fu")
  
  # plot
  p <- ggplot(data = plot_data, aes(x = .data$fu, 
                                    y = .data$mean, ymin = .data$ci_lb, ymax = .data$ci_ub)) +
    # connect means with a line
    # create a dummy group variable as geom_line only connects points that belong to the same group
    geom_line(aes(group = 1), colour = "red") +
    # plot means
    geom_point(colour = "green") + 
    # add error bars
    geom_errorbar(width = 0.1) +
    # plot title
    ggtitle(paste0("EQ-5D values by ", name_fu, ": mean and 95% confidence intervals")) +
    # manipulate x-axis
    scale_x_discrete(name = "time") +
    # manipulate y-axis
    scale_y_continuous(name = "EQ-5D value")
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' eq5d_utility_by_group_plot: Mean EQ-5D values and 95\% confidence intervals: all vs by groupvar
#' 
#' @param df Data frame with the EQ-5D value and grouping columns
#' @param name_utility Character string naming the column of pre-calculated
#'   EQ-5D values. If \code{NULL} (default), the column \code{"utility"} is
#'   used. The values are analysed as they stand; see
#'   \code{\link{eq5d}} for calculating them from the dimensions.
#' @param name_groupvar Character string for the grouping column. If NULL (default), the analysis is performed on the full population.
#' @return Summary plot and data used for plotting
#' @inheritSection eq5d_utility_over_time_plot Confidence intervals
#' @export
#' @examples
#' example_data$value <- eq5d3l(example_data[, c("mo", "sc", "ua", "pd", "ad")],
#'                              country = "GB")
#' tmp <- eq5d_utility_by_group_plot(
#'  example_data,
#'  name_utility = "value",
#'  name_groupvar = "procedure"
#' )
#' tmp$p
#' tmp$plot_data

eq5d_utility_by_group_plot <- function(df,
                       name_utility = NULL,
                       name_groupvar = NULL) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, name_utility = name_utility)
  name_utility <- temp$name_utility
  # check existence of columns 
  names_all <- c(name_groupvar, name_utility)
  .check_columns(df, name_groupvar = name_groupvar, name_utility = name_utility)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  if (is.null(name_groupvar)) {
    df$groupvar <- "All"
  } else {
    names(df)[names(df) == name_groupvar] <- "groupvar"
  }
  df <- .prep_utility(df = df, name = name_utility)
  df <- df[, c("groupvar", "utility"), drop = FALSE]

  ### analysis ###

  # prepare data for plotting
  plot_data <- .summary_mean_ci(df = df, group_by = "groupvar")
  # Shown by label, the total's distinct from every group's (review A06,
  # A07): a group called "All groups" made the factor levels below collide.
  plot_data$groupvar <- .group_labels(plot_data$groupvar)
  tot <- .total_label("All groups", plot_data$groupvar)

  # add totals
  plot_data_total <- .summary_mean_ci(df = df, group_by = NULL)
  plot_data_total$groupvar <- tot
  plot_data_total <- plot_data_total[, c("groupvar", setdiff(names(plot_data_total), "groupvar")), drop = FALSE]

  # combine
  groupvar_levels <- c(plot_data$groupvar, tot)
  plot_data <- rbind(plot_data, plot_data_total)
  plot_data$groupvar <- factor(plot_data$groupvar, levels = groupvar_levels)
  plot_data <- plot_data[order(plot_data$groupvar), , drop = FALSE]

  groupvar_label <- if (is.null(name_groupvar)) "Group" else name_groupvar
  # plot
  p <- ggplot(data = plot_data, aes(x = .data$groupvar, y = .data$mean, ymin = .data$ci_lb, ymax = .data$ci_ub)) +
    # plot means
    geom_bar(colour = "blue", fill = "blue", stat = "identity", width = 0.5) + 
    geom_point() +
    # add error bars
    geom_errorbar(width = 0.1, size = 1) +
    # plot title
    ggtitle(paste0("Mean EQ-5D values 95% confidence intervals: all vs by ", groupvar_label)) +
    # manipulate x-axis
    scale_x_discrete(name = groupvar_label) +
    # manipulate y-axis
    scale_y_continuous(name = "EQ-5D value",
                       limits = c(0, 1),
                       breaks = seq(from = 0, to = 1, by = 0.1),
                       expand = c(0, 0))
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' eq5d_utility_change_by_group_plot: EQ-5D values: smoothed lines and confidence intervals by groupvar
#' 
#' @param df Data frame with the EQ-5D value, follow-up and grouping columns
#' @param name_utility Character string naming the column of pre-calculated
#'   EQ-5D values. If \code{NULL} (default), the column \code{"utility"} is
#'   used. The values are analysed as they stand; see
#'   \code{\link{eq5d}} for calculating them from the dimensions.
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column.
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @param name_groupvar Character string for the grouping column. If NULL (default), the analysis is performed on the full population.
#' @return Summary plot and data used for plotting
#' @inheritSection eq5d_utility_over_time_plot Confidence intervals
#' @export
#' @examples
#' example_data$value <- eq5d3l(example_data[, c("mo", "sc", "ua", "pd", "ad")],
#'                              country = "GB")
#' tmp <- eq5d_utility_change_by_group_plot(
#'  example_data,
#'  name_utility = "value",
#'  name_fu = "time",
#'  levels_fu = c('Pre-op', 'Post-op'),
#'  name_groupvar = "procedure"
#' )
#' tmp$p
#' tmp$plot_data

eq5d_utility_change_by_group_plot <- function(df,
                       name_utility = NULL,
                       name_fu = NULL,
                       levels_fu = NULL,
                       name_groupvar = NULL) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, name_utility = name_utility,
                     name_fu = name_fu, levels_fu = levels_fu)
  name_utility <- temp$name_utility
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  # check existence of columns 
  names_all <- c(name_utility, name_fu, name_groupvar)
  .check_columns(df, name_utility = name_utility,
                 name_fu = name_fu,
                 name_groupvar = name_groupvar)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  if (is.null(name_groupvar)) {
    df$groupvar <- factor("All")
  } else {
    names(df)[names(df) == name_groupvar] <- "groupvar"
    df$groupvar <- factor(df$groupvar)
  }
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  df <- .prep_utility(df = df, name = name_utility)
  df <- df[, c("fu", "groupvar", "utility"), drop = FALSE]
  
  ### analysis ###
  
  # prepare data for plotting
  plot_data <- .summary_mean_ci(df = df, group_by = c("fu", "groupvar"))

  groupvar_label <- if (is.null(name_groupvar)) "Group" else name_groupvar
  # plot
  p <- ggplot(data = plot_data, aes(x = .data$fu,
                                    y = .data$mean, ymin = .data$ci_lb, ymax = .data$ci_ub,
                                    group = .data$groupvar, colour = .data$groupvar)) +
    # plot means
    geom_point() +
    # add error bars
    geom_errorbar(width = 0.1, alpha = 0.5) +
    # connect means with a smooth line
    geom_line() +
    # plot title
    ggtitle(paste0("Longitudinal EQ-5D values by ", name_fu, " and ", groupvar_label, ":\nmeans and 95% confidence intervals")) +
    # manipulate x-axis
    scale_x_discrete(name = name_fu) +
    # manipulate y-axis
    scale_y_continuous(name = "EQ-5D value")
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' eq5d_utility_distribution_plot: EQ-5D values: smoothed lines and confidence intervals by groupvar
#' 
#' @param df Data frame with the EQ-5D value column
#' @param name_utility Character string naming the column of pre-calculated
#'   EQ-5D values. If \code{NULL} (default), the column \code{"utility"} is
#'   used. The values are analysed as they stand; see
#'   \code{\link{eq5d}} for calculating them from the dimensions.
#' @return Summary plot and data used for plotting
#' @export
#' @examples
#' example_data$value <- eq5d3l(example_data[, c("mo", "sc", "ua", "pd", "ad")],
#'                              country = "GB")
#' tmp <- eq5d_utility_distribution_plot(
#'  example_data,
#'  name_utility = "value"
#' )
#' tmp$p
#' tmp$plot_data

eq5d_utility_distribution_plot <- function(df, 
                       name_utility = NULL) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, name_utility = name_utility)
  name_utility <- temp$name_utility
  # check existence of columns 
  names_all <- c(name_utility)
  .check_columns(df, name_utility = name_utility)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  df <- .prep_utility(df = df, name = name_utility)
  df <- df[, "utility", drop = FALSE]

  ### analysis ###

  # prepare data for plotting
  util_tbl <- table(df$utility)
  plot_data <- data.frame(
    utility = as.numeric(names(util_tbl)),
    count   = as.integer(util_tbl),
    stringsAsFactors = FALSE
  )
  plot_data <- plot_data[order(plot_data$utility), , drop = FALSE]
  
  # plot
  p <- ggplot(plot_data, aes(x = .data$utility, y = .data$count)) +
    geom_bar(fill = "blue", stat = "identity") + 
    scale_x_continuous(name = "EQ-5D value") +
    scale_y_continuous(name = "Frequency") + 
    ggtitle("Distribution of EQ-5D values") + 
    theme(
      # reinstate axis.ticks
      axis.ticks = element_line(colour = "grey50"))
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' eq5d_utility_vas_scatter_plot: EQ-5D values: smoothed lines and confidence intervals by groupvar
#' 
#' @param df Data frame with the EQ-5D value and EQ VAS columns
#' @param name_utility Character string naming the column of pre-calculated
#'   EQ-5D values. If \code{NULL} (default), the column \code{"utility"} is
#'   used. The values are analysed as they stand; see
#'   \code{\link{eq5d}} for calculating them from the dimensions.
#' @param name_vas Character string for the VAS column
#' @return Summary plot and data used for plotting
#' @export
#' @examples
#' example_data$value <- eq5d3l(example_data[, c("mo", "sc", "ua", "pd", "ad")],
#'                              country = "GB")
#' tmp <- eq5d_utility_vas_scatter_plot(
#'    example_data,
#'    name_utility = "value",
#'    name_vas = "vas"
#'  )
#' tmp$p
#' tmp$plot_data

eq5d_utility_vas_scatter_plot <- function(df,
                        name_utility = NULL,
                        name_vas = NULL) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, name_utility = name_utility, name_vas = name_vas)
  name_utility <- temp$name_utility
  name_vas <- temp$name_vas
  # check existence of columns 
  names_all <- c(name_utility, name_vas)
  .check_columns(df, name_utility = name_utility, name_vas = name_vas)
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  df <- .prep_vas(df = df, name = name_vas)
  df <- .prep_utility(df = df, name = name_utility)
  df <- df[, c("vas", "utility"), drop = FALSE]

  ### analysis ###

  # prepare data for plotting
  plot_data <- df[order(df$vas, df$utility), , drop = FALSE]
  
  # plot
  p <- ggplot(plot_data, aes(x = .data$vas, y = .data$utility)) +
    geom_point(colour = "blue") + 
    scale_x_continuous(name = "EQ VAS") +
    scale_y_continuous(name = "EQ-5D value") + 
    ggtitle("EQ-5D values plotted against EQ VAS")
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}