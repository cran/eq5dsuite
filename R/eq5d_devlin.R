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
  tmp <- .freqtab(df, names_eq5d, name_cat, levels_cat, eq5d_version)
  tmp[-(NROW(tmp)-(1:2)),]
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
  mc <- as.list(match.call()[-1])
  
  if(is.null(name_fu)){
    if("follow-up" %in% colnames(df)) mc$name_fu <- "follow_up"
    if("fu" %in% colnames(df)) mc$name_fu <- "fu"
  }
  do.call(.freqtab, mc)
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
  temp <- .get_names(names_eq5d = names_eq5d, 
                     eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- names_eq5d
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d, eq5d_version = eq5d_version, add_state = TRUE)
  df <- df[, "state", drop = FALSE]

  ### analysis ####

  # all states (non-NA only), aggregated and sorted
  df_cc <- df[!is.na(df$state), , drop = FALSE]
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
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
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

  df <- df[order(df$id, df$groupvar, df$fu), , drop = FALSE]
  # check uniqueness of id-groupvar-fu combinations
  .check_uniqueness(df, group_by = c("id", "groupvar", "fu"))

  ### analysis ###

  # calculate change
  df <- .pchc(df = df, level_fu_1 = levels_fu[1])
  df <- df[!is.na(df$state), , drop = FALSE]
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
#' @return Summary data frame
#' @export
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
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
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
  df <- df[order(df$id, df$groupvar, df$fu), , drop = FALSE]
  # check uniqueness of id-groupvar-fu combinations
  .check_uniqueness(df, group_by = c("id", "groupvar", "fu"))
  
  ### analysis ###
  
  # calculate change
  df <- .pchc(df = df, level_fu_1 = levels_fu[1], add_noprobs = TRUE)
  df <- df[!is.na(df$state), , drop = FALSE]

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

  # summarise classes within those with problems
  df_probs <- df[df$state_noprobs != "No problems", , drop = FALSE]
  summary_with_probs <- aggregate(rep(1L, nrow(df_probs)),
    by = list(groupvar = df_probs$groupvar, fu = df_probs$fu,
              state_noprobs = df_probs$state_noprobs), FUN = sum)
  names(summary_with_probs)[names(summary_with_probs) == "x"] <- "n"
  key_wp <- paste(summary_with_probs$groupvar, summary_with_probs$fu, sep = "\001")
  summary_with_probs$p <- summary_with_probs$n /
    tapply(summary_with_probs$n, key_wp, sum)[key_wp]

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
      retval[[paste(gv, f, "n", sep = "_")]] <- ifelse(is.na(vals_n[retval$state_noprobs]), 0, vals_n[retval$state_noprobs])
      retval[[paste(gv, f, "p", sep = "_")]] <- ifelse(is.na(vals_p[retval$state_noprobs]), 0, vals_p[retval$state_noprobs])
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
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  names(df)[names(df) == name_id] <- "id"
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  # sort by id - time

  df <- df[order(df$id, df$fu), , drop = FALSE]
  # check uniqueness of id-fu combinations
  .check_uniqueness(df, group_by = c("id", "fu"))
  
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

  # compute lagged difference (dplyr::lag equivalent)
  prev_val <- c(NA_real_, head(df_long$value, -1))
  df_long$diff <- prev_val - df_long$value
  df_long$level_change <- ifelse(is.na(prev_val), NA_character_,
                                 paste0(prev_val, "-", df_long$value))
  # set baseline to NA
  df_long$diff[as.character(df_long$fu) == as.character(level_fu_1)] <- NA_real_
  df_long$level_change[as.character(df_long$fu) == as.character(level_fu_1)] <- NA_character_

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
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary data frame
#' @export
#' @examples
#' df <- data.frame(make_all_EQ_states(version = "5L"))
#' eq5d_profile_lss_utility_summary(
#'   df, 
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'   eq5d_version = "3L", 
#'   country = "US"
#' )

eq5d_profile_lss_utility_summary <- function(df, 
                      names_eq5d = NULL,
                      eq5d_version = NULL,
                      country){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(names_eq5d = names_eq5d, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_lss = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country)
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
  temp <- .get_names(names_eq5d = names_eq5d, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
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
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary data frame
#' @export
#' @examples
#' eq5d_profile_lfs_mean_utility(
#'   example_data, 
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'   eq5d_version = "3L",
#'   country = "UK"
#' )

eq5d_profile_lfs_mean_utility <- function(df, 
                      names_eq5d = NULL,
                      eq5d_version = NULL,
                      country){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(names_eq5d = names_eq5d, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_lfs = TRUE, eq5d_version = eq5d_version,
                   add_utility = TRUE, country = country)
  df <- df[, c("lfs", "utility"), drop = FALSE]

  ### analysis ###

  # exclude NAs and count per (utility, lfs) combination
  df_cc <- df[!is.na(df$lfs), , drop = FALSE]
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
  names(retval)[names(retval) == "utility"] <- "EQ-5D Value"
  rownames(retval) <- NULL

  # return value
  return(retval)
}

#' eq5d_profile_lfs_utility_summary: Summary statistics of EQ-5D values by LFS (Level Frequency Score)
#' 
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary data frame
#' @export
#' @examples
#' eq5d_profile_lfs_utility_summary(
#'   example_data, 
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'   eq5d_version = "3L",
#'   country = "UK"
#' )

eq5d_profile_lfs_utility_summary <- function(df, 
                       names_eq5d = NULL,
                       eq5d_version = NULL,
                       country){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(names_eq5d = names_eq5d, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d, 
                   add_state = TRUE, 
                   add_lfs = TRUE, eq5d_version = eq5d_version,
                   add_utility = TRUE, country = country) 
  
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

#' eq5d_profile_density_curve: Generate a Health State Density Curve (HSDC) for EQ-5D Data
#'
#' This function calculates and plots the Health State Density Curve (HSDC) for a given
#' EQ-5D dataset. It concatenates dimension values to form health state profiles, filters
#' out invalid states based on the specified EQ-5D version, then computes the cumulative
#' distribution of profiles (profiles vs. observations). A diagonal reference line
#' indicates a perfectly even distribution. The function also calculates the Health State
#' Density Index (HSDI), representing how sharply the observed distribution deviates from
#' the diagonal.
#'
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @return A list containing:
#'   \item{plot_data}{A data frame with the cumulative distribution of profiles}
#'   \item{p}{A ggplot2 object showing the Health State Density Index}
#' @export
#' @examples
#' figure <- eq5d_profile_density_curve(
#'             df = example_data, 
#'             names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'             eq5d_version = "3L"
#'           )
#' figure$plot_data
#' figure$p
#' @importFrom utils head

eq5d_profile_density_curve <- function(df, names_eq5d, eq5d_version) {
  
  # Retrieve validated names & version from helper function
  temp <- .get_names(names_eq5d = names_eq5d, eq5d_version = eq5d_version)
  names_eq5d   <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  
  # Check existence of columns
  names_all <- c(names_eq5d)
  if (!all(names_all %in% colnames(df))) {
    stop("Provided column names not in the data frame. Stopping.")
  }
  
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
  hsdi <- round(2 * hsdi_area, 3)
  
  # Plot Health State Density Curve
  p <- ggplot2::ggplot(profile_freq, ggplot2::aes(
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
      subtitle = paste0("HSDI = ", hsdi),
      x        = "Cumulative proportion of observations",
      y        = "Cumulative proportion of profiles"
    ) +
    # Force the aspect ratio to 1:1
    ggplot2::coord_fixed(ratio = 1) +
    # Limit x,y range from 0 to 1
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::theme_minimal()
  
  return(list(plot_data = profile_freq, p = p))
}

#' eq5d_vas_summary: EQ VAS Score by timepoints
#' 
#' @param df Data frame with the VAS and the follow-up columns
#' @param name_vas Character string for the VAS column
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' @return Summary data frame
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
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
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
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
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
#' @param df Data frame with the EQ-5D and follow-up columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary data frame
#' @export
#' @examples
#' eq5d_utility_summary(
#'   example_data,
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'   name_fu = "time",
#'   levels_fu = c('Pre-op', 'Post-op'),
#'   eq5d_version = "3L",
#'   country = "UK"
#' )

eq5d_utility_summary <- function(df, 
                      names_eq5d = NULL,
                      name_fu = NULL,
                      levels_fu = NULL,
                      eq5d_version = NULL,
                      country){
  
  ### data preparation ###
  
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
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  df <- df[, c("fu", "utility"), drop = FALSE]

  ### analysis ###

  # summarise
  retval <- .summary_cts_by_fu(df = df, name_v = "utility")
  
  # return value
  return(retval)
}

#' eq5d_utility_summary_by_group:EQ-5D values: by groupvar
#' 
#' @param df Data frame with the EQ-5D, follow-up and grouping columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_groupvar Character string for the grouping column. If NULL (default), the analysis is performed on the full population.
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country.
#' @return Summary data frame
#' @export
#' @examples
#' eq5d_utility_summary_by_group(
#'   example_data,
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'   name_groupvar = "procedure",
#'   eq5d_version = "3L",
#'   country = "UK"
#' )

eq5d_utility_summary_by_group <- function(df,
                      names_eq5d = NULL,
                      name_groupvar = NULL,
                      eq5d_version = NULL,
                      country){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, 
                     names_eq5d = names_eq5d, 
                     eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d, name_groupvar)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  if (is.null(name_groupvar)) {
    df$groupvar <- "All"
  } else {
    names(df)[names(df) == name_groupvar] <- "groupvar"
  }
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country)
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

  # combine: pivot longer then wider (rows = stat names, cols = groups)
  combined <- rbind(summary_by, summary_total)
  stat_cols <- setdiff(names(combined), "groupvar")
  grp_vals  <- unique(combined$groupvar)
  retval <- data.frame(name = stat_cols, stringsAsFactors = FALSE)
  for (gv in grp_vals) {
    sub <- combined[combined$groupvar == gv, stat_cols, drop = FALSE]
    retval[[gv]] <- as.numeric(sub[1, ])
  }
  rownames(retval) <- NULL

  # return value
  return(retval)
}

#' eq5d_utility_norms_comparison:EQ-5D values: by age and groupvar
#' 
#' @param df Data frame with the EQ-5D, age, follow-up and grouping columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column.
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @param name_groupvar Character string for the grouping column. If NULL (default), the analysis is performed on the full population.
#' @param name_age Character string for the age column
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary data frame
#' @export
#' @examples
#' example_data$ageband <- factor(
#'   example_data$ageband,
#'   levels = c("20 to 29", "30 to 39", "40 to 49", "50 to 59", "60 to 69", "70 to 79", "80 to 89")
#' )
#' example_data <- example_data[example_data$gender %in% c("Male", "Female"),]
#' eq5d_utility_norms_comparison(
#'   example_data,
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'   name_fu = "time",
#'   levels_fu = c('Pre-op', 'Post-op'),
#'   name_groupvar = "gender",
#'   name_age = "ageband",
#'   eq5d_version = "3L",
#'   country = "UK"
#' )

eq5d_utility_norms_comparison <- function(df,
                      names_eq5d = NULL,
                      name_fu = NULL,
                      levels_fu = NULL,
                      name_groupvar = NULL,
                      name_age,
                      eq5d_version = NULL,
                      country){
  
  ### data preparation ###
  
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
  names_all <- c(names_eq5d, name_fu, name_groupvar, name_age)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  if (is.null(name_groupvar)) {
    df$groupvar <- "All"
  } else {
    names(df)[names(df) == name_groupvar] <- "groupvar"
  }
  names(df)[names(df) == name_age]      <- "age"
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  df <- df[, c("groupvar", "age", "fu", "utility"), drop = FALSE]

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
    stop("The 'age' column must be either numeric or factor. Stopping.")
  }

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
  summary_total_age$age_cat <- "Total"
  # summary of totals by age_cat (groupvar = "Total")
  summary_total_by <- .summary_table_4_4(df = df, group_by = "age_cat")
  summary_total_by$groupvar <- "Total"
  # overall total
  st <- .total_summary_4_4(df$utility)
  st$groupvar <- "Total"
  st$age_cat  <- "Total"
  summary_total_by_age <- st

  # combine all
  combined <- rbind(summary_by, summary_total_age, summary_total_by, summary_total_by_age)

  # pivot: rows = (groupvar, stat_name), cols = age_cat values
  stat_cols <- setdiff(names(combined), c("groupvar", "age_cat"))
  all_gc <- unique(combined[, c("groupvar", "age_cat"), drop = FALSE])
  grp_vals <- unique(combined$groupvar)
  age_vals <- unique(combined$age_cat)

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
  if (!all(names_all %in% colnames(df))) {
    stop("Provided column names not in dataframe. Stopping.")
  }

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

  # Sort by (id, groupvar, fu)
  df <- df[order(df$id, df$groupvar, df$fu), , drop = FALSE]

  # Check uniqueness: each (id, groupvar, fu) should appear at most once
  .check_uniqueness(df, group_by = c("id", "groupvar", "fu"))

  ### 2) Calculate PCHC (Paretian Classification) ###
  # .pchc() compares the first level in levels_fu (e.g. "Pre-op") vs. the second (e.g. "Post-op")
  df <- .pchc(df = df, level_fu_1 = levels_fu[1], add_noprobs = TRUE)
  df <- df[!is.na(df$state), , drop = FALSE]
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
#' two time points and summarizes the results for each group. The, it prodcuces 
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
  if (!all(names_all %in% colnames(df))) {
    stop("Provided column names not in dataframe. Stopping.")
  }
  
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

  # Sort by (id, groupvar, fu)
  df <- df[order(df$id, df$groupvar, df$fu), , drop = FALSE]

  # Ensure uniqueness of (id, groupvar, fu)
  .check_uniqueness(df, group_by = c("id", "groupvar", "fu"))

  ### 2) Analysis ###
  # .pchc() calculates difference columns like mo_diff, sc_diff, etc.
  df <- .pchc(df = df, level_fu_1 = levels_fu[1])
  df <- df[!is.na(df$state), , drop = FALSE]

  # Focus on those whose overall 'state' == "Improve"
  # then check dimension-specific improvements (e.g. mo_diff > 0)
  dimension_names <- c("mo_diff", "sc_diff", "ua_diff", "pd_diff", "ad_diff")

  df_imp <- df[df$state == "Improve", c("groupvar", "fu", dimension_names), drop = FALSE]
  # pivot longer manually
  df_long <- do.call(rbind, lapply(dimension_names, function(dn) {
    data.frame(groupvar = df_imp$groupvar,
               fu       = df_imp$fu,
               name     = dn,
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
  if (!all(names_all %in% colnames(df))) {
    stop("Provided column names not in dataframe. Stopping.")
  }
  
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

  # Sort by (id, groupvar, fu)
  df <- df[order(df$id, df$groupvar, df$fu), , drop = FALSE]

  # Ensure uniqueness of (id, groupvar, fu)
  .check_uniqueness(df, group_by = c("id", "groupvar", "fu"))

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
  df_long <- do.call(rbind, lapply(dimension_names, function(dn) {
    data.frame(groupvar = df_wor$groupvar,
               fu       = df_wor$fu,
               name     = dn,
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
  if (!all(names_all %in% colnames(df))) {
    stop("Provided column names not in dataframe. Stopping.")
  }
  
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

  # Sort by (id, groupvar, fu)
  df <- df[order(df$id, df$groupvar, df$fu), , drop = FALSE]

  # Check uniqueness
  .check_uniqueness(df, group_by = c("id", "groupvar", "fu"))

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
  df_long <- do.call(rbind, lapply(dimension_names, function(dn) {
    data.frame(groupvar = df_mix$groupvar,
               fu       = df_mix$fu,
               name     = dn,
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
#' @param df A data frame containing EQ-5D columns, a grouping variable, an ID column, and a follow-up column
#' @param names_eq5d A character vector of EQ-5D dimension names
#' @param name_fu A character string for the follow-up column
#' @param levels_fu A character vector of length 2, specifying the order of the follow-up levels (e.g., c("Pre-op","Post-op"))
#' @param name_id A character string for the patient ID column
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' @return A list with components:
#'   \item{plot_data}{The plot data with ranks and classification.}
#'   \item{p}{A \code{ggplot2} object displaying the HPG scatter plot.}
#' @export
#' @examples
#' tmp <- eq5d_profile_health_profile_grid(
#'            df = example_data, 
#'            names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'            name_fu = "time", 
#'            levels_fu = c("Pre-op", "Post-op"), 
#'            name_id = "id", 
#'            eq5d_version = "3L", 
#'            country = "UK"
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
  
  # Check columns exist
  names_all <- c(name_id,  names_eq5d, name_fu)
  if (!all(names_all %in% colnames(df))) {
    stop("Provided column names not in dataframe. Stopping.")
  }
  
  # Keep only relevant columns
  df <- df[, names_all, drop = FALSE]

  # Rename for internal use
  names(df)[names(df) == name_id] <- "id"

  # Prepare EQ-5D & Follow-up columns
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)

  # Sort by (id, fu)
  df <- df[order(df$id, df$fu), , drop = FALSE]

  # Check uniqueness: each (id, fu) should appear at most once
  .check_uniqueness(df, group_by = c("id", "fu"))

  ### 2) Calculate PCHC (Paretian Classification) ###
  # .pchc() compares the first level in levels_fu (e.g. "Pre-op") vs. the second (e.g. "Post-op")
  df <- .pchc(df = df, level_fu_1 = levels_fu[1], add_noprobs = TRUE)
  df <- df[!is.na(df$state), , drop = FALSE]
  # The factor levels for PCHC categories can be: "No problems", "No change", "Improve", "Worsen", "Mixed change"
  df$state_noprobs <- factor(
    df$state_noprobs,
    levels = c("No problems", "No change", "Improve", "Worsen", "Mixed change")
  )

  ### 3) Summarize for HPG ###
  # Create value set
  vs <- data.frame(profile = make_all_EQ_indexes(version = eq5d_version))
  vs$utility <- eq5d(vs$profile, country = country, version = eq5d_version)
  vs <- vs[order(-vs$utility), , drop = FALSE]
  vs$rank <- seq_len(nrow(vs))
  
  # Build profiles
  eqdims <- names_eq5d
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
  p <- ggplot(df, aes(x = .data$rank_t2, y = .data$rank_t1, color = .data$state, shape = .data$state)) +
    geom_point(size = 3) +
    geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "black") +
    scale_x_continuous(limits = c(1, max_rank), expand = c(0,0)) +
    scale_y_continuous(limits = c(1, max_rank), expand = c(0,0)) +
    labs(
      title    = "Health Profile Grid (HPG)",
      x        = paste0(levels_fu[1], " rank"),
      y        = paste0(levels_fu[2], " rank"),
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
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary plot and data used for plotting
#' @export
#' @examples
#' df <- data.frame(make_all_EQ_states(version = "5L"))
#' tmp <- eq5d_profile_lss_utility_plot(
#'  df, 
#'  names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'  eq5d_version = "5L", 
#'  country = "US"
#' )
#' tmp$p
#' tmp$plot_data

eq5d_profile_lss_utility_plot <- function(df,
                       names_eq5d = NULL,
                       eq5d_version = NULL,
                       country){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(names_eq5d = names_eq5d, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_lss = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country)
  df <- df[, c("lss", "utility"), drop = FALSE]

  ### analysis ###

  # prepare data for plotting: remove NAs, group by lss, summarise
  df2 <- df[!is.na(df$lss) & !is.na(df$utility), , drop = FALSE]
  plot_data <- do.call(rbind, lapply(split(df2$utility, df2$lss), function(vals) {
    data.frame(median = median(vals), min = min(vals), max = max(vals))
  }))
  plot_data$lss <- as.integer(rownames(plot_data))
  rownames(plot_data) <- NULL
  plot_data <- plot_data[order(plot_data$lss), , drop = FALSE]

  # graphical parameters
  x_breaks <- 1:100
  y_breaks <- seq(from = -1, to = 1, by = 0.2)

  # pivot longer for segment plot
  seg_data <- do.call(rbind, list(
    data.frame(lss = plot_data$lss, name = "Median", value = plot_data$median),
    data.frame(lss = plot_data$lss, name = "Lowest",  value = plot_data$min),
    data.frame(lss = plot_data$lss, name = "Highest", value = plot_data$max)
  ))
  seg_data$name <- factor(seg_data$name, levels = c("Median", "Lowest", "Highest"))

  # plot
  p <- ggplot() +
    # plot median, min and max
    geom_segment(data = seg_data,
                 aes(x = .data$lss - 0.2, xend = .data$lss + 0.2,
                     y = .data$value, yend = .data$value,
                     colour = .data$name)) +
    # connect values with a line segment
    geom_segment(data = plot_data, aes(x = .data$lss, xend = .data$lss, y = min, yend = max)) +
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
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary plot and data used for plotting
#' @export
#' @examples
#' tmp <- eq5d_profile_lfs_utility_plot(
#'  example_data, 
#'  names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'  eq5d_version = "3L",
#'  country = "UK"
#' )
#' tmp$p
#' tmp$plot_data

eq5d_profile_lfs_utility_plot <- function(df,
                        names_eq5d = NULL,
                        eq5d_version = NULL,
                        country){
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(names_eq5d = names_eq5d, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_lfs = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country)
  df <- df[, c("lfs", "utility"), drop = FALSE]

  ### analysis ###

  # prepare data for plotting: remove NAs, group by lfs, summarise
  df2 <- df[!is.na(df$lfs) & !is.na(df$utility), , drop = FALSE]
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
  
  # pivot longer for segment plot
  seg_data2 <- do.call(rbind, list(
    data.frame(lfs = plot_data$lfs, lfs_f = plot_data$lfs_f, name = "Median", value = plot_data$median),
    data.frame(lfs = plot_data$lfs, lfs_f = plot_data$lfs_f, name = "Lowest",  value = plot_data$min),
    data.frame(lfs = plot_data$lfs, lfs_f = plot_data$lfs_f, name = "Highest", value = plot_data$max)
  ))
  seg_data2$name <- factor(seg_data2$name, levels = c("Median", "Lowest", "Highest"))

  # plot
  p <- ggplot() +
    # plot median, min and max
    geom_segment(data = seg_data2,
                 aes(x = as.numeric(.data$lfs_f) - 0.5, xend = as.numeric(.data$lfs_f) + 0.5,
                     y = .data$value, yend = .data$value,
                     colour = .data$name)) +
    # connect values with a line segment
    geom_segment(data = plot_data, aes(x = as.numeric(.data$lfs_f), xend = as.numeric(.data$lfs_f), y = min, yend = max)) +
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
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
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
#' @param df Data frame with the VAS columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary plot and data used for plotting
#' @export
#' @examples
#' tmp <- eq5d_utility_over_time_plot(
#'  example_data,
#'  names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'  name_fu = "time",
#'  levels_fu = c('Pre-op', 'Post-op'),
#'  eq5d_version = "3L",
#'  country = "UK"
#' )
#' tmp$p
#' tmp$plot_data

eq5d_utility_over_time_plot <- function(df,
                       names_eq5d = NULL,
                       name_fu = NULL,
                       levels_fu = NULL,
                       eq5d_version = NULL,
                       country) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, names_eq5d = names_eq5d, 
                     name_fu = name_fu, levels_fu = levels_fu,
                     eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d, name_fu)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
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
#' @param df Data frame with the EQ-5D and grouping columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_groupvar Character string for the grouping column. If NULL (default), the analysis is performed on the full population.
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country.
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary plot and data used for plotting
#' @export
#' @examples
#' tmp <- eq5d_utility_by_group_plot(
#'  example_data,
#'  names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'  name_groupvar = "procedure",
#'  eq5d_version = "3L",
#'  country = "UK"
#' )
#' tmp$p
#' tmp$plot_data

eq5d_utility_by_group_plot <- function(df,
                       names_eq5d = NULL,
                       name_groupvar = NULL,
                       eq5d_version = NULL,
                       country) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(names_eq5d = names_eq5d, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(name_groupvar, names_eq5d)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  if (is.null(name_groupvar)) {
    df$groupvar <- "All"
  } else {
    names(df)[names(df) == name_groupvar] <- "groupvar"
  }
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country)
  df <- df[, c("groupvar", "utility"), drop = FALSE]

  ### analysis ###

  # prepare data for plotting
  plot_data <- .summary_mean_ci(df = df, group_by = "groupvar")

  # add totals
  plot_data_total <- .summary_mean_ci(df = df, group_by = NULL)
  plot_data_total$groupvar <- "All groups"
  plot_data_total <- plot_data_total[, c("groupvar", setdiff(names(plot_data_total), "groupvar")), drop = FALSE]

  # combine
  groupvar_levels <- c(plot_data$groupvar, "All groups")
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
#' @param df Data frame with the EQ-5D, follow-up and grouping columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column.
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @param name_groupvar Character string for the grouping column. If NULL (default), the analysis is performed on the full population.
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country.
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary plot and data used for plotting
#' @export
#' @examples
#' tmp <- eq5d_utility_change_by_group_plot(
#'  example_data,
#'  names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'  name_fu = "time",
#'  levels_fu = c('Pre-op', 'Post-op'),
#'  name_groupvar = "procedure",
#'  eq5d_version = "3L",
#'  country = "UK"
#' )
#' tmp$p
#' tmp$plot_data

eq5d_utility_change_by_group_plot <- function(df,
                       names_eq5d = NULL,
                       name_fu = NULL,
                       levels_fu = NULL,
                       name_groupvar = NULL,
                       eq5d_version = NULL,
                       country) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(df = df, names_eq5d = names_eq5d, 
                     name_fu = name_fu, levels_fu = levels_fu,
                     eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  name_fu <- temp$name_fu
  levels_fu <- temp$levels_fu
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d, name_fu, name_groupvar)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  if (is.null(name_groupvar)) {
    df$groupvar <- factor("All")
  } else {
    names(df)[names(df) == name_groupvar] <- "groupvar"
    df$groupvar <- factor(df$groupvar)
  }
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
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
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary plot and data used for plotting
#' @export
#' @examples
#' tmp <- eq5d_utility_distribution_plot(
#'  example_data,
#'  names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'  eq5d_version = "3L",
#'  country = "UK"
#' )
#' tmp$p
#' tmp$plot_data

eq5d_utility_distribution_plot <- function(df, 
                       names_eq5d = NULL,
                       eq5d_version = NULL,
                       country) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(names_eq5d = names_eq5d, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country)
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
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_vas Character string for the VAS column
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary plot and data used for plotting
#' @export
#' @examples
#' tmp <- eq5d_utility_vas_scatter_plot(
#'    example_data,
#'    names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'    name_vas = "vas",
#'    eq5d_version = "3L",
#'    country = "UK"
#'  )
#' tmp$p
#' tmp$plot_data

eq5d_utility_vas_scatter_plot <- function(df,
                        names_eq5d = NULL,
                        name_vas = NULL,
                        eq5d_version = NULL,
                        country) {
  
  ### data preparation ###
  
  # replace NULL names with defaults
  temp <- .get_names(names_eq5d = names_eq5d, name_vas = name_vas, eq5d_version = eq5d_version)
  names_eq5d <- temp$names_eq5d
  name_vas <- temp$name_vas
  eq5d_version <- temp$eq5d_version
  # check existence of columns 
  names_all <- c(names_eq5d, name_vas)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  df <- .prep_vas(df = df, name = name_vas)
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country)
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