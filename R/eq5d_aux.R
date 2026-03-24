#' Replace NULL names with default values
#'
#' This function takes in a list of parameters, which would be column names of the input data frame, and checks if they are null. Any nulls are replaced with default values, and the updated list of parameters is returned.
#'
#' @param df a data frame; only used/supplied if levels_fu needs to be defined
#' @param ... a list of parameters consisting of any/all of `names_eq5d`, `name_fu`, `levels_fu`, `eq5d_version`, and `name_vas`.
#' @return a list of parameters with null entries replaced with default values.
#' @examples
#' .get_names(names_eq5d = c("mo", "sc", "ua", "pd", "ad"))
#' .get_names(names_eq5d = NULL, eq5d_version = NULL, name_vas = NULL)
#' @export
#' 
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
      message("No ordering of time suppled. The time variable will be factorised according to the order in the data frame.")
      levels_fu <- unique(df[[name_fu]])
    }
    args[["levels_fu"]] <- levels_fu
  }
  # check eq5d_version
  if ("eq5d_version" %in% names_list) {
    eq5d_version <- args$eq5d_version
    if (is.null(eq5d_version)) {
      message("No EQ-5D version was provided. 5L version will be used.")
      eq5d_version <- "5L"
    }
    args[["eq5d_version"]] <- eq5d_version
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
  
  return(args)
}

#' Calculate the Level Frequency Score (LFS)
#'
#' This function calculates the Level Frequency Score (LFS) for a given EQ-5D state and a specified version of EQ-5D.
#' If at least one domain contains a missing entry, the whole LFS is set to be NA.
#'
#' @param s A character vector representing the EQ-5D state, e.g. 11123.
#' @param eq5d_version A character string specifying the version of EQ-5D, i.e. 3L or 5L.
#' @return A character vector representing the calculated LFS.
#' @examples
#' .get_lfs("333", "3L") # returns 003
#' .get_lfs("333", "5L") # returns 00300
#' .get_lfs("12345", "5L") # returns 11111
#' @export
#' 
.get_lfs <- function(s, eq5d_version) {
  
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
#' @param eq5d_version A character string specifying the version of EQ-5D, i.e. 3L or 5L.
#' @param country A character string representing the name of the country. This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return A data frame with an additional column named `utility` containing the calculated utility values. If the input country name is not found in the country_codes dataset, a list of available codes is printed, and subsequentyl an error message is displayed and the function stops.
#' @examples
#' df <- data.frame(state = c("11111", "11123", "32541"))
#' .add_utility(df, "5L", "DK")
#' @export
#' 
.add_utility <- function(df, eq5d_version, country) {
  
  pkgenv <- getOption("eq.env")
  
  # check whether the valuation for this country code exists
  country_code <- .fixCountries(countries = country, EQvariant = eq5d_version)
  if (is.na(country_code)) {
    message('No valid countries listed. These value sets are currently available.')
    eqvs_display(version = eq5d_version)
    stop('Stopping.')
  }
  
  df$utility <- eq5d(x = df$state, country = country_code, version = eq5d_version)
  # 
  # # country identifiable; proceed to extract the data
  # vs <- pkgenv[[paste0("vsets", eq5d_version, "_combined")]] %>%
  #   select(state, !!(sym(country_code))) %>%
  #   rename(utility = !!quo_name(country_code))
  # 
  # # merge with df
  # df <- merge(df, vs, all.x = TRUE) 
  
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
#' @param eq5d_version character indicating the version of the EQ-5D questionnaire to use (either "3L" or "5L")
#' @param country character indicating the country to retrieve the quality of life score for
#' @return a modified data frame with EQ-5D domain columns renamed to default names, and, if necessary, with added columns for state, LSS, LFS, and/or utility. If any of the checks fail (e.g. EQ-5D columns are not in an integer format), an error message is displayed and the function is stopping.
#' @examples
#' set.seed(1234)
#' df <- data.frame(mo = sample(1:5, 3), sc = sample(1:5, 3), 
#'   ua = sample(1:5, 3), pd = sample(1:5, 3), ad = sample(1:5, 3))
#' .prep_eq5d(df, names = c("mo", "sc", "ua", "pd", "ad"), 
#'   add_state = TRUE, add_lss = TRUE)
#' .prep_eq5d(df, names = c("mo", "sc", "ua", "pd", "ad"),
#'   add_state = TRUE, add_lss = TRUE, add_lfs = TRUE, add_utility = TRUE,
#'   eq5d_version = "5L", country = "ES")
#' @export

.prep_eq5d <- function(df, names,
                       add_state = FALSE,
                       add_lss = FALSE,
                       add_lfs = FALSE,
                       add_utility = FALSE,
                       eq5d_version = NULL,
                       country = NULL) {

  # confirm correct length
  if (length(names) != 5)
    stop("Argument dim_names not of length 5. Stopping.")

  # confirm numeric format
  df_eq5d <- df[, names, drop = FALSE]

  x <- as.matrix(df_eq5d)
  xorig <- x
  x[,] <- as.integer(x)
  if(c(eq5d_version %in% c('3L', 'Y3L', 'XWR'),0)[1]) {
    x[!x %in% 1:3] <- NA
  } else {
    x[!x %in% 1:5] <- NA
  }
  if(sum(is.na(as.vector(x)))>sum(is.na(as.vector(xorig)))) warning(paste0(sum(is.na(as.vector(x)))-sum(is.na(as.vector(xorig))), " observations were coerced to NAs as they were not interpretable as integer values in the range allowed by the EQ-5D descriptive system."))
  df_eq5d[,] <- x

  # confirm EQ-5D version if required
  if (!is.null(eq5d_version))
    if (!(tolower(eq5d_version) %in% c("3l", "5l", 'y3l')))
      stop("EQ-5D version can only be 3L, 3l, Y3L, y3l, 5L or 5l. Stopping.")

  df[, names] <- df_eq5d

  # rename EQ-5D columns to standard names
  std_names <- c("mo", "sc", "ua", "pd", "ad")
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
#' @param levels Levels to factorise the FU variable into.
#' @return A data frame with the follow-up variable renamed as "fu" and factorised.
#' @examples
#' df <- data.frame(id = c(1, 1, 2, 2),
#'   visit = c("baseline", "follow-up", "baseline", "follow-up"))
#' .prep_fu(df = df, name = "visit", levels = c("baseline", "follow-up"))
#' @export

.prep_fu <- function(df, name = NULL, levels = NULL) {

  names(df)[names(df) == name] <- "fu"
  df$fu <- factor(df$fu, levels = levels)

  return(df = df)
}

#' Data checking/preparation: VAS variable
#' 
#' The function prepares the data for VAS (Visual Analogue Scale) analyses. 
#' 
#' @param df A data frame.
#' @param name Column name in the data frame that holds the VAS score. The column can only contain integers or NAs
#' @return A modified data frame with the VAS score renamed to "vas". If any checks fail (e.g. column is not numeric), an error message is displayed and the function is stopping.
#' @examples
#' df <- data.frame(vas_score = c(20, 50, 80, NA, 100))
#' .prep_vas(df = df, name = "vas_score")
#' df <- data.frame(vas_score = c(20.5, 50, 80, NA, 100))
#' .prep_vas(df = df, name = "vas_score")
#' @export
#' 
.prep_vas <- function(df, name) {
  
  # extract data
  x <- as.vector(as.data.frame(df)[, name])
  
  
  xorig <- x <- as.integer(x)
  x[!x %in% 0:100] <- NA
  if(sum(is.na(x))>sum(is.na(xorig))) warning(paste0(sum(is.na(x))>sum(is.na(xorig)), " observations were coerced to NAs as they were not interpretable as integer values in the range allowed by the EQ-5D descriptive system."))
  df[,name] <- x
  
  # # remove NAs
  # v <- v[!is.na(v)]
  # # confirm numeric format
  # if (!is.numeric(v))
  #   stop("VAS column must be in a numeric format. Stopping.")
  # # confirm integers only
  # if (!all(floor(v) == v))
  #   stop("VAS column can only contain integers or NAs. Stopping.")
  
  # all checks passed; rename column
  names(df)[names(df) == name] <- "vas"

  # return value
  return(df)
}

#' Check the uniqueness of groups
#' This function takes a data frame `df` and a vector of columns `group_by`, and checks whether the combinations of values in the columns specified by `group_by` are unique. If the combinations are not unique, a warning message is printed.
#' @param df A data frame.
#' @param group_by A character vector of column names in `df` that specify the groups to check for uniqueness.
#' @return No return value, called for side effects: it will stop with an error if any group combinations are not unique.
#' @examples
#' df <- data.frame(id = c(1, 1, 1, 1, 2, 2),
#'                  fu = rep(c("baseline", "follow-up"), 3),
#'                  value = rnorm(6))
#' .check_uniqueness(df, c("id", "fu"))
#' @export
.check_uniqueness <- function(df, group_by) {

  keys <- do.call(paste, c(df[group_by], list(sep = "\001")))
  counts <- tabulate(match(keys, unique(keys)))

  if (!all(counts == 1L))
    message(paste0("Warning: there are non-unique ",
                   paste(group_by, collapse = "-"),
                   " combinations."))
}

#' Get the mode of a vector.
#'
#' This function calculates the mode of a numeric or character vector. 
#' If there are multiple modes, the first one is returned. 
#' The code is taken from an \href{https://www.tutorialspoint.com/r/r_mean_median_mode.htm}{R help page}.
#'
#' @param v A numeric or character vector.
#' @return The mode of `v`.
#' @examples
#' .getmode(c(1, 2, 3, 3))
#' .getmode(c("a", "b", "b", "c"))
#' @export
#' 
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
#' @examples
#' set.seed(1234)
#' df <- data.frame(eq5d = rep(rnorm(5), 2),
#'                  fu = rep(c(1, 0, 1, 0, 1), 2))
#' .summary_table_2_1(df, c("eq5d", "fu"))
#' @export

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
#' @param eq5d_version Version of the EQ-5D instrument
#' @return Summary data frame.

.freqtab<- function(df,
                    names_eq5d = NULL,
                    name_fu = NULL,
                    levels_fu = NULL,
                    eq5d_version = NULL,
                    add_summary_problems_change = TRUE) {
  
  ### data preparation ###
  
  if(is.null(name_fu)) {
    df$`_all` <- "All"
    name_fu <- "_all"
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
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
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

  # complete case dataset: remove NA values
  df_cc <- df_long[!is.na(df_long$value), , drop = FALSE]

  # summary: individual levels
  summary_dim <- .summary_table_2_1(df = df_cc, group_by = c("eq5d", "value", "fu"))
  names(summary_dim)[names(summary_dim) == "value"] <- "level"
  summary_dim$level <- as.character(summary_dim$level)

  # summary: total
  summary_total <- .summary_table_2_1(df = df_cc, group_by = c("eq5d", "fu"))
  summary_total$level <- "Total"

  # summary: some problems and change
  df_probs <- df_cc[df_cc$value != 1, , drop = FALSE]
  summary_problems <- aggregate(rep(1L, nrow(df_probs)),
                                by = list(eq5d = df_probs$eq5d, fu = df_probs$fu),
                                FUN = sum)
  names(summary_problems)[names(summary_problems) == "x"] <- "n"
  # merge with totals
  summary_problems <- merge(summary_problems,
                            summary_total[, c("eq5d", "fu", "n")],
                            by = c("eq5d", "fu"), suffixes = c("", "_total"))
  summary_problems$freq <- summary_problems$n / summary_problems$n_total
  summary_problems$n_total <- NULL
  suffix <- if (eq5d_version == "3L") "2+3" else "2+3+4+5"
  summary_problems$level <- paste0("Number reporting any problems (levels ", suffix, ")")

  # change in numbers reporting problems since previous time point
  # sort by (eq5d, fu) with fu ordered by levels_fu
  sp_sub <- summary_problems[, c("eq5d", "fu", "n")]
  sp_sub$fu_ord <- match(as.character(sp_sub$fu), as.character(levels_fu))
  sp_sub <- sp_sub[order(sp_sub$eq5d, sp_sub$fu_ord), ]
  sp_sub$fu_ord <- NULL
  # lag n within each eq5d group
  sp_split <- split(sp_sub, sp_sub$eq5d)
  change_list <- lapply(sp_split, function(g) {
    g$n_prev <- c(NA_real_, head(g$n, -1))
    g$n_change <- g$n - g$n_prev
    g$freq <- g$n_change / g$n_prev
    g$n <- g$n_change
    g$n_prev <- NULL
    g$n_change <- NULL
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

#' .pchctab: Changes in health according to the PCHC (Paretian Classification of Health Change)
#' 
#' @param df Data frame with the EQ-5D, grouping, id and follow-up columns
#' @param name_id Character string for the patient id column
#' @param name_groupvar Character string for the grouping column
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @param add_noprobs if set to TRUE, level corresponding to "no problems" will be added to the table
#' @return Summary data frame
#' @examples
#' .pchctab(df = example_data,
#'   name_id = "id",
#'   name_groupvar = "procedure",
#'   name_fu = "time",
#'   levels_fu = c('Pre-op', 'Post-op')
#' )
#' @export

.pchctab <- function(df,
                  name_id,
                  name_groupvar,
                  names_eq5d = NULL,
                  name_fu = NULL, 
                  levels_fu = NULL,
                  add_noprobs = FALSE) {
  
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
  
  
  # Enforce EQ-5D levels
  tmp <- as.matrix(df[, names_eq5d])
  tmp[,] <- as.integer(tmp)
  tmp[!tmp %in% 1:5] <- NA
  df[, names_eq5d] <- tmp
  rm(tmp)
  
  
  
  # all columns defined and exist; only leave relevant columns now
  df <- df[, names_all, drop = FALSE]
  # further checks and data preparation
  names(df)[names(df) == name_id]       <- "id"
  names(df)[names(df) == name_groupvar] <- "groupvar"
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  # sort by id - groupvar - time
  df <- df[order(df$id, df$groupvar, df$fu), , drop = FALSE]
  # check uniqueness of id-groupvar-fu combinations
  .check_uniqueness(df, group_by = c("id", "groupvar", "fu"))
  
  
  
  
  ### analysis ###
  
  uids <- data.frame(id = unique(df$id))
  uids$groupvar <- df$groupvar[match(uids$id, df$id)]
  lvls <- structure(.Data = levels(df$fu), .Names = levels(df$fu))
  grps <- structure(.Data = unique(df$groupvar), .Names = unique(df$groupvar))
  grpr <- lapply(grps, function(grp) which(uids$groupvar == grp))
  
  idstates <- lapply(lvls, FUN = function(thisfu) {
    tmp <- df[df$fu == thisfu, c("id", names_eq5d)]
    as.matrix(tmp[match(uids$id, tmp$id),2:6])
  })
  
  progress <- as.data.frame(lapply(structure(.Data = 1:length(lvls), .Names = lvls), FUN = function(thisfu) {
    if(thisfu == 1) return(rep(NA, NROW(uids)))
    tmp <- sign(idstates[[thisfu]]-idstates[[thisfu-1]])
    factor(1*(rowSums(1*(tmp == 1))>0)-1*(rowSums(1*(tmp == -1))>0)+2*(rowSums(tmp == 0)==5), levels = c(2, -1, 1, 0), labels = c("No change", "Improve", "Worsen", "Mixed change"))   
    
  }))
  
  
  problems <- as.data.frame(lapply(idstates, function(idstate) {
    factor(2-1*(rowSums(idstate>1)>0), labels = c("Total with problems", "No problems"))
  }))
  
  colnames(progress) <- colnames(problems)<- lvls
  
  tmp <- do.call(rbind,lapply(grps, FUN = function(grp) {
    cbind(groupvar = grp, do.call(rbind, lapply(lvls[-1], function(lvl) {
      tmp <- cbind(fu = lvl, 
                   rbind(as.data.frame(table(progress[grpr[[grp]],lvl], useNA = 'always')),
                         if(add_noprobs) as.data.frame(table(problems[grpr[[grp]],lvl], useNA = 'no'))))
      colnames(tmp) <- c('fu', 'state', 'n')
      
      tmp$p <- c(tmp$n/sum(tmp$n[-which(is.na(tmp$state))]))
      tmp$p[which(is.na(tmp$state))] <- NA
      tmp <- rbind(tmp, data.frame(fu = lvl, state = 'Grand total', n = sum(tmp$n[1:4]), p = 1))
      tmp
    })))
  }))
  
  tmp$state <- factor(tmp$state, levels = c(levels(tmp$state), "Missing/NA"))
  tmp$state[is.na(tmp$state)] <- 'Missing/NA'
  
  
  # combine & tidy up: spread (groupvar, fu, n/p) into wide columns
  all_states <- levels(tmp$state)
  retval <- data.frame(state = all_states, stringsAsFactors = FALSE)

  # build column names in order: for each (groupvar, fu), add _n and _p columns
  for (grp in unique(tmp$groupvar)) {
    for (f in levels_fu[-1]) {
      sub <- tmp[tmp$groupvar == grp & tmp$fu == f, , drop = FALSE]
      vals_n <- setNames(as.numeric(sub$n), as.character(sub$state))
      vals_p <- setNames(as.numeric(sub$p), as.character(sub$state))
      cn <- paste(grp, f, "n", sep = "_")
      cp <- paste(grp, f, "p", sep = "_")
      retval[[cn]] <- ifelse(is.na(vals_n[retval$state]), 0, vals_n[retval$state])
      retval[[cp]] <- ifelse(is.na(vals_p[retval$state]), 0, vals_p[retval$state])
    }
  }

  rownames(retval) <- NULL
  return(retval)
}


#' Wrapper to determine Paretian Classification of Health Change
#' 
#' This internal function determines Paretian Classification of Health Change (PCHC) for each combination of the variables specified in the `group_by` argument. 
#' It is used in the code for table_2_4-table_2_5 and figure_2_1-figure_2_4. 
#' An EQ-5D health state is deemed to be `better` than another if it is better on at least one dimension and is no worse on any other dimension.
#' An EQ-5D health state is deemed to be `worse` than another if it is worse in at least one dimension and is no better in any other dimension.
#' @param df A data frame with EQ-5D states and follow-up variable. The dataset is assumed to be have been ordered correctly.
#' @param level_fu_1 Value of the first (i.e. earliest) follow-up. Would normally be defined as levels_fu[1].
#' @param add_noprobs Logical value indicating whether to include a separate classification for those without problems (default is FALSE)
#' @return A data frame with PCHC value for each combination of the grouping variables. 
#' If 'add_noprobs' is TRUE, a separate classification for those without problems is also included.
#' @examples
#' df <- data.frame(id = c(1, 1, 2, 2),
#'                  fu = c(1, 2, 1, 2),
#'                  mo = c(1, 1, 1, 1),
#'                  sc = c(1, 1, 5, 1),
#'                  ua = c(1, 1, 4, 3),
#'                  pd = c(1, 1, 1, 3),
#'                  ad = c(1, 1, 1, 1))
#' .pchc(df, level_fu_1 = 1, add_noprobs = TRUE)
#' @export

.pchc <- function(df, level_fu_1, add_noprobs = FALSE) {

  levels_eq5d <- c("mo", "sc", "ua", "pd", "ad")

  # initialise positive, negative & zero difference counts
  df$better <- 0L
  df$worse  <- 0L

  for (dom in levels_eq5d) {
    dom_diff <- paste0(dom, "_diff")

    # lag shift: previous row's value minus current (dplyr::lag equivalent)
    df[[dom_diff]] <- c(NA_real_, head(df[[dom]], -1)) - df[[dom]]
    # baseline rows: set diff to NA
    df[[dom_diff]][df$fu == level_fu_1] <- NA_real_

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
#' @param df A data frame containing the FU and continuous variable of interest. The dataset must contain an ordered `fu` variable.
#' @param name_v A character string with the name of the continuous variable in `df` to be summarised.
#' @return Data frame with one row for each statistic and one column for each FU. 
#' @examples
#' df <- data.frame(fu = c(1,1,2,2,3,3), 
#'                  vas = c(7,8,9,NA,7,6))
#' .summary_cts_by_fu(df, name_v = "vas")
#' @importFrom stats median quantile sd
#' @importFrom moments kurtosis
#' @export

.summary_cts_by_fu <- function(df, name_v) {

  ### prepare dataset ###

  names(df)[names(df) == name_v] <- "v"

  fu_levels <- if (is.factor(df$fu)) levels(df$fu) else sort(unique(df$fu))

  # summarise non-NA values per fu level
  stats_list <- lapply(fu_levels, function(f) {
    v <- df$v[df$fu == f & !is.na(df$v)]
    data.frame(
      fu = f,
      Mean = mean(v),
      `Standard error` = sd(v) / sqrt(length(v)),
      Median = median(v),
      Mode = .getmode(v),
      `Standard deviation` = sd(v),
      Kurtosis = kurtosis(v),
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
    v <- df$v[df$fu == f]
    miss_n <- sum(is.na(v))
    tot <- length(v)
    data.frame(
      fu = f,
      `Missing (n)` = miss_n,
      `Total sample` = tot,
      `Missing (%)` = miss_n / tot,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  summary_total_na <- do.call(rbind, total_na_list)

  # combine
  combined <- merge(summary, summary_total_na, by = "fu")

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

#' Summary wrapper for Table 4.3
#' 
#' This internal function creates a summary of the data frame for Table 4.3. 
#' It groups the data by the variables specified in `group_by` and calculates various summary statistics.
#' 
#' @param df A data frame.
#' @param group_by A character vector of names of variables by which to group the data.
#' @return A data frame with the summary statistics.
#' @examples
#' df <- data.frame(group = c("A", "A", "B", "B"), 
#'                  utility = c(0.5, 0.7, 0.8, 0.9))
#' .summary_table_4_3(df, group_by = "group")
#' @export

.summary_table_4_3 <- function(df, group_by) {

  grp_key <- do.call(paste, c(df[group_by], list(sep = "\001")))
  parts <- split(df$utility, grp_key)

  result_list <- lapply(names(parts), function(k) {
    u <- parts[[k]]
    group_vals <- df[match(k, grp_key), group_by, drop = FALSE]
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
#' @examples
#' df <- data.frame(group = c("A", "A", "B", "B"), 
#'                  utility = c(0.5, 0.7, 0.8, 0.9))
#' .summary_table_4_4(df, group_by = "group")
#' @export

.summary_table_4_4 <- function(df, group_by) {

  grp_key <- do.call(paste, c(df[group_by], list(sep = "\001")))
  parts <- split(df$utility, grp_key)

  result_list <- lapply(names(parts), function(k) {
    u <- parts[[k]]
    group_vals <- df[match(k, grp_key), group_by, drop = FALSE]
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
#' @examples
#' df <- data.frame(group = c("A", "A", "B", "B"), 
#'                  utility = c(0.5, 0.7, 0.8, 0.9))
#' .summary_mean_ci(df, group_by = "group")
#' @export
#'
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

  grp_key <- do.call(paste, c(df[group_by], list(sep = "\001")))
  parts <- split(df$utility, grp_key)

  result_list <- lapply(names(parts), function(k) {
    u <- parts[[k]]
    group_vals <- df[match(k, grp_key), group_by, drop = FALSE]
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
#' @examples
#' # generate 10 colours for base colour "green"
#' .gen_colours("green", 10)
#' # generate 7 colours for base colour "orange"
#' .gen_colours("orange", 7)
#' @importFrom grDevices colorRampPalette
#' @export
#'
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
#' @export
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
#' @param cols A vector of colors to use for the bars.
#' @param text_rotate A logical indicating whether to rotate the text labels for the bars.
#' @return A ggplot object containing the PCHC plot.
#' @examples
#' df <- data.frame(
#'   name = rep(c("Dim1", "Dim2"), each = 2),
#'   p = c(0.6, 0.4, 0.7, 0.3),
#'   groupvar = rep(c("Group A", "Group B"), 2)
#' )
#' colors <- c("Group A" = "#1b9e77", "Group B" = "#d95f02")
#' .pchc_plot_by_dim(df, ylab = "Proportion", title = "Example Plot", cols = colors)
#' @export

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