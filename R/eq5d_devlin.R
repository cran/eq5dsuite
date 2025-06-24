#' Table 1.1.1: Frequency of levels by dimensions, cross-sectional
#' 
#' @param df Data frame with the EQ-5D and follow-up columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @return Summary data frame.
#' @examples
#' table_1_1_1(
#'  df = example_data[example_data$time == "Pre-op",],
#'  names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'  eq5d_version = "3L"
#' )
#' @export

table_1_1_1<- function(df, 
                      names_eq5d = NULL,
                      eq5d_version = NULL) {
  do.call(.freqtab, as.list(match.call()[-1]))
}


#' Table 1.1.2: Frequency of levels by dimensions, separated by category
#' 
#' @param df Data frame with the EQ-5D and follow-up columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_cat Character string for the category  column. If NULL, no grouping is used, and the table reports for the total population, i.e. equal to table 1.1.1.
#' @param levels_cat Character vector containing the order of the values in the category column, if the wish is to have these presented in a particular order. 
#' If NULL (default value), unless the variable is a factor, the levels will be ordered in the order of appearance in df.
#' @param eq5d_version Version of the EQ-5D instrument
#' @return Summary data frame.
#' @examples
#' table_1_1_2(
#'  df = example_data[example_data$time == "Pre-op",],
#'  names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'  name_cat = "procedure",
#'  levels_cat = c("Hip Replacement", "Knee Replacement"),
#'  eq5d_version = "3L"
#' )
#' @export
#' 
table_1_1_2<- function(df, 
                       names_eq5d = NULL,
                       name_cat = NULL,
                       levels_cat = NULL,
                       eq5d_version = NULL) {
  tmp <- .freqtab(df, names_eq5d, name_cat, levels_cat, eq5d_version)
  tmp[-(NROW(tmp)-(1:2)),]
}

#' Table 1.2.1: Frequency of levels by dimensions, by follow-up
#' 
#' @param df Data frame with the EQ-5D and follow-up columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column. If NULL, the function will check if there is a column named "follow-up" or "fu", in which case the first of those will be used.
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @param eq5d_version Version of the EQ-5D instrument
#' @return Summary data frame.
#' @examples
#' table_1_2_1(
#'   df = example_data,
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'   name_fu = "time",
#'   levels_fu = c("Pre-op" , "Post-op"),
#'   eq5d_version = "3L"
#' )
#' @export

table_1_2_1<- function(df, 
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

#' Table 1.1.3: Prevalence of the 10 most frequently observed self-reported health states
#' 
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @param n Number of most frequently observed states to display (default 10)
#' @return Summary data frame
#' @export
#' @examples
#' table_1_1_3(
#'   df = example_data[example_data$time == "Pre-op",],
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'   eq5d_version = "3L",
#'   n = 10
#' )
#' @importFrom rlang .data

table_1_1_3 <- function(df, 
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
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d, eq5d_version = eq5d_version, add_state = TRUE) %>%
    select('state')
  
  ### analysis ####
  
  # all states
  states_all <- df %>%
    # non-NA values
    filter(!is.na(.data$state)) %>%
    # group by state
    group_by(.data$state) %>%
    # add count
    summarise(n = n()) %>%
    # add percentage
    mutate(p = n / sum(n)) %>%
    arrange(-n) %>%
    # cumulative percentage
    mutate(cum_p = cumsum(.data$p))
  
  # most frequent n non-NA states
  states_top <- states_all %>%
    filter(!is.na(.data$state)) %>%
    slice_head(n = n)
  
  # worst state
  
  state_worst <- if (eq5d_version == "5L") "55555" else "33333"
  
  if(!state_worst %in% states_top[,1]) {
    states_worst <- states_all %>%
      filter(.data$state == state_worst) %>%
      mutate(cum_p = 1)  
    states_worst <- states_worst[c(1,1),]
    states_worst[1,] <- NA
    states_worst[1,1] <- '...'
  } else {
    states_worst <- .data$all_states[0,]
  }
  
  
  
  # missing data
  state_na <- df %>%
    summarise(n = sum(is.na(.data$state)), p = sum(is.na(.data$state)) / n()) %>%
    mutate(state = "Missing")
  
  # combine and tidy up
  retval <- bind_rows(states_top, states_worst, state_na) %>%
    rename(`Health state` = 'state',
           Frequency = 'n',
           Percentage = 'p',
           `Cumulative percentage` = 'cum_p')
  
  # return value
  return(retval)
}

#' Table 1.2.2: Changes in health according to the PCHC (Paretian Classification of Health Change)
#' 
#' @param df Data frame with the EQ-5D, grouping, id and follow-up columns
#' @param name_id Character string for the patient id column
#' @param name_groupvar Character string for the grouping column
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @return Summary data frame
#' @export
#' @examples
#' table_1_2_2(
#'   df = example_data,
#'   name_id = "id",
#'   name_groupvar = "procedure",
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'   name_fu = "time",
#'   levels_fu = c("Pre-op" , "Post-op")
#' )
#' @importFrom rlang .data

table_1_2_2 <- function(df,
                      name_id,
                      name_groupvar,
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
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- df %>%
    rename(id = !!quo_name(name_id),
           groupvar = !!quo_name(name_groupvar))
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  # sort by id - groupvar - time
  df <- df %>%
    arrange(.data$id, .data$groupvar, .data$fu)
  # check uniqueness of id-groupvar-fu combinations
  .check_uniqueness(df, group_by = c("id", "groupvar", "fu"))
  
  ### analysis ###
  
  # calculate change
  df <- .pchc(df = df, level_fu_1 = levels_fu[1]) %>%
    filter(!is.na(.data$state)) %>%
    mutate(state = 
             factor(.data$state,
                    # levels = c("Improve", "Mixed change", "No change", "Worsen")))
                    levels = c("No change", "Improve", "Worsen", "Mixed change")))
  
  # summarise by groupvar, fu & state
  summary_dim <- df %>%
    group_by(.data$groupvar, .data$fu, .data$state) %>%
    summarise(n = n()) %>%
    mutate(p = n / sum(n)) %>%
    ungroup()
  
  # summarise totals
  summary_total <- summary_dim %>%
    group_by(.data$groupvar, .data$fu) %>%
    summarise(n = sum(n), p = sum(.data$p), .groups = "drop") %>%
    # add label
    mutate(state = "Grand Total") 
  
  # combine & tidy up
  retval <- bind_rows(summary_dim, summary_total) %>%
    # reshape into a long format to subsequently impose order on columns in pivot_wider
    pivot_longer(cols = 'n':'p') %>%
    # finally reshape wider
    pivot_wider(id_cols = 'state', 
                names_from = c('groupvar', 'fu', 'name'), 
                values_from = 'value',
                # fill NAs with 0
                values_fill = list('value' = 0))
  
  # return value
  return(retval)
}

#' Table 1.2.3: Changes in health according to the PCHC, taking account of those with no problems
#' 
#' @param df Data frame with the EQ-5D, grouping, id and follow-up columns
#' @param name_id Character string for the patient id column
#' @param name_groupvar Character string for the grouping column
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @return Summary data frame
#' @export
#' @examples
#' table_1_2_3(
#'   df = example_data,
#'   name_id = "id",
#'   name_groupvar = "procedure",
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'   name_fu = "time",
#'   levels_fu = c("Pre-op" , "Post-op")
#' )
#' @importFrom rlang .data

table_1_2_3 <- function(df, 
                      name_id,
                      name_groupvar,
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
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- df %>%
    rename(id = !!quo_name(name_id),
           groupvar = !!quo_name(name_groupvar))
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  # sort by id - groupvar - time
  df <- df %>%
    arrange('id', 'groupvar', 'fu')
  # check uniqueness of id-groupvar-fu combinations
  .check_uniqueness(df, group_by = c("id", "groupvar", "fu"))
  
  ### analysis ###
  
  # calculate change
  df <- .pchc(df = df, level_fu_1 = levels_fu[1], add_noprobs = TRUE) %>%
    filter(!is.na(.data$state))
  
  # separate out those with problems & calculate percentages
  summary_by_probs_status <- df %>%
    mutate(state_noprobs = case_when((.data$state_noprobs == "No problems") ~ "No problems",
                                       TRUE ~ "Total with problems")) %>%
    group_by(.data$groupvar, .data$fu, .data$state_noprobs) %>%
    summarise(n = n()) %>%
    mutate(p = n / sum(n))
  
  # summarise classes within those with problems
  summary_with_probs <- df %>%
    filter(.data$state_noprobs != "No problems") %>%
    group_by(.data$groupvar, .data$fu, .data$state_noprobs) %>%
    summarise(n = n()) %>%
    mutate(p = n / sum(n))
  
  # combine & tidy up
  retval <- bind_rows(summary_with_probs, summary_by_probs_status) %>%
    # impose order
    mutate(state_noprobs = factor(.data$state_noprobs, 
                                    levels = c("No change", "Improve", "Worsen", "Mixed change",
                                               "Total with problems", "No problems"))) %>%
    # reshape into a long format to subsequently impose order on columns in pivot_wider
    pivot_longer(cols = 'n':'p') %>%
    # finally reshape into a wide format
    pivot_wider(id_cols = 'state_noprobs', 
                names_from = c('groupvar', 'fu', 'name'), 
                values_from = 'value',
                # fill NAs with 0
                values_fill = list('value' = 0)) %>%
    arrange(.data$state_noprobs)
  
  # return value
  return(retval)
}

#' Table 1.2.4: Changes in levels in each dimension, percentages of total and of type of change
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
#' table_1_2_4(
#'   df = example_data,
#'   name_id = "id",
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'   name_fu = "time",
#'   levels_fu = c("Pre-op" , "Post-op")
#' )
#' @importFrom rlang .data

table_1_2_4 <- function(df, 
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
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- df %>%
    rename(id = !!quo_name(name_id))
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  # sort by id - time
  df <- df %>%
    arrange(id, 'fu')
  # check uniqueness of id-fu combinations
  .check_uniqueness(df, group_by = c("id", "fu"))
  
  ### analysis ###
  
  # calculate change
  levels_eq5d <- c("mo", "sc", "ua", "pd", "ad")
  level_fu_1 <- levels_fu[1]
  
  df_diff <- df %>%
    # reshape into a long format
    pivot_longer(cols = 'mo':'ad', names_to = "domain") %>%
    select('domain', 'id', 'fu', 'value') %>%
    arrange(.data$domain, .data$id, .data$fu) %>%
    # calculate lagged difference
    mutate(diff = lag(.data$value) - .data$value,
           level_change = str_c(lag(.data$value), "-", .data$value)) %>%
    # replace entry from 1st follow-up with NA
    mutate(diff = case_when(.data$fu == level_fu_1 ~ NA_real_,
                            TRUE ~ diff)) %>%
    # remove NAs
    filter(!is.na(.data$diff)) %>%
    # classify the difference
    mutate(diff = case_when(.data$diff > 0 ~ "Better",
                            .data$diff < 0 ~ "Worse",
                            .data$diff == 0 ~ "No change")) %>%
    # order domain column
    mutate(domain = factor(.data$domain, levels = levels_eq5d)) %>%
    arrange(.data$domain)
  
  # % total
  p_total <- df_diff %>%
    group_by(.data$domain, .data$diff, .data$level_change) %>%
    summarise(n = n()) %>%
    group_by(.data$domain) %>%
    mutate(p = n / sum(n)) %>%
    mutate(type = "% Total")
  
  # % type
  p_type <- df_diff %>%
    group_by(.data$domain, .data$diff, .data$level_change) %>%
    summarise(n = n()) %>%
    mutate(p = n / sum(n)) %>%
    mutate(type = "% Type")
  
  # combine and tidy up
  retval <- rbind(p_total, p_type) %>%
    select('domain', 'type', 'diff', 'level_change', 'p') %>%
    pivot_wider(id_cols = c('diff', 'level_change'), names_from = c('domain', 'type'), values_from = 'p',
                values_fill = list('p' = 0)) %>%
    # arrange rows
    mutate(diff = factor(.data$diff, levels = c("No change", "Better", "Worse"))) %>%
    arrange(.data$diff, .data$level_change) %>%
    # arrange rows
    select('diff', 'level_change', 
           !!!syms(apply(expand_grid(levels_eq5d, c("% Total", "% Type")), 
                         1, paste, collapse = "_")))
  
  # return value
  return(retval)
}

#' Table 1.3.1: Summary statistics for the EQ-5D values by all the different LSSs (Level Sum Scores)
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
#' table_1_3_1(
#'   df, 
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'   eq5d_version = "3L", 
#'   country = "US"
#' )
#' @importFrom rlang .data

table_1_3_1 <- function(df, 
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
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d, 
                   add_state = TRUE, 
                   add_lss = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country) %>%
    # leave relevant columns only
    select('lss', 'utility')
  
  ### analysis ###
  
  # summarise non-NA values
  summary <- df %>%
    filter(!is.na(.data$lss)) %>%
    group_by(.data$lss) %>%
    summarise(Number = n(),
              Mean = mean(.data$utility),
              `Standard Deviation` = sd(.data$utility),
              Median = median(.data$utility),
              Minimum = min(.data$utility),
              Maximum = max(.data$utility),
              Range = max(.data$utility) - min(.data$utility),
              .groups = "drop")
  
  # missing data
  summary_na <- df %>%
    summarise(Number = sum(is.na(.data$lss))) %>%
    mutate(lss = "Missing")
  
  # combine and tidy up
  retval <- bind_rows(summary %>% mutate(lss = as.character(.data$lss)), summary_na) %>%
    rename(LSS = 'lss')
  
  # return value
  return(retval)
}

#' Table 1.3.2: Distribution of the EQ-5D states by LFS (Level Frequency Score)
#' 
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @return Summary data frame
#' @export
#' @examples
#' table_1_3_2(
#'   example_data, 
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'   eq5d_version = "3L"
#' )
#' @importFrom rlang .data

table_1_3_2 <- function(df, 
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
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d, 
                   add_state = TRUE, 
                   add_lfs = TRUE, eq5d_version = eq5d_version) %>%
    # leave relevant columns only
    select('lfs')
  
  ### analysis ###
  
  # summarise non-NA values
  summary <- df %>%
    filter(!is.na(.data$lfs)) %>%
    # summary by each utility-lfs combination
    group_by(.data$lfs) %>%
    summarise(n = n()) %>%
    # add percentage
    mutate(p = n / sum(.data$n)) %>%
    arrange(-n) %>%
    # cumulative percentage
    mutate(cum_p = cumsum(.data$p)) %>%
    # tidy up
    rename(LFS = 'lfs',
           Frequency = 'n',
           `%` = 'p',
           `Cum (%)` = 'cum_p')
  
  # missing data
  summary_na <- df %>%
    summarise(Frequency = sum(is.na(.data$lfs))) %>%
    mutate(LFS = "Missing")
  
  # combine
  retval <- bind_rows(summary, summary_na)
  
  # return value
  return(retval)
}

#' Table 1.3.3: Number of observations in the LFS (Level Frequency Score) according to the EQ-5D values
#' 
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary data frame
#' @export
#' @examples
#' table_1_3_3(
#'   example_data, 
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'   eq5d_version = "3L",
#'   country = "UK"
#' )
#' @importFrom rlang .data

table_1_3_3 <- function(df, 
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
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d, 
                   add_state = TRUE, 
                   add_lfs = TRUE, eq5d_version = eq5d_version, 
                   add_utility = TRUE, country = country) %>%
    # leave relevant columns only
    select('lfs', 'utility')
  
  ### analysis ###
  
  # summarise for each utility-LFS combination
  retval <- df %>%
    # exclude NAs
    filter(!is.na(.data$lfs)) %>%
    # summary by each utility-LFS combination
    group_by(.data$utility) %>%
    count(lfs = factor(.data$lfs), .drop = FALSE) %>%
    # arrange by LFS to preserve order when reshaping
    arrange(.data$lfs) %>%
    # reshape into wide format
    pivot_wider(id_cols = 'utility', names_from = 'lfs', values_from = 'n',
                values_fill = list('n' = 0)) %>%
    # order rows
    arrange(.data$utility) %>%
    # add row totals
    mutate(Total = sum(c_across(everything()))) %>%
    # tidy up
    rename(`EQ-5D Value` = 'utility')
  
  # return value
  return(retval)
}

#' Table 1.3.4: Summary statistics of EQ-5D values by LFS (Level Frequency Score)
#' 
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary data frame
#' @export
#' @examples
#' table_1_3_4(
#'   example_data, 
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'   eq5d_version = "3L",
#'   country = "UK"
#' )
#' @importFrom rlang .data

table_1_3_4 <- function(df, 
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
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d, 
                   add_state = TRUE, 
                   add_lfs = TRUE, eq5d_version = eq5d_version,
                   add_utility = TRUE, country = country) 
  
  ### analysis ###
  
  # summarise non-NA values
  retval <- df %>%
    # remove missing data
    filter(!is.na(.data$utility)) %>%
    group_by(.data$lfs) %>%
    summarise(Frequency = length(.data$utility),
              Mean = mean(.data$utility), 
              SD = sd(.data$utility),
              Median = median(.data$utility),
              Minimum = min(.data$utility),
              Maximum = max(.data$utility),
              Range = max(.data$utility)-min(.data$utility)) %>%
    # tidy up
    arrange(.data$lfs) %>%
    rename(LFS = 'lfs')
  
  # return value
  return(retval)
}

#' Figure 1.4.1: Generate a Health State Density Curve (HSDC) for EQ-5D Data
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
#' figure <- figure_1_4_1(
#'             df = example_data, 
#'             names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'             eq5d_version = "3L"
#'           )
#' figure$plot_data
#' figure$p
#' @importFrom rlang .data
#' @importFrom utils head

figure_1_4_1 <- function(df, names_eq5d, eq5d_version) {
  
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
  df <- df %>%
    dplyr::select(dplyr::all_of(names_all))
  
  # Prepare data (adds 'state' column via .prep_eq5d)
  df <- .prep_eq5d(
    df            = df, 
    names         = names_eq5d,
    add_state     = TRUE,
    add_lfs       = FALSE, 
    eq5d_version  = eq5d_version
  )
  
  # Check for NA in 'state', show warning and filter
  n_na <- sum(is.na(df$state))
  if (n_na > 0) {
    warning(n_na, " excluded observations with invalid EQ-5D profiles.")
    df <- dplyr::filter(df, !is.na(.data$state))
  }
  
  # Calculate frequencies & cumulative proportions
  profile_freq <- df %>%
    dplyr::count(.data$state, name = "Frequency") %>%
    dplyr::arrange(dplyr::desc(.data$Frequency)) %>%
    dplyr::mutate(
      CumPropObservations = cumsum(.data$Frequency) / sum(.data$Frequency),
      CumPropStates = dplyr::row_number() / dplyr::n()
    )
  
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

#' Table 2.1: EQ VAS Score by timepoints
#' 
#' @param df Data frame with the VAS and the follow-up columns
#' @param name_vas Character string for the VAS column
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' @return Summary data frame
#' @export
#' @examples
#' table_2_1(
#'   example_data,
#'   name_vas = 'vas', 
#'   name_fu = 'time', 
#'   levels_fu = c('Pre-op', 'Post-op')
#' )
#' @importFrom rlang .data

table_2_1 <- function(df, 
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
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_vas(df = df, name = name_vas)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  
  ### analysis ###
  
  # values calculated in the .summary_cts_by_fu wrapper
  retval <- .summary_cts_by_fu(df = df, name_v = "vas")
  
  # return value
  return(retval)
}

#' Table 2.2: EQ VAS Scores frequency of mid-points
#' 
#' @param df Data frame with the VAS column
#' @param name_vas Character string for the VAS column
#' @param add_na_total Logical, whether to add summary of the missing, and across the Total, data
#' @return Summary data frame
#' @export
#' @examples
#' table_2_2(
#'   example_data,
#'   name_vas = 'vas', 
#'   add_na_total =  TRUE
#'   )
#' @importFrom rlang .data

table_2_2 <- function(df, 
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
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_vas(df = df, name = name_vas)
  
  ### analysis ###
  
  # define ranges and midpoints
  retval <- data.frame(
    l = c(0:2, seq(from = 3, to = 93, by = 5), 98:100),
    u = c(0:2, seq(from = 7, to = 97, by = 5), 98:100),
    Midpoint = c(0:2, seq(from = 5, to = 95, by = 5), 98:100)) %>%
    mutate(Frequency = NA)
  
  # populate the table
  # entries for each row
  for (i in 1:nrow(retval)) {
    retval_temp <- retval[i, ]
    # number of entries in df within a given range
    retval[i, "Frequency"] <- nrow(df %>% filter(.data$vas >= retval_temp$l & .data$vas <= retval_temp$u))
  }
  
  # add range column
  retval <- retval %>%
    mutate(Range = case_when(.data$l == u ~ as.character(l),
                             TRUE ~ str_c(l, u, sep = "-"))) %>%
    select(-'l', -'u') %>%
    select('Range', 'Midpoint', 'Frequency')
  
  # add totals & missing data if needed
  if (add_na_total) {
    n_total <- nrow(df)
    n_na <- nrow(df %>% filter(is.na(.data$vas)))
    retval <- bind_rows(retval,
                        data.frame(
                          Range = c("Total observed", "Missing", "Total sample"),
                          Midpoint = rep(NA, 3),
                          Frequency = c(n_total - n_na, n_na, n_total)))
  }
  
  # return value
  return(retval)
}

#' Table 3.1: EQ-5D values: by timepoints
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
#' table_3_1(
#'   example_data,
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'   name_fu = "time",
#'   levels_fu = c('Pre-op', 'Post-op'),
#'   eq5d_version = "3L",
#'   country = "UK"
#' )
#' @importFrom rlang .data

table_3_1 <- function(df, 
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
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  df <- df %>%
    select('fu', 'utility')
  
  ### analysis ###
  
  # summarise
  retval <- .summary_cts_by_fu(df = df, name_v = "utility")
  
  # return value
  return(retval)
}

#' Table 3.2 EQ-5D values: by groupvar
#' 
#' @param df Data frame with the EQ-5D, follow-up and grouping columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_groupvar Character string for the grouping column
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' @return Summary data frame
#' @export
#' @examples
#' table_3_2(
#'   example_data,
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'   name_groupvar = "procedure",
#'   eq5d_version = "3L",
#'   country = "UK"
#' )
#' @importFrom rlang .data

table_3_2 <- function(df,
                      names_eq5d = NULL,
                      name_groupvar,
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
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- df %>%
    rename(groupvar = !!quo_name(name_groupvar))
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country)
  df <- df %>%
    select('groupvar', 'utility')
  
  ### analysis ###
  
  # summary by category
  summary_by <- .summary_table_4_3(df = df, group_by = "groupvar")
  # summary_total
  summary_total <- .summary_table_4_3(df = df, group_by = NULL) %>%
    mutate(groupvar = "All groups")
  
  # combine
  retval <- bind_rows(summary_by, summary_total) %>%
    pivot_longer(-'groupvar') %>%
    pivot_wider(id_cols = 'name', names_from = 'groupvar', values_from = 'value')
  
  # return value
  return(retval)
}

#' Table 3.3 EQ-5D values: by age and groupvar
#' 
#' @param df Data frame with the EQ-5D, age, follow-up and grouping columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @param name_groupvar Character string for the grouping column
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
#' table_3_3(
#'   example_data,
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'   name_fu = "time",
#'   levels_fu = c('Pre-op', 'Post-op'),
#'   name_groupvar = "gender",
#'   name_age = "ageband",
#'   eq5d_version = "3L",
#'   country = "UK"
#' )
#' @importFrom rlang .data

table_3_3 <- function(df,
                      names_eq5d = NULL,
                      name_fu = NULL,
                      levels_fu = NULL,
                      name_groupvar,
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
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- df %>%
    rename(groupvar = !!quo_name(name_groupvar),
           age = !!quo_name(name_age))
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  df <- df %>%
    select('groupvar', 'age', 'fu', 'utility')
  
  # split age into categories
  if (is.numeric(df$age)) {
    # If age is numeric, convert to factor using predefined breaks
    age_breaks <- c(0, 18, seq(from = 25, to = 75, by = 10), Inf)
    df <- df %>%
      dplyr::mutate(age_cat = cut(
        .data$age, 
        breaks = age_breaks, 
        right  = FALSE
      )) %>%
      dplyr::mutate(
        age_cat = factor(
          .data$age_cat,
          levels = c("[0,18)", "[18,25)", "[25,35)", "[35,45)", "[45,55)", "[55,65)", "[65,75)", "[75,Inf)"),
          labels = c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
        )
      ) %>%
      dplyr::select(-.data$age)
    
  } else if (is.factor(df$age)) {
    df <- df %>%
      dplyr::rename(age_cat = .data$age)
  } else {
    stop("The 'age' column must be either numeric or factor. Stopping.")
  }
  
  ### analysis ###
  
  # summary by category
  summary_by <- .summary_table_4_4(df = df, group_by = c("groupvar", "age_cat"))
  
  # summary of totals by age
  summary_total_age <- .summary_table_4_4(df = df, group_by = c("groupvar")) %>%
    mutate(age_cat = "Total") 
  
  # summary of totals by groupvar
  summary_total_by <- .summary_table_4_4(df = df, group_by = c("age_cat")) %>%
    mutate(groupvar = "Total") 
  
  # summary of totals by groupvar and age
  summary_total_by_age <- .summary_table_4_4(df = df, group_by = NULL) %>%
    mutate(groupvar = "Total", age_cat = "Total")
  
  # combine and tidy up
  retval <- bind_rows(summary_by, summary_total_age, summary_total_by, summary_total_by_age) %>%
    pivot_longer(-('groupvar':'age_cat')) %>%
    pivot_wider(id_cols = c('groupvar', 'name'), names_from = 'age_cat', values_from = 'value')
  
  # return value
  return(retval)
}


#' Figure 1.2.1: Paretian Classification of Health Change (PCHC) by Group
#' This function computes PCHC categories between two time points for each subject, 
#' stratifies them by a grouping variable, and produces
#' a single bar chart with side-by-side bars showing the distribution of PCHC categories.
#' @param df Data frame containing EQ-5D dimensions, a grouping variable, patient ID, and follow-up columns
#' @param name_id Character string for the patient ID column
#' @param name_groupvar Character string for the grouping column (e.g., procedure)
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector of length 2 indicating the order of follow-up time points (e.g., c("Pre-op", "Post-op"))
#' @return A list with two elements:
#'   \item{plot_data}{A tibble of PCHC percentages by group}
#'   \item{p}{A ggplot2 object showing a bar chart with side-by-side bars for each PCHC category}
#' @export
#' @examples
#' result <- figure_1_2_1(
#'   df = example_data,
#'   name_id = "id",
#'   name_groupvar = "procedure",
#'   names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'   name_fu = "time",
#'   levels_fu = c("Pre-op", "Post-op")
#' )
#' result$p        # shows the plot
#' result$plot_data  # shows the summary table
#' @importFrom rlang .data

figure_1_2_1 <- function(df,
                         name_id,
                         name_groupvar,
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
  df <- df %>%
    dplyr::select(!!!syms(names_all))

  # Rename for internal use
  df <- df %>%
    dplyr::rename(
      id       = !!quo_name(name_id),
      groupvar = !!quo_name(name_groupvar)
    )

  # Prepare EQ-5D & Follow-up columns
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)

  # Sort by (id, groupvar, fu)
  df <- df %>%
    dplyr::arrange(.data$id, .data$groupvar, .data$fu)

  # Check uniqueness: each (id, groupvar, fu) should appear at most once
  .check_uniqueness(df, group_by = c("id", "groupvar", "fu"))

  ### 2) Calculate PCHC (Paretian Classification) ###
  # .pchc() compares the first level in levels_fu (e.g. "Pre-op") vs. the second (e.g. "Post-op")
  df <- .pchc(df = df, level_fu_1 = levels_fu[1], add_noprobs = TRUE) %>%
    dplyr::filter(!is.na(.data$state)) %>%
    # The factor levels for PCHC categories can be: "No problems", "No change", "Improve", "Worsen", "Mixed change"
    dplyr::mutate(
      state_noprobs = factor(
        .data$state_noprobs,
        levels = c("No problems", "No change", "Improve", "Worsen", "Mixed change")
      )
    )

  ### 3) Summarize for Single Bar Chart ###
  # We want one bar per groupvar for each state_noprobs
  plot_data <- df %>%
    dplyr::group_by(.data$groupvar, .data$state_noprobs) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(p = .data$n / sum(.data$n)) %>%
    dplyr::ungroup()

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
  plot_data_wide <- plot_data %>%
    tidyr::pivot_wider(
      id_cols = "state_noprobs",
      names_from = "groupvar",
      values_from = "p",
      values_fill = 0
    ) %>%
    dplyr::rename(`Change category` = "state_noprobs")

  ### 6) Return ###
  # Return list with summarized data & the ggplot object (with optional theme modifications)
  return(list(
    plot_data = plot_data_wide,
    p = .modify_ggplot_theme(p = p)  # if .modify_ggplot_theme is available
  ))
}

#' Figure 1.2.2: Percentage of Respondents Who Improved in Each EQ-5D Dimension, by Group
#' This function calculates how many respondents improved in each dimension between
#' two time points and summarizes the results for each group. The, it prodcuces 
#' a dimension-focused chart illustrating improvement percentages by dimension.
#' @param df Data frame containing EQ-5D columns, a grouping variable, an ID column, and a follow-up column
#' @param name_id Character string for the patient ID column
#' @param name_groupvar Character string for the grouping column (e.g. procedure)
#' @param names_eq5d Character vector of EQ-5D dimension names
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector of length 2, specifying the order of the follow-up levels (e.g. c("Pre-op","Post-op"))
#' @return A list containing:
#'   \item{plot_data}{A data frame of improvements by group and dimension}
#'   \item{p}{A ggplot2 object produced by `.pchc_plot_by_dim()`}
#' @export
#'
#' @examples
#' result <- figure_1_2_2(
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
#' @importFrom rlang .data
figure_1_2_2 <- function(df,
                         name_id,
                         name_groupvar,
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
  df <- df %>%
    dplyr::select(!!!syms(names_all))
  
  # Rename internal columns
  df <- df %>%
    dplyr::rename(
      id       = !!quo_name(name_id),
      groupvar = !!quo_name(name_groupvar)
    )
  
  # Prepare EQ-5D & Follow-up columns
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  
  # Sort by (id, groupvar, fu)
  df <- df %>%
    dplyr::arrange(.data$id, .data$groupvar, .data$fu)
  
  # Ensure uniqueness of (id, groupvar, fu)
  .check_uniqueness(df, group_by = c("id", "groupvar", "fu"))
  
  ### 2) Analysis ###
  # .pchc() calculates difference columns like mo_diff, sc_diff, etc.
  df <- .pchc(df = df, level_fu_1 = levels_fu[1]) %>%
    dplyr::filter(!is.na(.data$state))
  
  # Focus on those whose overall 'state' == "Improve"
  # then check dimension-specific improvements (e.g. mo_diff > 0)
  # e.g., mo_diff, sc_diff, ua_diff, pd_diff, ad_diff
  # (Ensure you have correct naming: "mo_diff":"ad_diff" or "da_diff" as needed)
  dimension_names <- c("mo_diff", "sc_diff", "ua_diff", "pd_diff", "ad_diff")
  
  plot_data <- df %>%
    dplyr::filter(.data$state == "Improve") %>%
    dplyr::select("groupvar", "fu", all_of(dimension_names)) %>%
    tidyr::pivot_longer(cols = all_of(dimension_names)) %>%
    dplyr::group_by(.data$groupvar, .data$fu, .data$name) %>%
    dplyr::summarise(
      n       = sum(.data$value > 0),  # number who improved in this dimension
      n_total = dplyr::n(),            # total in "Improve" group
      .groups = "drop"
    ) %>%
    dplyr::mutate(p = .data$n / .data$n_total) %>%
    # Convert dimension code (e.g. "mo_diff") to a nice label
    dplyr::mutate(
      name = factor(
        .data$name,
        levels = dimension_names,
        labels = c("Mobility", "Self-care", "Usual activities", "Pain & Discomfort", "Anxiety & Depression")
      )
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
  
  # For demonstration, we’ll pivot only by fu
  # to show p-values side by side. If you want groupvar in separate columns,
  # you'll do something like pivot_wider(names_from = c("groupvar","fu")...).
  plot_data_wide <- plot_data %>%
    tidyr::pivot_wider(
      id_cols     = c("groupvar", "name"),
      names_from  = "fu",
      values_from = "p",
      values_fill = 0
    ) %>%
    dplyr::rename(
      Group  = "groupvar",
      Values = "name"
    )
  
  return(list(
    plot_data = plot_data_wide,
    p         = .modify_ggplot_theme(p = p) # if you have a custom theme function
  ))
}


#' Figure 1.2.3: Percentage of Respondents Who Worsened in Each EQ-5D Dimension, by Group
#'
#' This function identifies respondents with a "Worsen" PCHC state (i.e., overall
#' health state got worse between levels_fu[1] and levels_fu[2]), checks
#' dimension-specific changes (e.g., mo_diff < 0), and summarizes by a grouping variable
#' (e.g., procedure) and time points. It returns a data table and a ggplot object.
#'
#' @param df A data frame containing EQ-5D columns, a grouping variable, an ID column, and a follow-up column
#' @param name_id A character string for the patient ID column
#' @param name_groupvar A character string for the grouping column (e.g., procedure)
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
#' result <- figure_1_2_3(
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
#' @importFrom rlang .data
figure_1_2_3 <- function(df,
                         name_id,
                         name_groupvar,
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
  df <- df %>%
    dplyr::select(!!!syms(names_all))
  
  # Rename internal columns
  df <- df %>%
    dplyr::rename(
      id       = !!quo_name(name_id),
      groupvar = !!quo_name(name_groupvar)
    )
  
  # Prepare EQ-5D & Follow-up columns
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  
  # Sort by (id, groupvar, fu)
  df <- df %>%
    dplyr::arrange(.data$id, .data$groupvar, .data$fu)
  
  # Ensure uniqueness of (id, groupvar, fu)
  .check_uniqueness(df, group_by = c("id", "groupvar", "fu"))
  
  ### 2) Analysis: PCHC ###
  
  # .pchc() calculates difference columns like mo_diff, sc_diff, etc.
  # level_fu_1 is the 'baseline' time point.
  df <- .pchc(df = df, level_fu_1 = levels_fu[1]) %>%
    dplyr::filter(!is.na(.data$state))
  
  # We focus on those whose overall state == "Worsen"
  # then check dimension-specific changes, e.g., mo_diff < 0
  dimension_names <- c("mo_diff", "sc_diff", "ua_diff", "pd_diff", "ad_diff")
  
  plot_data <- df %>%
    dplyr::filter(.data$state == "Worsen") %>%
    dplyr::select("groupvar", "fu", all_of(dimension_names)) %>%
    tidyr::pivot_longer(cols = all_of(dimension_names)) %>%
    dplyr::group_by(.data$groupvar, .data$fu, .data$name) %>%
    dplyr::summarise(
      n       = sum(.data$value < 0),  # # participants who got worse in that dimension
      n_total = dplyr::n(),            # total in "Worsen" group
      .groups = "drop"
    ) %>%
    dplyr::mutate(p = .data$n / .data$n_total) %>%
    # Convert dimension code (e.g. "mo_diff") to a nice label
    dplyr::mutate(
      name = factor(
        .data$name,
        levels = dimension_names,
        labels = c("Mobility", "Self-care", "Usual activities", "Pain & Discomfort", "Anxiety & Depression")
      )
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
  # pivot by fu. If you want separate columns for groupvar as well,
  # do pivot_wider(names_from = c("groupvar","fu"), ...)
  plot_data_wide <- plot_data %>%
    tidyr::pivot_wider(
      id_cols     = c("groupvar", "name"),
      names_from  = "fu",
      values_from = "p",
      values_fill = 0
    ) %>%
    dplyr::rename(
      Group  = "groupvar",
      Values = "name"
    )
  
  return(list(
    plot_data = plot_data_wide,
    p         = .modify_ggplot_theme(p = p) # if you have a custom theme function
  ))
}


#' Figure 1.2.4: Percentage of Respondents Who Had a Mixed Change Overall,
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
#' @param name_groupvar Character string for the grouping column (e.g. "procedure")
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
#' result <- figure_1_2_4(
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
#' @importFrom rlang .data
figure_1_2_4 <- function(df,
                         name_id,
                         name_groupvar,
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
  df <- df %>%
    dplyr::select(!!!syms(names_all))
  
  # Rename for internal use
  df <- df %>%
    dplyr::rename(
      id       = !!quo_name(name_id),
      groupvar = !!quo_name(name_groupvar)
    )
  
  # Prepare EQ-5D & Follow-up columns
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  
  # Sort by (id, groupvar, fu)
  df <- df %>%
    dplyr::arrange(.data$id, .data$groupvar, .data$fu)
  
  # Check uniqueness
  .check_uniqueness(df, group_by = c("id", "groupvar", "fu"))
  
  ### 2) Analysis: Identify Mixed Change ###
  
  # .pchc() adds difference columns like mo_diff, sc_diff, etc.
  # level_fu_1 is the 'baseline' time point
  df <- .pchc(df = df, level_fu_1 = levels_fu[1]) %>%
    dplyr::filter(!is.na(.data$state))
  
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
  plot_data <- df %>%
    dplyr::filter(.data$state == "Mixed change") %>%
    dplyr::select("groupvar", "fu", all_of(dimension_names)) %>%
    # pivot dimension columns
    tidyr::pivot_longer(cols = all_of(dimension_names)) %>%
    dplyr::group_by(.data$groupvar, .data$fu, .data$name) %>%
    dplyr::summarise(
      n_improve = sum(.data$value > 0),
      n_worsen  = sum(.data$value < 0),
      n_total   = dplyr::n(),
      .groups   = "drop"
    ) %>%
    dplyr::mutate(
      Improve = .data$n_improve / .data$n_total,
      Worsen  = .data$n_worsen  / .data$n_total
    ) %>%
    # Convert e.g. "mo_diff" -> "Mobility"
    dplyr::mutate(
      name = factor(
        .data$name,
        levels = dimension_names,
        labels = c("Mobility", "Self-care", "Usual activities", "Pain & Discomfort", "Anxiety & Depression")
      )
    ) %>%
    dplyr::select(-dplyr::contains("n_")) %>%
    # pivot "Improve" & "Worsen" into a single column
    tidyr::pivot_longer(
      cols      = c("Improve", "Worsen"),
      names_to  = "change",
      values_to = "p"
    ) %>%
    # Build a combined factor: "Improve: Pre-op" / "Worsen: Pre-op" etc.
    dplyr::mutate(
      fu = paste0(.data$change, ": ", .data$fu),
      fu = factor(.data$fu, levels = levels_fu_change)
    )
  
  ### 3) Plot ###
  
  plot_data2 <- plot_data
  plot_data2$groupvar <- paste(plot_data$groupvar, plot_data$fu)
  
  plot_data2 <- plot_data2 %>%
    mutate(
      name = factor(.data$name, levels = c("Mobility", "Self-care", "Usual activities", "Pain & Discomfort", "Anxiety & Depression")),
      change = factor(.data$change, levels = c("Improve", "Worsen"))
    ) %>%
    arrange(.data$name, .data$change)
  
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
  # If you want separate columns for groupvar as well, consider pivoting with names_from = c("groupvar","fu").
  plot_data_wide <- plot_data %>%
    tidyr::pivot_wider(
      id_cols     = c("groupvar", "name"),
      names_from  = "fu",
      values_from = "p",
      values_fill = 0
    ) %>%
    dplyr::rename(
      Group  = "groupvar",
      Values = "name"
    )
  
  return(list(
    plot_data = plot_data_wide,
    p         = .modify_ggplot_theme(p = p)
  ))
}

#' Figure 1.2.5: Health Profile Grid (HPG) for Two Time Points
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
#' tmp <- figure_1_2_5(
#'            df = example_data, 
#'            names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'            name_fu = "time", 
#'            levels_fu = c("Pre-op", "Post-op"), 
#'            name_id = "id", 
#'            eq5d_version = "3L", 
#'            country = "UK"
#'        )
#' @importFrom rlang .data

figure_1_2_5 <- function(df,
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
  df <- df %>%
    dplyr::select(!!!syms(names_all))
  
  # Rename for internal use
  df <- df %>%
    dplyr::rename(
      id       = !!quo_name(name_id)
    )
  
  # Prepare EQ-5D & Follow-up columns
  df <- .prep_eq5d(df = df, names = names_eq5d)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  
  # Sort by (id, groupvar, fu)
  df <- df %>%
    dplyr::arrange(.data$id, .data$fu)
  
  # Check uniqueness: each (id, groupvar, fu) should appear at most once
  .check_uniqueness(df, group_by = c("id", "fu"))
  
  ### 2) Calculate PCHC (Paretian Classification) ###
  # .pchc() compares the first level in levels_fu (e.g. "Pre-op") vs. the second (e.g. "Post-op")
  df <- .pchc(df = df, level_fu_1 = levels_fu[1], add_noprobs = TRUE) %>%
    dplyr::filter(!is.na(.data$state)) %>%
    # The factor levels for PCHC categories can be: "No problems", "No change", "Improve", "Worsen", "Mixed change"
    dplyr::mutate(
      state_noprobs = factor(
        .data$state_noprobs,
        levels = c("No problems", "No change", "Improve", "Worsen", "Mixed change")
      )
    )
  
  ### 3) Summarize for HPG ###
  # Create value set
  vs <- data.frame(profile = make_all_EQ_indexes(version = eq5d_version))
  vs$utility <- eq5d(vs$profile, country = country, version = eq5d_version)
  vs <- vs %>%
    dplyr::arrange(dplyr::desc(.data$utility)) %>%
    dplyr::mutate(rank = dplyr::row_number())
  
  # Build profiles
  eqdims <- names_eq5d
  for (d in eqdims) {
    df[[paste0(d, "_t2")]] <- df[[d]]
    df[[paste0(d, "_t1")]] <- df[[d]] + df[[paste0(d, "_diff")]]
  }
  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # T1 profile e.g. '11211'
      profile_t1 = as.integer(paste0(!!!syms(paste0(eqdims,"_t1")))),
      # T2 profile
      profile_t2 = as.integer(paste0(!!!syms(paste0(eqdims,"_t2"))))
    ) %>%
    dplyr::ungroup()
  
  # Get rank
  df <- df %>%
    dplyr::left_join(
      vs %>% dplyr::select(.data$profile, rank_t1 = rank),
      by = c("profile_t1" = "profile")
    ) %>%
    # T2
    dplyr::left_join(
      vs %>% dplyr::select(.data$profile, rank_t2 = rank),
      by = c("profile_t2" = "profile")
    )
  
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



#' Figure 1.3.1: EQ-5D values plotted against LSS
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
#' tmp <- figure_1_3_1(
#'  df, 
#'  names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'  eq5d_version = "5L", 
#'  country = "US"
#' )
#' tmp$p
#' tmp$plot_data
#' @importFrom rlang .data

figure_1_3_1 <- function(df,
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
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d, 
                   add_state = TRUE, 
                   add_lss = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country) %>%
    # leave relevant columns only
    select('lss', 'utility')
  
  ### analysis ###
  
  # prepare data for plotting
  plot_data <- df %>%
    select('lss', 'utility') %>%
    # remove NAs
    filter(!is.na(.data$lss) & !is.na(.data$utility)) %>%
    group_by(.data$lss) %>%
    # summarise
    summarise(median = median(.data$utility),
              min = min(.data$utility),
              max = max(.data$utility),
              .groups = "drop") %>%
    # order
    arrange(.data$lss)
  
  # graphical parameters
  # x-axis variable
  xvar_lab <- "LSS (Level Sum Score)"
  # axis breaks
  x_breaks <- 1:100
  y_breaks <- seq(from = -1, to = 1, by = 0.2)
  
  # plot
  p <- ggplot() +
    # plot median, min and max
    geom_segment(data = plot_data %>%
                   pivot_longer(-'lss') %>%
                   mutate(name = factor(.data$name, 
                                        levels = c("median", "min", "max"),
                                        labels = c("Median", "Lowest", "Highest"))), 
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
  plot_data <- plot_data %>%
    rename(LSS = 'lss',
           Median = 'median',
           Minimum = 'min',
           Maximum = 'max')
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' Figure 1.3.2: EQ-5D values plotted against LFS
#' 
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary plot and data used for plotting
#' @export
#' @examples
#' tmp <- figure_1_3_2(
#'  example_data, 
#'  names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'  eq5d_version = "3L",
#'  country = "UK"
#' )
#' tmp$p
#' tmp$plot_data
#' @importFrom rlang .data

figure_1_3_2 <- function(df,
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
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d, 
                   add_state = TRUE, 
                   add_lfs = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country) %>%
    # leave relevant columns only
    select('lfs', 'utility')
  
  ### analysis ###
  
  # prepare data for plotting
  plot_data <- df %>%
    select('lfs', 'utility') %>%
    # remove NAs
    filter(!is.na(.data$lfs) & !is.na(.data$utility)) %>%
    group_by(.data$lfs) %>%
    # summarise
    summarise(median = median(.data$utility),
              min = min(.data$utility),
              max = max(.data$utility),
              .groups = "drop") %>%
    # order
    arrange(-.data$median)
  
  # impose order on the x-axis
  lfs_levels <- plot_data$lfs
  plot_data <- plot_data %>%
    mutate(lfs_f = factor(.data$lfs, levels = lfs_levels))
  
  # axis breaks
  n <- length(lfs_levels)
  i <- seq(from = 1, to = n, by = 3)
  x_breaks <- (1:n)[i]
  x_labels <- lfs_levels[i]
  y_breaks <- seq(from = -1, to = 1, by = 0.2)
  
  # plot
  p <- ggplot() +
    # plot median, min and max
    geom_segment(data = plot_data %>%
                   pivot_longer(-c('lfs', 'lfs_f')) %>%
                   mutate(name = factor(.data$name, 
                                        levels = c("median", "min", "max"),
                                        labels = c("Median", "Lowest", "Highest"))), 
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
  plot_data <- plot_data %>%
    select(-'lfs_f') %>%
    rename(LFS = 'lfs',
           Median = 'median',
           Minimum = 'min',
           Maximum = 'max')
  
  return(list(plot_data = plot_data, p = p))
}

#' Figure 2.1: EQ VAS scores
#' 
#' @param df Data frame with the VAS column
#' @param name_vas Character string for the VAS column
#' @return Summary plot and data used for plotting
#' @export
#' @examples
#' tmp <- figure_2_1(example_data, name_vas = 'vas')
#' tmp$p
#' tmp$plot_data
#' @importFrom rlang .data

figure_2_1 <- function(df, name_vas = NULL){
  
  ### data preparation ###
  # replace NULL names with defaults
  temp <- .get_names(name_vas = name_vas)
  name_vas <- temp$name_vas
  # check existence of columns 
  names_all <- c(name_vas)
  if (!all(names_all %in% colnames(df)))
    stop("Provided column names not in dataframe. Stopping.")
  # all columns defined and exist; only leave relevant columns now
  df <- df %>%
    select(!!!syms(names_all)) 
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
  
  # output plotting data
  plot_data <- df %>%
    # count for each value between 0 & 100
    count(vas = factor(.data$vas, levels = 1:100), .drop = FALSE) %>%
    # order
    arrange(.data$vas) %>%
    rename(count = 'n')
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' Figure 2.2: Mid-point EQ VAS scores
#' 
#' @param df Data frame with the VAS column
#' @param name_vas Character string for the VAS column
#' @return Summary plot and data used for plotting
#' @export
#' @examples
#' tmp <- figure_2_2(example_data, name_vas = 'vas')
#' tmp$p
#' tmp$plot_data
#' @importFrom rlang .data

figure_2_2 <- function(df, name_vas = NULL){
  
  # produce output to be plotted (Table 3.2)
  # do not add totals
  plot_data <- table_2_2(df = df, name_vas = name_vas, add_na_total = FALSE) %>%
    # remove Range column, not needed
    select(-Range)
  
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

#' Figure 3.1: EQ-5D values by timepoints: mean values and 95\% confidence intervals
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
#' tmp <- figure_3_1(
#'  example_data,
#'  names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'  name_fu = "time",
#'  levels_fu = c('Pre-op', 'Post-op'),
#'  eq5d_version = "3L",
#'  country = "UK"
#' )
#' tmp$p
#' tmp$plot_data
#' @importFrom rlang .data

figure_3_1 <- function(df,
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
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  df <- df %>%
    select('fu', 'utility')
  
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
    ggtitle(str_c("EQ-5D values by ", name_fu, ": mean and 95% confidence intervals")) +
    # manipulate x-axis
    scale_x_discrete(name = "time") +
    # manipulate y-axis
    scale_y_continuous(name = "EQ-5D value")
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' Figure 3.2: Mean EQ-5D values and 95\% confidence intervals: all vs by groupvar
#' 
#' @param df Data frame with the EQ-5D and grouping columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_groupvar Character string for the grouping column
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary plot and data used for plotting
#' @export
#' @examples
#' tmp <- figure_3_2(
#'  example_data,
#'  names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'  name_groupvar = "procedure",
#'  eq5d_version = "3L",
#'  country = "UK"
#' )
#' tmp$p
#' tmp$plot_data
#' @importFrom rlang .data

figure_3_2 <- function(df,
                       names_eq5d = NULL,
                       name_groupvar,
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
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- df %>%
    rename(groupvar = !!quo_name(name_groupvar))
  df <- .prep_eq5d(df = df, names = names_eq5d, 
                   add_state = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country) %>%
    select('groupvar', 'utility')
  
  ### analysis ###
  
  # prepare data for plotting
  plot_data <- .summary_mean_ci(df = df, group_by = "groupvar")
  
  # add totals
  plot_data_total <- .summary_mean_ci(df = df, group_by = NULL) %>%
    mutate(groupvar = "All groups") %>%
    select('groupvar', everything())
  
  # combine
  groupvar_levels <- c(plot_data$groupvar, "All groups")
  plot_data <- bind_rows(plot_data, plot_data_total) %>%
    mutate(groupvar = factor(.data$groupvar, levels = groupvar_levels)) %>%
    arrange(.data$groupvar)
  
  # plot
  p <- ggplot(data = plot_data, aes(x = .data$groupvar, y = .data$mean, ymin = .data$ci_lb, ymax = .data$ci_ub)) +
    # plot means
    geom_bar(colour = "blue", fill = "blue", stat = "identity", width = 0.5) + 
    geom_point() +
    # add error bars
    geom_errorbar(width = 0.1, size = 1) +
    # plot title
    ggtitle(str_c("Mean EQ-5D values 95% confidence intervals: all vs by ", name_groupvar)) +
    # manipulate x-axis
    scale_x_discrete(name = name_groupvar) +
    # manipulate y-axis
    scale_y_continuous(name = "EQ-5D value",
                       limits = c(0, 1),
                       breaks = seq(from = 0, to = 1, by = 0.1),
                       expand = c(0, 0))
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' Figure 3.3: EQ-5D values: smoothed lines and confidence intervals by groupvar
#' 
#' @param df Data frame with the EQ-5D, follow-up and grouping columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param name_fu Character string for the follow-up column
#' @param levels_fu Character vector containing the order of the values in the follow-up column. 
#' If NULL (default value), the levels will be ordered in the order of appearance in df.
#' @param name_groupvar Character string for the grouping column
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary plot and data used for plotting
#' @export
#' @examples
#' tmp <- figure_3_3(
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
#' @importFrom rlang .data

figure_3_3 <- function(df, 
                       names_eq5d = NULL,
                       name_fu = NULL,
                       levels_fu = NULL,
                       name_groupvar,
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
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- df %>%
    rename(groupvar = !!quo_name(name_groupvar)) %>%
    # factorise groupvar variable
    mutate(groupvar = factor(.data$groupvar))
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country)
  df <- .prep_fu(df = df, name = name_fu, levels = levels_fu)
  df <- df %>%
    select('fu', 'groupvar', 'utility')
  
  ### analysis ###
  
  # prepare data for plotting
  plot_data <- .summary_mean_ci(df = df, group_by = c("fu", "groupvar"))
  
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
    ggtitle(str_c("Longitudinal EQ-5D values by ", name_fu, " and ", name_groupvar, ":\nmeans and 95% confidence intervals")) +
    # manipulate x-axis
    scale_x_discrete(name = name_fu) +
    # manipulate y-axis
    scale_y_continuous(name = "EQ-5D value")
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' Figure 3.4: EQ-5D values: smoothed lines and confidence intervals by groupvar
#' 
#' @param df Data frame with the EQ-5D columns
#' @param names_eq5d Character vector of column names for the EQ-5D dimensions
#' @param eq5d_version Version of the EQ-5D instrument
#' @param country A character string representing the name of the country. 
#' This could be in a 2-letter format, full name or short name, as specified in the country_codes datasets.
#' @return Summary plot and data used for plotting
#' @export
#' @examples
#' tmp <- figure_3_4(
#'  example_data,
#'  names_eq5d = c("mo", "sc", "ua", "pd", "ad"), 
#'  eq5d_version = "3L",
#'  country = "UK"
#' )
#' tmp$p
#' tmp$plot_data
#' @importFrom rlang .data

figure_3_4 <- function(df, 
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
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country) %>%
    select('utility')
  
  ### analysis ###
  
  # prepare data for plotting
  plot_data <- df %>%
    group_by(.data$utility) %>%
    summarise(count = n()) %>%
    arrange(.data$utility)
  
  # plot
  p <- ggplot(plot_data, aes(x = .data$utility, y = count)) +
    geom_bar(fill = "blue", stat = "identity") + 
    scale_x_continuous(name = "EQ-5D value") +
    scale_y_continuous(name = "Frequency") + 
    ggtitle("Distribution of EQ-5D values") + 
    theme(
      # reinstate axis.ticks
      axis.ticks = element_line(colour = "grey50"))
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}

#' Figure 3.5: EQ-5D values: smoothed lines and confidence intervals by groupvar
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
#' tmp <- figure_3_5(
#'    example_data,
#'    names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
#'    name_vas = "vas",
#'    eq5d_version = "3L",
#'    country = "UK"
#'  )
#' tmp$p
#' tmp$plot_data
#' @importFrom rlang .data

figure_3_5 <- function(df,
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
  df <- df %>%
    select(!!!syms(names_all)) 
  # further checks and data preparation
  df <- .prep_vas(df = df, name = name_vas)
  df <- .prep_eq5d(df = df, names = names_eq5d,
                   add_state = TRUE,
                   add_utility = TRUE, eq5d_version = eq5d_version, country = country) %>%
    select('vas', 'utility')
  
  ### analysis ###
  
  # prepare data for plotting
  plot_data <- df %>%
    arrange(.data$vas, .data$utility)
  
  # plot
  p <- ggplot(plot_data, aes(x = .data$vas, y = .data$utility)) +
    geom_point(colour = "blue") + 
    scale_x_continuous(name = "EQ VAS") +
    scale_y_continuous(name = "EQ-5D value") + 
    ggtitle("EQ-5D values plotted against EQ VAS")
  
  return(list(plot_data = plot_data, p = .modify_ggplot_theme(p = p)))
}