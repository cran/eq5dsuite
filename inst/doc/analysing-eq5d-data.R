## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width  = 7,
  fig.height = 4
)
library(eq5dsuite)

## ----load-data----------------------------------------------------------------
# The example dataset is bundled with the package
data(example_data)
head(example_data)

## ----wex1-prep----------------------------------------------------------------
dim_names <- c("mo", "sc", "ua", "pd", "ad")

# Subset to hip replacement patients
hip_data <- example_data[example_data$procedure == "Hip Replacement", ]

# Add profile code and EQ-5D value
hip_data$profile_code <- toEQ5Dindex(
  x         = hip_data,
  dim.names = dim_names
)
hip_data$value <- eq5d3l(
  hip_data[, dim_names],
  country   = "UK",
  dim.names = dim_names
)

# Pre-operative subset
hip_preop <- hip_data[hip_data$time == "Pre-op", ]

## ----wex1-profile-------------------------------------------------------------
eq5d_profile_level_summary(
  df           = hip_preop,
  names_eq5d   = dim_names,
  eq5d_version = "3L"
)

## ----wex1-top-states----------------------------------------------------------
eq5d_profile_top_states(
  df           = hip_preop,
  names_eq5d   = dim_names,
  eq5d_version = "3L",
  n            = 5
)

## ----wex1-pchc, fig.cap = "PCHC classification for hip replacement patients."----
eq5d_profile_pchc_by_group_plot(
  df         = hip_data,
  name_id    = "id",
  names_eq5d = dim_names,
  name_fu    = "time",
  levels_fu  = c("Pre-op", "Post-op")
)$p +
  ggplot2::labs(
    title = "PCHC: hip replacement patients",
    x     = NULL,
    y     = "Percentage of respondents"
  ) +
  ggplot2::theme_minimal()

## ----wex1-better, fig.cap = "Dimensions improved among patients classified as Better."----
eq5d_profile_better_dimensions_by_group_plot(
  df         = hip_data,
  name_id    = "id",
  names_eq5d = dim_names,
  name_fu    = "time",
  levels_fu  = c("Pre-op", "Post-op")
)$p +
  ggplot2::theme_minimal()

## ----wex1-utility-------------------------------------------------------------
eq5d_utility_summary(
  df           = hip_data,
  name_fu      = "time",
  levels_fu    = c("Pre-op", "Post-op"),
  names_eq5d   = dim_names,
  eq5d_version = "3L",
  country      = "UK"
)

## ----wex1-vas-----------------------------------------------------------------
eq5d_vas_summary(
  df        = hip_data,
  name_vas  = "vas",
  name_fu   = "time",
  levels_fu = c("Pre-op", "Post-op")
)

## ----wex2-prep----------------------------------------------------------------
# Subset to two procedures, pre-operative only
procs <- c("Knee Replacement", "Groin Hernia")
comparison_data <- example_data[
  example_data$procedure %in% procs &
  example_data$time == "Pre-op", ]

# Add profile code and EQ-5D value
comparison_data$profile_code <- toEQ5Dindex(
  x         = comparison_data,
  dim.names = dim_names
)
comparison_data$value <- eq5d3l(
  comparison_data[, dim_names],
  country   = "UK",
  dim.names = dim_names
)

## ----wex2-profile-------------------------------------------------------------
eq5d_profile_level_summary_by_group(
  df           = comparison_data,
  names_eq5d   = dim_names,
  name_cat     = "procedure",
  eq5d_version = "3L"
)

## ----wex2-utility-summary-----------------------------------------------------
eq5d_utility_summary_by_group(
  df            = comparison_data,
  names_eq5d    = dim_names,
  name_groupvar = "procedure",
  eq5d_version  = "3L",
  country       = "UK"
)

## ----wex2-utility-plot, fig.cap = "Mean pre-operative EQ-5D values by procedure group."----
eq5d_utility_by_group_plot(
  df            = comparison_data,
  names_eq5d    = dim_names,
  name_groupvar = "procedure",
  eq5d_version  = "3L",
  country       = "UK"
)$p +
  ggplot2::theme_minimal()

## ----wex2-lss-hernia, fig.cap = "EQ-5D values by LSS — Groin Hernia."---------
hernia_data <- comparison_data[
  comparison_data$procedure == "Groin Hernia", ]

eq5d_profile_lss_utility_plot(
  hernia_data,
  names_eq5d   = dim_names,
  eq5d_version = "3L",
  country      = "UK"
)$p +
  ggplot2::labs(
    title = "Groin Hernia",
    x     = "Level Sum Score (LSS)",
    y     = "EQ-5D value"
  ) +
  ggplot2::scale_x_continuous(limits = c(5, 15),
                               breaks = seq(5, 15, 2)) +
  ggplot2::theme_minimal()

## ----wex2-lss-knee, fig.cap = "EQ-5D values by LSS — Knee Replacement."-------
knee_data <- comparison_data[
  comparison_data$procedure == "Knee Replacement", ]

eq5d_profile_lss_utility_plot(
  knee_data,
  names_eq5d   = dim_names,
  eq5d_version = "3L",
  country      = "UK"
)$p +
  ggplot2::labs(
    title = "Knee Replacement",
    x     = "Level Sum Score (LSS)",
    y     = "EQ-5D value"
  ) +
  ggplot2::scale_x_continuous(limits = c(5, 15),
                               breaks = seq(5, 15, 2)) +
  ggplot2::theme_minimal()

