# mod_analysis.R — one Analysis page for all three components
#
# The sidebar has two selects — Component, then Output — followed by whatever
# that output needs, then a single Run. The main area shows one card.
#
# Every analysis is one entry in ANALYSES below. The arguments each entry
# builds are exactly those the previous accordion-per-analysis version built,
# so the computation is unchanged; only the way the user reaches it differs.

COMPONENTS <- c("EQ-5D profiles" = "profile",
                "EQ-5D values"   = "values",
                "EQ VAS"         = "vas")

# Shorthands used by the registry's prep functions.
.ver  <- function(rv) rv$mapping$eq5d_version
.base <- function(rv) list(names_eq5d = DIMS_STD, eq5d_version = .ver(rv))
# The timepoint order set on the Data page. NULL when no timepoint is mapped,
# which leaves the analysis functions to their own default.
.fu_lv <- function(rv) rv$mapping$levels_fu
.pchc_args <- function(rv) list(name_id = "id", name_groupvar = "groupvar",
                                names_eq5d = DIMS_STD, name_fu = "fu",
                                levels_fu = .fu_lv(rv))

# needs codes: fu, id, groupvar, vas, utility
# opts codes:  utility_col, country, topn, two_fu, group_filter
#
# No analysis takes a value set any more: every one that uses EQ-5D values
# takes them from a column, which the Calculate EQ-5D values page produces.
a <- function(id, component, group, label, desc, type, fn, save_label,
              needs = character(0), opts = character(0), prep) {
  list(id = id, component = component, group = group, label = label,
       desc = desc, type = type, fn = fn, save_label = save_label,
       needs = needs, opts = opts, prep = prep)
}

ANALYSES <- list(

  # ── EQ-5D profiles: cross-sectional ────────────────────────────────────────
  a("111", "profile", "Cross-sectional",
    "Table 1.1.1 — Level frequencies by dimension",
    "Frequency (n and %) of each response level across all five EQ-5D dimensions.",
    "table", "eq5d_profile_level_summary", "Level frequencies (1.1.1)",
    prep = function(input, rv) list(df = rv$processed_data, args = .base(rv))),

  a("112", "profile", "Cross-sectional",
    "Table 1.1.2 — Level frequencies by group",
    "Frequency of response levels stratified by group.",
    "table", "eq5d_profile_level_summary_by_group", "Level freq. by group (1.1.2)",
    needs = "groupvar",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(names_eq5d = DIMS_STD, name_cat = "groupvar",
                  eq5d_version = .ver(rv)))),

  a("113", "profile", "Cross-sectional",
    "Table 1.1.3 — Most common health states",
    "The most frequently observed EQ-5D health states in the dataset.",
    "table", "eq5d_profile_top_states", "Most common states (1.1.3)",
    opts = "topn",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(names_eq5d = DIMS_STD, eq5d_version = .ver(rv),
                  n = as.integer(input$topn)))),

  # ── EQ-5D profiles: longitudinal ──────────────────────────────────────────
  a("121", "profile", "Longitudinal",
    "Table 1.2.1 — Level frequencies by timepoint",
    "Frequency of response levels at each timepoint.",
    "table", "eq5d_profile_change_summary", "Level freq. by timepoint (1.2.1)",
    needs = "fu",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(names_eq5d = DIMS_STD, name_fu = "fu",
                  levels_fu = .fu_lv(rv), eq5d_version = .ver(rv)))),

  a("122", "profile", "Longitudinal",
    "Table 1.2.2 — PCHC health change",
    paste("Paretian Classification of Health Change: how each respondent's",
          "profile moved between two timepoints. Ungrouped data are treated",
          "as one group."),
    "table", "eq5d_profile_pchc_table", "PCHC (1.2.2)",
    needs = c("fu", "id"),
    prep = function(input, rv) list(
      df = ensure_groupvar(rv$processed_data), args = .pchc_args(rv),
      prep = if ("groupvar" %in% names(rv$processed_data)) NULL else "groupvar")),

  a("123", "profile", "Longitudinal",
    "Table 1.2.3 — PCHC accounting for no problems",
    "PCHC separating those who reported no problems at baseline.",
    "table", "eq5d_profile_pchc_with_no_problems_table", "PCHC no-problems (1.2.3)",
    needs = c("fu", "id"),
    prep = function(input, rv) list(
      df = ensure_groupvar(rv$processed_data), args = .pchc_args(rv),
      prep = if ("groupvar" %in% names(rv$processed_data)) NULL else "groupvar")),

  a("124", "profile", "Longitudinal",
    "Table 1.2.4 — Level change between timepoints",
    "Proportion improving, stable or worsening in each dimension.",
    "table", "eq5d_profile_dimension_change_table", "Level change (1.2.4)",
    needs = c("fu", "id"),
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(name_id = "id", names_eq5d = DIMS_STD, name_fu = "fu",
                  levels_fu = .fu_lv(rv)))),

  a("121fig", "profile", "Longitudinal",
    "Figure 1.2.1 — PCHC categories by group",
    "Bar chart of the PCHC categories, by group.",
    "plot", "eq5d_profile_pchc_by_group_plot", "PCHC bar chart (fig. 1.2.1)",
    needs = c("fu", "id"),
    prep = function(input, rv) list(
      df = ensure_groupvar(rv$processed_data), args = .pchc_args(rv),
      prep = if ("groupvar" %in% names(rv$processed_data)) NULL else "groupvar")),

  a("122fig", "profile", "Longitudinal",
    "Figure 1.2.2 — Improvements by group and dimension",
    "Proportion improving in each dimension, by group.",
    "plot", "eq5d_profile_better_dimensions_by_group_plot",
    "Improvements chart (fig. 1.2.2)",
    needs = c("fu", "id"),
    prep = function(input, rv) list(
      df = ensure_groupvar(rv$processed_data), args = .pchc_args(rv),
      prep = if ("groupvar" %in% names(rv$processed_data)) NULL else "groupvar")),

  a("123fig", "profile", "Longitudinal",
    "Figure 1.2.3 — Worsenings by group and dimension",
    "Proportion worsening in each dimension, by group.",
    "plot", "eq5d_profile_worse_dimensions_by_group_plot",
    "Worsenings chart (fig. 1.2.3)",
    needs = c("fu", "id"),
    prep = function(input, rv) list(
      df = ensure_groupvar(rv$processed_data), args = .pchc_args(rv),
      prep = if ("groupvar" %in% names(rv$processed_data)) NULL else "groupvar")),

  a("124fig", "profile", "Longitudinal",
    "Figure 1.2.4 — Mixed changes by group and dimension",
    "Proportion with mixed change in each dimension, by group.",
    "plot", "eq5d_profile_mixed_dimensions_by_group_plot",
    "Mixed change chart (fig. 1.2.4)",
    needs = c("fu", "id"),
    prep = function(input, rv) list(
      df = ensure_groupvar(rv$processed_data), args = .pchc_args(rv),
      prep = if ("groupvar" %in% names(rv$processed_data)) NULL else "groupvar")),

  a("125fig", "profile", "Longitudinal",
    "Figure 1.2.5 — Health Profile Grid (HPG)",
    "Scatter plot comparing health profiles between two timepoints.",
    "plot", "eq5d_profile_health_profile_grid", "HPG scatter (fig. 1.2.5)",
    # The one analysis that takes a value set rather than a value column: it
    # ranks every state the instrument allows, including those absent from
    # the data, which needs them valued. See ?eq5d_profile_health_profile_grid.
    needs = c("fu", "id"), opts = c("country", "two_fu"),
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(names_eq5d = DIMS_STD,
                  name_fu = "fu", levels_fu = input$fu_levels,
                  name_id = "id", eq5d_version = .ver(rv),
                  country = input$country))),

  # ── EQ-5D profiles: summarising severity ──────────────────────────────────
  a("131", "profile", "Summarising severity",
    "Table 1.3.1 — Summary statistics by Level Sum Score (LSS)",
    "Summary statistics for EQ-5D values at each Level Sum Score.",
    "table", "eq5d_profile_lss_utility_summary", "LSS summary stats (1.3.1)",
    needs = "utility", opts = "utility_col",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = c(.base(rv), list(name_utility = input$utility_col)))),

  a("131fig", "profile", "Summarising severity",
    "Figure 1.3.1 — EQ-5D values vs Level Sum Score",
    "EQ-5D values plotted against the Level Sum Score.",
    "plot", "eq5d_profile_lss_utility_plot", "LSS scatter (fig. 1.3.1)",
    needs = "utility", opts = "utility_col",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = c(.base(rv), list(name_utility = input$utility_col)))),

  a("132", "profile", "Summarising severity",
    "Table 1.3.2 — Distribution by Level Frequency Score (LFS)",
    "Distribution of EQ-5D health states by Level Frequency Score.",
    "table", "eq5d_profile_lfs_distribution", "LFS distribution (1.3.2)",
    prep = function(input, rv) list(df = rv$processed_data, args = .base(rv))),

  a("134", "profile", "Summarising severity",
    "Table 1.3.4 — Summary statistics by Level Frequency Score (LFS)",
    "Summary statistics for EQ-5D values by Level Frequency Score.",
    "table", "eq5d_profile_lfs_utility_summary", "LFS summary stats (1.3.4)",
    needs = "utility", opts = "utility_col",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = c(.base(rv), list(name_utility = input$utility_col)))),

  a("132fig", "profile", "Summarising severity",
    "Figure 1.3.2 — EQ-5D values vs Level Frequency Score",
    "EQ-5D values plotted against the Level Frequency Score.",
    "plot", "eq5d_profile_lfs_utility_plot", "LFS scatter (fig. 1.3.2)",
    needs = "utility", opts = "utility_col",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = c(.base(rv), list(name_utility = input$utility_col)))),

  # ── EQ-5D profiles: informativity ─────────────────────────────────────────
  a("141", "profile", "Informativity",
    "Table 1.4.1 — Shannon's informativity indices",
    paste("Shannon's index H' in bits, its maximum H'max, and Shannon's",
          "evenness index J' = H'/H'max, for each dimension and for the",
          "health state: how much of the classification system the sample",
          "uses. By timepoint when one is selected."),
    "table", "eq5d_profile_shannon", "Shannon's indices (1.4.1)",
    prep = function(input, rv) list(
      df = rv$processed_data,
      # H'max depends on what the instrument allows, so eq5d_version matters
      # here as much as anywhere. name_fu is passed only when a timepoint is
      # mapped: without one the function reports the sample as a whole.
      args = c(.base(rv),
               if ("fu" %in% names(rv$processed_data))
                 list(name_fu = "fu", levels_fu = .fu_lv(rv))))),

  a("141fig", "profile", "Informativity",
    "Figure 1.4.1 — Health State Density Index (HSDI)",
    "Cumulative distribution of the observed health profiles.",
    "plot", "eq5d_profile_density_curve", "HSDI (fig. 1.4.1)",
    prep = function(input, rv) list(df = rv$processed_data, args = .base(rv))),

  # ── EQ-5D values ──────────────────────────────────────────────
  # These analyse an existing column of EQ-5D values rather than calculating
  # one, so they need a value column and no value set. apply_mapping() renames
  # whichever column the user mapped, or the Calculate EQ-5D values page
  # produced, to "utility".
  a("31", "values", "Summary",
    "Table 3.1 \u2014 EQ-5D value summary statistics",
    "Descriptive statistics for the EQ-5D values, by timepoint.",
    "table", "eq5d_utility_summary", "Utility summary stats (3.1)",
    needs = "utility", opts = "utility_col",
    prep = function(input, rv) {
      fu <- ensure_fu(rv$processed_data, rv$mapping)
      list(df = fu$df,
           prep = if (identical(fu$name_fu, "fu")) NULL else "fu_all",
           args = list(name_utility = input$utility_col, name_fu = fu$name_fu,
                       levels_fu = if (identical(fu$name_fu, "fu")) .fu_lv(rv)),
           # name_fu is synthetic when no timepoint is mapped, so it is left
           # out of the code shown to the user, as it always has been.
           call_args = list(name_utility = input$utility_col))
    }),

  a("fig34", "values", "Summary",
    "Figure 3.4 \u2014 EQ-5D value distribution",
    "Bar chart of the distribution of EQ-5D values.",
    "plot", "eq5d_utility_distribution_plot", "Utility bar chart (fig. 3.4)",
    needs = "utility", opts = "utility_col",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(name_utility = input$utility_col))),

  a("32", "values", "By group",
    "Table 3.2 \u2014 Mean EQ-5D value by group",
    "Mean EQ-5D value in each group.",
    "table", "eq5d_utility_summary_by_group", "Utility by group (3.2)",
    needs = c("utility", "groupvar"), opts = "utility_col",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(name_utility = input$utility_col, name_groupvar = "groupvar"))),

  a("fig32", "values", "By group",
    "Figure 3.2 \u2014 Mean EQ-5D value and 95% CI by group",
    "Mean EQ-5D value with 95% confidence intervals, by group.",
    "plot", "eq5d_utility_by_group_plot", "Utility CI bar chart (fig. 3.2)",
    needs = c("utility", "groupvar"), opts = "utility_col",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(name_utility = input$utility_col, name_groupvar = "groupvar"))),

  a("fig31", "values", "Over time",
    "Figure 3.1 \u2014 EQ-5D values by timepoint",
    "Box plots of the EQ-5D values at each timepoint.",
    "plot", "eq5d_utility_over_time_plot", "Utility box plots (fig. 3.1)",
    needs = c("utility", "fu"), opts = "utility_col",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(name_utility = input$utility_col, name_fu = "fu",
                  levels_fu = .fu_lv(rv)))),

  a("fig33", "values", "Over time",
    "Figure 3.3 \u2014 EQ-5D values by timepoint and group",
    "Mean EQ-5D value with 95% CI by timepoint, coloured by group.",
    "plot", "eq5d_utility_change_by_group_plot",
    "Utility by timepoint & group (fig. 3.3)",
    needs = c("utility", "fu", "groupvar"), opts = "utility_col",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(name_utility = input$utility_col, name_fu = "fu",
                  levels_fu = .fu_lv(rv), name_groupvar = "groupvar"))),

  a("fig35", "values", "Against the EQ VAS",
    "Figure 3.5 \u2014 EQ-5D values vs EQ VAS",
    "Scatter plot of EQ-5D values against the EQ VAS score.",
    "plot", "eq5d_utility_vas_scatter_plot", "Utility vs VAS (fig. 3.5)",
    needs = c("utility", "vas"), opts = "utility_col",
    prep = function(input, rv) list(
      df = rv$processed_data,
      args = list(name_utility = input$utility_col, name_vas = "vas"))),

  # ── EQ VAS ────────────────────────────────────────────────────────────────
  a("21", "vas", "Summary",
    "Table 2.1 — EQ VAS summary statistics by timepoint",
    "Descriptive statistics for the EQ VAS score, grouped by timepoint.",
    "table", "eq5d_vas_summary", "VAS summary stats (2.1)",
    needs = "vas", opts = "group_filter",
    prep = function(input, rv) {
      df <- rv$processed_data
      sel <- group_filter_label(input$group_filter)
      # The restriction is part of the result's record, so the generated
      # script analyses the same rows. It is applied by evaluating the very
      # code the script writes out.
      filter <- if (!is.null(sel) && "groupvar" %in% names(df))
        list(column = "groupvar", value = sel)
      df <- eq5dsuite:::.apply_filter(df, filter)
      fu <- ensure_fu(df, rv$mapping)
      list(df = fu$df, filter = filter,
           prep = if (identical(fu$name_fu, "fu")) NULL else "fu_all",
           args = list(name_vas = "vas", name_fu = fu$name_fu,
                       levels_fu = if (identical(fu$name_fu, "fu")) .fu_lv(rv)),
           call_args = list(name_vas = "vas"))
    }),

  a("22", "vas", "Summary",
    "Table 2.2 — EQ VAS frequency of mid-points",
    "Frequency distribution of EQ VAS scores by mid-point intervals.",
    "table", "eq5d_vas_distribution_table", "VAS mid-points (2.2)",
    needs = "vas",
    prep = function(input, rv) list(df = rv$processed_data,
                                    args = list(name_vas = "vas"))),

  a("fig21", "vas", "Distribution",
    "Figure 2.1 — EQ VAS distribution (histogram)",
    "Histogram of the EQ VAS distribution.",
    "plot", "eq5d_vas_histogram", "VAS histogram (fig. 2.1)",
    needs = "vas",
    prep = function(input, rv) list(df = rv$processed_data,
                                    args = list(name_vas = "vas"))),

  a("fig22", "vas", "Distribution",
    "Figure 2.2 — EQ VAS distribution by mid-points (box plot)",
    "Box plots of the EQ VAS distribution by mid-point interval.",
    "plot", "eq5d_vas_grouped_distribution_plot", "VAS box plot (fig. 2.2)",
    needs = "vas",
    prep = function(input, rv) list(df = rv$processed_data,
                                    args = list(name_vas = "vas")))
)
rm(a)

analysis_spec <- function(id) {
  for (s in ANALYSES) if (identical(s$id, id)) return(s)
  NULL
}

# Choices for the Output select, as an optgroup list.
analysis_choices <- function(component) {
  specs <- Filter(function(s) identical(s$component, component), ANALYSES)
  out <- list()
  for (g in unique(vapply(specs, `[[`, character(1L), "group"))) {
    inner <- Filter(function(s) identical(s$group, g), specs)
    out[[g]] <- stats::setNames(
      vapply(inner, `[[`, character(1L), "id"),
      vapply(inner, `[[`, character(1L), "label"))
  }
  out
}

# What a spec still needs, in words. Empty means it can run.
unmet_needs <- function(spec, rv) {
  m <- rv$mapping
  if (is.null(m)) return("data")
  lab <- c(fu = "a Timepoint column", id = "a Patient ID column",
           groupvar = "a Group column", vas = "an EQ VAS column",
           utility = "an EQ-5D value column")
  out <- character(0L)
  for (n in spec$needs) {
    if (identical(n, "utility")) {
      # Any of the value columns will do: one mapped on the Data page, or any
      # calculated on the Calculate EQ-5D values page.
      if (!length(value_columns(rv))) out <- c(out, lab[["utility"]])
    } else if (is.null(m[[paste0("name_", n)]]) ||
               !nzchar(m[[paste0("name_", n)]])) {
      out <- c(out, lab[[n]])
    }
  }
  out
}

# ── Catalogue of available analyses ───────────────────────────────────────────
#
# A plain-language guide to the registry above, keyed by its ids: a title
# with every abbreviation spelt out, and what each analysis produces and when
# it is useful. Which analyses are offered is decided from the registry's
# `needs` and from the data themselves; see analysis_prerequisites().
CATALOGUE <- list(
  "111" = list(
    title = "Level frequencies by dimension",
    about = "How many respondents reported each level (no problems up to extreme problems) on each of the five dimensions. The usual first table for describing a sample's EQ-5D profiles."),
  "112" = list(
    title = "Level frequencies by group",
    about = "The same level frequencies, side by side for each group. Useful for comparing the health profiles of, say, two treatment arms or patient groups."),
  "113" = list(
    title = "Most common health states",
    about = "The EQ-5D health states (five-digit profiles) reported most often, with their frequencies. Shows how concentrated a sample is in a few states."),
  "121" = list(
    title = "Level frequencies at each timepoint",
    about = "Level frequencies on each dimension at every timepoint, and how the number reporting any problems changes between timepoints. A first look at change over time, at the level of the sample."),
  "122" = list(
    title = "Paretian Classification of Health Change (PCHC)",
    about = "Classifies each respondent's change between two timepoints as better, worse, mixed (better on some dimensions, worse on others) or no change, without weighting the dimensions. Use it to describe individual change in health profiles."),
  "123" = list(
    title = "Paretian Classification of Health Change (PCHC), separating those with no problems",
    about = "The PCHC with respondents who reported no problems at both timepoints shown on their own, since they cannot improve. Useful where many report full health."),
  "124" = list(
    title = "Level changes in each dimension",
    about = "For each dimension, the share of respondents moving between each pair of levels between two timepoints, and whether that is better, worse or no change. Shows which transitions drive overall change."),
  "121fig" = list(
    title = "Paretian Classification of Health Change (PCHC) by group, chart",
    about = "A bar chart of the PCHC categories (better, worse, mixed, no change) for each group. For comparing patterns of change between groups at a glance."),
  "122fig" = list(
    title = "Dimensions that improved, by group",
    about = "Among respondents who got better, the share who improved on each dimension, for each group. Shows where improvement happened."),
  "123fig" = list(
    title = "Dimensions that worsened, by group",
    about = "Among respondents who got worse, the share who worsened on each dimension, for each group. Shows where deterioration happened."),
  "124fig" = list(
    title = "Dimensions with mixed change, by group",
    about = "Among respondents with a mixed change, the share who improved and who worsened on each dimension, for each group."),
  "125fig" = list(
    title = "Health Profile Grid (HPG)",
    about = "Plots each respondent's health state at one timepoint against the other, with every state of the instrument ranked from best to worst by a value set. Points above or below the diagonal show improvement or deterioration, and how large it is."),
  "131" = list(
    title = "EQ-5D values by Level Sum Score (LSS)",
    about = "Summary statistics of the EQ-5D values for each Level Sum Score -- the sum of the five dimension levels, a simple measure of severity. Shows how values relate to the overall amount of problems reported."),
  "131fig" = list(
    title = "EQ-5D values against the Level Sum Score (LSS), chart",
    about = "EQ-5D values plotted against the Level Sum Score. Shows the spread of values at each severity level."),
  "132" = list(
    title = "Distribution by Level Frequency Score (LFS)",
    about = "How respondents are distributed across Level Frequency Scores -- how many dimensions are at each level, regardless of which dimensions. A severity summary that does not need a value set."),
  "134" = list(
    title = "EQ-5D values by Level Frequency Score (LFS)",
    about = "Summary statistics of the EQ-5D values for each Level Frequency Score. Shows how values vary among profiles with the same mix of levels."),
  "132fig" = list(
    title = "EQ-5D values against the Level Frequency Score (LFS), chart",
    about = "EQ-5D values plotted against the Level Frequency Score."),
  "141" = list(
    title = "Shannon's informativity indices",
    about = "Shannon's index H', its maximum and the evenness index J' for each dimension and for the whole health state: how much of the classification system the sample uses. Useful for comparing how informative samples or instruments are."),
  "141fig" = list(
    title = "Health State Density Curve and Index (HSDC, HSDI)",
    about = "The cumulative share of observations against the cumulative share of observed health states, and the index summarising it: 1 when observations are spread evenly over the states that occur, lower when they are concentrated in a few."),
  "31" = list(
    title = "EQ-5D value summary statistics",
    about = "Mean, standard deviation, median, range and other statistics of the EQ-5D values, at each timepoint when one is selected. The standard summary of EQ-5D values."),
  "fig34" = list(
    title = "Distribution of EQ-5D values, chart",
    about = "A bar chart of the EQ-5D values. Shows gaps, spikes and clusters that summary statistics hide."),
  "32" = list(
    title = "Mean EQ-5D value by group",
    about = "The mean EQ-5D value and its spread in each group. For comparing groups on a single number."),
  "fig32" = list(
    title = "Mean EQ-5D value by group, with 95% confidence intervals (CI)",
    about = "The mean EQ-5D value in each group with its 95% confidence interval, as a chart."),
  "fig31" = list(
    title = "EQ-5D values at each timepoint, box plots",
    about = "Box plots of the EQ-5D values at each timepoint. Shows how the whole distribution moves over time."),
  "fig33" = list(
    title = "EQ-5D values over time by group, with 95% confidence intervals (CI)",
    about = "The mean EQ-5D value at each timepoint, one line per group, with 95% confidence intervals. For comparing how groups change over time."),
  "fig35" = list(
    title = "EQ-5D values against the EQ visual analogue scale (EQ VAS)",
    about = "A scatter plot of each respondent's EQ-5D value against their EQ VAS score -- the value from a value set against their own 0-100 rating of their health."),
  "21" = list(
    title = "EQ visual analogue scale (EQ VAS) summary statistics",
    about = "Mean, standard deviation, median and other statistics of the EQ VAS score (0-100), at each timepoint, optionally for one group."),
  "22" = list(
    title = "EQ visual analogue scale (EQ VAS) frequencies by interval",
    about = "How many respondents gave EQ VAS scores in each interval of the 0-100 scale. Shows heaping at round numbers."),
  "fig21" = list(
    title = "EQ visual analogue scale (EQ VAS) distribution, histogram",
    about = "A histogram of the EQ VAS scores."),
  "fig22" = list(
    title = "EQ visual analogue scale (EQ VAS) by interval, box plots",
    about = "Box plots of the EQ VAS scores within each interval of the scale.")
)

# What the data hold, for deciding which analyses can run. Computed once
# from the confirmed data, not per analysis.
# Which rows hold a complete, valid EQ-5D profile.
valid_profiles <- function(df, rv) {
  ver <- rv$mapping$eq5d_version %||% "3L"
  max_level <- eq5dsuite:::.max_level(ver)
  dims <- intersect(DIMS_STD, names(df))
  if (length(dims) == 5L)
    Reduce(`&`, lapply(dims, function(d)
      eq5dsuite:::.dim_status(df[[d]], max_level = max_level) == "ok"))
  else rep(FALSE, nrow(df))
}

analysis_data_facts <- function(rv) {
  df <- rv$processed_data
  if (is.null(df)) return(NULL)
  ver <- rv$mapping$eq5d_version %||% "3L"
  dims <- intersect(DIMS_STD, names(df))
  profile_ok <- valid_profiles(df, rv)
  has <- function(col) col %in% names(df)
  fu <- if (has("fu")) as.character(df$fu) else rep(NA_character_, nrow(df))
  # Only the timepoints selected on the Data page count: the analyses factor
  # the timepoint by them and leave every other row out. Counting all of
  # them offered paired analyses the selection could not support (review
  # Q09).
  lv <- rv$mapping$levels_fu
  if (length(lv)) fu[!fu %in% as.character(lv)] <- NA_character_
  grp <- if (has("groupvar")) as.character(df$groupvar) else rep(NA_character_, nrow(df))
  vas <- if (has("vas")) suppressWarnings(as.numeric(df$vas)) else rep(NA_real_, nrow(df))
  vas_ok <- !is.na(vas) & vas >= 0 & vas <= 100
  vcols <- value_columns(rv)
  util_ok <- if (length(vcols))
    Reduce(`|`, lapply(vcols, function(c) is.finite(suppressWarnings(as.numeric(df[[c]])))))
    else rep(FALSE, nrow(df))
  paired <- FALSE
  pchc_states <- character(0L)
  if (has("fu") && has("id")) {
    keep <- profile_ok & !is.na(fu) & !is.na(df$id)
    tp_per_id <- tapply(fu[keep], as.character(df$id[keep]),
                        function(x) length(unique(x)))
    paired <- any(tp_per_id >= 2L, na.rm = TRUE)
    # The change categories present, classified as the analyses classify
    # them: .pchc() on the selected timepoints, in order, each respondent
    # with themselves. The figures of improvement, worsening and mixed
    # change each need someone in their category.
    if (paired) {
      lvs <- if (length(lv)) as.character(lv) else unique(fu[keep])
      d <- data.frame(id = df$id[keep], fu = factor(fu[keep], levels = lvs),
                      df[keep, dims, drop = FALSE])
      d <- d[order(d$id, d$fu), , drop = FALSE]
      st <- eq5dsuite:::.pchc(d, level_fu_1 = lvs[1L])$state
      pchc_states <- unique(st[!is.na(st)])
    }
  }
  # Whether the paired analyses can pair the records: one per respondent and
  # selected timepoint, and, for those split by group, one group per
  # respondent. The package's own check decides, so the app and the analysis
  # cannot disagree (review A03, A04).
  pair_ok <- function(by_group) {
    if (!has("fu") || !has("id")) return(TRUE)
    d <- data.frame(id = df$id, fu = fu, stringsAsFactors = FALSE)
    if (by_group) d$groupvar <- grp
    isTRUE(tryCatch(eq5dsuite:::.check_pairs(d, by_group = by_group),
                    error = function(e) FALSE))
  }
  one_per_visit <- pair_ok(FALSE)
  list(
    one_per_visit = one_per_visit,
    one_group    = !has("groupvar") || !one_per_visit || pair_ok(TRUE),
    profiles     = any(profile_ok),
    timepoints   = length(unique(fu[profile_ok & !is.na(fu)])),
    paired       = paired,
    pchc_states  = pchc_states,
    groups       = any(profile_ok & !is.na(grp)),
    vas          = any(vas_ok),
    utility      = any(util_ok),
    utility_fu   = any(util_ok & !is.na(fu)),
    utility_grp  = any(util_ok & !is.na(grp)),
    utility_vas  = any(util_ok & vas_ok),
    value_sets   = length(get_country_choices(ver)) > 0L)
}

# Why an analysis cannot run on these data, in words; empty when it can.
# Starts from the registry's `needs` (unmet_needs()), then checks that the
# data in those columns can support it -- a Timepoint column with one
# timepoint does not make a change analysis possible.
analysis_prerequisites <- function(spec, rv, facts = analysis_data_facts(rv)) {
  if (is.null(facts)) return("validated data")
  out <- unmet_needs(spec, rv)
  if (length(out)) return(out)
  n <- spec$needs
  if (identical(spec$component, "profile") && !facts$profiles)
    out <- c(out, "at least one complete, valid EQ-5D profile")
  if (identical(spec$group, "Longitudinal") && facts$timepoints < 2L)
    out <- c(out, "at least two timepoints")
  if (all(c("fu", "id") %in% n) && !facts$paired)
    out <- c(out, "a respondent with valid profiles at two timepoints")
  if (all(c("fu", "id") %in% n) && !isTRUE(facts$one_per_visit))
    out <- c(out, paste0("one record per respondent at each timepoint (some ",
                         "respondents have more than one)"))
  else if (spec$id %in% PAIRED_BY_GROUP && !isTRUE(facts$one_group))
    out <- c(out, "each respondent in the same group at every timepoint")
  if ("groupvar" %in% n && !"utility" %in% n && !facts$groups)
    out <- c(out, "a group recorded for some respondents")
  if ("vas" %in% n && !"utility" %in% n && !facts$vas)
    out <- c(out, "EQ VAS scores between 0 and 100")
  if ("utility" %in% n) {
    if (!facts$utility) out <- c(out, "EQ-5D values in a value column")
    else if ("vas" %in% n && !facts$utility_vas)
      out <- c(out, "EQ-5D values and EQ VAS scores for the same respondents")
    else if ("groupvar" %in% n && !facts$utility_grp)
      out <- c(out, "EQ-5D values for respondents with a group")
    else if ("fu" %in% n && !facts$utility_fu)
      out <- c(out, "EQ-5D values at a timepoint")
  }
  if ("country" %in% spec$opts && !facts$value_sets)
    out <- c(out, "a value set for this instrument")
  if (!length(out) && spec$id %in% names(PCHC_CATEGORY_NEEDED)) {
    need <- PCHC_CATEGORY_NEEDED[[spec$id]]
    if (!need[1L] %in% facts$pchc_states) out <- c(out, need[2L])
  }
  out
}

# The paired analyses that split respondents by group (they take
# .pchc_args()), and so need each respondent to stay in one group.
PAIRED_BY_GROUP <- c("122", "123", "121fig", "122fig", "123fig", "124fig")

# The figures that plot one change category, and what they need.
PCHC_CATEGORY_NEEDED <- list(
  "122fig" = c("Improve",      "a respondent who improved"),
  "123fig" = c("Worsen",       "a respondent who got worse"),
  "124fig" = c("Mixed change", "a respondent with a mixed change"))

# Why the options chosen for an analysis leave it nothing to run on; empty
# when they do. analysis_prerequisites() asks whether some choice of options
# can work -- what the catalogue offers; this asks whether the current one
# does, for the Run button. With pairs only at pre/post, the Health Profile
# Grid is offered, but pre/mid cannot run (verification V01). It looks at
# the same rows the analysis's prep would.
analysis_option_unmet <- function(spec, input, rv) {
  df <- rv$processed_data
  if (is.null(df)) return(character(0L))
  out <- character(0L)
  if ("two_fu" %in% spec$opts && all(c("fu", "id") %in% names(df))) {
    sel <- as.character(input$fu_levels %||% character(0L))
    # Not exactly two chosen: the Run handler says so itself.
    if (length(sel) == 2L) {
      fu <- as.character(df$fu)
      keep <- valid_profiles(df, rv) & fu %in% sel & !is.na(df$id)
      both <- tapply(fu[keep], as.character(df$id[keep]),
                     function(x) length(unique(x)) == 2L)
      if (!any(both, na.rm = TRUE))
        out <- c(out, sprintf(
          "a respondent with valid profiles at both \"%s\" and \"%s\"",
          sel[1L], sel[2L]))
    }
  }
  if ("group_filter" %in% spec$opts && "vas" %in% spec$needs &&
      "vas" %in% names(df)) {
    sel <- group_filter_label(input$group_filter)
    if (!is.null(sel) && "groupvar" %in% names(df)) {
      # The rows the analysis keeps: the prep's own filter, then the
      # selected timepoints.
      g <- eq5dsuite:::.apply_filter(df, list(column = "groupvar", value = sel))
      lv <- rv$mapping$levels_fu
      if (length(lv) && "fu" %in% names(g))
        g <- g[as.character(g$fu) %in% as.character(lv), , drop = FALSE]
      vas <- suppressWarnings(as.numeric(g$vas))
      if (!any(!is.na(vas) & vas >= 0 & vas <= 100))
        out <- c(out, paste0("EQ VAS scores between 0 and 100 in the group \"",
                             sel, "\""))
    }
  }
  out
}

# The analyses that can run on the confirmed data, in registry order.
available_analyses <- function(rv) {
  facts <- analysis_data_facts(rv)
  if (is.null(facts)) return(list())
  Filter(function(s) !length(analysis_prerequisites(s, rv, facts)), ANALYSES)
}

# The modal itself is catalogue_modal() in mod_catalogue.R.

# ── UI ────────────────────────────────────────────────────────────────────────

mod_analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  page_shell(
    sidebar_title = "Analysis",
    sidebar = shiny::tagList(
      shiny::actionButton(ns("catalogue"),
                          "See catalogue of available analyses",
                          class = "btn-outline-primary w-100 mb-3",
                          icon = shiny::icon("list")),
      shiny::selectInput(ns("component"), "Component",
                         choices = COMPONENTS, selected = "profile"),
      shiny::selectInput(ns("output"), "Output",
                         choices = analysis_choices("profile")),
      shiny::uiOutput(ns("desc")),
      shiny::uiOutput(ns("opts")),
      shiny::uiOutput(ns("run_ui"))
    ),
    shiny::uiOutput(ns("guard")),
    shiny::uiOutput(ns("result"))
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_analysis_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$guard <- shiny::renderUI(analysis_guard(rv, ns))
    analysis_guard_server(input, session)

    # An analysis chosen from the catalogue, waiting for its component's
    # outputs to be offered.
    pending_output <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$component, {
      want <- pending_output()
      pending_output(NULL)
      ch <- analysis_choices(input$component)
      shiny::updateSelectInput(
        session, "output", choices = ch,
        selected = if (!is.null(want) && want %in% unlist(ch)) want)
    })

    # ── Catalogue ────────────────────────────────────────────────────────────
    shiny::observeEvent(input$catalogue, {
      shiny::showModal(catalogue_modal(ns, available_analyses(rv)))
    })

    # Select the analysis in the sidebar; do not run it.
    shiny::observeEvent(input$catalogue_pick, {
      s <- analysis_spec(input$catalogue_pick)
      shiny::req(s)
      shiny::removeModal()
      if (identical(input$component, s$component)) {
        shiny::updateSelectInput(session, "output", selected = s$id)
      } else {
        pending_output(s$id)
        shiny::updateSelectInput(session, "component", selected = s$component)
      }
    })

    spec <- shiny::reactive({
      shiny::req(input$output)
      analysis_spec(input$output)
    })

    missing <- shiny::reactive({
      s <- spec(); shiny::req(s)
      unmet_needs(s, rv)
    })

    output$desc <- shiny::renderUI({
      s <- spec(); shiny::req(s)
      hint(s$desc)
    })

    # The selected timepoints present in the data, in the selected order --
    # the ones the analyses use. Every value, sorted, used to be offered.
    timepoints <- shiny::reactive({
      df <- rv$processed_data
      if (is.null(df) || !"fu" %in% names(df)) return(character(0L))
      present <- unique(as.character(df[["fu"]]))
      present <- present[!is.na(present)]
      lv <- as.character(rv$mapping$levels_fu)
      if (length(lv)) lv[lv %in% present] else sort(present)
    })

    # What the data themselves still lack for this analysis, beyond the
    # variables it needs: the same checks as the catalogue.
    data_unmet <- shiny::reactive({
      s <- spec(); shiny::req(s)
      if (length(missing())) return(character(0L))
      analysis_prerequisites(s, rv)
    })

    # And what the options chosen leave it without (verification V01).
    option_unmet <- shiny::reactive({
      s <- spec(); shiny::req(s)
      if (length(missing()) || length(data_unmet())) return(character(0L))
      analysis_option_unmet(s, input, rv)
    })

    groups <- shiny::reactive({
      df <- rv$processed_data
      if (is.null(df) || !"groupvar" %in% names(df)) return(character(0L))
      sort(unique(as.character(df[["groupvar"]])))
    })

    # ── The controls this output needs ───────────────────────────────────────
    output$opts <- shiny::renderUI({
      s <- spec(); shiny::req(s)
      if (length(missing()) > 0L) return(NULL)
      bits <- list()

      if ("utility_col" %in% s$opts) {
        cols <- value_columns(rv)
        bits <- c(bits, list(shiny::selectInput(
          ns("utility_col"), "Utility column",
          choices = cols,
          selected = shiny::isolate(input$utility_col) %||% cols[1L])))
      }
      if ("country" %in% s$opts) {
        ch <- get_country_choices(.ver(rv))
        bits <- c(bits, list(shiny::selectizeInput(
          ns("country"), paste0("EQ-5D-", .ver(rv), " value set"),
          choices = c("(select)" = "", ch),
          selected = shiny::isolate(input$country) %||% "")))
      }
      if ("topn" %in% s$opts) {
        bits <- c(bits, list(shiny::numericInput(
          ns("topn"), "How many states", value = 10L,
          min = 1L, max = 50L, step = 1L)))
      }
      if ("two_fu" %in% s$opts) {
        tp <- timepoints()
        bits <- c(bits, list(
          if (length(tp) < 2L)
            note("warning", "At least two timepoints are needed.")
          else shiny::selectInput(
            ns("fu_levels"), "Compare which two timepoints",
            choices = tp, selected = tp[1:2], multiple = TRUE)))
      }
      if ("group_filter" %in% s$opts) {
        g <- groups()
        if (length(g) > 0L) {
          bits <- c(bits, list(shiny::selectInput(
            ns("group_filter"), "Restrict to group",
            choices = c("All groups" = GROUP_FILTER_ALL,
                        stats::setNames(group_filter_value(g), g)),
            selected = GROUP_FILTER_ALL)))
        }
      }
      if (length(bits) == 0L) return(NULL)
      shiny::tagList(bits)
    })

    output$run_ui <- shiny::renderUI({
      shiny::req(spec())
      miss <- missing()
      if (identical(miss, "data")) return(NULL)
      if (length(miss) > 0L) {
        # A value column is the one missing piece the user cannot supply on
        # the Data page unless their file already has one, so send them to the
        # page that produces it.
        wants_value <- "an EQ-5D value column" %in% miss
        # One string, not several children: Shiny puts whitespace between a
        # tag's children, which would show as "value column ." here.
        return(shiny::tagList(
          note("warning", paste0(
            "This analysis needs ", paste(miss, collapse = " and "),
            if (wants_value)
              ". Calculate one from the EQ-5D dimensions, or select a column you
               already have on the Data page."
            else ". Select it on the Data page.")),
          if (wants_value)
            shiny::actionButton(ns("goto_values"), "Calculate EQ-5D values",
                                class = "btn-primary w-100",
                                icon = shiny::icon("calculator"))
        ))
      }
      if (length(data_unmet()))
        return(note("warning", paste0(
          "The confirmed data cannot support this analysis: it needs ",
          paste(data_unmet(), collapse = " and "), ".")))
      if (length(option_unmet()))
        return(note("warning", paste0(
          "These options leave nothing to analyse: this needs ",
          paste(option_unmet(), collapse = " and "),
          ". Choose others above.")))
      shiny::actionButton(ns("run"), "Run", class = "btn-primary w-100",
                          icon = shiny::icon("play"))
    })

    shiny::observeEvent(input$goto_values, goto_page(session, "values"))

    # ── Run ──────────────────────────────────────────────────────────────────
    res <- shiny::reactiveValues(data = NULL, plot = NULL, call = NULL,
                                 spec = NULL)

    shiny::observeEvent(input$run, {
      s <- spec()
      shiny::req(s, rv$processed_data, length(missing()) == 0L,
                 length(data_unmet()) == 0L, length(option_unmet()) == 0L)

      if ("utility_col" %in% s$opts &&
          !isTRUE(input$utility_col %in% value_columns(rv))) {
        shiny::showNotification("Choose a utility column first.",
                                type = "warning", duration = 4)
        return()
      }
      if ("country" %in% s$opts &&
          !isTRUE(input$country %in% get_country_choices(.ver(rv)))) {
        shiny::showNotification("Choose a value set first.",
                                type = "warning", duration = 4)
        return()
      }
      if ("two_fu" %in% s$opts && length(input$fu_levels %||% character(0L)) != 2L) {
        shiny::showNotification("Select exactly two timepoints.",
                                type = "warning", duration = 5)
        return()
      }

      p  <- s$prep(input, rv)
      # The code shown to the user, and the record the script is built from,
      # are the same arguments deparsed the same way -- and the same
      # restriction of the rows, where there is one.
      cs <- format_call(s$fn, c(list(df = quote(analysis_data)), p$args),
                        filter = p$filter)
      rec <- list(fn = s$fn, args = p$args, prep = p$prep, type = s$type,
                  filter = p$filter)

      tryCatch({
        out <- run_quietly(do.call(pkg_fn(s$fn), c(list(df = p$df), p$args)))
        if (identical(s$type, "plot")) {
          res$plot <- out$p
          res$data <- NULL
          save_result(rv, s$save_label, cs, "plot", plot = out$p, call = rec)
        } else {
          res$data <- out
          res$plot <- NULL
          save_result(rv, s$save_label, cs, "table", data = out, call = rec)
        }
        res$call <- cs
        res$spec <- s
      }, error = function(e) err_notify(e))
    })

    # Clear the shown result when the user picks a different output.
    shiny::observeEvent(input$output, {
      res$data <- NULL; res$plot <- NULL; res$call <- NULL; res$spec <- NULL
    })

    # And when the data it came from change: a confirmed dataset or variable
    # selection, completed validation, or an overwritten value column. The
    # saved results were cleared on these, but the one on screen stayed,
    # showing the old data's numbers (review Q12).
    shiny::observeEvent(rv$revision, {
      res$data <- NULL; res$plot <- NULL; res$call <- NULL; res$spec <- NULL
    }, ignoreInit = TRUE)

    # ── The one result card ──────────────────────────────────────────────────
    output$result <- shiny::renderUI({
      if (is.null(rv$processed_data)) return(NULL)
      s <- res$spec
      if (is.null(s)) {
        cur <- spec()
        miss <- missing()
        return(bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, hint(
          if ("an EQ-5D value column" %in% miss)
            "This analysis needs a column of EQ-5D values. Calculate one on the
             Calculate EQ-5D values page, or select a column you already have on
             the Data page."
          else if (length(miss) > 0L)
            "Select the variables this analysis needs on the Data page, then come back."
          else paste0("Press Run to produce ", cur$label, ".")))))
      }
      bslib::card(
        full_screen = TRUE, fill = FALSE,
        bslib::card_header(s$label),
        bslib::card_body(fillable = FALSE,
          if (identical(s$type, "plot")) plot_frame(ns("plot"))
          else analysis_table_output(ns("table")),
          call_display(ns("call"))
        )
      )
    })

    output$table <- render_analysis_table(shiny::reactive(res$data))
    output$plot  <- shiny::renderPlot(res$plot)
    output$call  <- shiny::renderText(res$call)
  })
}
