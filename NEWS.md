
# eq5dsuite 2.1.0

## Changes affecting existing analyses

- **Changed output:** `"Missing (%)"` in `eq5d_vas_summary()` and `eq5d_utility_summary()` now reports percentages from 0 to 100, rather than proportions from 0 to 1. For example, one missing observation among three is reported as `33.33`, rather than `0.33`. The denominator is the total number of rows at that timepoint; timepoints with no rows return `NA`. The app and exports use the same units. 

- **Changed output for invalid inputs:** fractional, non-finite and out-of-range dimension levels return `NA` with a warning, rather than being truncated or encoded as valid profiles. Fractional levels are not rounded. Valid records and their order are unchanged.

- **Changed output for invalid health-state codes:** scoring functions and `toEQ5Ddims()` require finite, whole-number codes. For example, `11111.9` returns `NA` with a warning, rather than being scored as `11111`.

- **Breaking change:** EQ-5D value analyses use a pre-calculated column specified by `name_utility`, rather than selecting a value set. This also applies to LSS and LFS analyses that use EQ-5D values. The Health Profile Grid retains `eq5d_version` and `country`, because it ranks all possible states using the selected value set.

## Input validation

- `eq5d_validate()` identifies fractional, non-finite and out-of-range dimension levels and counts rejected values as missing.
- `eqxw_UK()` and `eqxwr_UK()` require finite ages. Non-finite ages return `NA` with a warning; no upper age limit has been introduced.
- Ambiguous age-band labels such as `"under 20"`, `"<20"` and `"50+"` return `NA` with a warning requesting an exact age. `"65+"` resolves to 65, giving the same mapped value as its previous interpretation. Closed bands retain their midpoints, and bare numeric labels are interpreted as exact ages.
- Dimension selections must identify distinct columns, including when names differ only in case. The UK mapping functions also accept validated column positions.
- `eq5d_apply_mapping()` applies column assignments together, allowing dimension names to swap safely. Duplicate assignments and conflicts with existing column names are rejected.
- Custom dimension names are supported consistently by the UK mapping functions. An unusable record no longer causes an otherwise valid vector of health states to be interpreted as aggregate values.
- `make_dummies()` supports its documented matrix input.


## Value sets and cache management

- Cached value sets must contain every expected health state exactly once, with no additional states or non-finite values. Accepted tables are placed in canonical state order, so row order does not affect scoring or crosswalk results.
- Value set codes are checked without regard to case, preventing ambiguous duplicates.
- Adding, removing and renaming user-defined value sets validates the operation before changing the registry. Failed saves restore the previous runtime state.
- User-defined renames preserve direct and crosswalk values across saving and reloading. Built-in codes cannot be renamed at runtime; changes to these codes are distributed through package releases.
- Cache files are written to a temporary file, checked for readability and then moved into place. Failed writes leave the previous cache intact.
- An unreadable cache is replaced only after a successful backup. If the backup cannot be created, saving stops and the original file remains unchanged. Existing backups are not overwritten.
- Published renames that conflict with an existing value set are reported in `migration_conflicts`. Both value sets are retained, and the rename remains pending.
- If a rename succeeds but its migration record cannot be saved, the renamed values are retained and the update is reported as incomplete. Unrecorded renames are listed in `migrations_unrecorded`, and the last-check date remains unchanged. A subsequent check can record the completed rename without repeating it.
- `update_value_sets()` distinguishes successful checks from download, validation, migration and installation failures through `checked`, `failed` and `install_failed`.
- Failed or malformed migration downloads and unresolved rename conflicts leave the update incomplete. A successfully downloaded empty migration list is accepted.
- The last-check date advances only after a complete, successful update check.
- Loading the package no longer creates the cache directory. The directory is created when data are first written.
- Redirecting the package cache also redirects its last-check and migration records.

## Analysis and reporting

- Change tables compare consecutive observations from the same respondent, in timepoint order. Records without an ID or outside `levels_fu` are excluded from pairing. Dimension-change and PCHC analyses follow the same pairing rules.
- Level summaries include every level of the selected instrument. An unreported level or problem has a count of zero when a denominator exists; `NA` indicates that no denominator is available. Relative change remains `NA` when the earlier count is zero.
- `eq5d_profile_change_summary()` passes supplied argument values consistently when called from another function. Grouped level summaries retain the appropriate change rows when some categories are absent.
- The Health Profile Grid supports custom dimension names and EQ-5D-Y-3L. It requires exactly two timepoints and explains when no respondent has a valid profile at both. Invalid profiles are excluded with a warning.
- Health Profile Grid axis labels correspond to the timepoints plotted. Points, classification colours and the diagonal are unchanged.
- `make_all_EQ_states()` and `make_all_EQ_indexes()` accept `"Y3L"`, using the same 243 states as EQ-5D-3L.
- **Documentation correction:** the Health State Density Index description follows Zamora et al. (2018). The index is twice the area under the density curve: it equals 1 when observed profiles are equally frequent and decreases as their frequencies become more concentrated. Only observed profiles contribute to the calculation. For $S$ observed profiles and $N$ observations, the attainable range is $1/S + (S - 1)/N$ to 1. **The calculation and existing results are unchanged.** Interpretations based on the previous description should be reviewed.
- Added `eq5d_profile_shannon()` for Shannon’s index and evenness.
- EQ-5D value functions return unnamed numeric vectors in input order.

## Value sets and mappings

- Added the Nigerian EQ-5D-5L value set (`NG`).
- Added `eqxwr_UK()` for mapping EQ-5D-3L responses to the 2026 UK EQ-5D-5L value set.
- UK value set codes use `GB`, with `UK` retained as a deprecated alias.
- Documentation dates and distinguishes NICE’s recommendations for topics started before and after its interim methods statement, *Implementing the EQ-5D-5L value set* (PMG51, 27 August 2026). The reference title and vignette citation details have also been updated. These documentation changes do not alter calculated values.
- Removed interactive value set selection and strengthened custom value set validation.
- `eqvs_display(return_df = TRUE)` returns the requested data frame without printing it.

## Shiny app

- Updated the visual style, with bundled fonts for offline use.
- Combined Results and Export into one page, with result previews, reordering, removal and individual downloads.
- Added an analysis catalogue with descriptions and example outputs, organised by EQ-5D profiles, EQ-5D values and EQ VAS.
- Analysis availability and the Run button reflect the data, selected timepoints and analysis options.
- Added detailed validation messages identifying affected values and records.
- Renamed column-selection controls to “Variables” and added suggested utility-column names based on the method and value set.
- Added the installed version and a notification when a newer version is available on CRAN.
- Results clear when their underlying data change. Generated scripts preserve input handling, group restrictions and result order.
- Value set selections follow the scoring method’s target instrument. Download filenames distinguish repeated analyses, and bulk archives include a results manifest.

## Deployment and maintenance

- The package declares `R (>= 4.1.0)`, matching its dependency requirements.
- Replaced deprecated `.Names` arguments with `names` in `toEQ5Ddims()` and `make_all_EQ_states()`. Returned values and attributes are unchanged.
- Added a GitHub Actions workflow for package checks across Linux, Windows and macOS, covering current, development and previous R releases and R 4.1.
- Added online deployment support, configurable upload limits, session cleanup and a Shiny Server deployment guide at `inst/shiny/DEPLOY.md`.
- Privacy notices explain that uploads and generated reports use temporary server files that are removed when the session ends. The deployment guide distinguishes private temporary directories from memory-backed storage.
- Internal data-reading, validation, age-band and formatting helpers support consistent behaviour between the app and generated scripts.
- Expanded tests and documentation.
- Removed `providercode` from `example_data`.

---

# eq5dsuite 2.0.0 (Breaking change release)

## API rename — all 31 analysis functions have new descriptive names

All `table_X_X_X()` and `figure_X_X_X()` functions have been renamed to
descriptive equivalents following the `eq5d_<domain>_<what>()` convention.
The old numeric names no longer exist; update any existing code using the
table below.

| Old name | New name |
|---|---|
| `table_1_1_1` | `eq5d_profile_level_summary` |
| `table_1_1_2` | `eq5d_profile_level_summary_by_group` |
| `table_1_1_3` | `eq5d_profile_top_states` |
| `table_1_2_1` | `eq5d_profile_change_summary` |
| `table_1_2_2` | `eq5d_profile_pchc_table` |
| `table_1_2_3` | `eq5d_profile_pchc_with_no_problems_table` |
| `table_1_2_4` | `eq5d_profile_dimension_change_table` |
| `figure_1_2_1` | `eq5d_profile_pchc_by_group_plot` |
| `figure_1_2_2` | `eq5d_profile_better_dimensions_by_group_plot` |
| `figure_1_2_3` | `eq5d_profile_worse_dimensions_by_group_plot` |
| `figure_1_2_4` | `eq5d_profile_mixed_dimensions_by_group_plot` |
| `figure_1_2_5` | `eq5d_profile_health_profile_grid` |
| `table_1_3_1` | `eq5d_profile_lss_utility_summary` |
| `table_1_3_2` | `eq5d_profile_lfs_distribution` |
| `table_1_3_3` | `eq5d_profile_lfs_mean_utility` |
| `table_1_3_4` | `eq5d_profile_lfs_utility_summary` |
| `figure_1_3_1` | `eq5d_profile_lss_utility_plot` |
| `figure_1_3_2` | `eq5d_profile_lfs_utility_plot` |
| `figure_1_4_1` | `eq5d_profile_density_curve` |
| `table_2_1` | `eq5d_vas_summary` |
| `table_2_2` | `eq5d_vas_distribution_table` |
| `figure_2_1` | `eq5d_vas_histogram` |
| `figure_2_2` | `eq5d_vas_grouped_distribution_plot` |
| `table_3_1` | `eq5d_utility_summary` |
| `table_3_2` | `eq5d_utility_summary_by_group` |
| `table_3_3` | `eq5d_utility_norms_comparison` |
| `figure_3_1` | `eq5d_utility_over_time_plot` |
| `figure_3_2` | `eq5d_utility_by_group_plot` |
| `figure_3_3` | `eq5d_utility_change_by_group_plot` |
| `figure_3_4` | `eq5d_utility_distribution_plot` |
| `figure_3_5` | `eq5d_utility_vas_scatter_plot` |

## New features

* **`update_value_sets()`** — check for and install new EQ-5D value sets
  from the online repository without requiring a package update. 
  Value sets are hosted at
  <https://github.com/MathsInHealth/eq5dsuite-value-sets>.

* **Automatic value set migration** — when value set codes change (e.g.
  when a country publishes a second value set and the original code is
  disambiguated with a year suffix), `update_value_sets()` automatically
  applies the necessary renames.

* **Package documentation** — `?eq5dsuite` now opens a package-level help
  page listing all exported functions organised by category.

* **Vignettes** — five vignettes are now available via
  `browseVignettes("eq5dsuite")`:
  - *Getting started* — installation, value calculation, value set
    management
  - *Analysing EQ-5D data* — complete analytical workflow using NHS
    PROMs data
  - *Crosswalk methods* — when and how to use each crosswalk method
  - *Custom value sets* — adding, saving, and managing custom value sets
  - *Keeping value sets up to date* — using the online update system

## Bug fixes and improvements

* `make_dummies()` — column matching is now case-insensitive and no longer
  renames columns in place. 

---

# eq5dsuite 1.0.1

* Added a `NEWS.md` file to track changes to the package.
* New EQ-5D value sets available
* New analysis functions  (figure 1_2_5 and figure 1_4_1)
* Updated example data 

# eq5dsuite 1.0.2

* Re-written parts of the code to reduce package dependencies.
* Added a Shiny app for interactive EQ-5D data analysis.
* Improved utility calculation workflows and support for pipeline-based use.

# eq5dsuite 1.0.3

* EQ-5D-3L Netherlands value set renamed: VS_code `NL` → `NL_2006` (Name: `Netherlands_2006`).
* New EQ-5D-3L value set: Netherlands_2026 (VS_code `NL_2026`, doi: 10.1007/s10198-025-01892-2).
* New EQ-5D-5L value set: United Kingdom (VS_code `UK`, doi: 10.1016/j.jval.2026.03.008).