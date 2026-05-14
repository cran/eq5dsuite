# eq5dsuite <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/eq5dsuite)](https://CRAN.R-project.org/package=eq5dsuite)
[![R-CMD-check](https://github.com/MathsInHealth/eq5dsuite-r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MathsInHealth/eq5dsuite-r/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

R implementation of [eq5dsuite](https://github.com/MathsInHealth/eq5dsuite) 
— a standardised suite of tools for EQ-5D analysis across R, Stata, and 
Excel. This package provides a comprehensive set of functions for 
calculating EQ-5D values and analysing EQ-5D data following the 
recommendations of Devlin et al. (2020).

## Features

- EQ-5D value calculation for the **EQ-5D-3L**, **EQ-5D-5L**, and 
  **EQ-5D-Y-3L** instruments
- **39+ published national value sets** for the EQ-5D-3L, plus a 
  growing library of EQ-5D-5L and EQ-5D-Y-3L sets
- Three **crosswalk methods**: original (Van Hout et al., 2012), 
  reverse (Van Hout & Shaw, 2021), and UK-specific NICE-recommended 
  mapping (Hernández Alava et al., 2023)
- Support for **user-defined custom value sets**
- **Automatic value set updates** via `update_value_sets()` — install 
  newly published value sets without waiting for a CRAN update
- **35+ analysis functions** for profile analysis, EQ-5D value 
  analysis, and EQ-VAS analysis
- An interactive **Shiny application** for point-and-click access 
  to the same analytical workflow
- Integration with `dplyr` and `ggplot2` for reproducible, 
  script-based analyses
- Publication-ready outputs in both PDF and HTML formats

## Installation

Install the released version from CRAN:

```r
install.packages("eq5dsuite")
```

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("MathsInHealth/eq5dsuite-r")
```

## Quick start

```r
library(eq5dsuite)

# Calculate EQ-5D-3L values using the UK value set
eq5d3l(
  data.frame(mo = 1, sc = 2, ua = 3, pd = 2, ad = 1),
  country   = "UK",
  dim.names = c("mo", "sc", "ua", "pd", "ad")
)

# View available value sets
eqvs_display(version = "3L")

# Check for newly published value sets
update_value_sets()

# Launch the Shiny application
run_app()
```

## Documentation

- Full function reference: `?eq5dsuite`
- Vignettes:

```r
browseVignettes("eq5dsuite")
```

Available vignettes:
- *Getting started* — installation, value calculation, value set management
- *Analysing EQ-5D data* — complete workflow using NHS PROMs data
- *Crosswalk methods* — when and how to use each crosswalk method
- *Custom value sets* — adding, saving, and managing custom value sets
- *Keeping value sets up to date* — using the online update system

## Value sets repository

Country-specific value sets are maintained at:

**[eq5dsuite-value-sets](https://github.com/MathsInHealth/eq5dsuite-value-sets)**

New value sets are added as they are published in the literature.
Use `update_value_sets()` to install them without waiting for a 
CRAN package update.

## Cross-platform suite

eq5dsuite is available across three platforms:

| Platform | Repository | Status |
|---|---|---|
| R | [eq5dsuite-r](https://github.com/MathsInHealth/eq5dsuite-r) | Available on CRAN |
| Stata | [eq5dsuite-stata](https://github.com/MathsInHealth/eq5dsuite-stata) | Available |
| Excel | [eq5dsuite-excel](https://github.com/MathsInHealth/eq5dsuite-excel) | Available |

## Issues and feedback

For R-specific issues please use the 
[issue tracker](https://github.com/MathsInHealth/eq5dsuite-r/issues) 
in this repository.

For cross-platform concerns please use the 
[umbrella issue tracker](https://github.com/MathsInHealth/eq5dsuite/issues).

For general enquiries contact 
[info@mathsinhealth.com](mailto:info@mathsinhealth.com).

## Citation

If you use eq5dsuite in your research, please cite:

> Estévez-Carrillo A, Rivero-Arias O, Schlackow I, Rand K.
> eq5dsuite: An R Package for Describing and Analysing EQ-5D Data.
> *The R Journal* (forthcoming).

## License

This package is licensed under the GPL (>= 2).