# eq5dsuite

<!-- badges: start -->
<!-- badges: end -->

The goal of this package is to provide a suite of functions for manipulating and analysing EQ-5D data. 

The file structure in the `R` folder is as follows:

* init.R: description of the processes taking place every time the package is loaded, e.g. pre-calculation of the crosswalk matrices; based on the analogous file from the `eqxwr` package.

* utils.R; EQ_functions.R; eqxwr.R - files based on the files with the same name as those from the `eqxwr` package but expanded to accept 3L data

* eqxw.R - file containing forward crosswalk functions; adapted from eqxwr.R

* eq5d_devlin.R - file containing code for Tables and Figures from the Devlin book (one file per Table or Figure)

* eq5d_aux.R - secondary files, e.g. wrappers, used in eq5d_devlin.R

In addition, 

* data/example_data.rda is an example dataset that can be used to test eq5d_devlin.R functions

## Installation

You can install the development version of eq5dsuite from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MathsInHealth/eq5dsuite")
```
