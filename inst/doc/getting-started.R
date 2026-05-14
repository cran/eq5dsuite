## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>"
)
library(eq5dsuite)

## ----install-cran, eval = FALSE-----------------------------------------------
# install.packages("eq5dsuite")

## ----install-github, eval = FALSE---------------------------------------------
# remotes::install_github("MathsInHealth/eq5dsuite-r")

## ----display-value-sets-------------------------------------------------------
# List all available EQ-5D-3L value sets
eqvs_display(version = "3L")

## ----eq5d3l-example-----------------------------------------------------------
# Example data with EQ-5D-3L responses
eq5d3l_data <- data.frame(
  id = 1:5,
  mo = c(1, 2, 1, 3, 2),
  sc = c(1, 1, 2, 2, 1),
  ua = c(1, 2, 1, 3, 2),
  pd = c(2, 2, 1, 3, 3),
  ad = c(1, 1, 2, 2, 1)
)

# Calculate EQ-5D-3L values using the UK value set
eq5d3l_data$value <- eq5d3l(
  eq5d3l_data,
  country   = "UK",
  dim.names = c("mo", "sc", "ua", "pd", "ad")
)

eq5d3l_data

## ----eq5d5l-example-----------------------------------------------------------
eq5d5l_data <- data.frame(
  id = 1:5,
  mo = c(1, 2, 3, 1, 2),
  sc = c(1, 1, 2, 1, 3),
  ua = c(2, 1, 3, 1, 2),
  pd = c(2, 3, 2, 1, 4),
  ad = c(1, 2, 1, 3, 2)
)

eq5d5l_data$value <- eq5d5l(
  eq5d5l_data,
  country   = "IT",
  dim.names = c("mo", "sc", "ua", "pd", "ad")
)

eq5d5l_data

## ----eq5dy3l-example----------------------------------------------------------
eq5dy3l_data <- data.frame(
  id = 1:5,
  mo = c(1, 2, 1, 2, 3),
  sc = c(1, 1, 2, 1, 2),
  ua = c(2, 1, 1, 3, 2),
  pd = c(1, 2, 3, 2, 1),
  ad = c(2, 1, 2, 1, 3)
)

eq5dy3l_data$value <- eq5dy3l(
  eq5dy3l_data,
  country   = "SI",
  dim.names = c("mo", "sc", "ua", "pd", "ad")
)

eq5dy3l_data

## ----custom-vs, eval = FALSE--------------------------------------------------
# # Create a custom value set data frame
# custom_vs <- data.frame(
#   state = make_all_EQ_indexes(version = "3L"),
#   MY_VS = runif(243)
# )
# 
# # Register it temporarily for this session
# eqvs_add(
#   custom_vs,
#   version     = "3L",
#   country     = "My Country",
#   countryCode = "MC",
#   VSCode      = "MC",
#   description = "Custom value set for demonstration",
#   saveOption  = 1
# )
# 
# # Use the custom value set
# eq5d3l(c(11111, 12321), country = "MC")

## ----update-vs, eval = FALSE--------------------------------------------------
# update_value_sets()

