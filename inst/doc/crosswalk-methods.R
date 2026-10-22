## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>"
)
library(eq5dsuite)

## ----eqxw-example-------------------------------------------------------------
eq5d5l_data <- data.frame(
  id = 1:5,
  mo = c(1, 2, 3, 1, 2),
  sc = c(1, 1, 2, 1, 3),
  ua = c(2, 1, 3, 1, 2),
  pd = c(2, 3, 2, 1, 4),
  ad = c(1, 2, 1, 3, 2)
)

# Apply crosswalk using UK 3L value set
eq5d5l_data$value_xw <- eqxw(
  eq5d5l_data,
  country   = "GB",
  dim.names = c("mo", "sc", "ua", "pd", "ad")
)

eq5d5l_data

## ----eqxwr-example------------------------------------------------------------
eq5d3l_data <- data.frame(
  id = 1:5,
  mo = c(1, 2, 1, 3, 2),
  sc = c(1, 1, 2, 2, 1),
  ua = c(1, 2, 1, 3, 2),
  pd = c(2, 2, 1, 3, 3),
  ad = c(1, 1, 2, 2, 1)
)

# Apply reverse crosswalk using Spain 5L value set
eq5d3l_data$value_rxw <- eqxwr(
  eq5d3l_data,
  country   = "ES",
  dim.names = c("mo", "sc", "ua", "pd", "ad")
)

eq5d3l_data

## ----eqxw-uk-example----------------------------------------------------------
uk_data <- data.frame(
  id   = 1:5,
  mo   = c(1, 2, 1, 3, 2),
  sc   = c(1, 1, 2, 2, 1),
  ua   = c(1, 2, 1, 3, 2),
  pd   = c(2, 2, 1, 3, 3),
  ad   = c(1, 1, 2, 2, 1),
  age  = c(45, 62, 38, 71, 55),
  male = c(1,  0,  1,  0,  1)
)

uk_data$value_uk <- eqxw_UK(
  uk_data,
  age  = "age",
  male = "male"
)

uk_data

## ----eqxwr-uk-example---------------------------------------------------------
uk3l <- data.frame(
  id   = 1:5,
  mo   = c(1, 2, 1, 3, 2),
  sc   = c(1, 1, 2, 2, 1),
  ua   = c(1, 2, 1, 3, 2),
  pd   = c(2, 2, 1, 3, 3),
  ad   = c(1, 1, 2, 2, 1),
  age  = c(45, 62, 38, 71, 55),
  male = c(1,  0,  1,  0,  1)
)

uk3l$value_uk5l <- eqxwr_UK(uk3l, age = "age", male = "male")

uk3l

## ----mean-value-example-------------------------------------------------------
# a mean EQ-5D-3L value of 0.54 in women aged 35
eqxwr_UK(0.54, age = 35, male = 0, bwidth = "default")

# the bandwidth matters: a narrower one weights fewer states
eqxwr_UK(0.54, age = 35, male = 0, bwidth = 0.1)

