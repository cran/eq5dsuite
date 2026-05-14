## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>"
)
library(eq5dsuite)

## ----update-basic, eval = FALSE-----------------------------------------------
# update_value_sets()

## ----update-specific, eval = FALSE--------------------------------------------
# # Check only EQ-5D-5L value sets
# update_value_sets(versions = "5L")
# 
# # Check 3L and Y3L only
# update_value_sets(versions = c("3L", "Y3L"))

## ----update-noninteractive, eval = FALSE--------------------------------------
# # Install without asking for confirmation
# update_value_sets(ask = FALSE)

