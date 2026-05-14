## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>"
)
library(eq5dsuite)

## ----add-custom-vs------------------------------------------------------------
# Generate all valid 3L health state codes
states <- make_all_EQ_indexes(version = "3L")

# Create a custom value set (random values for illustration)
set.seed(42)
custom_vs <- data.frame(
  state  = states,
  MY_VS  = runif(243, min = -0.5, max = 1.0)
)

head(custom_vs)

## ----register-vs, eval = FALSE------------------------------------------------
# # Register for current session only (saveOption = 1)
# eqvs_add(
#   custom_vs,
#   version     = "3L",
#   country     = "My Country",
#   countryCode = "MC",
#   VSCode      = "MY_VS",
#   description = "Custom value set — demonstration",
#   saveOption  = 1
# )
# 
# # Verify it appears in the list
# eqvs_display(version = "3L")
# 
# # Use it for value calculation
# eq5d3l(c(11111, 12321), country = "MY_VS")

## ----save-vs, eval = FALSE----------------------------------------------------
# eqvs_add(
#   custom_vs,
#   version     = "3L",
#   country     = "My Country",
#   countryCode = "MC",
#   VSCode      = "MY_VS",
#   description = "Custom value set — permanent",
#   saveOption  = 2  # saves to user cache
# )

## ----load-vs, eval = FALSE----------------------------------------------------
# eqvs_load(country = "MY_VS", version = "3L")

## ----drop-vs, eval = FALSE----------------------------------------------------
# eqvs_drop(
#   country    = "MY_VS",
#   version    = "3L",
#   saveOption = 1  # remove from current session only
# )

## ----custom-xw, eval = FALSE--------------------------------------------------
# # Use custom 3L value set with original crosswalk
# eqxw(
#   eq5d5l_data,
#   country   = "MY_VS",
#   dim.names = c("mo", "sc", "ua", "pd", "ad")
# )

## ----update-vs, eval = FALSE--------------------------------------------------
# update_value_sets()

