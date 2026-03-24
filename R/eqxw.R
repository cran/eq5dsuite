#' @title .pstate5t3
#' @description Takes a 15 x 5 matrix with probabilities per level/dimension, and creates an 3125x243 matrix with probabilities per state
#' @param probs 15 x 5 matrix with probabilities per level/dimension, typically saved in .EQxwprob
#' @return An 3125x243 matrix with probabilities per state
.pstate5t3 <-  function(probs = .EQxwprob) {
  
  allst5l <- make_all_EQ_states()
  allst3l <- make_all_EQ_states(version = '3L')
  
  t(Reduce('*', lapply(0:4, function(i)    probs[i*3+allst3l[, i+1],allst5l[,i+1]])))
}



#' @title eqxw
#' @description Get crosswalk values
#' @param x A vector of 5-digit EQ-5D-5L state indexes or a matrix/data.frame with columns corresponding to EQ-5D state dimensions
#' @param country String vector indicating country names or  ISO3166 Alpha 2 / 3 country codes.
#' @param dim.names A vector of dimension names to identify dimension columns
#' @return A vector of reverse crosswalk values or data.frame with one column per reverse crosswalk set requested.
#' @examples 
#' eqxw(c(11111, 12521, 32123, 55555), 'US')
#' eqxw(make_all_EQ_states('5L'), c('DK', 'US'))
#' @export
eqxw <- function(x, country = NULL, dim.names = c("mo", "sc", "ua", "pd", "ad")) {
  eq5d(x = x, country = country, version = "xw", dim.names = dim.names)
}

#' @title eqxw_UK
#' @description Crosswalks EQ-5D-5L responses to EQ-5D-3L utilities using NICE's mapping.
#' @param x A vector of 5-digit EQ-5D-5L states (domain scores) or a summary score.
#' @param age  A numeric vector or column name (if `x` is a data frame). Can be either:
#'   (1) a numeric age between 18 and 100, which will be automatically grouped into NICE-defined age bands (18-35, 35-45, 45-55, 55-65, +65), or
#'   (2) a factor/character/numeric vector already representing the NICE age bands with values 1-5 indicating age bands (18-35, 35-45, 45-55, 55-65, +65).
#' @param male A numeric vector (1=male, 0=female) or column name indicating gender.
#' @param dim.names A vector of dimension names for EQ-5D states (default: c("mo", "sc", "ua", "pd", "ad")).
#' @param bwidth Numeric. Bandwidth for kernel smoothing when using summary scores.
#' @return A vector or data frame with crosswalked EQ-5D-3L utilities.
#' @examples 
#' eqxw_UK(c(11111, 12345, 32423, 55555), age = c(30, 40, 55, 70), male = c(1, 0, 1, 0))
#' @importFrom stats dnorm weighted.mean
#' @export

eqxw_UK <- function(x, age, male, dim.names = c("mo", "sc", "ua", "pd", "ad"), bwidth = 0) {
 
  # Load crosswalk data
  pkgenv <- getOption("eq.env")
  if (is.null(pkgenv) || is.null(pkgenv$crosswalk_NICE[["5L"]])) {
    stop("Missing NICE crosswalk data in `options(eq.env)`. Please set the environment.")
  }
  cw <- pkgenv$crosswalk_NICE[["5L"]]
  names(cw)[match(c("X_age",  "X_male"),  names(cw))] <- c("age",  "male")
  
  # Determine input type and construct data
  if (is.data.frame(x)) {
    if (!all(dim.names %in% names(x))) 
      stop("Provided dimension names not found in data.")
    if (!(age %in% names(x)) || !(male %in% names(x))) 
      stop("Age and male must be column names in `x`.")
    df <- data.frame(
      Domain = toEQ5Dindex(as.matrix(x[ , dim.names])),
      age    = x[[age]],
      male   = as.numeric(x[[male]]),
      stringsAsFactors = FALSE
    )
    
  } else if (is.vector(x)) {
    df <- data.frame(
      Domain = as.character(x),
      age = age,
      male = male,
      stringsAsFactors = FALSE
    )
  } else {
    stop("`x` must be either a data frame or a character/numeric vector of EQ-5D states.")
  }
  
  # Process age into age bands
  df$age_band <- if (is.numeric(df$age)) {
    as.numeric(cut(df$age,
                   breaks = c(1, 18, 35, 45, 55, 65, 100),
                   labels = c("1", "1", "2", "3", "4", "5"),
                   right  = FALSE))
  } else {
    as.numeric(as.character(df$age))
  }

  is_domain <- all(grepl("^[1-5]{5}$", df$Domain))
  
  if (is_domain) {
    res <- merge(df,
                 cw[ , c("Domain", "age", "male", "Output")],
                 by.x = c("Domain", "age_band", "male"),
                 by.y = c("Domain", "age",      "male"),
                 all.x = TRUE, sort = FALSE)
  } else {
    
    df$score <- as.numeric(df$Domain)
    cw$score   <- as.numeric(cw$Domain)
    
    if (bwidth == 0) {
      res <- merge(df,
                   cw[ , c("score", "age", "male", "Output")],
                   by.x = c("score", "age_band", "male"),
                   by.y = c("score", "age",      "male"),
                   all.x = TRUE, sort = FALSE)
      
    } else {
      
      # kernel-weighted estimate
      res <- df
      res$Output <- mapply(
        function(a, m, s) {
          sub <- cw[cw$age == a & cw$male == m, ]
          w   <- stats::dnorm((sub$score - s) / bwidth)
          if (length(w)) stats::weighted.mean(sub$Output, w) else NA_real_
        },
        df$age_band, df$male, df$score, SIMPLIFY = TRUE)
    }
  }

  return(res$Output)
}
