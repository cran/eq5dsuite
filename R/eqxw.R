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
  do.call(eq5d, c(as.list(match.call())[-1],version = "xw"))
}
