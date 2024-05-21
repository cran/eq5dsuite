#' @title .pstate3t5
#' @description Takes a N x 25 matrix with probabilities per level/dimension, and creates an N * 3125 matrix with probabilities per state
#' @param PPP N x 25 matrix with probabilities per level/dimension created by EQrxwprobs
#' @return An N * 3125 matrix with probabilities per state
.pstate3t5 <-function(PPP) {
  vallst <- as.vector(as.matrix(expand.grid(21:25, 16:20, 11:15, 6:10, 1:5))[, 5:1])
  t1l<- lapply(seq_len(dim(PPP)[2]), function(x) PPP[, x])
  do.call(cbind, lapply(seq_len(3125), function(i) {
    t1l[[vallst[[i]]]]*
      t1l[[vallst[[i+3125]]]]*
      t1l[[vallst[[i+6250]]]]*
      t1l[[vallst[[i+9375]]]]*
      t1l[[vallst[[i+12500]]]]
  }))
}

#' @title .EQxwrprob
#' @description Takes a matrix of parameters for reverse crosswalk model, returns 243 x 25 matrix of state/level transition probabilities.
#' @param par Matrix of model parameters 
#' @return An 243 * 25 matrix with probabilities for state level transitions.
.EQxwrprob <- function(par=NULL) {
  if(is.null(par)) stop('No parameters provided.')
  par <- as.matrix(par)
  
  # Logistic function by way of hyperbolic tangent
  lp <- function(eta) 0.5+0.5*tanh(eta*0.5)
  
  # Parameters for transitions between levels 1-->2, 2-->3, 3-->4, and 4-->5, per dimension
  Zx<-t(par[1:4,])            
  # Parameters for adjacent dimensions (10), age, age^2, and gender
  Bx<-par[5:14,]
  
  tmp <- diag(3)[,2:3]
  Y <- - do.call(cbind, lapply(as.list(expand.grid(1:3, 1:3, 1:3, 1:3, 1:3)[,5:1]), function(x) tmp[x,]))
  
  # Model matrix: dummies for EQ-5D-3L (i.e. mo2, mo3, sc2 ...ad2, ad3), age, age^2, gender
  # Y <- - as.matrix(cbind(inp[, 1:10], inp[,11]/10, (inp[,11]/10)^2, inp[,12])) # Note the "-" first; these are negatives
  do.call(cbind, lapply(1:5, function(i) {
    # inverse logit, first the level transitions
    # t(Zx[, rep(i, NROW(inp)])) creates a N * 4 matrix with the corresponding level transition parameters in the four columns
    tmp <- lp(Zx[rep.int(i,243),] + 
                # Y %*% Bx[,i] is a matrix multiplication of the model matrix and the corresponding coefficients
                (Y %*% Bx[,i])[,1])
    # subtract previous level from next, creating a matrix with column 1 representing p for level 1, column 2 for level 2, etc.
    cbind(tmp,1)-cbind(0, tmp)
  }))
}

#' @title eqxwr
#' @description Get reverse crosswalk values
#' @param x A vector of 5-digit EQ-5D-3L state indexes or a matrix/data.frame with columns corresponding to EQ-5D state dimensions
#' @param country String vector indicating country names or  ISO3166 Alpha 2 / 3 country codes.
#' @param dim.names A vector of dimension names to identify dimension columns
#' @return A vector of reverse crosswalk values or data.frame with one column per reverse crosswalk set requested.
#' @examples 
#' eqxwr(c(11111, 12321, 32123, 33333), 'US')
#' eqxwr(make_all_EQ_states('3L'), c('DK', 'US'))
#' @export
eqxwr <- function(x, country = NULL, dim.names = c("mo", "sc", "ua", "pd", "ad")) {
  pkgenv <- getOption("eq.env")
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  if(length(dim(x)) == 2) {
    colnames(x) <- tolower(colnames(x))
    if(is.null(colnames(x))) {
      message("No column names")
      if(NCOL(x) == 5) {
        message("Given 5 columns, will assume dimensions in conventional order: MO, SC, UA, PD, AD.")
        colnames(x) <- dim.names
      } 
    }
    if(!all(dim.names %in% colnames(x))) stop("Provided dimension names not available in matrix/data.frame.")
    x <- toEQ5Dindex(x = x, dim.names = dim.names)
  }
  
  country <- .fixCountries(country)
  if(any(is.na(country))) {
    isnas <- which(is.na(country))
    for(i in isnas)  warning('Country ', names(country)[i], ' not found. Dropped.')
    country <- country[!is.na(country)]
  }
  
  if(length(country)==0) {
    message('No valid countries listed. These value sets are currently available.')
    eqvs_display(version = "5L")
    stop('No valid countries listed.')
  }
  
  x <- as.integer(x)
  x[!regexpr("^[1-3]{5}$", x)==1] <- NA
  
  if(length(country)>1) {
    names(country) <- country
    return(do.call(cbind, lapply(country, function(count) eqxwr(x, count, dim.names))))
  }
  
  xout <- rep(NA, length(x))
  
  xout[!is.na(x)] <- pkgenv$xwrsets[match(x[!is.na(x)], pkgenv$states_3L$state), country]
  xout
  
}
