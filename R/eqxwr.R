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
  # Parameters for the ten EQ-5D-3L level dummies (mo2, mo3, sc2, ... ad2, ad3).
  # The reverse crosswalk model has no age or gender term; only eqxwr_UK(),
  # which uses the separate NICE DSU mapping, is age- and sex-specific.
  Bx<-par[5:14,]
  
  tmp <- diag(3)[,2:3]
  Y <- - do.call(cbind, lapply(as.list(expand.grid(1:3, 1:3, 1:3, 1:3, 1:3)[,5:1]), function(x) tmp[x,]))
  
  # Model matrix: dummies for EQ-5D-3L (i.e. mo2, mo3, sc2 ... ad2, ad3).
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
#' @param country A country code or value set code identifying the value set
#'   to use, as listed by \code{eqvs_display()}. Matching is case-insensitive.
#'   For countries with more than one value set (for example Germany, with
#'   \code{"DE_TTO"} and \code{"DE_VAS"}), the value set code must be
#'   given; supplying the country code alone raises an error listing the
#'   available codes.
#' @param dim.names A vector of dimension names to identify dimension columns
#' @return A numeric vector of reverse crosswalk values, or a data.frame with
#'   one column per reverse crosswalk set requested. The result is an unnamed numeric vector, one element per
#'   element or row of \code{x} and in the same order.
#' @details This is the reverse crosswalk of van Hout and Shaw (2021), which
#'   maps EQ-5D-3L responses to EQ-5D-5L values for any available value set.
#'   It does not use age or sex. It is a different mapping from
#'   \code{\link{eqxwr_UK}}, which implements the NICE Decision Support Unit
#'   mapping for the United Kingdom only and is age- and sex-specific. Do not
#'   substitute one for the other.
#' @references van Hout B, Shaw JW (2021). Mapping EQ-5D-3L to EQ-5D-5L.
#'   \emph{Value in Health} 24(9):1285-1293. \doi{10.1016/j.jval.2021.03.009}
#' @seealso \code{\link{eqxwr_UK}}, \code{\link{eqxw}}
#' @inheritSection eq5d Missing and invalid responses
#' @examples 
#' eqxwr(c(11111, 12321, 32123, 33333), 'US')
#' eqxwr(make_all_EQ_states('3L'), c('DK', 'US'))
#' @export
eqxwr <- function(x, country = NULL, dim.names = c("mo", "sc", "ua", "pd", "ad")) {
  pkgenv <- getOption("eq.env")
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  if(length(dim(x)) == 2) {
    if(is.null(colnames(x))) {
      message("No column names")
      if(NCOL(x) == 5) {
        message("Given 5 columns, will assume dimensions in conventional order: MO, SC, UA, PD, AD.")
        colnames(x) <- dim.names
      } 
    }
    # Columns are found by toEQ5Dindex(), as in eq5d(). Lowercasing the data
    # but not dim.names refused upper-case names that were exactly the
    # column names (review A12).
    x <- toEQ5Dindex(x = x, dim.names = dim.names, quiet = TRUE)
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
  
  # A code outside the EQ-5D-3L is NA with a warning (see .validate_states()).
  x <- .validate_states(x, top = 3L, instrument = "EQ-5D-3L")
  
  if(length(country)>1) {
    names(country) <- country
    return(do.call(cbind, lapply(country, function(count) eqxwr(x, count, dim.names))))
  }
  
  xout <- rep(NA, length(x))
  
  xout[!is.na(x)] <- pkgenv$xwrsets[match(x[!is.na(x)], pkgenv$states_3L$state), country]
  xout
  
}

#' @title eqxwr_UK
#' @description Map EQ-5D-3L health states, or mean EQ-5D-3L values, to EQ-5D
#'   values based on the UK EQ-5D-5L value set (Rowen et al. 2026), using the
#'   mapping published by the NICE Decision Support Unit (Hernández Alava et
#'   al. 2023).
#'
#' @details
#' This is the mapping NICE's interim methods statement of 27 August 2026
#' (NICE 2026, PMG51) specifies when EQ-5D-5L data from a relevant study are
#' not available and EQ-5D-3L data are used instead: the 3L responses are
#' mapped to utility values on the UK EQ-5D-5L value set, the reference case
#' for topics started after that date. (Checked against the statement on 4
#' October 2026.)
#'
#' The mapping is age- and sex-specific. It is \strong{not} the reverse
#' crosswalk of van Hout and Shaw (2021) implemented in \code{\link{eqxwr}},
#' which takes neither age nor sex and is available for many value sets; the two
#' give different answers and are not interchangeable. See \code{\link{eqxw_UK}}
#' for the opposite direction.
#'
#' \subsection{Two kinds of input}{
#' \strong{Health states}, for individual respondents. Give \code{x} as 5-digit
#' EQ-5D-3L state indexes, or as a matrix or data frame with one column per
#' dimension. Each respondent's state, age and sex are looked up directly and
#' \code{bwidth} is ignored. This is the usual case.
#'
#' \strong{EQ-5D values}, for a \emph{mean} value such as one reported in a
#' published study. Give \code{x} as values on the UK EQ-5D-3L value set. There
#' is no unique health state behind a mean, so the mapped value is a weighted
#' average of the mapped values of the states whose EQ-5D value lies near it,
#' using an Epanechnikov kernel of half-width \code{bwidth} within the age band
#' and sex given. Set \code{bwidth = "default"} to apply the DSU's recommended
#' bandwidths for mean values: 0.1 for values above 0.6, and 0.4 for values at
#' or below 0.6. \strong{This input is not intended for individual-level data};
#' map individual respondents from their health states.
#' }
#'
#' \subsection{Age and sex}{
#' Both are required. Age is used in years and grouped into the five bands of
#' the mapping: 16-34, 35-44, 45-54, 55-64 and 65 or over. There is no upper
#' limit. Ages below 16 are outside the estimation sample and return \code{NA}
#' with a warning; values of 1 to 5 are treated as ages, never as band numbers.
#' Sex must be 1 for male and 0 for female; \code{TRUE} and \code{FALSE} are
#' accepted. Any other value returns \code{NA} with a warning.
#' }
#'
#' Invalid or missing health states, ages or sexes return \code{NA}.
#'
#' @param x EQ-5D-3L health states, as a vector of 5-digit state indexes or as a
#'   matrix or data frame with one column per EQ-5D dimension; or a vector of
#'   mean EQ-5D values on the UK EQ-5D-3L value set. See Details.
#' @param age Age in years, as a vector or, when \code{x} is a data frame, the
#'   name of a column in it.
#' @param male 1 for male and 0 for female (\code{TRUE}/\code{FALSE} are
#'   accepted), as a vector or, when \code{x} is a data frame, a column name.
#' @param dim.names A vector of dimension names to identify dimension columns.
#' @param bwidth Kernel half-width, used only for value input: a finite
#'   number of at least 0, or one per row. \code{0} (the default) matches
#'   values exactly. \code{"default"} applies the DSU's
#'   recommended bandwidths for mean values exactly as the DSU's own R command
#'   (April 2026 release) does: 0.1 for a value above 0.6 and 0.4 for a value
#'   of 0.6 or below. NICE's FAQ to its interim methods statement (PMG51)
#'   describes 0.6 itself as in the 0.1 range; the two differ only for a value
#'   of exactly 0.6, where a numeric \code{bwidth} can be given instead.
#' @return An unnamed numeric vector of mapped EQ-5D values, one per row of
#'   \code{x} and in the same order.
#' @references
#' Hernández Alava M, Pudney S, Wailoo A (2023). Estimating the Relationship
#' Between EQ-5D-5L and EQ-5D-3L: Results from a UK Population Study.
#' \emph{PharmacoEconomics} 41(2):199-207. \doi{10.1007/s40273-022-01218-7}
#'
#' Hernández-Alava M, Pudney S (2017). Econometric modelling of multiple
#' self-reports of health states: The switch from EQ-5D-3L to EQ-5D-5L in
#' evaluating drug therapies for rheumatoid arthritis.
#' \emph{Journal of Health Economics} 55:139-152.
#' \doi{10.1016/j.jhealeco.2017.06.013}
#'
#' Rowen D, Mukuria C, Bray N, et al. (2026). A UK value set for the EQ-5D-5L.
#' \emph{Value in Health} 29(5):858-869. \doi{10.1016/j.jval.2026.03.008}
#'
#' National Institute for Health and Care Excellence (2026). Interim methods
#' statement: implementing the EQ-5D-5L value set (PMG51). Published 27 August
#' 2026. \url{https://www.nice.org.uk/process/pmg51}
#' @source
#' The mapping tables are taken from the publicly available commands for
#' mapping between the EQ-5D-3L and the EQ-5D-5L published by the NICE
#' Decision Support Unit (April 2026 release, which uses the UK EQ-5D-5L value
#' set of Rowen et al. 2026):
#' \url{https://sheffield.ac.uk/nice-dsu/methods-development/mapping-eq-5d-5l-3l}
#'
#' As the DSU requests, work using these mappings cites Hernández Alava,
#' Pudney and Wailoo (2023) and, for the underlying model, Hernández-Alava and
#' Pudney (2017). See \code{file.show(system.file("COPYRIGHTS", package =
#' "eq5dsuite"))}.
#' @seealso \code{\link{eqxw_UK}} for EQ-5D-5L to EQ-5D-3L, and
#'   \code{\link{eqxwr}} for the van Hout and Shaw reverse crosswalk.
#' @examples
#' # health states, for individual respondents
#' eqxwr_UK(c(11111, 12321, 32123, 33333),
#'          age = c(30, 40, 55, 70), male = c(1, 0, 1, 0))
#'
#' # a mean EQ-5D-3L value from a published study, with the recommended bandwidth
#' eqxwr_UK(0.54, age = 35, male = 0, bwidth = "default")
#' @export
eqxwr_UK <- function(x, age, male, dim.names = c("mo", "sc", "ua", "pd", "ad"),
                     bwidth = 0) {
  .eqxw_NICE(x = x, age = age, male = male, direction = "3Lto5L",
             dim.names = dim.names, bwidth = bwidth, .fname = "eqxwr_UK")
}
