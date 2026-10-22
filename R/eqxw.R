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
#' @param country A country code or value set code identifying the value set
#'   to use, as listed by \code{eqvs_display()}. Matching is case-insensitive.
#'   For countries with more than one value set (for example Germany, with
#'   \code{"DE_TTO"} and \code{"DE_VAS"}), the value set code must be
#'   given; supplying the country code alone raises an error listing the
#'   available codes.
#' @param dim.names A vector of dimension names to identify dimension columns
#' @return A numeric vector of crosswalk values, or a data.frame with one
#'   column per crosswalk set requested. The result is an unnamed numeric vector, one element per
#'   element or row of \code{x} and in the same order.
#' @inheritSection eq5d Missing and invalid responses
#' @examples 
#' eqxw(c(11111, 12521, 32123, 55555), 'US')
#' eqxw(make_all_EQ_states('5L'), c('DK', 'US'))
#' @export
eqxw <- function(x, country = NULL, dim.names = c("mo", "sc", "ua", "pd", "ad")) {
  eq5d(x = x, country = country, version = "xw", dim.names = dim.names)
}

#' @title eqxw_UK
#' @description Map EQ-5D-5L health states, or mean EQ-5D-5L values, to EQ-5D
#'   values based on the UK EQ-5D-3L value set (Dolan 1997), using the mapping
#'   published by the NICE Decision Support Unit (Hernández Alava et al. 2023).
#'
#' @details
#' NICE's interim methods statement of 27 August 2026 (NICE 2026, PMG51) makes
#' the UK EQ-5D-5L value set the reference case: EQ-5D-5L data collected in a
#' study should be valued directly with it, that is
#' \code{eq5d5l(x, country = "GB")}. The statement applies to topics started
#' after it was published -- for technology appraisals, those whose invitation
#' to participate was issued after that date. For topics started before it,
#' NICE says EQ-5D-5L data should continue to be mapped to EQ-5D-3L utility
#' values, which is what \code{eqxw_UK()} does; it also reproduces earlier
#' analyses. (Checked against the statement on 4 October 2026; NICE's methods
#' manuals PMG36 and PMG20 are to be updated to match it.)
#'
#' The mapping is age- and sex-specific. It is not the crosswalk of van Hout et
#' al. (2012) implemented in \code{\link{eqxw}}, and it is not the reverse
#' crosswalk of van Hout and Shaw (2021) implemented in \code{\link{eqxwr}};
#' those take neither age nor sex. See \code{\link{eqxwr_UK}} for the opposite
#' direction.
#'
#' \subsection{Two kinds of input}{
#' \strong{Health states}, for individual respondents. Give \code{x} as 5-digit
#' EQ-5D-5L state indexes, or as a matrix or data frame with one column per
#' dimension. Each respondent's state, age and sex are looked up directly and
#' \code{bwidth} is ignored. This is the usual case.
#'
#' \strong{EQ-5D values}, for a \emph{mean} value such as one reported in a
#' published study. Give \code{x} as values on the UK EQ-5D-5L value set. There
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
#' @param x EQ-5D-5L health states, as a vector of 5-digit state indexes or as a
#'   matrix or data frame with one column per EQ-5D dimension; or a vector of
#'   mean EQ-5D values on the UK EQ-5D-5L value set. See Details.
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
#' @seealso \code{\link{eqxwr_UK}} for EQ-5D-3L to EQ-5D-5L, \code{\link{eqxw}}
#'   and \code{\link{eqxwr}} for the van Hout crosswalks, and \code{eq5d5l()}
#'   for direct valuation.
#' @examples
#' # health states, for individual respondents
#' eqxw_UK(c(11111, 12345, 32423, 55555),
#'         age = c(30, 40, 55, 70), male = c(1, 0, 1, 0))
#'
#' # a mean EQ-5D-5L value from a published study, with the recommended bandwidth
#' eqxw_UK(0.72, age = 55, male = 1, bwidth = "default")
#' @export
eqxw_UK <- function(x, age, male, dim.names = c("mo", "sc", "ua", "pd", "ad"),
                    bwidth = 0) {
  .eqxw_NICE(x = x, age = age, male = male, direction = "5Lto3L",
             dim.names = dim.names, bwidth = bwidth, .fname = "eqxw_UK")
}
