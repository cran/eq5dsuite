#' @title example_data
#' @description A dataset containing patient-level data in a long format.
#' @format A data frame with 10000 rows and 14 variables:
#' \describe{
#'   \item{\code{id}}{double Patient id}
#'   \item{\code{time}}{character Follow-up (Pre-op / Post-op)}
#'   \item{\code{mo}}{double EQ-5D-5L Mobility dimension}
#'   \item{\code{sc}}{double EQ-5D-5L Self-care dimension}
#'   \item{\code{ua}}{double EQ-5D-5L Usual activities dimension}
#'   \item{\code{pd}}{double EQ-5D-5L Pain / discomfort dimension}
#'   \item{\code{ad}}{double EQ-5D-5L Anxiety/depression dimension}
#'   \item{\code{vas}}{double Value of the VAS scale measurememnt}
#'   \item{\code{providercode}}{character Provider code}
#'   \item{\code{procedure}}{character Type of surgery}
#'   \item{\code{year}}{character Year of intervention}
#'   \item{\code{ageband}}{character Age in pre-defined ranges} 
#'   \item{\code{gender}}{character Patient's gender (Female / Male)}
#'}
#' @docType data
#' @keywords datasets
#' @name example_data
#' @usage data(example_data)
NULL


