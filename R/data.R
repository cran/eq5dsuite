#' @title example_data
#' @description A dataset containing patient-level data in a long format.
#' @format A data frame with 6600 rows and 13 variables:
#' \describe{
#'   \item{\code{id}}{double Patient id}
#'   \item{\code{mo}}{double EQ-5D-5L Mobility dimension}
#'   \item{\code{sc}}{double EQ-5D-5L Self-care dimension}
#'   \item{\code{ua}}{double EQ-5D-5L Usual activities dimension}
#'   \item{\code{pd}}{double EQ-5D-5L Pain / discomfort dimension}
#'   \item{\code{ad}}{double EQ-5D-5L Anxiety/depression dimension}
#'   \item{\code{vas}}{double Value of the VAS scale measurememnt}
#'   \item{\code{fu}}{character Follow-up (baseline / follow-up)}
#'   \item{\code{year_range}}{character Time period for the follow-up (2009-2010 / 2010-2011 / 2011-2012); could be used as an alternative follow-up variable}
#'   \item{\code{month}}{double Another alternative follow-up variable (1, ..., 15)}
#'   \item{\code{surgtype}}{character Type of surgery (Cataract / Hernia / Hip / Knee / Veins)}
#'   \item{\code{gender}}{character Patient's gender (Female / Male)}
#'   \item{\code{age}}{double Patient's age in years} 
#'}
#' @docType data
#' @keywords datasets
#' @name example_data
#' @usage data(example_data)
NULL


