#' @title example_data
#' @description A sample of patient-level EQ-5D-3L data from the NHS Patient
#'   Reported Outcome Measures (PROMs) programme, in long format: 5,000
#'   patients, each with one pre-operative and one post-operative record.
#'
#'   Missing values in the EQ-5D dimensions and in the EQ VAS are recorded with
#'   numeric codes rather than \code{NA}: \code{9} in \code{mo}, \code{sc},
#'   \code{ua}, \code{pd} and \code{ad} (1,323 cells across 497 records), and
#'   \code{999} in \code{vas} (671 records). Both the analysis functions and
#'   the value functions (\code{eq5d3l()}, \code{eq5d()}, \code{eqxwr()} and
#'   so on) treat these as invalid: they set them to \code{NA}, with a
#'   warning. Recode them to \code{NA} first to avoid the warning. \code{ageband} and \code{gender} use
#'   \code{NA} directly (970 records each).
#' @format A data frame with 10000 rows and 12 variables:
#' \describe{
#'   \item{\code{id}}{integer Patient id; each id appears twice}
#'   \item{\code{time}}{character Follow-up (Pre-op / Post-op)}
#'   \item{\code{mo}}{integer EQ-5D-3L Mobility dimension (1-3; 9 = missing)}
#'   \item{\code{sc}}{integer EQ-5D-3L Self-care dimension (1-3; 9 = missing)}
#'   \item{\code{ua}}{integer EQ-5D-3L Usual activities dimension (1-3; 9 = missing)}
#'   \item{\code{pd}}{integer EQ-5D-3L Pain/discomfort dimension (1-3; 9 = missing)}
#'   \item{\code{ad}}{integer EQ-5D-3L Anxiety/depression dimension (1-3; 9 = missing)}
#'   \item{\code{vas}}{integer EQ VAS score (0-100; 999 = missing)}
#'   \item{\code{procedure}}{character Type of surgery (Groin Hernia / Hip Replacement / Knee Replacement / Varicose Vein)}
#'   \item{\code{year}}{character Financial year of the operation, April to March (2013/14 to 2016/17)}
#'   \item{\code{ageband}}{character Age in pre-defined ranges (20 to 29, ..., 80 to 89; NA where not recorded)}
#'   \item{\code{gender}}{character Patient's gender (Female / Male / Not specified; NA where not recorded)}
#'}
#' @docType data
#' @keywords datasets
#' @name example_data
#' @usage data(example_data)
NULL
