#' @title SMR01 submission deadline
#'
#' @description \code{submission_deadline} takes the final date for which data
#' from the General/Acute Inpatient and Day Case (SMR01) dataset are included
#' in the current publication. It returns the date on which the completeness of
#' SMR01 submissions to ISD for individual hospitals is reported in the current
#' publication.
#'
#' @details The submission deadline is always the final day of the month in
#' which the previous publication was published, plus 42 days.
#'
#' @param end_date The cut-off date for SMR01 data to be included in the
#' current HSMR publication, supplied with \code{Date} class. Must be the final
#' day of either March, June, September or December.
#'
#' @examples
#' extract_end_date <- lubridate::dmy(30092018)
#'
#' submission_deadline(end_date = extract_end_date)
#' format(submission_deadline(end_date = extract_end_date), "%d %B %Y")
#' format(submission_deadline(end_date = extract_end_date), "%d/%m/%Y")


submission_deadline <- function(end_date) {

  if (class(end_date) != "Date") {
    stop("The extract end date must be provided in date format")
  }

  if(!(format(end_date, "%d %B") %in% c("31 March",
                                        "30 June",
                                        "30 September",
                                        "31 December"))) {
    stop("The extract end date must be the final day of either March, June, ",
         "September or December")
  }

  # It's + 41 days as ceiling_date returns the first day of the next month,
  # rather than the final day of the current month
  lubridate::ceiling_date(hsmr::pub_date(end_date = end_date, pub = "previous"),
                          unit = "months") + lubridate::days(41)
}
