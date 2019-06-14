#' @title HSMR publication date
#'
#' @description \code{pub_date} takes the final date for which SMR data are
#' included in the current publication. It uses this cut-off date to calculate
#' the publication dates of the previous, current and next HSMR publications.
#'
#' @details The publication date for HSMR is always the third last Tuesday of
#' the month.
#'
#' @param end_date The cut-off date for data to be included in the current HSMR
#' publication, supplied with \code{Date} class. Must be the final day of
#' either March, June, September or December.
#' @param pub A \code{character} string specifying the publication of interest.
#' Valid options are `previous`, `current` and `next`.
#'
#' @examples
#' extract_end_date <- lubridate::dmy(30092018)
#'
#' pub_date(end_date = extract_end_date, pub = "previous")
#' format(pub_date(end_date = extract_end_date, pub = "current"), "%d %B %Y")
#' format(pub_date(end_date = extract_end_date, pub = "next"), "%d/%m/%Y")
#'
#' @export

pub_date <- function(end_date, pub = c("previous", "current", "next")) {

  pub <- match.arg(pub)

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

  # Calculate p; the extract end date for the selected publication
  if (pub == "previous") {
    p <- lubridate::ceiling_date(lubridate::add_with_rollback(end_date,
                                                              -months(3)),
                      unit = "month") - lubridate::days(1)
  }

  if (pub == "current") {
    p <- end_date
  }

  if (pub == "next") {
    p <- lubridate::ceiling_date(lubridate::add_with_rollback(end_date,
                                                              months(3)),
                      unit = "month") - lubridate::days(1)
  }


  # The publication date is always five months after p and is on the third last
  # Tuesday of the month
  # Calculate the first and last day of the month of publication (p + 5)
  # Then calculate n; the number of Tuesdays in the month of publication
  first <- lubridate::floor_date(lubridate::add_with_rollback(p,
                                                              months(5)),
                                 unit = "month")

  last <- lubridate::ceiling_date(lubridate::add_with_rollback(p,
                                                               months(5)),
                                  unit = "month") - lubridate::days(1)

  n <- sum(format(seq(first, last, "day"), "%w") == 2)

  # Return the date of the third last Tuesday in the month of publication
  RcppBDT::getNthDayOfWeek(n - 2,
                           2,
                           lubridate::month(
                             lubridate::add_with_rollback(p,
                                                          months(5))),
                           lubridate::year(
                             lubridate::add_with_rollback(p,
                                                          months(5))))
}
