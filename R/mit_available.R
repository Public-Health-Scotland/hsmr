#' @title Management Information Tool made available to health boards
#'
#' @description \code{mit_available} takes the final date for which SMR data
#' are included in the current publication. It uses this cut-off date to
#' calculate the date on which data for the current publication are made
#' available to select health board employees via the management information
#' tool.
#'
#' @details The management information tool is a dashboard which accompanies
#' the full report, summary report and Excel tables as part of the HSMR
#' publication. It contains all of the data which the other parts of the
#' publication are based upon.
#'
#' The management tool is made available to health boards on the last Friday of
#' the month, the month before publication.
#'
#' @param end_date The cut-off date for data to be included in the current HSMR
#' publication, supplied with \code{Date} class. Must be the final day of
#' either March, June, September or December.
#'
#' @examples
#' mit_available(end_date = lubridate::dmy(30092018))
#' format(mit_available(end_date = lubridate::dmy(31122018)), "%d %B %Y")
#' format(mit_available(end_date = lubridate::dmy(31032019)), "%d/%m/%Y")
#'
#' @export

mit_available <- function(end_date) {

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

  # Calculate the first day of the month, the month before publication
  first <- lubridate::floor_date(hsmr::pub_date(end_date = end_date,
                                                pub = "current"),
                                 unit = "month") - months(1)

  # Calculate the last day of the month, the month before publication
  last <- lubridate::ceiling_date(first, unit = "month") - lubridate::days(1)

  # Calculate the number of Fridays in the month before publication
  n <- sum(format(seq(first, last, "day"), "%w") == 5)

  # Return the date of the last Friday in the month before publication
  RcppBDT::getNthDayOfWeek(n,
                           5,
                           lubridate::month(hsmr::pub_date(end_date = end_date,
                                                           pub = "current")
                                            - months(1)),
                           lubridate::year(hsmr::pub_date(end_date = end_date,
                                                          pub = "current")))
}
