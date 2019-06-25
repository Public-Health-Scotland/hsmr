
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
