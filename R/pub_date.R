pub_date <- function(end_date, pub = c("previous", "current", "next")) {

  pub <- match.arg(pub)

  if (class(end_date) != "Date") {
    stop("The extract end date must be provided in date format")
  }

  if(!(format(end_date, "%d %B") %in% c("31 March", "30 June", "30 September",
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


  # The publication date is always the third last Tuesday of the month
  # Calculate n; the number of Tuesdays in the month of publication
  first <- lubridate::floor_date(lubridate::add_with_rollback(p,
                                                              months(5)),
                                 unit = "month")

  last <- lubridate::ceiling_date(lubridate::add_with_rollback(p,
                                                               months(5)),
                                  unit = "month") - lubridate::days(1)

  n <- sum(format(seq(first, last, "day"), "%w") == 2)

  # Return the date of the third last Tuesday in the month of publication
  return(RcppBDT::getNthDayOfWeek(n - 2,
                                  2,
                                  lubridate::month(
                                    lubridate::add_with_rollback(p,
                                                                 months(5))),
                                  lubridate::year(
                                    lubridate::add_with_rollback(p,
                                                                 months(5)))))
}
