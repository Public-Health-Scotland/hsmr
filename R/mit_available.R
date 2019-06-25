mit_available <- function(end_date) {

  first <- lubridate::floor_date(hsmr::pub_date(end_date = end_date,
                                                pub = "current"),
                                 unit = "month") - months(1)

  last <- lubridate::floor_date(hsmr::pub_date(end_date = end_date,
                                               pub = "current"),
                                unit = "month") - lubridate::days(1)

  n <- sum(format(seq(first, last, "day"), "%w") == 5)

  RcppBDT::getNthDayOfWeek(n,
                           5,
                           lubridate::month(hsmr::pub_date(end_date = end_date,
                                                           pub = "current")
                                            - months(1)),
                           lubridate::year(hsmr::pub_date(end_date = end_date,
                                                          pub = "current")))
}

mit_available(end_date)
pub_date(end_date, "previous")

mit_available(end_date = lubridate::dmy(31122018))
