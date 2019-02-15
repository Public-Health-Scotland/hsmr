pub_date <- function(end_date, pub = c("previous", "current", "next")) {

  pub <- match.arg(pub)

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

  first <- lubridate::as_date(cut(lubridate::add_with_rollback(p,
                                                               months(5)),
                                  "month"))

  last <- lubridate::as_date(cut(first + 31, "month")) - lubridate::days(1)

  n <- sum(format(seq(first, last, "day"), "%w") == 2)

  return(RcppBDT::getNthDayOfWeek(n - 2,
                                  2,
                                  lubridate::month(
                                    lubridate::add_with_rollback(p,
                                                                 months(5))),
                                  lubridate::year(
                                    lubridate::add_with_rollback(p,
                                                                 months(5)))))
}

format(pub_date(end_date = z_end_date, pub = "current"), "%d %B %Y")


