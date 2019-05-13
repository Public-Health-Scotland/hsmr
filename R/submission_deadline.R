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

  lubridate::ceiling_date(hsmr::pub_date(end_date = end_date, pub = "previous"),
                          unit = "months") + lubridate::days(41)
}
