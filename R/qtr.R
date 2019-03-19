#' @title Assign a date to a quarter
#'
#' @description
#'
# Needs documentation
# Possibly re-name functions to something more informative also


# Current quarter
qtr <- function(first_day, format = c("long", "short")) {

  format <- match.arg(format)

  if (class(first_day) != "Date") {
    stop("The first day of the quarter must be provided in date format")
  }

  if(!(format(first_day, "%d %B") %in% c("01 January", "01 April", "01 July",
                                         "01 October"))) {
    stop("The beginning of a quarter must be the first day of either January, ",
         "April, September or December")
  }

  if (format == "long") {
    return(paste0(as.character(lubridate::month(first_day,
                                                label = TRUE,
                                                abbr = FALSE)),
                  " to ",
                  format(zoo::as.yearmon(first_day + months(2)), "%B %Y")))
  }

  if (format == "short") {
    return(paste0(as.character(lubridate::month(first_day,
                                                label = TRUE,
                                                abbr = TRUE)),
                  "-",
                  zoo::as.yearmon(first_day + months(2))))
  }

}

# Tests
qtr(lubridate::dmy(01012011), format = "short")
qtr(lubridate::dmy(01102011), format = "long")


#' @export
# Quarter end
qtr_end <- function(first_day, quarter = c("current", "next")) {

  quarter <- match.arg(quarter)

  if (class(first_day) != "Date") {
    stop("The first day of the quarter must be provided in date format")
  }

  if(!(format(first_day, "%d %B") %in% c("01 January", "01 April", "01 July",
                                         "01 October"))) {
    stop("The beginning of a quarter must be the first day of either January, ",
         "April, September or December")
  }

  if (quarter == "current") {
    return(format(zoo::as.yearmon(first_day + months(2)), "%B %Y"))
  }

  if (quarter == "next") {
    return(format(zoo::as.yearmon(first_day + months(5)), "%B %Y"))
  }

}

# Tests
qtr_end(lubridate::dmy(01072011), quarter = "current")
qtr_end(lubridate::dmy(01042011), quarter = "next")

#' @export
# Previous quarter
qtr_prev <- function(first_day) {

  if (class(first_day) != "Date") {
    stop("The first day of the quarter must be provided in date format")
  }

  if(!(format(first_day, "%d %B") %in% c("01 January", "01 April", "01 July",
                                         "01 October"))) {
    stop("The beginning of a quarter must be the first day of either January, ",
         "April, September or December")
  }

  return(paste0(as.character(lubridate::month(first_day - months(3),
                                              label = TRUE,
                                              abbr = FALSE)),
                " to ",
                format(zoo::as.yearmon(first_day - months(1)),"%B %Y")))
}

# Tests
qtr_prev(lubridate::dmy(01012018))

