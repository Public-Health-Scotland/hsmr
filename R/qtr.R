#' @title Assign a date to a quarter
#'
#' @description
#'
#' The qtr functions take the first day of a quarter (the first of
#' either January, April, July or October) and calculate the relevant
#' quarter-related value from it.
#'
#' \itemize{
#' \item \code{qtr} returns the current quarter in either short or long format.
#'
#' \item \code{qtr_end} returns the last month in the quarter, either of the
#' current or following quarter.
#'
#' \item \code{qtr_prev} returns the previous quarter in long format.
#' }
#'
#' @param first_day The first day of a quarter, supplied with \code{Date} class.
#' @param format Should the quarter be provided in short or long format?
#' @param quarter Should the final month of the current or next quarter be
#' provided?
#'
#' @examples
#' qtr(first_day = lubridate::dmy(01012018), format = "short")
#' qtr(first_day = lubridate::dmy(01102018), format = "long")
#'
#' qtr_end(first_day = lubridate::dmy(01072018), quarter = "current")
#' qtr_end(first_day = lubridate::dmy(01042018), quarter = "next")
#'
#' qtr_prev(first_day = lubridate::dmy(01012018))


#' @export
#' @rdname qtr
qtr <- function(first_day, format = c("long", "short")) {

  format <- match.arg(format)

  if (class(first_day) != "Date") {
    stop("The first day of the quarter must be provided in date format")
  }

  if(!(format(first_day, "%d %B") %in% c("01 January",
                                         "01 April",
                                         "01 July",
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


#' @export
#' @rdname qtr
qtr_end <- function(first_day, quarter = c("current", "next")) {

  quarter <- match.arg(quarter)

  if (class(first_day) != "Date") {
    stop("The first day of the quarter must be provided in date format")
  }

  if(!(format(first_day, "%d %B") %in% c("01 January",
                                         "01 April",
                                         "01 July",
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


#' @export
#' @rdname qtr
qtr_prev <- function(first_day) {

  if (class(first_day) != "Date") {
    stop("The first day of the quarter must be provided in date format")
  }

  if(!(format(first_day, "%d %B") %in% c("01 January",
                                         "01 April",
                                         "01 July",
                                         "01 October"))) {
    stop("The beginning of a quarter must be the first day of either January, ",
         "April, September or December")
  }

  paste0(as.character(lubridate::month(first_day - months(3),
                                       label = TRUE,
                                       abbr = FALSE)),
         " to ",
         format(zoo::as.yearmon(first_day - months(1)),"%B %Y"))
}
