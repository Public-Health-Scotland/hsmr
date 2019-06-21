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
#' @rdname yr
yr <- function(end_date){

  paste0(lubridate::month(end_date - lubridate::years(1) + lubridate::days(1),
               label = TRUE, abbr = FALSE),
         " ",
         format(zoo::as.yearmon(end_date - lubridate::years(1) +
                                  lubridate::days(1)),"%Y"),
         " to ",
         format(end_date,"%B %Y"))

}
