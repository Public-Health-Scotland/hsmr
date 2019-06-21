#' @title Assign a date to a year
#'
#' @description
#'
#' The \code{yr} functions take the end date of the extract period for the
#' Hospital Standardised Mortality Ratios Publication and returns the latest
#' year in text form.
#'
#' @param end_date The end date of the extract period, supplied with \code{Date}
#'  class.
#'
#' @examples
#' yr(end_date = lubridate::dmy(31032019))
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
