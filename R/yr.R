#' @title Assign a date to a year
#'
#' @description
#'
#' The \code{yr} functions take the end date of the extract period for the
#' Hospital Standardised Mortality Ratios Publication and returns the latest
#' year in text form.
#'
#' @param end_date The cut-off date for data to be included in the current HSMR
#' publication, supplied with \code{Date} class. Must be the final day of
#' either March, June, September or December.
#'
#' @examples
#' yr(end_date = lubridate::dmy(31032019))
#' @export
#' @rdname yr
yr <- function(end_date){

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

  paste0(lubridate::month(end_date - lubridate::years(1) + lubridate::days(1),
               label = TRUE, abbr = FALSE),
         " ",
         format(zoo::as.yearmon(end_date - lubridate::years(1) +
                                  lubridate::days(1)),"%Y"),
         " to ",
         format(end_date,"%B %Y"))

}
