#' @title Create text for Funnel Plot outliers
#'
#' @description
#'
#' test
#'
#'
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
#'
#' @export
funnel_text <- function(smr_data){

  smr_data %<>%
    filter(period == 3 & smr > ucl)

  n_hosps <- nrow(smr_data)

  if(n_hosps == 0){

    output <- paste0("* There are no hospitals above the ",
                     "upper control limit of 3 ",
                     "standard deviations above the Scottish average.")

  }

  if(n_hosps > 1){

    output <- paste0("* There are ", n_hosps, " above the upper control limit ",
                     "of 3 standard deviations above the Scottish average. ",
                     "They are: ", paste0(smr_data$location_name,
                                          collapse = ", "))

  }

  return(output)
}
