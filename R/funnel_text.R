#' @title Create text for Funnel Plot outliers
#'
#' @description
#'
#' The publication summary documents for the HSMR publication has main points
#' relating to outliers on the funnel plot. The \code{funnel_text} function
#' identifies outlying hospitals and returns the text for the publication
#' document.
#'
#'
#' @param smr_data Should the final\code{tibble} output from the \code{smr_data}
#' function.
#'
#'
#' @examples
#' funnel_text(smr_data)
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

  if(n_hosps == 1){

    output <- paste0("* There is one hospital above the upper control limit ",
                     "of 3 standard deviations above the Scottish average",
                     ": ", paste0(smr_data$location_name, " (",
                                  janitor::round_half_up(smr_data$smr, 2), ")",
                                          collapse = ", "))

  }

  if(n_hosps > 1){

    output <- paste0("* There are ", n_hosps, " above the upper control limit ",
                     "of 3 standard deviations above the Scottish average",
                     ": ", paste0(smr_data$location_name, " (",
                                  janitor::round_half_up(smr_data$smr, 2), ")",
                                          collapse = ", "))

  }

  return(output)
}
