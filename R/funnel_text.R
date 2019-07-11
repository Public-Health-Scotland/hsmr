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
#' @param indicator indicates whether the function produces text for hospitals
#' sitting above the upper control limit or hospitals sitting below the lower
#' control limit.
#'
#'
#' @examples
#' funnel_text(smr_data, indicator = "above")
#' funnel_text(smr_data, indicator = "below")
#'
#' @export
funnel_text <- function(smr_data, indicator = c("above", "below")){

  match.arg(indicator)

  ### 1 - Error Handling ----
  if(!tibble::is_tibble(smr_data)){

    stop(paste0("The smr_data argument provided to the function ",
                "must be in tibble format. Verify whether ",
                "an object is a tibble or not with ",
                "the tibble::is_tibble() function"))
  }

  if(!all(c("period", "deaths", "pred", "pats", "smr", "crd_rate",
          "location_type", "location", "hb", "location_name",
          "completeness_date", "period_label", "death_scot", "pred_scot",
          "pats_scot", "smr_scot") %in% names(smr_data))){

    stop(paste0("smr_data object must be objected returned from smr_data()",
                " function."))

  }

  if(indicator == "above"){

    smr_data %<>%
      dplyr::filter(period == 3 & smr > ucl)

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

  if(indicator == "below"){

    smr_data %<>%
      dplyr::filter(period == 3 & smr < lcl)

    n_hosps <- nrow(smr_data)

    if(n_hosps == 0){

      output <- paste0("* There are no hospitals below the ",
                       "lower control limit of 3 ",
                       "standard deviations above the Scottish average.")

    }

    if(n_hosps == 1){

      output <- paste0("* There is one hospital below the lower control limit ",
                       "of 3 standard deviations below the Scottish average",
                       ": ", paste0(smr_data$location_name, " (",
                                    janitor::round_half_up(smr_data$smr, 2), ")",
                                    collapse = ", "))

    }

    if(n_hosps > 1){

      output <- paste0("* There are ", n_hosps, " below the lower control limit ",
                       "of 3 standard deviations below the Scottish average",
                       ": ", paste0(smr_data$location_name, " (",
                                    janitor::round_half_up(smr_data$smr, 2), ")",
                                    collapse = ", "))

    }

    return(output)
  }
}
