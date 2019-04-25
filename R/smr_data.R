#' @title Produce HSMR dataset
#'
#' @description Returns the minimal tidy data set for the HSMR dataset.
#'
#'
#' @details \code{smr_data} expects a \code{tibble} with data extracted
#' from SMR01 which has already been run through \code{\link{smr_wrangling}},
#' then \code{\link{smr_pmorbs}} and finally \code{\link{smr_model}}. The data
#' must be run through these three functions first as \code{smr_data} is the
#' final step of a four-step process.
#'
#' @param smr01 Input tibble for admissions, see details.
#' @param index To define whether data produced are to be quarterly or annual.
#'
#' @return If the class is not initiated correctly, nothing is returned.
#'
#' @importFrom dplyr %>%
#' @importFrom magrittr %<>%
#'
#' @export


smr_data <- function(smr01, index){

  ### 1 - Error handling ----

  if(!tibble::is_tibble(smr01)){

    stop(paste0("The smr01 argument provided to the function ",
                "must be in tibble format. Verify whether ",
                "an object is a tibble or not with ",
                "the tibble::is_tibble() function"))
  }

  if(!all(c("pred_eq") %in% names(smr01))){

    stop(paste0("smr01 object must be objected returned from smr_model()",
                " function."))
  }

  if(index == "Q"){

    reg_length <- 12

  }

  if(index == "M"){

    reg_length <- 36

  }

  if(index == "Y"){

    reg_length <- 1

  }

  smr01 %<>%
    dplyr::rename(period_name = period) %>%
    dplyr::mutate(period = as.numeric(as.factor(period_name)))


  ### 2 - Create Scotland-level aggregation ----

  hsmr_scot <- smr01 %>%
    dplyr::group_by(period) %>%
    dplyr::summarise(deaths = sum(death30),
                     pred   = sum(pred_eq),
                     pats   = length(death30)) %>%
    dplyr::mutate(smr           = deaths/pred,
                  crd_rate      = (deaths/pats) * 100,
                  location_type = "Scotland",
                  location      = "Scot") %>%
    dplyr::ungroup()


  ### 3 - Create Hospital-level aggregation ----

  hsmr_hosp <- smr01 %>%
    dplyr::group_by(period, location) %>%
    dplyr::summarise(deaths = sum(death30),
                     pred   = sum(pred_eq),
                     pats   = length(death30)) %>%
    dplyr::mutate(smr           = deaths/pred,
                  crd_rate      = (deaths/pats) * 100,
                  location_type = "hospital") %>%
    dplyr::ungroup() #%>%

  # TO DO: NEED TO FILTER ON PUBLISHED HOSPITALS
  # filter(location %in% )


  ### 4 - Create HB-level aggregation ----

  hsmr_hb <- smr01 %>%
    dplyr::group_by(period, hbtreat_currentdate) %>%
    dplyr::summarise(deaths = sum(death30),
                     pred   = sum(pred_eq),
                     pats   = length(death30)) %>%
    dplyr::mutate(smr           = deaths/pred,
                  crd_rate      = (deaths/pats) * 100,
                  location_type = "NHS Board") %>%
    dplyr::ungroup() %>%
    dplyr::rename(location = hbtreat_currentdate)


  ### 5 - Merge dataframes and calculate regression line ----

  # Merge data and match on location name
  smr_data <- dplyr::bind_rows(hsmr_scot, hsmr_hosp, hsmr_hb) %>%
    dplyr::left_join(hospitals, by = "location") %>%
    dplyr::filter(!is.na(location_name))

  if (index == "Y"){

    smr_data %<>%
      dplyr::group_by(period) %>%
      dplyr::mutate(death_scot = max(deaths),
                    pred_scot = max(pred),
                    pats_scot = max(pats),
                    smr_scot = death_scot/pred_scot) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(smr = smr/smr_scot,
                    pred = deaths/smr)

  }

  ### 6 - Return data ----

  return(smr_data)

}

### END OF SCRIPT ###
