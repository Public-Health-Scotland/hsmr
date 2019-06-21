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
#' @param index A character string specifying whether the data are to be
#' produced monthly, quarterly or annually. Valid options are 'M', 'Q' and 'Y'.
#' @param hospital_lookup A lookup tibble containing hospital names and
#' location codes.
#'
#' @return If the class is not initiated correctly, nothing is returned.
#'
#' @importFrom dplyr %>%
#' @importFrom magrittr %<>%
#'
#' @export


smr_data <- function(smr01, index = c("M", "Q", "Y"), hospital_lookup) {

  index <- match.arg(index)

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


  smr01 %<>%
    dplyr::rename(period_name = period) %>%
    tidylog::mutate(period = as.numeric(as.factor(period_name)))


  ### 2 - Create Scotland-level aggregation ----

  hsmr_scot <- smr01 %>%
    tidylog::group_by(period) %>%
    tidylog::summarise(deaths = sum(death30),
                       pred   = sum(pred_eq),
                       pats   = length(death30)) %>%
    tidylog::mutate(smr                 = deaths/pred,
                    crd_rate            = (deaths/pats) * 100,
                    location_type       = "Scotland",
                    location            = "Scot",
                    hbtreat_currentdate = "Scotland") %>%
    dplyr::ungroup()


  ### 3 - Create Hospital-level aggregation ----

  hsmr_hosp <- smr01 %>%
    tidylog::mutate(location = case_when(
      location == "C206H" ~ "C418H",
      location == "G207H" ~ "G107H",
      location == "G306H" ~ "G405H",
      location == "G516H" ~ "G405H",
      location == "Y104H" ~ "Y146H",
      location == "S08000018" ~ "S08000029",
      location == "S08000027" ~ "S08000030",
      TRUE ~ location
    )) %>%
    tidylog::group_by(period, hbtreat_currentdate, location) %>%
    tidylog::summarise(deaths = sum(death30),
                       pred   = sum(pred_eq),
                       pats   = length(death30)) %>%
    tidylog::mutate(smr           = deaths/pred,
                    crd_rate      = (deaths/pats) * 100,
                    location_type = "hospital") %>%
    dplyr::ungroup()


  ### 4 - Create HB-level aggregation ----

  hsmr_hb <- smr01 %>%
    tidylog::group_by(period, hbtreat_currentdate) %>%
    tidylog::summarise(deaths = sum(death30),
                       pred   = sum(pred_eq),
                       pats   = length(death30)) %>%
    tidylog::mutate(smr           = deaths/pred,
                    crd_rate      = (deaths/pats) * 100,
                    location_type = "NHS Board",
                    location      = hbtreat_currentdate) %>%
    dplyr::ungroup()


  ### 5 - Merge dataframes and calculate regression line ----

  # Merge data and match on location name
  smr_data <- dplyr::bind_rows(hsmr_scot, hsmr_hosp, hsmr_hb) %>%
    tidylog::left_join(hospital_lookup, by = "location") %>%
    tidylog::filter(!is.na(location_name)) %>%
    tidylog::mutate(hbtreat_currentdate = case_when(
      hbtreat_currentdate == "S08000018" ~ "S08000029",
      hbtreat_currentdate == "S08000027" ~ "S08000030",
      TRUE ~ hbtreat_currentdate),
      location_name = case_when(location == "C418H" ~
                                  "Royal Alexandria/Vale of Leven",
                                hbtreat_currentdate == "S08100001" ~
                                  "Golden Jubilee",
                                TRUE ~ location_name),
      completeness_date = hsmr::submission_deadline(end_date),
      period_label = yr(end_date)) %>%
    rename(hb = hbtreat_currentdate)

  if (index == "Y"){

    smr_data %<>%
      tidylog::group_by(period) %>%
      tidylog::mutate(death_scot = max(deaths),
                      pred_scot = max(pred),
                      pats_scot = max(pats),
                      smr_scot = death_scot/pred_scot) %>%
      dplyr::ungroup() %>%
      tidylog::mutate(smr = smr/smr_scot,
                      pred = deaths/smr) %>%

      # Locations with an SMR of zero produce 'NaN' for pred, so replace those
      # values of pred with zero
      tidylog::mutate(pred = replace(pred, smr == 0, 0))

  }

  ### 6 - Return data ----

  return(smr_data)

}

### END OF SCRIPT ###
