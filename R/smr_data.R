#' @title Produce HSMR dataset
#'
#' @description Returns the minimal tidy data set for the HSMR dataset.
#'
#'
#' @details \code{smr_data} expects a \code{tibble} with data extracted
#' from SMR01 which has already been run through \code{smr_wrangling} and then
#' \code{smr_pmorbs} and finally \code{smr_model}. The data MUST be run through
#' these three functions first as \code{smr_data} is the final step of a
#' four-step process.
#'
#' @param smr01 Input tibble for admissions, see details.
#' @param index To define whether data produced are to be quarterly or annual.
#'
#' @return If the class is not initiated correctly, nothing is returned.
#'
#' @examples
#'
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

  smr01 <- smr01 %>%
    rename(period_name = period) %>%
    mutate(period = as.numeric(as.factor(period_name)))


  ### 2 - Create Scotland-level aggregation ----

  z_hsmr_scot <- smr01 %>%
    group_by(period) %>%
    summarise(deaths = sum(death30),
              pred   = sum(pred_eq),
              pats   = length(death30)) %>%
    mutate(smr           = deaths/pred,
           crd_rate      = (deaths/pats) * 100,
           location_type = "Scotland",
           location      = "Scot") %>%
    ungroup()


  ### 3 - Create Hospital-level aggregation ----

  z_hsmr_hosp <- smr01 %>%
    group_by(period, location) %>%
    summarise(deaths = sum(death30),
              pred   = sum(pred_eq),
              pats   = length(death30)) %>%
    mutate(smr           = deaths/pred,
           crd_rate      = (deaths/pats) * 100,
           location_type = "hospital") %>%
    ungroup() #%>%

  # TO DO: NEED TO FILTER ON PUBLISHED HOSPITALS
  # filter(location %in% )


  ### 4 - Create HB-level aggregation ----

  z_hsmr_hb <- smr01 %>%
    group_by(period, hbtreat_currentdate) %>%
    summarise(deaths = sum(death30),
              pred   = sum(pred_eq),
              pats   = length(death30)) %>%
    mutate(smr           = deaths/pred,
           crd_rate      = (deaths/pats) * 100,
           location_type = "NHS Board") %>%
    ungroup() %>%
    rename(location = hbtreat_currentdate)


  ### 5 - Merge dataframes and calculate regression line ----

  # Merge data and match on location name
  smr_data <- bind_rows(z_hsmr_scot, z_hsmr_hosp, z_hsmr_hb) %>%
    left_join(z_hospitals, by = "location") %>%
    drop_na(location_name)

  if (index == "Y"){

    smr_data %<>%
      group_by(period) %>%
      mutate(death_scot = max(deaths),
             pred_scot = max(pred),
             pats_scot = max(pats),
             smr_scot = death_scot/pred_scot) %>%
      ungroup() %>%
      mutate(smr = smr/smr_scot,
             pred = deaths/smr)

  }

  ### 6 - Return data class ----

  structure(
    list(
      df = smr_data,
      colnames = colnames(smr_data),
      type = colnames(smr_data)[!colnames(smr_data)
                                %in% c("period",	"deaths",	"pred",
                                       "pats",	"smr",	"crd_rate",
                                       "location_type",	"location",
                                       "location_name")]
    ),
    class = "smr_data")

}

### END OF SCRIPT ###
