#' @title Logistic Regression model for HSMR
#'
#' @description Carries out risk-adjustment for HSMR data.
#'
#'
#' @details \code{smr_model} expects a \code{tibble} of data extracted from
#' SMR01 that has already been through \code{smr_wrangling} and then
#' \code{smr_pmorbs}. This is the third step of a four-step process. The final
#' step is \code{smr_data}. \code{smr_model} returns the SMR01 data with a
#' probability of death within 30 days of admission appended on to the file.
#'
#'
#' @param smr01 Input tibble for admissions, see details.
#' @param base_start The beginning of the time period for data that is run
#' through the logistic regression model.
#' @param base_end The end of the time period for data that is run
#' through the logistic regression model.
#' @param index To define whether data produced are to be quarterly or annual.
#'
#'
#' @examples
#'
#'
#' @export


smr_model <- function(smr01, base_start, base_end, index = "Q"){

  ### 1 - Error handling ----

  if(!tibble::is_tibble(smr01)){

    stop(paste0("The smr01 argument provided to the function ",
                "must be in tibble format. Verify whether ",
                "an object is a tibble or not with ",
                "the tibble::is_tibble() function"))
  }

  if(!all(c("pmorbs1_sum", "pmorbs5_sum", "n_emerg") %in% names(smr01))){

    stop(paste0("smr01 object must be objected returned from smr_pmorbs()",
                " function."))
  }

  if(index != "Q" & index != "M" & index != "Y"){

    stop(paste0("Invalid argument for Index. Index argument can only take the",
                " values Q, M or Y."))

  }

  if(index == "M"){

    smr01 %<>%
      mutate(month = as.POSIXct(admission_date, format = "%m-%Y")) %>%
      rename(period = month)

  }

  if(index == "Q"){

    smr01 %<>%
      rename(period = quarter)

  }

  if(index == "Y"){

    warning(paste0("Annual HSMRs are only to be produced on a rolling basis. ",
                   "Therefore, data provided to this function MUST cover ",
                   "a period which can be measured in whole years. E.g. ",
                   "January 2011 to December 2014 (4 whole years)."))

    #smr01 %<>%

  }


  ### 2 - Create patient level file ----

  # Select first episode of final CIS for each patient
  smr01 %<>%
    group_by(link_no, period) %>%
    mutate(last_cis = max(cis_marker)) %>%
    ungroup() %>%
    filter(epinum == 1 & cis_marker == last_cis) %>%

    # Remove rows where SIMD, admfgrp and ipdc are missing as variables are
    # required for modelling/predicted values
    drop_na(simd) %>%
    filter(admfgrp %in% 1:6) %>%
    filter(ipdc %in% 1:2) %>%

    # If a patient dies within 30 days of admission in two subsequent quarters
    # then remove the second record to avoid double counting deaths
    filter(!(link_no == c(0, head(link_no, -1)) &
               1 == c(0, head(death30, -1))))



  ### 3 - Logistic Regression ----

  # Create subset of data for modelling
  z_data_lr <- smr01 %>%

    # Select baseline period rows
    filter(admission_date >= base_start & admission_date <= base_end) %>%

    # Select required variables for model
    select(n_emerg, comorbs_sum, pmorbs1_sum, pmorbs5_sum, age_in_years, sex,
           surgmed, pdiag_grp, admfgrp, admgrp, ipdc, simd, death30) %>%

    # Calculate total number of deaths and total number of patients for each
    # combination of variables
    group_by(n_emerg, comorbs_sum, pmorbs1_sum, pmorbs5_sum, age_in_years, sex,
             surgmed, pdiag_grp, admfgrp, admgrp, ipdc, simd) %>%
    summarise(x = sum(death30),
              n = length(death30)) %>%
    ungroup()

  # Run logistic regression
  z_risk_model <- glm(cbind(x, n - x) ~ n_emerg + comorbs_sum + pmorbs1_sum +
                        pmorbs5_sum + age_in_years + factor(sex) +
                        factor(surgmed) + factor(pdiag_grp) + factor(admfgrp) +
                        factor(admgrp) + factor(ipdc) + factor(simd),
                      data = z_data_lr,
                      family = "binomial",
                      model = FALSE,
                      y = FALSE)

  # Delete unnecessary model information using bespoke function in order to retain
  # special class of object for predicted probabilities below
  z_risk_model <- clean_model(z_risk_model)

  smr01 %<>%

    # Calculate predicted probabilities
    mutate(pred_eq = predict.glm(z_risk_model, ., type = "response")) %>%

    # Remove rows with no probability calculated
    drop_na(pred_eq)

  return(smr01)


}

### END OF SCRIPT ###
