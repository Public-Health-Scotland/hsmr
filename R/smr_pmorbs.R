#' @title Prior-morbidities Wrangling for HSMR data
#'
#' @description Creates the remainder of necessary variables to be used in the
#' HSMR model.
#'
#'
#' @details \code{smr_pmorbs} expects a \code{tibble} of data extracted from
#' SMR01 that has already been through \code{\link{smr_wrangling}}.
#' It also expects a \code{tibble} of data extracted from SMR01 covering a
#' time-period that begins five years prior to that of the data in \code{smr01}.
#' This is so that the function is able to calculate the Elixhauser weighted grouping for
#' Comorbidities weighting for the previous five years.
#' It also expects a \code{tibble} for the Charlson Index lookups.
#'
#'
#' @param smr01 Input tibble for admissions, see details.
#' @param smr01_minus5 Input tibble for admissions going back five years, see
#' details.
#' @param morbs Input tibble for the Elixhauser for comorbidities lookup.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @import data.table


smr_pmorbs <- function(smr01, smr01_minus5){

  ### 1 - Error handling ----

  if(!tibble::is_tibble(smr01) | 
     !tibble::is_tibble(smr01_minus5)) {
     

    stop(paste0("All arguments provided to the function ",
                "must be in tibble format. Verify whether ",
                "an object is a tibble or not with ",
                "the tibble::is_tibble() function"))
  }

  if(!all(c("date_of_death", "dthdays", "death30", "diag1_4", "diag2",
            "diag3", "diag4", "diag5", "diag6", "pdiag_grp", "comorbs_sum",
            "epinum", "death_inhosp_max", "simd") %in% names(smr01))){

    stop(paste0("smr01 must be the object returned from the smr_wrangling()",
                " function."))
  }

  if(!all(c("link_no", "admission_date", "discharge_date",
            "admission_type", "cis_marker") %in% names(smr01_minus5))){

    stop(paste0("smr01_minus5 object doesn't contain all of the required ",
                "variables. Must contain:
                link_no
                admission_date
                discharge_date
                admission_type
                cis_marker"))
  }

  if(!is.numeric(smr01$link_no)){

    stop(paste0("Link_no must be a numeric"))

  }

  if(!is.numeric(smr01$cis_marker)){

    stop(paste0("cis_marker must be a numeric"))

  }

  if(!lubridate::is.POSIXct(smr01$admission_date)){

    stop(paste0("Admission_date variable must be POSIXct of format",
                " %Y-%m-%d"))

  }

  if(!lubridate::is.POSIXct(smr01$discharge_date)){

    stop(paste0("Discharge_date variable must be POSIXct of format",
                " %Y-%m-%d"))

  }

  setDT(smr01)
  setDT(smr01_minus5)
  
  # smr01_minus5 contains data for the current 3year training period and the 5 years before that
  # Convert to data.table and make sure dates are Date type
  smr01_minus5[, admission_date := as.Date(admission_date)]
  smr01_minus5[, epinum := seq_len(.N), by = .(link_no, cis_marker)] # number episodes within cis
  
  #  ensure data is sorted
  setorder(smr01_minus5, link_no, admission_date)
  
  # from the smr01_minus5 data retrieve admissions for the current three year publication period
  current_admissions <- smr01_minus5[admission_date >= start_date & admission_date <= end_date]
  current_admissions[, row_id := .I]  # temporary ID to track original rows and for data joins
  current_admissions[, five_year_cutoff := admission_date - 1825]
  current_admissions[, one_year_cutoff := admission_date - 365]
  
  # Rename 'admission_date' in smr01_minus5 to 'prior_admission_date'
  setnames(smr01_minus5, "admission_date", "prior_admission_date")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Prior morbs 5 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # From smr01_minus5 which holds all prior admissions, retrieve relevant prior
  # admissions in the past 5 years
  # "Find rows in smr01_minus5 that match each row in current_admissions based 
  # on the conditions in the "on" argument"
  # i.e For each row in current_admissions, find all rows in smr01_minus5 such 
  # that is for same link_no (smr01_minus5$link_no == current_admissions$link_no)
  # and the prior_admission_date of the prior admission is within the 5 years 
  # before the current admission date
  # The row_id from current admission will be part of prior_admissions_5 also 
  # when joined so we can know what current admission, the matched prior admission 
  # is for
  prior_admissions_5 <- smr01_minus5[
    current_admissions,
    on = .(link_no,
           prior_admission_date < admission_date, prior_admission_date >= five_year_cutoff),
    allow.cartesian = TRUE,
    nomatch = NULL,
    .(link_no,prior_admission_date = x.prior_admission_date,main_condition,admission_type,row_id)
  ]
  
  # retrieve main conditions for each prior admission within past five years
  # for each row_id, Aggregate the matched main_conditions in the past 5 years into a list
  prior_conditions_5_dt <- prior_admissions_5[, .(prior_conditions_5 = list(unique(main_condition))), by = row_id]
  
  # calculate the morbidity scores for each set of identified conditions
  pmorb5_scores <-   prior_conditions_5_dt %>%
    unnest(prior_conditions_5) %>% 
    comorbidity(id = "row_id", code = "prior_conditions_5", map = "elixhauser_icd10_quan", assign0 = FALSE) %>%
    mutate(pmorbs_5 = score(., weights = "vw", assign0 = TRUE)) %>% 
    select(row_id,pmorbs_5)
  
  # append the scores to the prior conditions5 data table
  prior_conditions_5_dt <- prior_conditions_5_dt[pmorb5_scores, on = "row_id"]
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Prior morbs 1 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  prior_admissions_1 <- smr01_minus5[
    current_admissions,
    on = .(link_no,
           prior_admission_date < admission_date, prior_admission_date >= one_year_cutoff),
    allow.cartesian = TRUE,
    nomatch = NULL,
    .(link_no,prior_admission_date = x.prior_admission_date,main_condition,admission_type,row_id)
  ]
  
  # retrieve main conditions for each prior admission within the past one year
  # for each row_id, Aggregate the matched main_conditions in the past one year into a list
  prior_conditions_1_dt <- prior_admissions_1[, .(prior_conditions_1 = list(unique(main_condition))), by = row_id]
  
  # calculate the morbidity scores for each set of identified conditions
  pmorb1_scores <-   prior_conditions_1_dt %>%
    unnest(prior_conditions_1) %>% 
    comorbidity(id = "row_id", code = "prior_conditions_1", map = "elixhauser_icd10_quan", assign0 = FALSE) %>%
    mutate(pmorbs_1 = score(., weights = "vw", assign0 = TRUE)) %>% 
    select(row_id,pmorbs_1)
  
  # append the scores to the prior conditions5 data table
  prior_conditions_1_dt <- prior_conditions_1_dt[pmorb1_scores, on = "row_id"]
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Number of prior emergency admissions ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Filter smr01_minus5 for emergency admissions (where starting episode of the cis is an emergency admission )
  smr01_minus5_emerg <- smr01_minus5[
    epinum == 1 & admission_type %in% c(18, 20:48)
  ]
  
  # For each link_no in every row in current_admissions, find all rows in smr01_minus5_emerg for that link 
  # within a one year period
  prior_emerg_admissions_1 <- smr01_minus5_emerg[
    current_admissions,
    on = .(link_no,
           prior_admission_date < admission_date, prior_admission_date >= one_year_cutoff),
    allow.cartesian = TRUE,
    nomatch = NULL,
    .(link_no,prior_admission_date = x.prior_admission_date,main_condition,admission_type,row_id)
  ]
  
  # retrieve aggregate counts of previous emergency admissions for each row
  prior_emerg_counts_1_dt <- prior_emerg_admissions_1[, .(n_emerg = .N), by = row_id]
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Merge new pmorbs1, pmorbs5 and n_emerg back to current admissions ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Merge back to current_admissions
  current_admissions <- merge(current_admissions, prior_conditions_5_dt, by = "row_id", all.x = TRUE)
  current_admissions <- merge(current_admissions, prior_conditions_1_dt, by = "row_id", all.x = TRUE)
  current_admissions <- merge(current_admissions, prior_emerg_counts_1_dt, by = "row_id", all.x = TRUE)
  
  #TODO uncomment this line if i want the episode level output for inspections
  #return(current_admissions) 
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Aggregating to cis and merge to smr01 data  ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # for each cis, retrieve prior morb scores and number of emergencies at the start of the cis, i.e from first episode in cis
  # the smr-model is built on first episode in CIS
  current_admissions <- current_admissions[, .(
    pmorbs_1 = pmorbs_1[1],  pmorbs_5 = pmorbs_5[1], 
    n_emerg = n_emerg[1]),
    by = .(link_no, cis_marker)]
  
  # merge to smr01 
  smr01 <- merge(smr01, current_admissions[, .(link_no,cis_marker, pmorbs_1, pmorbs_5, n_emerg)],
                 by = c("link_no", "cis_marker"),
                 all.x = TRUE)
  
  # Fill NAs with zero
  smr01[, `:=`(
    pmorbs_1 = fifelse(is.na(pmorbs_1), 0, pmorbs_1),
    pmorbs_5 = fifelse(is.na(pmorbs_5), 0, pmorbs_5),
    n_emerg = fifelse(is.na(n_emerg), 0, n_emerg)
  )]
  
  # rename columns to expected names
  setnames(smr01, 
           old = c("pmorbs_1", "pmorbs_5", "n_emerg"), 
           new = c("pmorbs1_sum", "pmorbs5_sum", "n_emerg"))
  
  smr01<- as_tibble(smr01)
  
  return(smr01)
  
}

### END OF SCRIPT ###
