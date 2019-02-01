#' @title Prior-morbidities Wrangling for HSMR data
#'
#' @description Creates the remainder of necessary variables to be used in the
#' HSMR model.
#'
#'
#' @details \code{smr_pmorbs} expects a \code{tibble} of data extracted from
#' SMR01 that has already been through \code{smr_wrangling}.
#' It also expects a \code{tibble} of data extracted from SMR01 covering a
#' time-period that begins five years prior to that of the data in @param smr01.
#' This is so that the function is able to calculate the Charlson Index for
#' Comorbidities weighting for the previous five years.
#' It also expects a \code{tibble} for the Charlson Index lookups.
#'
#'
#' @param smr01 Input tibble for admissions, see details.
#' @param smr01_minus5 Input tibble for admissions going back five years, see
#' details.
#' @param morbs Input tibble for the charlson index for comorbidities lookup.
#'
#'
#' @examples
#'
#'
#' @export


smr_pmorbs <- function(smr01, smr01_minus5, morbs){

  ### 1 - Error handling ----

  if(!tibble::is_tibble(smr01) | !tibble::is_tibble(smr01_minus5) |
     !tibble::is_tibble(morbs)) {

    stop(paste0("All arguments provided to the function ",
                "must be in tibble format. Verify whether ",
                "an object is a tibble or not with ",
                "the tibble::is_tibble() function"))
  }

  if(!all(c("date_of_death", "dthdays", "death30", "diag1_4", "diag2",
            "diag3", "diag4", "diag5", "diag6", "pdiag_grp", "comorbs_sum",
            "epinum", "death_inhosp_max", "simd") %in% names(smr01))){

    stop(paste0("smr01 object must be objected returned from smr_wrangling()",
                " function."))
  }

  if(!all(c("link_no", "admission_date", "discharge_date",
            "old_smr1_tadm_code", "cis_marker") %in% names(smr01_minus5))){

    stop(paste0("smr01_minus5 object doesn't contain all of the required ",
                "variables. Must contain:
                link_no
                admission_date
                discharge_date
                old_smr1_tadm_code
                cis_marker"))
  }

  if(!is.numeric(smr01$link_no)){

    stop(paste0("Link_no must be a numeric"))

  }

  if(!is.numeric(smr01$cis_marker)){

    stop(paste0("cis_marker must be a numeric"))

  }

  if(!is.POSIXct(smr01$admission_date)){

    stop(paste0("Admission_date variable must be POSIXct of format",
                " %Y-%m-%d"))

  }

  if(!is.POSIXct(smr01$discharge_date)){

    stop(paste0("Discharge_date variable must be POSIXct of format",
                " %Y-%m-%d"))

  }

  ### 2 - Creating Prior Morbidities ----
  # Vector of unique link numbers used for filtering below
  z_unique_id <- smr01 %>%
    distinct(link_no) %>%
    pull(link_no)

  # Create the following variables:
  # diag1   = ICD10 code for main condition to 3 and 4 digits, separated by an
  #           underscore
  # pmorbs  = Charlson Index grouping (1-17) for main condition (0 if none apply)
  # pmorbs5_1 to pmorbs1_17 = initialise empty vectors for use in loop below
  # n_emerg                 = initialise empty vector for use in loop below

  smr01_minus5 %<>%
    mutate(diag1 = paste(substr(main_condition, 1, 3),
                         substr(main_condition, 1, 4),
                         sep = "_")) %>%

    # Create the pmorbs variable using a join to the z_morbs dataset
    fuzzy_left_join(select(morbs, pmorbs = morb, diag1_z = diag),
                    by = c("diag1" = "diag1_z"),
                    match_fun = str_detect) %>%

    # Remove the joining variable
    select(-ends_with("_z")) %>%

    # Replace cases with no match with zero
    replace_na(list(pmorbs = 0)) %>%
    mutate(pmorbs5_1  = 0,
           pmorbs5_2  = 0,
           pmorbs5_3  = 0,
           pmorbs5_4  = 0,
           pmorbs5_5  = 0,
           pmorbs5_6  = 0,
           pmorbs5_7  = 0,
           pmorbs5_8  = 0,
           pmorbs5_9  = 0,
           pmorbs5_10 = 0,
           pmorbs5_11 = 0,
           pmorbs5_12 = 0,
           pmorbs5_13 = 0,
           pmorbs5_14 = 0,
           pmorbs5_15 = 0,
           pmorbs5_16 = 0,
           pmorbs5_17 = 0,
           pmorbs1_1  = 0,
           pmorbs1_2  = 0,
           pmorbs1_3  = 0,
           pmorbs1_4  = 0,
           pmorbs1_5  = 0,
           pmorbs1_6  = 0,
           pmorbs1_7  = 0,
           pmorbs1_8  = 0,
           pmorbs1_9  = 0,
           pmorbs1_10 = 0,
           pmorbs1_11 = 0,
           pmorbs1_12 = 0,
           pmorbs1_13 = 0,
           pmorbs1_14 = 0,
           pmorbs1_15 = 0,
           pmorbs1_16 = 0,
           pmorbs1_17 = 0,
           n_emerg    = 0) %>%

    # In order to increase the efficiency of the following for loop:
    # Only keep records with link numbers which appear in the main extract
    # (z_smr01)

    filter(link_no %in% z_unique_id) %>%

    # In order to increase the efficiency of the following for loop:
    # Keep all records after the start date and only keep records before the
    # start date
    # which have a valid Charlson Index grouping

    filter(admission_date >= z_start_date_l |
             (admission_date < z_start_date_l & pmorbs != 0))

  # For every row in the pmorbs extract, look at each of the prior 50 rows and
  # IF the previous episode belongs to the same person
  # AND the admission date on the episode is after the start date
  # AND the pmorbs value belongs to one of the Charlson index groups
  # AND the time between the two episodes is either 5 or 1 year(s)
  # THEN assign the correct Charlson Index weighting. These weightings are saved
  # in the 34 (pmorbs5_1 to pmorbs1_17) vectors initiliased above.

  # NOTE: This section of code uses the data.table package rather than dplyr

  # convert tibble to data.table format
  smr01_minus5 <- data.table(smr01_minus5)

  for(i in 1:50) {

    # 1:50 because the 95th percentile of episode counts per patient was 51

    # Pre-calculating several variables so this only has to be done once per
    # iteration and doesn't have to be repeated for every group
    # old_admission = number of days between current record and previous ith
    #                 admission
    # old_pmorbs    = the pmorbs group the ith previous record is assigned to
    # old_link      = the link number of the ith previous record
    smr01_minus5[, `:=` (old_admission = (admission_date -
                                            shift(admission_date, i))/60/60/24,
                         old_pmorbs = shift(pmorbs, i),
                         old_link = shift(link_no, i))]

    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 1  &
                   old_admission <= 1825 & old_link == link_no, pmorbs5_1 := 5]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 2 &
                   old_admission <= 1825 & old_link == link_no, pmorbs5_2 := 11]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 3 &
                   old_admission <= 1825 & old_link == link_no, pmorbs5_3 := 13]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 4 &
                   old_admission <= 1825 & old_link == link_no, pmorbs5_4 := 4]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 5 &
                   old_admission <= 1825 & old_link == link_no, pmorbs5_5 := 14]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 6 &
                   old_admission <= 1825 & old_link == link_no, pmorbs5_6 := 3]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 7 &
                   old_admission <= 1825 & old_link == link_no, pmorbs5_7 := 8]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 8 &
                   old_admission <= 1825 & old_link == link_no, pmorbs5_8 := 9]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 9 &
                   old_admission <= 1825 & old_link == link_no, pmorbs5_9 := 6]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 10 &
                   old_admission <= 1825 & old_link == link_no, pmorbs5_10 := 4]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 11 &
                   old_admission <= 1825 & old_link == link_no, pmorbs5_11 := 8]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 12 &
                   old_admission <= 1825 & old_link == link_no, pmorbs5_12 := -1]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 13 &
                   old_admission <= 1825 & old_link == link_no, pmorbs5_13 := 1]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 14 &
                   old_admission <= 1825 & old_link == link_no, pmorbs5_14 := 10]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 15 &
                   old_admission <= 1825 & old_link == link_no, pmorbs5_15 := 14]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 16 &
                   old_admission <= 1825 & old_link == link_no, pmorbs5_16 := 18]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 17 &
                   old_admission <= 1825 & old_link == link_no, pmorbs5_17 := 2]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 1 &
                   old_admission <= 365 & old_link == link_no, pmorbs1_1 := 5]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 2 &
                   old_admission <= 365 & old_link == link_no, pmorbs1_2 := 11]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 3 &
                   old_admission <= 365 & old_link == link_no, pmorbs1_3 := 13]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 4 &
                   old_admission <= 365 & old_link == link_no, pmorbs1_4 := 4]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 5 &
                   old_admission <= 365 & old_link == link_no, pmorbs1_5 := 14]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 6 &
                   old_admission <= 365 & old_link == link_no, pmorbs1_6 := 3]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 7 &
                   old_admission <= 365 & old_link == link_no, pmorbs1_7 := 8]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 8 &
                   old_admission <= 365 & old_link == link_no, pmorbs1_8 := 9]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 9 &
                   old_admission <= 365 & old_link == link_no, pmorbs1_9 := 6]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 10 &
                   old_admission <= 365 & old_link == link_no, pmorbs1_10 := 4]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 11 &
                   old_admission <= 365 & old_link == link_no, pmorbs1_11 := 8]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 12 &
                   old_admission <= 365 & old_link == link_no, pmorbs1_12 := -1]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 13 &
                   old_admission <= 365 & old_link == link_no, pmorbs1_13 := 1]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 14 &
                   old_admission <= 365 & old_link == link_no, pmorbs1_14 := 10]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 15 &
                   old_admission <= 365 & old_link == link_no, pmorbs1_15 := 14]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 16 &
                   old_admission <= 365 & old_link == link_no, pmorbs1_16 := 18]
    smr01_minus5[admission_date >= z_start_date_l & old_pmorbs == 17 &
                   old_admission <= 365 & old_link == link_no, pmorbs1_17 := 2]

  }


  # Calculate the sum of the Charlson Index weightings for each CIS, for both 1
  # and 5 years prior to admission
  # smr01_minus5 will be automatically converted back to a tibble here
  smr01_minus5 %<>%
    mutate(pmorbs1_sum = rowSums(select(., starts_with("pmorbs1")))) %>%
    mutate(pmorbs5_sum = rowSums(select(., starts_with("pmorbs5")))) %>%
    group_by(link_no, cis_marker) %>%
    mutate_at(vars(ends_with("_sum")), max) %>%

    # Add epinum to filter down to first episode within a CIS for the
    # calculation of the number of previous emergency admissions
    mutate(epinum = row_number()) %>%
    ungroup() %>%
    #filter(epinum == 1) %>%
    mutate(n_emerg = 0)


  # Convert back to a data.table for the number of previous emergency
  # admissions
  smr01_minus5 <- data.table(smr01_minus5)


  ### 3 - Previous emergency admissions ----

  # For every row in the pmorbs extract, look at each of the prior 50 rows and
  # IF the previous episode belongs to the same person
  # AND the time between the two episodes is 1 year
  # AND the previous episode is an emergency admission
  # THEN increase the number of emergency admissions by one in the n_emerg
  # vector initiliased above.

  for (i in 1:50) {

    # 1:50 because the 95th percentile of episode counts per patient was 51

    smr01_minus5[, `:=`(old_admission = (admission_date -
                                           shift(admission_date, i))/60/60/24,
                        old_tadm = shift(old_smr1_tadm_code, i),
                        old_link = shift(link_no, i))]

    smr01_minus5[admission_date >= z_start_date_l & old_link == link_no &
                   old_tadm >= 4 & old_admission <= 365, n_emerg := n_emerg + 1]

  }

  # Select required variables from smr01_minus5
  smr01_minus5 %<>%
    group_by(link_no, cis_marker) %>%
    mutate(n_emerg = first(n_emerg)) %>%
    ungroup() %>%
    filter(epinum == 1) %>%
    select(link_no, cis_marker, pmorbs1_sum, pmorbs5_sum, n_emerg)

  # Join smr01_minus5 on to the main tibble
  smr01 %<>%
    left_join(smr01_minus5, by = c("link_no", "cis_marker"))

  return(smr01)


}

### END OF SCRIPT ###
