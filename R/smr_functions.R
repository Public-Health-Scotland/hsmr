smr_wrangling <- function(smr01, gro, pdiags, postcode, morbs){

  ### 1 - Match deaths data to SMR01 ----
  # Remove duplicate records on link_no
  # The deaths file is matched on to SMR01 by link_no,
  # therefore link_no needs to be unique
  gro %<>%
    distinct(link_no, .keep_all = TRUE)

  # Match deaths data on to SMR01 data
  smr01 %<>%
    left_join(gro, by = "link_no") %>%

    # Sort data by link_no, cis_marker, adm_date and dis_date as per guidance
    arrange(link_no, cis_marker, admission_date, discharge_date)


  ### 2 - Basic SMR01 processing ----

  # Create the following variables:
  # death_inhosp = 1 if the patient died in hospital during that episode of care
  # dthdays      = the number of days from admission till death
  # death30      = 1 if the patient died within 30 days of admission date
  # quarter_name = quarter name in text form
  # quarter      = quarter in number form (quarter 1 = Jan - Mar 2011)
  # location     = recodes some hospital codes for combined sites
  # diag1_4      = main condition ICD-10 code to 4 digits
  # diagx        = ICD-10 code to 3 and 4 digits, separated by an underscore
  # pdiag_grp    = matches the primary diagnosis group on the 4-digit ICD-10 code
  # wcomorbsx    = matches the charlson index weighting if the relevant ICD-10
  #                codes are present in any of the five "other diagnosis"
  #                positions
  # comorbs_sum  = sum of the wcomorbsx values across the episode

  smr01 %<>%
    mutate(death_inhosp = if_else(between(as.numeric(discharge_type), 40, 49),
                                  1, 0),
           dthdays = interval(admission_date, date_of_death) / days(1),
           death30 = case_when(
             between(dthdays, 0, 30) ~ 1,
             TRUE ~ 0),
           quarter_name = paste0(year, "Q", quarter),
           quarter = as.numeric(as.factor(quarter_name)),
           diag1_4 = substr(main_condition, 1, 4),
           diag2 = paste(substr(other_condition_1, 1, 3),
                         substr(other_condition_1, 1, 4),
                         sep = "_"),
           diag3 = paste(substr(other_condition_2, 1, 3),
                         substr(other_condition_2, 1, 4),
                         sep = "_"),
           diag4 = paste(substr(other_condition_3, 1, 3),
                         substr(other_condition_3, 1, 4),
                         sep = "_"),
           diag5 = paste(substr(other_condition_4, 1, 3),
                         substr(other_condition_4, 1, 4),
                         sep = "_"),
           diag6 = paste(substr(other_condition_5, 1, 3),
                         substr(other_condition_5, 1, 4),
                         sep = "_")) %>%

    # Create the pdiag_grp and wcomorbsx variables using joins to the z_morbs
    # dataset
    left_join(select(pdiags,
                     pdiag_grp = shmi_diagnosis_group,
                     diag1_4),
              by = "diag1_4") %>%

    # Fuzzy joins add the (in this case, not needed) joining variable by default,
    # so append these with "_z" so they can be easily removed afterwards
    fuzzy_left_join(select(morbs, wcomorbs1 = wmorbs, diag2_z = diag),
                    by = c("diag2" = "diag2_z"),
                    match_fun = str_detect) %>%
    fuzzy_left_join(select(morbs, wcomorbs2 = wmorbs, diag3_z = diag),
                    by = c("diag3" = "diag3_z"),
                    match_fun = str_detect) %>%
    fuzzy_left_join(select(morbs, wcomorbs3 = wmorbs, diag4_z = diag),
                    by = c("diag4" = "diag4_z"),
                    match_fun = str_detect) %>%
    fuzzy_left_join(select(morbs, wcomorbs4 = wmorbs, diag5_z = diag),
                    by = c("diag5" = "diag5_z"),
                    match_fun = str_detect) %>%
    fuzzy_left_join(select(morbs, wcomorbs5 = wmorbs, diag6_z = diag),
                    by = c("diag6" = "diag6_z"),
                    match_fun = str_detect) %>%

    # Remove joining variables
    select(-ends_with("_z")) %>%

    # Replace cases with no match with zero
    replace_na(list(wcomorbs1 = 0,
                    wcomorbs2 = 0,
                    wcomorbs3 = 0,
                    wcomorbs4 = 0,
                    wcomorbs5 = 0)) %>%
    mutate(wcomorbs2 = replace(wcomorbs2,
                               wcomorbs2 == wcomorbs1,
                               0),
           wcomorbs3 = replace(wcomorbs3,
                               wcomorbs3 == wcomorbs1 | wcomorbs3 == wcomorbs2,
                               0),
           wcomorbs4 = replace(wcomorbs4,
                               wcomorbs4 == wcomorbs1 | wcomorbs4 == wcomorbs2 |
                                 wcomorbs4 == wcomorbs3,
                               0),
           wcomorbs5 = replace(wcomorbs5,
                               wcomorbs5 == wcomorbs1 | wcomorbs5 == wcomorbs2 |
                                 wcomorbs5 == wcomorbs3 |
                                 wcomorbs5 == wcomorbs4,
                               0)) %>%
    mutate(comorbs_sum = rowSums(select(., starts_with("wcomorbs")))) %>%

    # Create two further variables at CIS level:
    # epinum = the episode number for each individual episode within the CIS
    # death_inhosp_max = 1 if the patient died in hospital during any episode of
    # the CIS
    group_by(link_no, cis_marker) %>%
    mutate(epinum = row_number(),
           death_inhosp_max = max(death_inhosp)) %>%
    ungroup() %>%

    # Sort data as per guidance and remove variables no longer required
    arrange(link_no, cis_marker, admission_date, discharge_date) %>%
    select(-contains("condition"), -starts_with("wcomorbs"), -quarter_name)

  ### 4 - SIMD ----

  # Fix formatting of postcode variable
  smr01 %<>%

    # First remove all spaces from postcode variable
    mutate(postcode = gsub("\\s", "", postcode),

           # Then add space (or spaces) at appropriate juncture (depending on
           # the number of characters) to get the postcode into 7-character
           # format
           postcode = case_when(
             is.na(postcode) ~ NA_character_,
             str_length(postcode) == 5 ~ sub("(.{2})", "\\1  ", postcode),
             str_length(postcode) == 6 ~ sub("(.{3})", "\\1 ", postcode),
             TRUE ~ postcode
           )) %>%

    # Join to the postcode lookup
    left_join(postcode, by = "postcode") %>%

    # Assign the appropriate SIMD value to a patient depending on the year they
    # were admitted
    mutate(simd = case_when(
      year >= 2014 ~ simd_2016,
      year < 2014 ~ simd_2012
    )) %>%

    # Remove the not needed year-specific SIMD variables
    select(-c(simd_2012, simd_2016))

  return(smr01)

}


smr_pmorbs <- function(smr01, smr01_minus5, morbs){

  ### 1 - Creating Prior Morbidities
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
    filter(epinum == 1) %>%
    mutate(n_emerg = 0)


  # Convert back to a data.table for the number of previous emergency
  # admissions
  smr01_minus5 <- data.table(smr01_minus5)


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
    select(link_no, cis_marker, pmorbs1_sum, pmorbs5_sum, n_emerg)

  # Join smr01_minus5 on to the main tibble
  smr01 %<>%
    left_join(smr01_minus5, by = c("link_no", "cis_marker"))


}


smr_model <- function(smr01, base_start, base_end, index){

  ### 5 - Create patient level file ----

  # Select first episode of final CIS for each patient
  smr01 %<>%
    group_by(link_no, quarter) %>%
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



  ### SECTION 4 - MODELLING ----

  # Create subset of data for modelling
  z_data_lr <- smr01 %>%

    # Select baseline period rows
    filter(quarter <= 12) %>%

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



}


smr_data <- function(smr01, index){

  ### SECTION 5 - CREATE MINIMAL TIDY DATASET ----

  ### 1 - Create Scotland-level aggregation ----

  z_hsmr_scot <- smr01 %>%
    group_by(quarter) %>%
    summarise(deaths = sum(death30),
              pred   = sum(pred_eq),
              pats   = length(death30)) %>%
    mutate(smr           = deaths/pred,
           crd_rate      = (deaths/pats) * 100,
           location_type = "Scotland",
           location      = "Scot") %>%
    ungroup()


  ### 2 - Create Hospital-level aggregation ----

  z_hsmr_hosp <- smr01 %>%
    group_by(quarter, location) %>%
    summarise(deaths = sum(death30),
              pred   = sum(pred_eq),
              pats   = length(death30)) %>%
    mutate(smr           = deaths/pred,
           crd_rate      = (deaths/pats) * 100,
           location_type = "hospital") %>%
    ungroup() #%>%

  # TO DO: NEED TO FILTER ON PUBLISHED HOSPITALS
  # filter(location %in% )


  ### 3 - Create HB-level aggregation ----

  z_hsmr_hb <- smr01 %>%
    group_by(quarter, hbtreat_currentdate) %>%
    summarise(deaths = sum(death30),
              pred   = sum(pred_eq),
              pats   = length(death30)) %>%
    mutate(smr           = deaths/pred,
           crd_rate      = (deaths/pats) * 100,
           location_type = "NHS Board") %>%
    ungroup() %>%
    rename(location = hbtreat_currentdate)


  ### 4 - Merge dataframes and calculate regression line ----

  # Merge data and match on location name
  smr_data <- bind_rows(z_hsmr_scot, z_hsmr_hosp, z_hsmr_hb) %>%
    left_join(z_hospitals, by = "location") %>%
    drop_na(location_name) %>%

    # Create quarter variable used in linear model - every data point in the first
    # year is considered to come from one time point (baseline period)
    mutate(quarter_reg = if_else(quarter <= 12, 0, quarter - 12))

  # Run linear regression
  z_reg_line <- lm(smr ~ quarter_reg * location_name, data = smr_data)

  # Create reg variable of predicted values
  smr_data %<>%
    mutate(reg = predict(z_reg_line, ., type = "response"))

}











### 5 - Save data ----

# Create data folder and save smr_data as an RDA file
devtools::use_data(smr_data)

# Tidy workspace
rm(smr_data)
rm(list = ls(pattern = "^z"))

### END OF SCRIPT ###
