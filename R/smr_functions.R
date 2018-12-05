#' @title Clean up logistic regression model
#'
#' @description Strips logistic regression model object of extraneous data that
#' are not required in order to calculate the probability of death within
#' 30 days of admission. This is a necessary step as the object will likely be
#' quite large and take up significant amounts of memory when producing the
#' probabilities.
#'
#'
#' @details \code{clean_model} expects an object of class \code{"glm"}.
#'
#'
#' @param cm Input \code{glm}.
#'
#'
#' @examples
#'
#'
#' @export

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

### END OF SCRIPT ###
