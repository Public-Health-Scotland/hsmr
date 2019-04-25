#' @title Initial Wrangling for HSMR data
#'
#' @description Creates the majority of necessary variables to be used in the
#' HSMR model.
#'
#'
#' @details \code{smr_wrangling} expects a \code{tibble} of data extracted from
#' SMR01. It also expects \code{tibbles} of data extracted from the GRO deaths
#' database and lookups for primary diagnosis, postcode (for deprivation) and
#' morbidities (comorbs_sum). This is the first step in a four-step process.
#' The next steps are \code{\link{smr_pmorbs}}, then \code{\link{smr_model}}
#' and finally \code{\link{smr_data}}.
#'
#'
#' @param smr01 Input tibble for admissions, see details.
#' @param gro Input tibble for deaths, see details.
#' @param pdiags Input tibble for primary diagnosis groupings lookup.
#' @param postcode Input tibble for deprivation lookup.
#' @param morbs Input tibble for the charlson index for comorbidities lookup.
#' @param spec Input tibble for for specialty groupings lookup.
#'
#' @importFrom dplyr %>%
#' @importFrom magrittr %<>%
#'
#' @export

smr_wrangling <- function(smr01, gro, pdiags, postcode, morbs, spec) {

  ### 1 - Error handling ----

  if(!tibble::is_tibble(smr01) | !tibble::is_tibble(gro) |
     !tibble::is_tibble(pdiags) | !tibble::is_tibble(postcode) |
     !tibble::is_tibble(morbs) | !tibble::is_tibble(spec)) {

    stop(paste0("All arguments provided to the function ",
                "must be in tibble format. Verify whether ",
                "an object is a tibble or not with ",
                "the tibble::is_tibble() function"))
  }

  if(!all(c("link_no", "admission_date", "discharge_date", "cis_marker",
            "postcode", "discharge_type", "sex", "admgrp",
            "admfgrp", "ipdc", "age_grp", "quarter", "location",
            "main_condition", "other_condition_1", "other_condition_2",
            "other_condition_3", "other_condition_4", "other_condition_5",
            "specialty", "ipdc", "age_in_years", "hbtreat_currentdate",
            "year") %in% names(smr01))) {

    stop(paste0("Object smr01 does not contain the required variables. ",
                "Must contain:
                link_no
                admission_date
                discharge_date
                cis_marker
                postcode
                discharge_type
                sex
                admgrp
                admfgrp
                ipdc
                age_in_years
                age_grp
                quarter
                year
                location
                hbtreat_currentdate
                main_condition
                other_condition_1 to other_condition_5
                specialty
                ipdc"))
  }

  if(!all(c("diag1_4", "shmi_diagnosis_group") %in% names(pdiags))){

    stop(paste0("Object pdiags does not contain the required variables.",
                "Must contain:
                diag1_4
                shmi_diagnosis_group"))

  }

  if(!all(c("morb", "wmorbs", "diag") %in% names(morbs))){

    stop(paste0("Object morbs does not contain the required variables.",
                "Must contain:
                morb
                wmorbs
                diag"))

  }

  if(!all(c("link_no", "date_of_death") %in% names(gro))) {

    stop(paste0("Object gro does not contain the required variables.",
                "Must contain:
                link_no
                date_of_death"))
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

  if(!all(1:140 %in% pdiags$shmi_diagnosis_group)){

    stop(paste0("Primary diagnosis lookup does not contain all 140 ",
                "primary diagnosis groupings."))

  }

  if(!all(1:17 %in% morbs$morb)){

    stop(paste0("Co-morbidities lookup does not contain all 17 comorbidity
                groups."))

  }

  ### 2 - Match deaths data to SMR01 ----
  # Remove duplicate records on link_no
  # The deaths file is matched on to SMR01 by link_no,
  # therefore link_no needs to be unique
  gro %<>%
    dplyr::distinct(link_no, .keep_all = TRUE)

  # Match deaths data on to SMR01 data
  smr01 %<>%
    dplyr::left_join(gro, by = "link_no") %>%

    # Sort data by link_no, cis_marker, adm_date and dis_date as per guidance
    dplyr::arrange(link_no, cis_marker, admission_date, discharge_date)


  ### 3 - Basic SMR01 processing ----

  # Create the following variables:
  # death_inhosp = 1 if the patient died in hospital during that episode of care
  # dthdays      = the number of days from admission till death
  # death30      = 1 if the patient died within 30 days of admission date
  # quarter_name = quarter name in text form
  # quarter      = quarter in number form (quarter 1 = Jan - Mar 2011)
  # location     = recodes some hospital codes for combined sites
  # diag1_4      = main condition ICD-10 code to 4 digits
  # diagx        = ICD-10 code to 3 and 4 digits, separated by an underscore
  # pdiag_grp    = matches the primary diagnosis group on the 4-digit ICD-10
  #                code
  # wcomorbsx    = matches the charlson index weighting if the relevant ICD-10
  #                codes are present in any of the five "other diagnosis"
  #                positions
  # comorbs_sum  = sum of the wcomorbsx values across the episode

  smr01 %<>%
    dplyr::mutate(death_inhosp = dplyr::if_else(dplyr::between(
      as.numeric(discharge_type), 40, 49),
      1, 0),
      dthdays = lubridate::interval(admission_date, date_of_death) /
        lubridate::days(1),
      death30 = dplyr::case_when(
        dplyr::between(dthdays, 0, 30) ~ 1,
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

    # Create the pdiag_grp and wcomorbsx variables using joins to the morbs
    # dataset
    dplyr::left_join(dplyr::select(pdiags,
                                   pdiag_grp = shmi_diagnosis_group,
                                   diag1_4),
                     by = "diag1_4") %>%

    # Match on specialty grouping by the specialty variable
    dplyr::left_join(spec, by = "specialty") %>%

    # Fuzzy joins add the (in this case, not needed) joining variable by
    # default, so append these with "_z" so they can be easily removed
    # afterwards
    fuzzyjoin::fuzzy_left_join(dplyr::select(morbs,
                                             wcomorbs1 = wmorbs,
                                             comorbs1 = morb,
                                             diag2_z = diag),
                               by = c("diag2" = "diag2_z"),
                               match_fun = stringr::str_detect) %>%
    fuzzyjoin::fuzzy_left_join(dplyr::select(morbs,
                                             wcomorbs2 = wmorbs,
                                             comorbs2 = morb,
                                             diag3_z = diag),
                               by = c("diag3" = "diag3_z"),
                               match_fun = stringr::str_detect) %>%
    fuzzyjoin::fuzzy_left_join(dplyr::select(morbs,
                                             wcomorbs3 = wmorbs,
                                             comorbs3 = morb,
                                             diag4_z = diag),
                               by = c("diag4" = "diag4_z"),
                               match_fun = stringr::str_detect) %>%
    fuzzyjoin::fuzzy_left_join(dplyr::select(morbs,
                                             wcomorbs4 = wmorbs,
                                             comorbs4 = morb,
                                             diag5_z = diag),
                               by = c("diag5" = "diag5_z"),
                               match_fun = stringr::str_detect) %>%
    fuzzyjoin::fuzzy_left_join(dplyr::select(morbs,
                                             wcomorbs5 = wmorbs,
                                             comorbs5 = morb,
                                             diag6_z = diag),
                               by = c("diag6" = "diag6_z"),
                               match_fun = stringr::str_detect) %>%

    # Remove joining variables
    dplyr::select(-dplyr::ends_with("_z")) %>%

    # Replace cases with no match with zero
    tidyr::replace_na(list(wcomorbs1 = 0,
                           wcomorbs2 = 0,
                           wcomorbs3 = 0,
                           wcomorbs4 = 0,
                           wcomorbs5 = 0,
                           comorbs1  = 0,
                           comorbs2  = 0,
                           comorbs3  = 0,
                           comorbs4  = 0,
                           comorbs5  = 0)) %>%
    # Comorbs cleaning
    dplyr::mutate(wcomorbs1 = replace(wcomorbs1,
                                      comorbs1 == 12 &
                                        (comorbs2 != 6 & comorbs3 != 6 &
                                           comorbs4 != 6 & comorbs5 != 6),
                                      2),
                  wcomorbs2 = replace(wcomorbs2,
                                      comorbs2 == 12 &
                                        (comorbs1 != 6 & comorbs3 != 6 &
                                           comorbs4 != 6 & comorbs5 != 6),
                                      2),
                  wcomorbs3 = replace(wcomorbs3,
                                      comorbs3 == 12 &
                                        (comorbs2 != 6 & comorbs1 != 6 &
                                           comorbs4 != 6 & comorbs5 != 6),
                                      2),
                  wcomorbs4 = replace(wcomorbs4,
                                      comorbs4 == 12 &
                                        (comorbs2 != 6 & comorbs3 != 6 &
                                           comorbs1 != 6 & comorbs5 != 6),
                                      2),
                  wcomorbs5 = replace(wcomorbs5,
                                      comorbs5 == 12 &
                                        (comorbs2 != 6 & comorbs3 != 6 &
                                           comorbs4 != 6 & comorbs1 != 6),
                                      2)) %>%
    dplyr::mutate(wcomorbs1 = replace(wcomorbs1,
                                      comorbs1 == 11 & (comorbs2 == 15 |
                                                          comorbs3 == 15 |
                                                          comorbs4 == 15 |
                                                          comorbs5 == 15),
                                      0),
                  wcomorbs2 = replace(wcomorbs2,
                                      comorbs2 == 11 & (comorbs1 == 15 |
                                                          comorbs3 == 15 |
                                                          comorbs4 == 15 |
                                                          comorbs5 == 15),
                                      0),
                  wcomorbs3 = replace(wcomorbs3,
                                      comorbs3 == 11 & (comorbs2 == 15 |
                                                          comorbs1 == 15 |
                                                          comorbs4 == 15 |
                                                          comorbs5 == 15),
                                      0),
                  wcomorbs4 = replace(wcomorbs4,
                                      comorbs4 == 11 & (comorbs2 == 15 |
                                                          comorbs3 == 15 |
                                                          comorbs1 == 15 |
                                                          comorbs5 == 15),
                                      0),
                  wcomorbs5 = replace(wcomorbs5,
                                      comorbs5 == 11 & (comorbs2 == 15 |
                                                          comorbs3 == 15 |
                                                          comorbs4 == 15 |
                                                          comorbs1 == 15),
                                      0)) %>%
    dplyr::mutate(wcomorbs2 = replace(wcomorbs2,
                                      comorbs2 == comorbs1,
                                      0),
                  wcomorbs3 = replace(wcomorbs3,
                                      comorbs3 == comorbs1 |
                                        comorbs3 == comorbs2,
                                      0),
                  wcomorbs4 = replace(wcomorbs4,
                                      comorbs4 == comorbs1 |
                                        comorbs4 == comorbs2 |
                                        comorbs4 == comorbs3,
                                      0),
                  wcomorbs5 = replace(wcomorbs5,
                                      comorbs5 == comorbs1 |
                                        comorbs5 == comorbs2 |
                                        comorbs5 == comorbs3 |
                                        comorbs5 == comorbs4,
                                      0)) %>%
    dplyr::mutate(comorbs_sum = rowSums(
      dplyr::select(., dplyr::starts_with("wcomorbs")))) %>%

    # Create two further variables at CIS level:
    # epinum = the episode number for each individual episode within the CIS
    # death_inhosp_max = 1 if the patient died in hospital during any episode
    # of the CIS
    dplyr::group_by(link_no, cis_marker) %>%
    dplyr::mutate(epinum = dplyr::row_number(),
                  death_inhosp_max = max(death_inhosp)) %>%
    dplyr::ungroup() %>%

    # Sort data as per guidance and remove variables no longer required
    dplyr::arrange(link_no, cis_marker, admission_date, discharge_date) %>%
    dplyr::select(-dplyr::contains("condition"),
                  -dplyr::starts_with("wcomorbs"),
                  -comorbs1, -comorbs2, -comorbs3, -comorbs4, -comorbs5,
                  -quarter_name)

  ### 4 - SIMD ----

  # Fix formatting of postcode variable
  smr01 %<>%

    # First remove all spaces from postcode variable
    dplyr::mutate(postcode = gsub("\\s", "", postcode),

                  # Then add space (or spaces) at appropriate juncture
                  # (depending on the number of characters) to get the postcode
                  # into 7-character format
                  postcode = dplyr::case_when(
                    is.na(postcode)
                    ~ NA_character_,
                    stringr::str_length(postcode) == 5
                    ~ sub("(.{2})", "\\1  ", postcode),
                    stringr::str_length(postcode) == 6
                    ~ sub("(.{3})", "\\1 ", postcode),
                    TRUE
                    ~ postcode
                  )) %>%

    # Join to the postcode lookup
    dplyr::left_join(postcode, by = "postcode") %>%

    # Assign the appropriate SIMD value to a patient depending on the year they
    # were admitted
    dplyr::mutate(simd = dplyr::case_when(
      year >= 2014 ~ simd_2016,
      year < 2014 ~ simd_2012
    )) %>%

    # Remove the not needed year-specific SIMD variables
    dplyr::select(-c(simd_2012, simd_2016)) %>%
    dplyr::mutate(simd = dplyr::case_when(
      is.na(simd) & postcode == "NK010AA" ~ 6,
      is.na(simd) & postcode != "NK010AA" ~ 7,
      TRUE ~ simd
    ))

  return(smr01)

}

### END OF SCRIPT ###
