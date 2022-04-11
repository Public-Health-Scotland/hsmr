#' @title minimal tidy data set for long term trends in crude mortality report
#' production.
#'
#' @description Returns the minimal tidy data set for long terms trend data
#' which feeds into the reproducible analytical pipeline of the quarterly
#' Hospital Standardised Mortality Ratios publication.
#'
#'
#' @details \code{create_trends} expects a \code{tibble} with data extracted
#' from SMR01, a \code{tibble} with data extracted from NRS deaths records, a
#' \code{tibble} with population estimates and a \code{tibble} with a
#' lookup file for deprivation.
#'
#'
#' @param smr01 Input tibble for admissions, see details.
#' @param gro Input tibble for deaths, see details.
#' @param pop Input tibble for population estimates.
#' @param dep Input tibble for deprivation lookup.
#' @param spec Input tibble for the specialty groupings lookup.
#' @param hospital_lookup Input tibble for the hospital name lookup.
#'
#' @return If the class is not initiated correctly, nothing is returned.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#'
#' @export

create_trends <- function(smr01, gro, pop, dep, spec, hospital_lookup) {

  if(!tibble::is_tibble(smr01) | !tibble::is_tibble(gro) |
     !tibble::is_tibble(pop) | !tibble::is_tibble(dep) |
     !tibble::is_tibble(spec) | !tibble::is_tibble(hospital_lookup)) {

    stop(paste0("All arguments provided to the function ",
                "must be in tibble format. Verify whether ",
                "an object is a tibble or not with ",
                "the tibble::is_tibble() function"))
  }

  if(!all(c("link_no", "admission_date", "discharge_date", "cis_marker",
            "postcode", "specialty", "hbtreat_currentdate", "discharge_type",
            "sex", "location", "main_condition", "other_condition_1",
            "other_condition_2", "other_condition_3", "other_condition_4",
            "other_condition_5", "admgrp", "admfgrp", "ipdc", "age_grp",
            "quarter", "year") %in% names(smr01))) {

    stop(paste0("Object smr01 does not contain the required variables.",
                "Must contain:
                link_no
                admission_date
                discharge_date
                cis_marker
                postcode
                specialty
                hbtreat_currentdate
                discharge_type
                sex
                location
                main_condition
                other_condition_1
                other_condition_2
                other_condition_3
                other_condition_4
                other_condition_5
                admgrp
                admfgrp
                ipdc
                age_grp
                quarter
                year"))
  }

  if(!all(c("link_no", "date_of_death", "hbres_currentdate", "quarter",
            "year") %in% names(gro))) {

    stop(paste0("Object gro does not contain the required variables.",
                "Must contain:
                link_no
                date_of_death
                hbres_currentdate
                quarter
                year"))
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


  ### 1 - Deaths Data ----

  # Removing duplicate records on link_no as the deaths file is matched on to
  # smr01 by link_no, and link_no needs to be unique
  gro %<>%
    tidylog::distinct(link_no, .keep_all = TRUE)

  # Matching deaths data on to smr01 data
  smr01 %<>%
    tidylog::left_join(tidylog::select(gro, link_no, date_of_death),
                       by = "link_no") %>%

    # Sorting data by link_no, cis_marker, adm_date and dis_date
    dplyr::arrange(link_no, cis_marker, admission_date, discharge_date)

  # Formatting deaths data for aggregations later on
  gro %<>%
    tidylog::group_by(quarter, year) %>%
    tidylog::mutate(adm_first = min(date_of_death)) %>%
    tidylog::mutate(quarter_full = hsmr::qtr(as.Date(adm_first), "long"),
                    quarter_short = hsmr::qtr(as.Date(adm_first), "short")) %>%
    dplyr::ungroup() %>%
    tidylog::filter(date_of_death > end_date - years(5))

  ### 2 - SIMD ----

  # Fix formatting of postcode variable
  smr01 %<>%

    # First remove all spaces from postcode variable
    tidylog::mutate(postcode = gsub("\\s", "", postcode),

                    # Then add space (or spaces) at appropriate juncture
                    # (depending on the number of characters) to get the
                    # postcode into 7-character format
                    postcode = dplyr::case_when(
                      is.na(postcode) ~ NA_character_,
                      stringr::str_length(postcode) == 5
                      ~ sub("(.{2})", "\\1  ", postcode),
                      stringr::str_length(postcode) == 6
                      ~ sub("(.{3})", "\\1 ", postcode),
                      TRUE ~ postcode
                    )) %>%

    # Join to the postcode lookup
    tidylog::left_join(dep, by = "postcode") %>%
    tidylog::left_join(spec, by = "specialty") %>%

    # Assign the appropriate SIMD value to a patient depending on the year they
    # were admitted
    tidylog::mutate(simd = dplyr::case_when(
      year >=2017 ~ simd_2020,
      year >= 2014 & year < 2017 ~ simd_2016,
      year > 2009 & year < 2014 ~ simd_2012,
      year <= 2009 ~ simd_2009
    )) %>%

    # Remove the not needed year-specific SIMD variables
    tidylog::select(-c(simd_2009:simd_2020))


  ### 3 - Manipulations ----

  smr01_quarter <- smr01 %>%
    tidylog::mutate(death_inhosp = dplyr::if_else(dplyr::between(
      as.numeric(discharge_type), 40, 49),
      1, 0),
      dthdays = lubridate::interval(admission_date, date_of_death) /
        lubridate::days(1),
      death30 = dplyr::case_when(
        dplyr::between(dthdays, 0, 30) ~ 1,
        TRUE ~ 0),
      quarter = paste0(year, "Q", quarter)) %>%
    tidylog::group_by(link_no, cis_marker) %>%
    tidylog::mutate(epinum    = dplyr::row_number(),
                    death_inhosp_max   = max(death_inhosp),
                    discharge_date_cis = max(discharge_date)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(dthdays_dis =
                      lubridate::interval(discharge_date_cis, date_of_death) /
                      lubridate::days(1),
                    death30_dis = dplyr::case_when(
                      dplyr::between(dthdays_dis, 0, 30) ~ 1,
                      TRUE ~ 0),
                    depth_of_coding = dplyr::case_when(
                      !is.na(other_condition_3)                            ~ 3,
                      is.na(other_condition_3) & !is.na(other_condition_2) ~ 2,
                      is.na(other_condition_2) & !is.na(other_condition_1) ~ 1,
                      TRUE                                                 ~ 0
                    )) %>%
    dplyr::arrange(link_no, cis_marker, admission_date, discharge_date) %>%
    tidylog::group_by(link_no, quarter) %>%
    tidylog::mutate(last_cis = max(cis_marker)) %>%
    tidylog::group_by(quarter) %>%
    tidylog::mutate(adm_first = min(admission_date)) %>%
    tidylog::mutate(quarter_full = hsmr::qtr(as.Date(adm_first), "long"),
                    quarter_short = hsmr::qtr(as.Date(adm_first), "short")) %>%
    dplyr::ungroup() %>%
    tidylog::filter(epinum == 1 & cis_marker == last_cis) %>%

    # Patients are counted once per quarter and it is possible for a patient
    # to die within 30 days of 2 admissions within different quarters.
    # In order to only count a death once, the second admission in the
    # subsequent quarter is removed and only the first death within 30 days is
    # counted
    tidylog::filter(!(link_no == c(0, head(link_no, -1)) &
                        1 == c(0, head(death30, -1)))) %>%
    tidylog::filter(admission_date > end_date - lubridate::years(5)) %>%
    tidylog::mutate(quarter = as.numeric(as.factor(quarter))) %>%

    # Combine institutions

    tidylog::mutate(location = case_when(
      location == "C206H" ~ "C418H", # Vale of Leven as Royal Alexandra Hospital
      location == "G207H" ~ "G107H", # Stobhill Hospital as Glasgow Royal Infirmary
      location == "G306H" ~ "G405H", # New Victoria Hospital as Queen Elizabeth University Hospital
      location == "G516H" ~ "G405H", # West Glasgow as Queen Elizabeth University Hospital
      location == "Y104H" ~ "Y146H", # Dumfries & Galloway Royal Infirmary Old as Dumfries & Galloway Royal Infirmary
      location == "R101H" ~ "R103H", # Balfour Hospital as The Balfour
      TRUE ~ location
    ))



  smr01_month <- smr01 %>%
    tidylog::mutate(death_inhosp = dplyr::if_else(dplyr::between(
      as.numeric(discharge_type), 40, 49),
      1, 0),
      dthdays = lubridate::interval(admission_date, date_of_death) /
        lubridate::days(1),
      death30 = dplyr::case_when(
        dplyr::between(dthdays, 0, 30) ~ 1,
        TRUE ~ 0),
      month = paste0(year, "M", month)) %>%
    tidylog::group_by(link_no, cis_marker) %>%
    tidylog::mutate(epinum    = dplyr::row_number(),
                    death_inhosp_max   = max(death_inhosp),
                    discharge_date_cis = max(discharge_date)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(dthdays_dis =
                      lubridate::interval(discharge_date_cis, date_of_death) /
                      lubridate::days(1),
                    death30_dis = dplyr::case_when(
                      dplyr::between(dthdays_dis, 0, 30) ~ 1,
                      TRUE ~ 0),
                    depth_of_coding = dplyr::case_when(
                      !is.na(other_condition_3)                            ~ 3,
                      is.na(other_condition_3) & !is.na(other_condition_2) ~ 2,
                      is.na(other_condition_2) & !is.na(other_condition_1) ~ 1,
                      TRUE                                                 ~ 0
                    )) %>%
    dplyr::arrange(link_no, cis_marker, admission_date, discharge_date) %>%
    tidylog::group_by(link_no, month) %>%
    tidylog::mutate(last_cis = max(cis_marker)) %>%
    tidylog::group_by(month) %>%
    tidylog::mutate(adm_first = min(admission_date)) %>%
    tidylog::mutate(month_label = paste(
                                    lubridate::month(adm_first, label = TRUE, abbr = TRUE),
                                    year(adm_first))) %>%
    dplyr::ungroup() %>%
    tidylog::filter(epinum == 1 & cis_marker == last_cis) %>%

    # Patients are counted once per month and it is possible for a patient
    # to die within 30 days of 2 admissions within different months.
    # In order to only count a death once, the second admission in the
    # subsequent month is removed and only the first death within 30 days is
    # counted
    tidylog::filter(!(link_no == c(0, head(link_no, -1)) &
                        1 == c(0, head(death30, -1)))) %>%
    tidylog::filter(admission_date > end_date - lubridate::years(5)) %>%
    tidylog::mutate(month = as.numeric(as.factor(month))) %>%

    # Combine institutions

    tidylog::mutate(location = case_when(
      location == "C206H" ~ "C418H", # Vale of Leven as Royal Alexandra Hospital
      location == "G207H" ~ "G107H", # Stobhill Hospital as Glasgow Royal Infirmary
      location == "G306H" ~ "G405H", # New Victoria Hospital as Queen Elizabeth University Hospital
      location == "G516H" ~ "G405H", # West Glasgow as Queen Elizabeth University Hospital
      # Dumfries old not in the data anymore
      location == "Y104H" ~ "Y146H", # Dumfries & Galloway Royal Infirmary Old as Dumfries & Galloway Royal Infirmary
      # Balfour old code stopped in June 2019
      location == "R101H" ~ "R103H", # Balfour Hospital as The Balfour
      TRUE ~ location
    ))

  ### 4 - Aggregation ----

  # Crude Rates (Scotland) - All Admissions ----
  scot_all_adm <- smr01_quarter %>%
    tidylog::group_by(quarter, quarter_full, quarter_short) %>%
    tidylog::summarise(deaths      = sum(death30),
                       scot_deaths = sum(death30),
                       pats        = length(death30),
                       scot_pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = "All Admissions",
                    sub_grp = "All Admissions",
                    hbtreat_currentdate = "Scotland",
                    location = "Scot",
                    agg_label = "Scotland")

  # Crude Rates (Hosp) - All Admissions ----
  hosp_all_adm <- smr01_quarter %>%
    tidylog::group_by(quarter, hbtreat_currentdate, location,
                      quarter_full, quarter_short) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    tidylog::group_by(quarter,
                      quarter_full, quarter_short) %>%
    tidylog::mutate(scot_deaths = sum(deaths),
                       scot_pats   = sum(pats)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = "All Admissions",
                    sub_grp = "All Admissions",
                    agg_label = "Hospital")

  # Crude Rates (HB) - All Admissions ----
  hb_all_adm <- smr01_quarter %>%
    tidylog::group_by(quarter, hbtreat_currentdate, quarter_full,
                      quarter_short) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    tidylog::group_by(quarter,
                      quarter_full, quarter_short) %>%
    tidylog::mutate(scot_deaths = sum(deaths),
                    scot_pats   = sum(pats)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = "All Admissions",
                    sub_grp = "All Admissions",
                    location = hbtreat_currentdate,
                    agg_label = "Board")

  # Combine All Admissions tibbles together
  all_adm <- dplyr::bind_rows(scot_all_adm, hb_all_adm, hosp_all_adm) %>%
    tidylog::group_by(quarter, label) %>%
    tidylog::mutate(scot_deaths = max(deaths),
                    scot_pats   = max(pats))

  # Crude Rates (Scotland) - Specialty/Admission type ----
  scot_adm <- smr01_quarter %>%
    tidylog::group_by(quarter, quarter_full, quarter_short, admgrp) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      admgrp == 1 ~ "Elective",
      admgrp == 2 ~ "Non-Elective"
    ),
    hbtreat_currentdate = "Scotland",
    location = "Scot",
    sub_grp = "Admission Type",
    agg_label = "Scotland",
    scot_deaths = deaths,
    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location, quarter, quarter_full,
                    quarter_short,deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Crude Rates (Health Board) - Specialty/Admission type ----
  hb_adm <- smr01_quarter %>%
    tidylog::group_by(quarter, hbtreat_currentdate, quarter_full, quarter_short,
                       admgrp) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      admgrp == 1 ~ "Elective",
      admgrp == 2 ~ "Non-Elective"
    ),
    location = hbtreat_currentdate,
    sub_grp = "Admission Type",
    agg_label = "Board",
    scot_deaths = deaths,
    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location, quarter, quarter_full,
                    quarter_short,deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Crude Rates (Hospital) - Specialty/Admission type ----
  hosp_adm <- smr01_quarter %>%
    tidylog::group_by(quarter, hbtreat_currentdate, location, quarter_full, quarter_short,
                       admgrp) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      admgrp == 1 ~ "Elective",
      admgrp == 2 ~ "Non-Elective"
    ),
    sub_grp = "Admission Type",
    agg_label = "Hospital",
    scot_deaths = deaths,
    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location, quarter, quarter_full,
                    quarter_short,deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Combine Admission Type tibbles together
  adm_type <- dplyr::bind_rows(scot_adm, hb_adm, hosp_adm) %>%
    tidylog::group_by(quarter, label) %>%
    tidylog::mutate(scot_deaths = max(deaths),
                    scot_pats   = max(pats))

  # Crude Rates (Scotland) - Age group ----
  scot_age <- smr01_quarter %>%
    tidylog::group_by(quarter, quarter_full, quarter_short, age_grp) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      age_grp == 1 ~ "0-19 years",
      age_grp == 2 ~ "20-39 years",
      age_grp == 3 ~ "40-59 years",
      age_grp == 4 ~ "60-79 years",
      age_grp == 5 ~ "80+ years"
    ),
    hbtreat_currentdate = "Scotland",
    sub_grp = "Age Group",
    location = "Scot",
    agg_label = "Scotland",
    scot_deaths = deaths,
    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location, quarter, quarter_full,
                    quarter_short, deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Crude Rates (Health Board) - Age group ----
  hb_age <- smr01_quarter %>%
    tidylog::group_by(quarter, hbtreat_currentdate, quarter_full, quarter_short, age_grp) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      age_grp == 1 ~ "0-19 years",
      age_grp == 2 ~ "20-39 years",
      age_grp == 3 ~ "40-59 years",
      age_grp == 4 ~ "60-79 years",
      age_grp == 5 ~ "80+ years"
    ),
    sub_grp = "Age Group",
    location = hbtreat_currentdate,
    agg_label = "Board",
    scot_deaths = deaths,
    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location, quarter, quarter_full,
                    quarter_short, deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Crude Rates (Hospital) - Age group ----
  hosp_age <- smr01_quarter %>%
    tidylog::group_by(quarter, hbtreat_currentdate, location, quarter_full, quarter_short,
                      age_grp) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      age_grp == 1 ~ "0-19 years",
      age_grp == 2 ~ "20-39 years",
      age_grp == 3 ~ "40-59 years",
      age_grp == 4 ~ "60-79 years",
      age_grp == 5 ~ "80+ years"
    ),
    sub_grp = "Age Group",
    agg_label = "Hospital",
    scot_deaths = deaths,
    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location, quarter, quarter_full,
                    quarter_short, deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Combine Age Group tibbles together
  age_group <- dplyr::bind_rows(scot_age, hb_age, hosp_age) %>%
    tidylog::group_by(quarter, label) %>%
    tidylog::mutate(scot_deaths = max(deaths),
                    scot_pats   = max(pats))

  # Crude Rates (Scotland) - Sex ----
  scot_sex <- smr01_quarter %>%
    tidylog::group_by(quarter, quarter_full, quarter_short, sex) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female"
    ),
    hbtreat_currentdate = "Scotland",
    sub_grp = "Sex",
    location = "Scot",
    agg_label = "Scotland",
    scot_deaths = deaths,
    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location, quarter, quarter_full,
                    quarter_short, deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Crude Rates (Health Board) - Sex ----
  hb_sex <- smr01_quarter %>%
    tidylog::group_by(quarter, hbtreat_currentdate, quarter_full, quarter_short, sex) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female"
    ),
    sub_grp = "Sex",
    location = hbtreat_currentdate,
    agg_label = "Board",
    scot_deaths = deaths,
    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location, quarter, quarter_full,
                    quarter_short, deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Crude Rates (Hospital) - Sex ----
  hosp_sex <- smr01_quarter %>%
    tidylog::group_by(quarter, hbtreat_currentdate, location, quarter_full, quarter_short, sex) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female"
    ),
    sub_grp = "Sex",
    agg_label = "Hospital",
    scot_deaths = deaths,
    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location, quarter, quarter_full,
                    quarter_short, deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Combine Sex tibbles together
  sex <- dplyr::bind_rows(scot_sex, hb_sex, hosp_sex) %>%
    tidylog::group_by(quarter, label) %>%
    tidylog::mutate(scot_deaths = max(deaths),
                    scot_pats   = max(pats))

  # Crude Rates (Scotland) Place of Death ----
  scot_place_of_death <- smr01_quarter %>%
    tidylog::group_by(quarter, quarter_full, quarter_short,
                      death_inhosp_max) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      death_inhosp_max == 1 ~ "Died in Hospital",
      death_inhosp_max == 0 ~ "Died in Community"
    ),
    hbtreat_currentdate = "Scotland",
    location = "Scot",
    sub_grp = "Place of Death",
    agg_label = "Scotland",
    scot_deaths = deaths,
    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location, quarter, quarter_full,
                    quarter_short, deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Crude Rates (Health Board) Place of Death ----
  hb_place_of_death <- smr01_quarter %>%
    tidylog::group_by(quarter, hbtreat_currentdate, quarter_full, quarter_short,
                      death_inhosp_max) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      death_inhosp_max == 1 ~ "Died in Hospital",
      death_inhosp_max == 0 ~ "Died in Community"
    ),
    location = hbtreat_currentdate,
    sub_grp = "Place of Death",
    agg_label = "Board",
    scot_deaths = deaths,
    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location, quarter, quarter_full,
                    quarter_short, deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Crude Rates (Hospital) Place of Death ----
  hosp_place_of_death <- smr01_quarter %>%
    tidylog::group_by(quarter, hbtreat_currentdate, location, quarter_full, quarter_short,
                      death_inhosp_max) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      death_inhosp_max == 1 ~ "Died in Hospital",
      death_inhosp_max == 0 ~ "Died in Community"
    ),
    sub_grp = "Place of Death",
    agg_label = "Hospital",
    scot_deaths = deaths,
    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location, quarter, quarter_full,
                    quarter_short, deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Combine Depth of Coding tibbles together
  place_of_death <- dplyr::bind_rows(scot_place_of_death, hb_place_of_death, hosp_place_of_death) %>%
    tidylog::group_by(quarter, label) %>%
    tidylog::mutate(scot_deaths = max(deaths),
                    scot_pats   = max(pats))


  # Crude Rates (Scotland) - Deprivation ----
  scot_dep <- smr01_quarter %>%
    tidylog::group_by(quarter, quarter_full, quarter_short, simd) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      is.na(simd) ~ "Unknown",
      simd == 1   ~ "1 - Most Deprived",
      simd == 2   ~ "2",
      simd == 3   ~ "3",
      simd == 4   ~ "4",
      simd == 5   ~ "5 - Least Deprived"
    ),
    hbtreat_currentdate = "Scotland",
    location = "Scot",
    sub_grp = "Deprivation",
    agg_label = "Scotland",
    scot_deaths = deaths,
    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location, quarter, quarter_full,
                    quarter_short, deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Crude Rates (Health Board) - Deprivation ----
  hb_dep <- smr01_quarter %>%
    tidylog::group_by(quarter, hbtreat_currentdate, quarter_full, quarter_short, simd) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      is.na(simd) ~ "Unknown",
      simd == 1   ~ "1 - Most Deprived",
      simd == 2   ~ "2",
      simd == 3   ~ "3",
      simd == 4   ~ "4",
      simd == 5   ~ "5 - Least Deprived"
    ),
    location = hbtreat_currentdate,
    sub_grp = "Deprivation",
    agg_label = "Board",
    scot_deaths = deaths,
    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location, quarter, quarter_full,
                    quarter_short, deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Crude Rates (Hospital) - Deprivation ----
  hosp_dep <- smr01_quarter %>%
    tidylog::group_by(quarter, hbtreat_currentdate, location, quarter_full, quarter_short, simd) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      is.na(simd) ~ "Unknown",
      simd == 1   ~ "1 - Most Deprived",
      simd == 2   ~ "2",
      simd == 3   ~ "3",
      simd == 4   ~ "4",
      simd == 5   ~ "5 - Least Deprived"
    ),
    sub_grp = "Deprivation",
    agg_label = "Hospital",
    scot_deaths = deaths,
    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location, quarter, quarter_full,
                    quarter_short, deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Combine Deprivation tibbles together
  deprivation <- dplyr::bind_rows(scot_dep, hb_dep, hosp_dep) %>%
    tidylog::group_by(quarter, label) %>%
    tidylog::mutate(scot_deaths = max(deaths),
                    scot_pats   = max(pats))

  # Crude Rates (Scotland) - Specialty ----
  scot_spec <- smr01_quarter %>%
    tidylog::group_by(quarter, quarter_full, quarter_short, spec_grp) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      is.na(spec_grp) ~ "Unknown",
      spec_grp == 1   ~ "Community",
      spec_grp == 2   ~ "Dental",
      spec_grp == 3   ~ "Emergency",
      spec_grp == 4   ~ "Medical",
      spec_grp == 5   ~ "Mental",
      spec_grp == 6   ~ "Other",
      spec_grp == 7   ~ "Paediatrics",
      spec_grp == 8   ~ "Surgery",
      spec_grp == 9   ~ "Women & Newborn"
    ),
    hbtreat_currentdate = "Scotland",
    location = "Scot",
    sub_grp = "Specialty",
    agg_label = "Scotland",
    scot_deaths = deaths,
    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location, quarter, quarter_full,
                    quarter_short, deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Crude Rates (Health Board) - Specialty ----
  hb_spec <- smr01_quarter %>%
    tidylog::group_by(quarter, hbtreat_currentdate, quarter_full, quarter_short, spec_grp) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      is.na(spec_grp) ~ "Unknown",
      spec_grp == 1   ~ "Community",
      spec_grp == 2   ~ "Dental",
      spec_grp == 3   ~ "Emergency",
      spec_grp == 4   ~ "Medical",
      spec_grp == 5   ~ "Mental",
      spec_grp == 6   ~ "Other",
      spec_grp == 7   ~ "Paediatrics",
      spec_grp == 8   ~ "Surgery",
      spec_grp == 9   ~ "Women & Newborn"
    ),
    location = hbtreat_currentdate,
    sub_grp = "Specialty",
    agg_label = "Board",
    scot_deaths = deaths,
    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location, quarter, quarter_full,
                    quarter_short, deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Crude Rates (Hospital) - Specialty ----
  hosp_spec <- smr01_quarter %>%
    tidylog::group_by(quarter, hbtreat_currentdate, location, quarter_full, quarter_short, spec_grp) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      is.na(spec_grp) ~ "Unknown",
      spec_grp == 1   ~ "Community",
      spec_grp == 2   ~ "Dental",
      spec_grp == 3   ~ "Emergency",
      spec_grp == 4   ~ "Medical",
      spec_grp == 5   ~ "Mental",
      spec_grp == 6   ~ "Other",
      spec_grp == 7   ~ "Paediatrics",
      spec_grp == 8   ~ "Surgery",
      spec_grp == 9   ~ "Women & Newborn"
    ),
    sub_grp = "Specialty",
    agg_label = "Hospital",
    scot_deaths = deaths,
    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location, quarter, quarter_full,
                    quarter_short, deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Combine Specialty tibbles together
  specialty <- dplyr::bind_rows(scot_spec, hb_spec, hosp_spec) %>%
    tidylog::group_by(quarter, label) %>%
    tidylog::mutate(scot_deaths = max(deaths),
                    scot_pats   = max(pats))

  # Depth of Coding - Scotland ----
  scot_depth <- smr01_quarter %>%
    tidylog::group_by(quarter, quarter_full, quarter_short, depth_of_coding) %>%
    tidylog::summarise(deaths = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::group_by(quarter, quarter_full, quarter_short) %>%
    tidylog::mutate(pats = sum(deaths)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      depth_of_coding == 0 ~ "0",
      depth_of_coding == 1 ~ "1",
      depth_of_coding == 2 ~ "2",
      depth_of_coding == 3 ~ "3+"
    ),
    hbtreat_currentdate = "Scotland",
    location = "Scot",
    sub_grp = "Depth of Coding",
    agg_label = "Scotland",
    scot_deaths = deaths,
    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location,  quarter, quarter_full,
                    quarter_short, deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)


  # Depth of Coding - Health Board ----
  hb_depth <- smr01_quarter %>%
    tidylog::group_by(quarter, quarter_full, quarter_short,
                      hbtreat_currentdate, depth_of_coding) %>%
    tidylog::summarise(deaths = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::group_by(quarter, quarter_full, quarter_short,
                      hbtreat_currentdate) %>%
    tidylog::mutate(pats = sum(deaths)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      depth_of_coding == 0 ~ "0",
      depth_of_coding == 1 ~ "1",
      depth_of_coding == 2 ~ "2",
      depth_of_coding == 3 ~ "3+"
    ),
    location = hbtreat_currentdate,
    sub_grp = "Depth of Coding",
    agg_label = "Board",
    scot_deaths = deaths,
    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location,  quarter, quarter_full,
                    quarter_short, deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)


  # Depth of Coding - Hospitals ----
  hosp_depth <- smr01_quarter %>%
    tidylog::group_by(quarter, quarter_full, quarter_short,
                      hbtreat_currentdate, location, depth_of_coding) %>%
    tidylog::summarise(deaths = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::group_by(quarter, quarter_full, quarter_short,
                      hbtreat_currentdate, location) %>%
    tidylog::mutate(pats = sum(deaths)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      depth_of_coding == 0 ~ "0",
      depth_of_coding == 1 ~ "1",
      depth_of_coding == 2 ~ "2",
      depth_of_coding == 3 ~ "3+"
    ),
    sub_grp = "Depth of Coding",
    agg_label = "Hospital",
    scot_deaths = deaths,
    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location,  quarter, quarter_full,
                    quarter_short, deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Combine Depth of Coding tibbles together
  depth_of_coding <- dplyr::bind_rows(scot_depth, hb_depth, hosp_depth) %>%
    tidylog::group_by(quarter, label) %>%
    tidylog::mutate(scot_deaths = max(deaths),
                    scot_pats   = max(pats))

  # Symptom Coding - Scotland ----
  scot_symptoms <- smr01_quarter %>%
    tidylog::filter(!is.na(main_condition)) %>%
    tidylog::mutate(symptom_flag = if_else(substr(main_condition, 1, 1) == "R",
                                           1, 0)) %>%
    tidylog::group_by(quarter, quarter_full, quarter_short) %>%
    tidylog::summarise(deaths = sum(symptom_flag),
                    pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(hbtreat_currentdate = "Scotland",
                    location = "Scot",
                    sub_grp = "Symptom Coding",
                    label   = "Symptom Coding",
                    agg_label = "Scotland",
                    scot_deaths = deaths,
                    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location,  quarter, quarter_full,
                    quarter_short, deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Symptom Coding - Health Board ----
  hb_symptoms <- smr01_quarter %>%
    tidylog::filter(!is.na(main_condition)) %>%
    tidylog::mutate(symptom_flag = if_else(substr(main_condition, 1, 1) == "R",
                                           1, 0)) %>%
    tidylog::group_by(quarter, quarter_full, quarter_short,
                      hbtreat_currentdate) %>%
    tidylog::summarise(deaths = sum(symptom_flag),
                    pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(location = hbtreat_currentdate,
                    sub_grp = "Symptom Coding",
                    label   = "Symptom Coding",
                    agg_label = "Board",
                    scot_deaths = deaths,
                    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location,  quarter, quarter_full,
                    quarter_short, deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Symptom Coding - Hospital ----
  hosp_symptoms <- smr01_quarter %>%
    tidylog::filter(!is.na(main_condition)) %>%
    tidylog::mutate(symptom_flag = if_else(substr(main_condition, 1, 1) == "R",
                                           1, 0)) %>%
    tidylog::group_by(quarter, quarter_full, quarter_short,
                      hbtreat_currentdate, location) %>%
    tidylog::summarise(deaths = sum(symptom_flag),
                    pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(sub_grp = "Symptom Coding",
                    label   = "Symptom Coding",
                    agg_label = "Hospital",
                    scot_deaths = deaths,
                    scot_pats   = pats) %>%
    tidylog::select(hbtreat_currentdate, location,  quarter, quarter_full,
                    quarter_short, deaths, pats, scot_deaths, scot_pats,
                    sub_grp, label, agg_label)

  # Combine Depth of Coding tibbles together
  symptom_coding <- dplyr::bind_rows(scot_symptoms, hb_symptoms,
                                     hosp_symptoms) %>%
    tidylog::group_by(quarter) %>%
    tidylog::mutate(scot_deaths = max(deaths),
                    scot_pats   = max(pats))


  # Merge dataframes together
  subgroups <- dplyr::bind_rows(all_adm, age_group, sex, adm_type, deprivation,
                                    specialty, place_of_death,
                                     depth_of_coding, symptom_coding) %>%
    tidylog::mutate(crd_rate = deaths/pats * 100) %>%
    dplyr::rename(hb = hbtreat_currentdate) %>%
    tidylog::select(hb, location, quarter, quarter_full, quarter_short,
                    deaths, pats, scot_deaths, scot_pats, crd_rate, sub_grp,
                    label, agg_label)

  # Crude Rate - Date of Discharge (Scotland) ----
  scot_dis <- smr01_quarter %>%
    tidylog::group_by(quarter, quarter_full, quarter_short) %>%
    tidylog::summarise(deaths = sum(death30_dis),
                       pats   = length(death30_dis)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label      = "Discharge",
                    hbtreat_currentdate = "Scotland",
                    location = "Scot",
                    sub_grp = "Discharge",
                    agg_label = "Scotland",
                    scot_deaths = deaths,
                    scot_pats   = pats)

  # Crude Rate - Date of Discharge (NHS Board) ----
  hb_dis <- smr01_quarter %>%
    tidylog::group_by(quarter, quarter_full, quarter_short,
                      hbtreat_currentdate) %>%
    tidylog::summarise(deaths = sum(death30_dis),
                       pats   = length(death30_dis)) %>%
    tidylog::group_by(quarter, quarter_full, quarter_short) %>%
    tidylog::mutate(scot_deaths = sum(deaths),
                       scot_pats   = sum(pats)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label     = "Discharge",
                    location  = hbtreat_currentdate,
                    sub_grp   = "Discharge",
                    agg_label = "Board")

  # Merge dataframes together
  dis <- dplyr::bind_rows(scot_dis, hb_dis) %>%
    tidylog::mutate(crd_rate = deaths/pats * 100) %>%
    dplyr::rename(hb = hbtreat_currentdate) %>%
    tidylog::select(hb, location, quarter, quarter_full, quarter_short,
                    deaths, pats, scot_deaths, scot_pats, crd_rate, sub_grp,
                    label, agg_label)

  # Population-based mortality ----
  scot_pop <- gro %>%
    tidylog::group_by(year, quarter, quarter_full, quarter_short) %>%
    tidylog::summarise(deaths = length(year),
                       scot_deaths = length(year)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(hbres_currentdate = "Scotland",
                    location = "Scot",
                    agg_label = "Scotland")

  hb_pop <- gro %>%
    tidylog::group_by(year, quarter, quarter_full, quarter_short,
                      hbres_currentdate) %>%
    tidylog::summarise(deaths = length(year)) %>%
    tidylog::group_by(year, quarter, quarter_full, quarter_short) %>%
    tidylog::mutate(scot_deaths = length(year)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(agg_label = "Board",
                    location  = hbres_currentdate)

  pop_deaths <- dplyr::bind_rows(scot_pop, hb_pop) %>%
    tidylog::left_join(pop, by = c("year", "hbres_currentdate" = "hb2014")) %>%
    tidylog::mutate(crd_rate     = deaths/pop * 1000,
                    quarter      = as.numeric(as.factor(paste0(year,
                                                               "Q",
                                                               quarter))),
                    label        = "Population",
                    sub_grp      = "Population") %>%
    dplyr::rename(hb = hbres_currentdate,
                  pats   = pop) %>%
    tidylog::filter(!is.na(pats)) %>%
    tidylog::group_by(quarter) %>%
    tidylog::mutate(scot_deaths = max(deaths),
                    scot_pats   = max(pats)) %>%
    tidylog::select(hb, location, quarter, quarter_full, quarter_short,
                    deaths, pats, scot_deaths, scot_pats,
                    crd_rate, sub_grp, label, agg_label)



  # Create minimal tidy dataset
  long_term_trends <- dplyr::bind_rows(subgroups, dis, pop_deaths) %>%
    mutate(completeness_date = hsmr::submission_deadline(end_date)) %>%
    tidylog::left_join(hospital_lookup, by = "location") %>%
    tidylog::filter(!is.na(location_name)) %>%


    # Recode location and healthboard codes to new ones

    tidylog::mutate(
    location_name = case_when(location == "C418H" ~
                                "Royal Alexandra/Vale of Leven",
                              location == "D102H" ~
                                "Golden Jubilee National Hospital",
                              location == "S08100001" ~
                                "Golden Jubilee",
                              location == "R103H" ~
                                "The Balfour",
                              TRUE ~ location_name))


  # Create template
  quarter_template <- data.frame(quarter = long_term_trends$quarter,
                                 quarter_short = long_term_trends$quarter_short,
                                 quarter_full =
                                   long_term_trends$quarter_full) %>%
    distinct(.keep_all = TRUE)

  # Data frame with a row for each location for each quarter
  location_template <- data.frame(hb = long_term_trends$hb,
                                  location = long_term_trends$location,
                                  location_name =
                                    long_term_trends$location_name,
                                  agg_label = long_term_trends$agg_label) %>%
    distinct(.keep_all =TRUE)


  # Number of Scot deaths/pats for each combination of sub_grp, label and quarter
  scot_deaths_template <- data.frame(sub_grp = long_term_trends$sub_grp,
                                     label = long_term_trends$label,
                                     quarter = long_term_trends$quarter,
                                     scot_deaths = long_term_trends$scot_deaths,
                                     scot_pats = long_term_trends$scot_pats,
                                     completeness_date =
                                       long_term_trends$completeness_date) %>%
    distinct(.keep_all = TRUE)

  # Combines above in to a data frame with a row for each combination of location,
  # sub_grp and label for each quarter
  trends_data_all <- merge(location_template, quarter_template) %>%
    left_join(scot_deaths_template) %>%
    left_join(long_term_trends) %>%
    replace_na(list(deaths=0, pats=0, crd_rate=0)) %>%
    na.omit()

  # Calculate the sum of deaths per subgroup per location
  trends_all_deaths <- group_by(trends_data_all, location, label) %>%
    summarise(all_deaths = sum(deaths)) %>%
    ungroup()

  # Merge above together and drop subgroups where there were no deaths
  trend_data_quarter <- left_join(trends_data_all, trends_all_deaths) %>%
    filter(all_deaths !=0) %>%
    select(-all_deaths) %>%
    mutate(time_period = "Quarter")



  ### 5 - Aggregation - Monthly ----

  # Crude Rates (Scotland) - All Admissions ----
  scot_all_adm_month <- smr01_month %>%
    tidylog::group_by(month, month_label) %>%
    tidylog::summarise(deaths      = sum(death30),
                       scot_deaths = sum(death30),
                       pats        = length(death30),
                       scot_pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = "All Admissions",
                    sub_grp = "All Admissions",
                    hbtreat_currentdate = "Scotland",
                    location = "Scot",
                    agg_label = "Scotland")

  # Crude Rates (Hosp) - All Admissions ----
  hosp_all_adm_month <- smr01_month %>%
    tidylog::group_by(month, month_label, hbtreat_currentdate, location) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    tidylog::group_by(month) %>%
    tidylog::mutate(scot_deaths = sum(deaths),
                    scot_pats   = sum(pats)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = "All Admissions",
                    sub_grp = "All Admissions",
                    agg_label = "Hospital")

  # Crude Rates (HB) - All Admissions ----
  hb_all_adm_month <- smr01_month %>%
    tidylog::group_by(month, month_label, hbtreat_currentdate) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    tidylog::group_by(month) %>%
    tidylog::mutate(scot_deaths = sum(deaths),
                    scot_pats   = sum(pats)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = "All Admissions",
                    sub_grp = "All Admissions",
                    location = hbtreat_currentdate,
                    agg_label = "Board")

  # Combine All Admissions tibbles together
  all_adm_month <- dplyr::bind_rows(scot_all_adm_month, hb_all_adm_month,
                                    hosp_all_adm_month) %>%
    tidylog::group_by(month, label) %>%
    tidylog::mutate(scot_deaths = max(deaths),
                    scot_pats   = max(pats)) %>%
    tidylog::mutate(crd_rate = deaths/pats * 100) %>%
    dplyr::rename(hb = hbtreat_currentdate) %>%
    tidylog::select(hb, location, month, month_label,
                    deaths, pats, scot_deaths, scot_pats, crd_rate, sub_grp,
                    label, agg_label) %>%
    mutate(completeness_date = hsmr::submission_deadline(end_date)) %>%
    tidylog::left_join(hospital_lookup, by = "location") %>%
    tidylog::filter(!is.na(location_name)) %>%


    # Recode location and healthboard codes to new ones

    tidylog::mutate(
      location_name = case_when(location == "C418H" ~
                                  "Royal Alexandra/Vale of Leven",
                                location == "D102H" ~
                                  "Golden Jubilee National Hospital",
                                location == "S08100001" ~
                                  "Golden Jubilee",
                                location == "R103H" ~
                                  "The Balfour",
                                TRUE ~ location_name))


  # Create template
  month_template <- data.frame(month = all_adm_month$month) %>%
    distinct(.keep_all = TRUE)

  # Data frame with a row for each location for each quarter
  location_template_month <- data.frame(hb = all_adm_month$hb,
                                        location = all_adm_month$location,
                                        location_name =
                                          all_adm_month$location_name,
                                        agg_label = all_adm_month$agg_label) %>%
    distinct(.keep_all =TRUE)


  # Number of Scot deaths/pats for each combination of sub_grp, label and quarter
  scot_deaths_template_month <- data.frame(sub_grp = all_adm_month$sub_grp,
                                           label = all_adm_month$label,
                                           month = all_adm_month$month,
                                           scot_deaths = all_adm_month$scot_deaths,
                                           scot_pats = all_adm_month$scot_pats,
                                           completeness_date =
                                             all_adm_month$completeness_date) %>%
    distinct(.keep_all = TRUE)

  # Combines above in to a data frame with a row for each combination of location,
  # sub_grp and label for each quarter
  trends_data_all_month <- merge(location_template_month, month_template) %>%
    left_join(scot_deaths_template_month) %>%
    left_join(all_adm_month) %>%
    replace_na(list(deaths=0, pats=0, crd_rate=0)) %>%
    na.omit()

  # Calculate the sum of deaths per subgroup per location
  trends_all_deaths_month <- group_by(trends_data_all_month, location, label) %>%
    summarise(all_deaths = sum(deaths)) %>%
    ungroup()

  # Merge above together and drop subgroups where there were no deaths
  trend_data_month <- left_join(trends_data_all_month, trends_all_deaths_month) %>%
    filter(all_deaths !=0) %>%
    select(-all_deaths) %>%
    mutate(time_period = "Month")

  # Merge monthly and quarterly together
  trend_data <- bind_rows(trend_data_month,trend_data_quarter) %>%
    change_hbcodes(version_to = "19") %>% 
    # excluding out of publication period records
    filter(!(quarter >20 & time_period == "Quarter")) 
  
  return(trend_data)


}

### END OF SCRIPT ###

