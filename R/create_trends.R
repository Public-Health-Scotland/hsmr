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
#'
#' @return If the class is not initiated correctly, nothing is returned.
#'
#' @importFrom dplyr %>%
#' @importFrom magrittr %<>%
#'
#'
#' @export

create_trends <- function(smr01, gro, pop, dep, spec) {

  if(!tibble::is_tibble(smr01) | !tibble::is_tibble(gro) |
     !tibble::is_tibble(pop) | !tibble::is_tibble(dep)) {

    stop(paste0("All arguments provided to the function ",
                "must be in tibble format. Verify whether ",
                "an object is a tibble or not with ",
                "the tibble::is_tibble() function"))
  }

  if(!all(c("link_no", "admission_date", "discharge_date", "cis_marker",
            "postcode", "specialty", "discharge_type", "sex", "admgrp",
            "admfgrp", "ipdc", "age_grp", "quarter",
            "year") %in% names(smr01))) {

    stop(paste0("Object smr01 does not contain the required variables.",
                "Must contain:
                link_no
                admission_date
                discharge_date
                cis_marker
                postcode
                specialty
                discharge_type
                sex
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

  ### 2 - SIMD ----

  # Fix formatting of postcode variable
  smr01 %<>%

    # First remove all spaces from postcode variable
    tidylog::mutate(postcode = gsub("\\s", "", postcode),

           # Then add space (or spaces) at appropriate juncture (depending on
           # the number of characters) to get the postcode into 7-character
           # format
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
      year >= 2014 ~ simd_2016,
      year > 2009 & year < 2014 ~ simd_2012,
      year <= 2009 ~ simd_2009
    )) %>%

    # Remove the not needed year-specific SIMD variables
    tidylog::select(-c(simd_2009:simd_2016))


  ### 3 - Manipulations ----

  smr01 %<>%
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
             TRUE ~ 0)) %>%
    dplyr::arrange(link_no, cis_marker, admission_date, discharge_date) %>%
    tidylog::group_by(link_no, quarter) %>%
    tidylog::mutate(last_cis = max(cis_marker)) %>%
    tidylog::group_by(quarter) %>%
    tidylog::mutate(adm_first = min(admission_date)) %>%
    tidylog::mutate(quarter_full = qtr(as.Date(adm_first), "long"),
                    quarter_short = qtr(as.Date(adm_first), "short")) %>%
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
    tidylog::mutate(quarter = as.numeric(as.factor(quarter)))


  ### 4 - Aggregation ----

  # Crude Rates (Scotland) - All Admissions
  scot_all_adm <- smr01 %>%
    tidylog::group_by(quarter, quarter_full, quarter_short) %>%
    tidylog::summarise(deaths = sum(death30),
              pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = "All Admissions",
           hbtreat_currentdate = "Scotland")

  # Crude Rates (Hosp) - All Admissions
  hosp_all_adm <- smr01 %>%
    tidylog::group_by(quarter, location, quarter_full, quarter_short) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = "All Admissions") %>%
    dplyr::rename(hbtreat_currentdate = location)

  # Crude Rates (HB) - All Admissions
  hb_all_adm <- smr01 %>%
    tidylog::group_by(quarter, hbtreat_currentdate, quarter_full, quarter_short) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = "All Admissions")

  # Crude Rates (Scotland) - Specialty/Admission type
  scot_adm <- smr01 %>%
    tidylog::group_by(quarter, quarter_full, quarter_short, admgrp) %>%
    tidylog::summarise(deaths = sum(death30),
              pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      admgrp == 1 ~ "Elective",
      admgrp == 2 ~ "Non-Elective"
    ),
    hbtreat_currentdate = "Scotland") %>%
    tidylog::select(hbtreat_currentdate, quarter, quarter_full, quarter_short, deaths, pats,
                    label)


  # Crude Rates (Scotland) - Age group
  scot_age <- smr01 %>%
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
    hbtreat_currentdate = "Scotland")%>%
    tidylog::select(hbtreat_currentdate, quarter, quarter_full, quarter_short, deaths, pats,
                    label)


  # Crude Rates (Scotland) - Sex
  scot_sex <- smr01 %>%
    tidylog::group_by(quarter, quarter_full, quarter_short, sex) %>%
    tidylog::summarise(deaths = sum(death30),
              pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female"
    ),
    hbtreat_currentdate = "Scotland")%>%
    tidylog::select(hbtreat_currentdate, quarter, quarter_full, quarter_short, deaths, pats,
                    label)

  # Crude Rates (Scotland) Place of Death
  scot_place_of_death <- smr01 %>%
    tidylog::group_by(quarter, quarter_full, quarter_short, death_inhosp_max) %>%
    tidylog::summarise(deaths = sum(death30),
                       pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      death_inhosp_max == 1 ~ "Died in Hospital",
      death_inhosp_max == 0 ~ "Died in Community"
    ),
    hbtreat_currentdate = "Scotland")%>%
    tidylog::select(hbtreat_currentdate, quarter, quarter_full, quarter_short, deaths, pats,
                    label)


  # Crude Rates (Scotland) - Deprivation
  scot_dep <- smr01 %>%
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
    hbtreat_currentdate = "Scotland") %>%
    tidylog::select(hbtreat_currentdate, quarter, quarter_full, quarter_short, deaths, pats,
                    label)

  # Crude Rates (Scotland) - Specialty
  scot_spec <- smr01 %>%
    tidylog::group_by(quarter, quarter_full, quarter_short, spec_grp) %>%
    tidylog::summarise(deaths = sum(death30),
              pats   = length(death30)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label = dplyr::case_when(
      is.na(spec_grp) ~ "Unknown",
      spec_grp == 1   ~ "Community",
      spec_grp == 2   ~ "Dental",
      spec_grp == 3   ~ "Medical",
      spec_grp == 4   ~ "Other",
      spec_grp == 5   ~ "Paediatrics",
      spec_grp == 6   ~ "Surgery",
      spec_grp == 7   ~ "Emergency",
      spec_grp == 8   ~ "Women & Newborn"
    ),
    hbtreat_currentdate = "Scotland") %>%
    tidylog::select(hbtreat_currentdate, quarter, quarter_full, quarter_short, deaths, pats,
                    label)

  # Merge dataframes together
  scot_subgroups <- dplyr::bind_rows(scot_all_adm, hb_all_adm, hosp_all_adm,
                                     scot_age, scot_sex, scot_adm,
                              scot_dep, scot_spec, scot_place_of_death) %>%
    tidylog::mutate(crd_rate = deaths/pats * 100) %>%
    dplyr::rename(hb2014 = hbtreat_currentdate) %>%
    tidylog::select(hb2014, quarter, quarter_full, quarter_short, deaths, pats, crd_rate, label)

  # Crude Rate - Date of Discharge (Scotland)
  scot_dis <- smr01 %>%
    tidylog::group_by(quarter, quarter_full, quarter_short) %>%
    tidylog::summarise(deaths = sum(death30_dis),
              pats   = length(death30_dis)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label      = "Discharge",
           hbtreat_currentdate = "Scotland")

  # Crude Rate - Date of Discharge (NHS Board)
  hb_dis <- smr01 %>%
    tidylog::group_by(quarter, quarter_full, quarter_short, hbtreat_currentdate) %>%
    tidylog::summarise(deaths = sum(death30_dis),
              pats   = length(death30_dis)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(label     = "Discharge")

  # Merge dataframes together
  dis <- dplyr::bind_rows(scot_dis, hb_dis) %>%
    tidylog::mutate(crd_rate = deaths/pats * 100) %>%
    dplyr::rename(hb2014 = hbtreat_currentdate) %>%
    tidylog::select(hb2014, quarter, quarter_full, quarter_short, deaths, pats, crd_rate, label)

  # Population-based mortality
  scot_pop <- gro %>%
    tidylog::group_by(quarter, year) %>%
    tidylog::mutate(adm_first = min(date_of_death)) %>%
    tidylog::mutate(quarter_full = qtr(as.Date(adm_first), "long"),
                    quarter_short = qtr(as.Date(adm_first), "short")) %>%
    dplyr::ungroup() %>%
    tidylog::filter(date_of_death > end_date - years(5)) %>%
    tidylog::group_by(year, quarter, quarter_full, quarter_short) %>%
    tidylog::summarise(deaths = length(year)) %>%
    dplyr::ungroup() %>%
    tidylog::mutate(hbres_currentdate = "Scotland")

  hb_pop <- gro %>%
    tidylog::group_by(quarter, year) %>%
    tidylog::mutate(adm_first = min(date_of_death)) %>%
    tidylog::mutate(quarter_full = qtr(as.Date(adm_first), "long"),
                    quarter_short = qtr(as.Date(adm_first), "short")) %>%
    dplyr::ungroup() %>%
    tidylog::filter(date_of_death > end_date - years(5)) %>%
    tidylog::group_by(year, quarter, quarter_full, quarter_short, hbres_currentdate) %>%
    tidylog::summarise(deaths = length(year)) %>%
    dplyr::ungroup()

  pop_deaths <- dplyr::bind_rows(scot_pop, hb_pop) %>%
    tidylog::left_join(pop, by = c("year", "hbres_currentdate" = "hb2014")) %>%
    tidylog::mutate(crd_rate     = deaths/pop * 1000,
           quarter      = as.numeric(as.factor(paste0(year, "Q", quarter))),
           label        = "Population") %>%
    dplyr::rename(hb2014 = hbres_currentdate,
           pats   = pop) %>%
    tidylog::select(hb2014, quarter, quarter_full, quarter_short, deaths, pats, crd_rate,
                    label)


  # Create minimal tidy dataset
  long_term_trends <- dplyr::bind_rows(scot_subgroups, dis, pop_deaths) %>%
    mutate(completeness_date = submission_deadline(end_date))

  return(long_term_trends)


  }

### END OF SCRIPT ###
