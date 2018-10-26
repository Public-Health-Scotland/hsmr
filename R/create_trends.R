#' @title minimal tidy data set for long term trends in crude mortality report
#' production.
#'
#' @description Returns the minimal tidy data set for long terms trend data
#' which feeds into the reproducible analytical pipeline of the quarterly
#' Hospital Standardised Mortality Ratios publicatoin.
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
#' @examples
#'
#'
#' @export

create_trends <- function(smr01, gro, pop, dep){

  if(("link_no" %!in% names(smr01)) |
     ("admission_date" %!in% names(smr01)) |
     ("discharge_date" %!in% names(smr01)) |
     ("cis_marker" %!in% names(smr01)) |
     ("postcode" %!in% names(smr01)) |
     ("specialty" %!in% names(smr01)) |
     ("discharge_type" %!in% names(smr01)) |
     ("sex" %!in% names(smr01)) |
     ("admgrp" %!in% names(smr01)) |
     ("admfgrp" %!in% names(smr01)) |
     ("ipdc" %!in% names(smr01)) |
     ("age_grp" %!in% names(smr01)) |
     ("quarter" %!in% names(smr01)) |
     ("year" %!in% names(smr01))){

    stop(paste0("Object smr01 does not contain the correct variables.",
         "Must contain:
         link_no
         admission_date
         discharge_data
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

  if(("link_no" %!in% names(gro)) |
     ("date_of_death" %!in% names(gro)) |
     ("hbres_currentdate" %!in% names(gro)) |
     ("quarter" %!in% names(gro)) |
     ("year" %!in% names(gro))){

    stop(paste0("Object gro does not contain the correct variables.",
                "Must contain:
                link_no
                date_of_death
                hbres_currentdate
                quarter
                year"))
  }


  ### 1 - Deaths Data ----

  # Removing duplicate records on link_no as the deaths file is matched on to
  # smr01 by link_no, and link_no needs to be unique
  gro %<>%
    distinct(link_no, .keep_all = TRUE)

  # Matching deaths data on to smr01 data
  smr01 %<>%
    left_join(select(gro, link_no, date_of_death), by = "link_no") %>%

    # Sorting data by link_no, cis_marker, adm_date and dis_date
    arrange(link_no, cis_marker, admission_date, discharge_date)

  ### 2 - SIMD ----

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
    left_join(dep, by = "postcode") %>%

    # Assign the appropriate SIMD value to a patient depending on the year they
    # were admitted
    mutate(simd = case_when(
      year >= 2014 ~ simd_2016,
      year > 2009 & year < 2014 ~ simd_2012,
      year <= 2009 ~ simd_2009
    )) %>%

    # Remove the not needed year-specific SIMD variables
    select(-c(simd_2009:simd_2016))


  ### 3 - Manipulations ----

  smr01 %<>%
    mutate(death_inhosp = if_else(between(as.numeric(discharge_type), 40, 49),
                                  1, 0),
           dthdays = interval(admission_date, date_of_death) / days(1),
           death30 = case_when(
             between(dthdays, 0, 30) ~ 1,
             TRUE ~ 0),
           quarter_name = paste0(year, "Q", quarter),
           quarter = as.numeric(as.factor(quarter_name))) %>%
    group_by(link_no, cis_marker) %>%
    mutate(epinum             = row_number(),
           death_inhosp_max   = max(death_inhosp),
           discharge_date_cis = max(discharge_date)) %>%
    ungroup() %>%
    mutate(dthdays_dis = interval(discharge_date_cis, date_of_death) / days(1),
           death30_dis = case_when(
             between(dthdays_dis, 0, 30) ~ 1,
             TRUE ~ 0)) %>%
    arrange(link_no, cis_marker, admission_date, discharge_date) %>%
    group_by(link_no, quarter) %>%
    mutate(last_cis = max(cis_marker)) %>%
    ungroup() %>%
    filter(epinum == 1 & cis_marker == last_cis) %>%

    # get David to write comment on why this is done
    filter(!(link_no == c(0, head(link_no, -1)) &
               1 == c(0, head(death30, -1))))


  ### 4 - Aggregation ----

  # Crude Rates (Scotland) - All Admissions
  z_scot_all_adm <- smr01 %>%
    group_by(quarter) %>%
    summarise(deaths = sum(death30),
              pats   = length(death30)) %>%
    ungroup() %>%
    mutate(label = "All Admissions",
           hbtreat_currentdate = "Scotland")

  # Crude Rates (Scotland) - Specialty/Admission type
  z_scot_specadm <- smr01 %>%
    group_by(quarter, surgmed, admgrp) %>%
    summarise(deaths = sum(death30),
              pats   = length(death30)) %>%
    ungroup() %>%
    mutate(label = case_when(
      surgmed == 1 & admgrp == 1 ~ "Elective/Non-Surgical",
      surgmed == 2 & admgrp == 1 ~ "Elective/Surgical",
      surgmed == 1 & admgrp == 2 ~ "Non-Elective/Non-Surgical",
      surgmed == 2 & admgrp == 2 ~ "Non-Elective/Surgical"
    ),
    hbtreat_currentdate = "Scotland") %>%
    select(hbtreat_currentdate, quarter, deaths, pats, label)


  # Crude Rates (Scotland) - Age group
  z_scot_age <- smr01 %>%
    group_by(quarter, age_grp) %>%
    summarise(deaths = sum(death30),
              pats   = length(death30)) %>%
    ungroup() %>%
    mutate(label = case_when(
      age_grp == 1 ~ "0-19 years",
      age_grp == 2 ~ "20-39 years",
      age_grp == 3 ~ "40-59 years",
      age_grp == 4 ~ "60-79 years",
      age_grp == 5 ~ "80+ years"
    ),
    hbtreat_currentdate = "Scotland")%>%
    select(hbtreat_currentdate, quarter, deaths, pats, label)


  # Crude Rates (Scotland) - Sex
  z_scot_sex <- smr01 %>%
    group_by(quarter, sex) %>%
    summarise(deaths = sum(death30),
              pats   = length(death30)) %>%
    ungroup() %>%
    mutate(label = case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female"
    ),
    hbtreat_currentdate = "Scotland")%>%
    select(hbtreat_currentdate, quarter, deaths, pats, label)


  # Crude Rates (Scotland) - Deprivation
  z_scot_dep <- smr01 %>%
    group_by(quarter, simd) %>%
    summarise(deaths = sum(death30),
              pats   = length(death30)) %>%
    ungroup() %>%
    mutate(label = case_when(
      is.na(simd) ~ "Unknown",
      simd == 1   ~ "1 - Most Deprived",
      simd == 2   ~ "2",
      simd == 3   ~ "3",
      simd == 4   ~ "4",
      simd == 5   ~ "5 - Least Deprived"
    ),
    hbtreat_currentdate = "Scotland") %>%
    select(hbtreat_currentdate, quarter, deaths, pats, label)

  # Merge dataframes together
  z_scot_subgroups <- bind_rows(z_scot_all_adm, z_scot_age,
                                z_scot_sex, z_scot_specadm,
                                z_scot_dep) %>%
    mutate(crd_rate = deaths/pats * 100) %>%
    rename(HB2014 = hbtreat_currentdate) %>%
    select(HB2014, quarter, deaths, pats, crd_rate, label)

  # Crude Rate - Date of Discharge (Scotland)
  z_scot_dis <- smr01 %>%
    group_by(quarter) %>%
    summarise(deaths = sum(death30_dis),
              pats   = length(death30_dis)) %>%
    ungroup() %>%
    mutate(label               = "Discharge",
           hbtreat_currentdate = "Scotland")

  # Crude Rate - Date of Discharge (NHS Board)
  z_hb_dis <- smr01 %>%
    group_by(quarter, hbtreat_currentdate) %>%
    summarise(deaths = sum(death30_dis),
              pats   = length(death30_dis)) %>%
    ungroup() %>%
    mutate(label     = "Discharge")

  # Merge dataframes together
  z_dis <- bind_rows(z_scot_dis, z_hb_dis) %>%
    mutate(crd_rate = deaths/pats * 100) %>%
    rename(HB2014 = hbtreat_currentdate) %>%
    select(HB2014, quarter, deaths, pats, crd_rate, label)

  # Population-based mortality
  z_scot_pop <- gro %>%
    group_by(year, quarter) %>%
    summarise(deaths = length(year)) %>%
    ungroup() %>%
    mutate(hbres_currentdate = "Scotland")

  z_hb_pop <- gro %>%
    group_by(year, quarter, hbres_currentdate) %>%
    summarise(deaths = length(year)) %>%
    ungroup()

  z_pop_deaths <- bind_rows(z_scot_pop, z_hb_pop) %>%
    left_join(pop, by = c("year", "hbres_currentdate" = "hb2014")) %>%
    mutate(crd_rate     = deaths/pop * 1000,
           quarter_name = paste0(year, "Q", quarter),
           quarter      = as.numeric(as.factor(quarter_name)),
           label        = "Population") %>%
    rename(HB2014 = hbres_currentdate,
           pats   = pop) %>%
    select(HB2014, quarter, deaths, pats, crd_rate, label)

  long_term_trends <- bind_rows(z_scot_subgroups, z_dis, z_pop_deaths)

  return(long_term_trends)

}
