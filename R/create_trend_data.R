#########################################################################
# Name of file - create_hsmr_data.R
# data release - Quarterly HSMR publication
# Original Authors - David Caldwell & Anna Price
# Orginal Date - August 2018
#
# Type - data extraction/preparation/modelling
# Written/run on - RStudio server
# Version of R - 3.2.3
#
# Description - Extracts SMR01 & deaths data and carries out required
# manipulations to create minimal tidy dataset for long term trends for HSMR
#
# Approximate run time - xx minutes
#########################################################################


### SECTION 1 - HOUSE KEEPING ----

### 1 - Load packages ----
library(odbc)          # Accessing SMRA
library(dplyr)         # For data manipulation in the "tidy" way
library(haven)         # For reading in spss files
library(janitor)       # For 'cleaning' variable names
library(magrittr)      # For %<>% operator
library(lubridate)     # For dates


### 2 - Define the database connection with SMRA
smra_connect <- suppressWarnings(
  dbConnect(
    odbc(),
    dsn = "SMRA",
    uid = .rs.askForPassword("SMRA Username:"),
    pwd = .rs.askForPassword("SMRA Password:")))


### 3 - Extract dates ----
# Define the dates that the data are extracted from and to
z_start_date   <- dmy(01012008)     # The beginning of the ten year trend
z_end_date     <- dmy(31032018)     # End date for the cut off for z_smr01


# Postcode lookups for SIMD 2016, 2012 and 2009
z_simd_2016 <- read_spss(paste0(
  "/conf/linkage/output/lookups/Unicode/Deprivation",
  "/postcode_2018_1.5_simd2016.sav")) %>%
  select(pc7, simd2016_sc_quintile)

z_simd_2012 <- read_spss(paste0(
  "/conf/linkage/output/lookups/Unicode/Deprivation/",
  "postcode_2016_1_simd2012.sav")) %>%
  select(pc7, simd2012_sc_quintile)

z_simd_2009 <- read_spss(paste0(
  "/conf/linkage/output/lookups/Unicode/Deprivation/",
  "postcode_2012_2_simd2009v2.sav")) %>%
  select(PC7, simd2009v2_sc_quintile) %>%
  clean_names()

# Population lookups for 2017
z_pop_est  <- read_spss(paste0(
  "/conf/linkage/output/lookups/populations/estimates/",
  "HB2014_pop_est_1981_2016.sav")) %>%
  clean_names() %>%
  group_by(year, hb2014) %>%
  summarise(pop = sum(pop)) %>%
  ungroup()

z_pop_proj <- read_spss(paste0(
  "/conf/linkage/output/lookups/populations/projections/",
  "HB2014_pop_proj_2016_2041.sav")) %>%
  clean_names() %>%
  filter(year >= 2017) %>%
  group_by(year, hb2014) %>%
  summarise(pop = sum(pop)) %>%
  ungroup()

# Combine population lookups into one lookup
# Ignore warning messages about vectorising labelled elements
z_pop <- bind_rows(z_pop_est, z_pop_proj)

# Aggregate lookup to get Scotland population and append to bottom
z_pop %<>%
  bind_rows(., z_pop %>%
              group_by(year) %>%
              summarise(pop = sum(pop)) %>%
              ungroup() %>%
              mutate(hb2014 = "Scotland"))


### SECTION 2 - DATA EXTRACTION----

### 1 - data extraction ----
# Source SQL queries
source("R/sql_queries_trends.R")


### 2 - Extract data ----
z_gro     <- as_tibble(dbGetQuery(smra_connect, z_query_gro)) %>%
  clean_names()

z_smr01   <- as_tibble(dbGetQuery(smra_connect, z_query_smr01_ltt)) %>%
  clean_names()


### SECTION 3 - DATA PREPARATION----


### 1 - Deaths Data ----
# Removing duplicate records on link_no as the deaths file is matched on to SMR01 by link_no
# link_no needs to be unique
z_gro <- z_gro %>%
  distinct(link_no, .keep_all = TRUE)

# Matching deaths data on to SMR01 data
z_smr01$date_of_death <- z_gro$date_of_death[match(z_smr01$link_no,z_gro$link_no)]

# Sorting data by link_no, cis_marker, adm_date and dis_date
z_smr01 <- z_smr01 %>%
  arrange(link_no, cis_marker, admission_date, discharge_date)


### 2 - SIMD ----

# Fix formatting of postcode variable (remove trailing spaces and any other
# unnecessary white space)
z_smr01$postcode <- sub("  ", " ", z_smr01$postcode)
z_smr01$postcode <- sub("   ", "  ", z_smr01$postcode)
z_smr01$postcode[which(regexpr(" ", z_smr01$postcode) == 5)] <- sub(" ", "", z_smr01$postcode[which(regexpr(" ", z_smr01$postcode) == 5)])

# Match SIMD 2016 onto years beyond 2014
names(z_simd_2016)                  <- c("postcode", "simd")
z_smr01$simd[which(z_smr01$year >= 2014)] <- z_simd_2016$simd[match(z_smr01$postcode, z_simd_2016$postcode)]

# Match SIMD 2012 onto years before 2014 and after 2009
names(z_simd_2012)                  <- c("postcode", "simd")
z_smr01$simd[which(z_smr01$year < 2014 & z_smr01$year > 2009)]  <- z_simd_2012$simd[match(z_smr01$postcode, z_simd_2012$postcode)]

# Match SIMD 2009 onto years before 2010
names(z_simd_2009)                  <- c("postcode", "simd")
z_smr01$simd[which(z_smr01$year < 2010)]  <- z_simd_2009$simd[match(z_smr01$postcode, z_simd_2009$postcode)]


### 3 - Manipulations ----

z_smr01 <- z_smr01 %>%
  mutate(death_inhosp = ifelse(discharge_type >= 40 & discharge_type <= 49, 1, 0),
         dthdays      = (date_of_death - admission_date)/60/60/24,
         death30      = ifelse(dthdays >= 0 & dthdays <= 30, 1, 0),
         death30      = ifelse(is.na(death30), 0, death30),
         quarter_name = paste(year, "Q", quarter, sep = ""),
         quarter      = as.numeric(as.factor(quarter_name))) %>%
  group_by(link_no, cis_marker) %>%
  mutate(epinum             = row_number(),
         death_inhosp_max   = max(death_inhosp),
         discharge_date_cis = max(discharge_date)) %>%
  ungroup() %>%
  mutate(dthdays_dis        = (date_of_death - discharge_date_cis),
         death30_dis        = ifelse(dthdays_dis >= 0 & dthdays_dis <= 30, 1, 0),
         death30_dis        = ifelse(is.na(death30_dis), 0, death30_dis)) %>%
  arrange(link_no, cis_marker, admission_date, discharge_date) %>%
  group_by(link_no, quarter) %>%
  mutate(last_cis = max(cis_marker)) %>%
  ungroup() %>%
  filter(epinum == 1 & cis_marker == last_cis)

cond <- c(z_smr01$link_no == c(0, z_smr01$link_no[-length(z_smr01$link_no)]) &
            1 == c(0, z_smr01$death30[-length(z_smr01$death30)]))

z_smr01 <- z_smr01[!cond,]


### 4 - Aggregation ----

# Crude Rates (Scotland) - All Admissions
z_scot_all_adm <- z_smr01 %>%
  group_by(quarter) %>%
  summarise(deaths = sum(death30),
            pats   = length(death30)) %>%
  ungroup() %>%
  mutate(label = "All Admissions",
         hbtreat_currentdate = "Scotland")

# Crude Rates (Scotland) - Specialty/Admission type
z_scot_specadm <- z_smr01 %>%
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
z_scot_age <- z_smr01 %>%
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
z_scot_sex <- z_smr01 %>%
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
z_scot_dep <- z_smr01 %>%
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
z_scot_subgroups <- rbind(z_scot_all_adm, z_scot_age,
                          z_scot_sex, z_scot_specadm,
                          z_scot_dep) %>%
  mutate(crd_rate = deaths/pats * 100) %>%
  rename(HB2014 = hbtreat_currentdate) %>%
  select(HB2014, quarter, deaths, pats, crd_rate, label)

# Crude Rate - Date of Discharge (Scotland)
z_scot_dis <- z_smr01 %>%
  group_by(quarter) %>%
  summarise(deaths = sum(death30_dis),
            pats   = length(death30_dis)) %>%
  ungroup() %>%
  mutate(label               = "Discharge",
         hbtreat_currentdate = "Scotland")

# Crude Rate - Date of Discharge (NHS Board)
z_hb_dis <- z_smr01 %>%
  group_by(quarter, hbtreat_currentdate) %>%
  summarise(deaths = sum(death30_dis),
            pats   = length(death30_dis)) %>%
  ungroup() %>%
  mutate(label       = "Discharge")

# Merge dataframes together
z_dis <- rbind(z_scot_dis, z_hb_dis) %>%
  mutate(crd_rate = deaths/pats * 100) %>%
  rename(HB2014 = hbtreat_currentdate) %>%
  select(HB2014, quarter, deaths, pats, crd_rate, label)

# Population-based mortality
z_scot_pop <- z_gro %>%
  group_by(year, quarter) %>%
  summarise(deaths = length(year)) %>%
  mutate(hbres_currentdate = "Scotland")

z_hb_pop <- z_gro %>%
  group_by(year, quarter, hbres_currentdate) %>%
  summarise(deaths = length(year))

z_pop_deaths <- rbind(z_scot_pop, z_hb_pop) %>%
  ungroup() %>%
  left_join(z_pop, by = c("year"= "Year", "hbres_currentdate" = "HB2014")) %>%
  mutate(crd_rate     = deaths/pop * 1000,
         quarter_name = paste(year, "Q", quarter, sep = ""),
         quarter      = as.numeric(as.factor(quarter_name)),
         label        = "Population") %>%
  rename(HB2014 = hbres_currentdate,
         pats   = pop) %>%
  select(HB2014, quarter, deaths, pats, crd_rate, label)

long_term_trends <- rbind(z_scot_subgroups, z_dis, z_pop_deaths)

readr::write_csv(long_term_trends, path = 'long_term_trends.csv')
