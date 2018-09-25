start <- proc.time()
#########################################################################
# Name of file - create_hsmr_data.R
# Data release - Quarterly HSMR publication
# Original Authors - David Caldwell
# Orginal Date - February 2018
#
# Type - Data extraction/preparation/modelling
# Written/run on - RStudio server
# Version of R - 3.2.3
#
# Description - Extracts SMR01 & deaths data and carries out required
# manipulations and modelling to create the minimal tidy dataset for HSMR
#
# Approximate run time - xx minutes
#########################################################################


### SECTION 1 - HOUSE KEEPING ----

### 1 - Load packages ----
library(odbc)          # For accessing SMRA databases
library(dplyr)         # For data manipulation in the "tidy" way
library(haven)         # For reading in SPSS files
library(readr)         # For reading in csv files
library(janitor)       # For 'cleaning' variable names
library(magrittr)      # For %<>% operator
library(lubridate)     # For dates
library(tidyr)         # For data manipulation in the "tidy" way
library(fuzzyjoin)     # For fuzzy joins
library(stringr)       # For string matching
library(data.table)    # For fast data manipulations


### 2 - Define the database connection with SMRA ----

z_smra_connect <- suppressWarnings(
  dbConnect(
    odbc(),
    dsn = "SMRA",
    uid = .rs.askForPassword("SMRA Username:"),
    pwd = .rs.askForPassword("SMRA Password:")))


### 3 - Extract dates ----


# Define the dates that the data are extracted from and to


# The beginning of baseline period
z_start_date   <- dmy(01012011)

# Five years earlier for the five year look-back (pmorbs5)
z_start_date_5 <- dmy(01012006)

# Beginning of the baseline period (pmorbs)
z_start_date_l <- dmy(01012011)

# End date for the cut off for data
z_end_date     <- dmy(31032018)


### 4 - Set filepaths ----

# Define lookups directory
z_lookups <- "R/reference_files/"


### 5 - Read in lookup files ----


# Primary Diagnosis Groupings
z_pdiag_grp_data <- read_spss(paste0(
  z_lookups,
  'shmi_diag_grps_lookup.sav')) %>%
  select(diag1_4, SHMI_DIAGNOSIS_GROUP) %>%
  clean_names()


# ICD-10 codes, their Charlson Index Groupings and CIG weights
# NOTE - C80 code is duplicated
z_morbs <- read_csv(paste0(z_lookups,
                           "morbs.csv")) %>%

  # Gather ICD codes into a single column
  gather(code, diag, diag_3:diag_4) %>%
  select(-code) %>%

  # Remove all NAs from the ICD-10 column
  drop_na(diag) %>%

  # Remove the second C80 entry
  distinct(diag, .keep_all = TRUE)


# Postcode lookups for SIMD 2016 and 2012
z_simd_2016 <- read_spss(paste0(
  "/conf/linkage/output/lookups/Unicode/Deprivation",
  "/postcode_2018_1.5_simd2016.sav")) %>%
  select(pc7, simd2016_sc_quintile)

z_simd_2012 <- read_spss(paste0(
  "/conf/linkage/output/lookups/Unicode/Deprivation/",
  "postcode_2016_1_simd2012.sav")) %>%
  select(pc7, simd2012_sc_quintile)


# Hospital names
z_hospitals <- read_csv(paste0(z_lookups,
                               "location_lookups.csv"))


### 6 - Source functions ----

source("R/data_prep_functions.R")



### SECTION 2 - DATA EXTRACTION----

# Source SQL queries
source("R/sql_queries_smr.R")

# Extract deaths and SMR01 data from SMRA databases
deaths  <- as_tibble(dbGetQuery(z_smra_connect, z_query_gro)) %>%
  clean_names()

z_smr01 <- as_tibble(dbGetQuery(z_smra_connect, z_query_smr01)) %>%
  clean_names()



### SECTION 3 - DATA PREPARATION----

### 1 - Match deaths data to SMR01 ----
# Remove duplicate records on link_no
# The deaths file is matched on to SMR01 by link_no,
# therefore link_no needs to be unique
deaths %<>%
  distinct(link_no, .keep_all = TRUE)

# Match deaths data on to SMR01 data
z_smr01 %<>%
  left_join(deaths, by = "link_no") %>%

  # Sort data by link_no, cis_marker, adm_date and dis_date as per guidance
  arrange(link_no, cis_marker, admission_date, discharge_date)

# Delete death tibble and remove from memory as no longer required
rm(deaths);gc()


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

z_smr01 %<>%
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
  left_join(select(z_pdiag_grp_data,
                   pdiag_grp = shmi_diagnosis_group,
                   diag1_4),
            by = "diag1_4") %>%

  # Fuzzy joins add the (in this case, not needed) joining variable by default,
  # so append these with "_z" so they can be easily removed afterwards
  fuzzy_left_join(select(z_morbs, wcomorbs1 = wmorbs, diag2_z = diag),
                  by = c("diag2" = "diag2_z"),
                  match_fun = str_detect) %>%
  fuzzy_left_join(select(z_morbs, wcomorbs2 = wmorbs, diag3_z = diag),
                  by = c("diag3" = "diag3_z"),
                  match_fun = str_detect) %>%
  fuzzy_left_join(select(z_morbs, wcomorbs3 = wmorbs, diag4_z = diag),
                  by = c("diag4" = "diag4_z"),
                  match_fun = str_detect) %>%
  fuzzy_left_join(select(z_morbs, wcomorbs4 = wmorbs, diag5_z = diag),
                  by = c("diag5" = "diag5_z"),
                  match_fun = str_detect) %>%
  fuzzy_left_join(select(z_morbs, wcomorbs5 = wmorbs, diag6_z = diag),
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
  mutate(wcomorbs2 = if_else(wcomorbs2 != wcomorbs1,
                             wcomorbs2,
                             0),
         wcomorbs3 = if_else(!(wcomorbs3 %in% c(wcomorbs1, wcomorbs2)),
                             wcomorbs3,
                             0),
         wcomorbs4 = if_else(!(wcomorbs4 %in% c(wcomorbs1, wcomorbs2,
                                                wcomorbs3)),
                             wcomorbs4,
                             0),
         wcomorbs5 = if_else(!(wcomorbs5 %in% c(wcomorbs1, wcomorbs2,
                                                wcomorbs3, wcomorbs4)),
                             wcomorbs5,
                             0),
         comorbs_sum = rowSums(select(., starts_with("wcomorbs")))) %>%

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
  select(-one_of(c("main_condition", "other_condition_1", "other_condition_2",
                   "other_condition_3", "other_condition_4",
                   "other_condition_5", "wcomorbs1", "wcomorbs2", "wcomorbs3",
                   "wcomorbs4", "wcomorbs5", "quarter_name")))

# Vector of unique link numbers used for filtering below
z_unique_id <- z_smr01 %>%
  distinct(link_no) %>%
  pull(link_no)


### 3 - Prior morbidities within previous 1 & 5 years ----

# Extract SMR01 data from SMRA database required for the prior morbidities
# ("pmorbs") look-back
data_pmorbs <- as_tibble(dbGetQuery(z_smra_connect,
                                    z_query_smr01_minus5)) %>%
  clean_names()

# Create the following variables:
# diag1   = ICD10 code for main condition to 3 and 4 digits, separated by an
#           underscore
# pmorbs  = Charlson Index grouping (1-17) for main condition (0 if none apply)
# pmorbs5_1 to pmorbs1_17 = initialise empty vectors for use in loop below
# n_emerg                 = initialise empty vector for use in loop below

data_pmorbs %<>%
  mutate(diag1 = paste(substr(main_condition, 1, 3),
                       substr(main_condition, 1, 4),
                       sep = "_")) %>%

  # Create the pmorbs variable using a join to the z_morbs dataset
  fuzzy_left_join(select(z_morbs, pmorbs = morb, diag1_z = diag),
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


data_pmorbs <- pmorbs_backup
data_pmorbs <- data.table(data_pmorbs)


# For every row in the pmorbs extract, look at each of the prior 50 rows and
# IF the previous episode belongs to the same person
# AND the admission date on the episode is after the start date
# AND the pmorbs value belongs to one of the Charlson index groups
# AND the time between the two episodes is either 5 or 1 year(s)
# THEN assign the correct Charlson Index weighting. These weightings are saved in the
# 34 (pmorbs5_1 to pmorbs1_17) vectors initiliased above.

for(i in 1:50){

  # 1:50 because the 95th percentile of episode counts per patient was 51
  print(i)

  data_pmorbs[, `:=`(old_admission = (admission_date - shift(admission_date, i))/60/60/24,
                     old_pmorbs = shift(pmorbs, i), old_link = shift(link_no, i))]

  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs == 1 & old_link == link_no &
                old_admission <= 1825, pmorbs5_1:=5]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 2 & old_link== link_no &
                old_admission<= 1825, pmorbs5_2:=11]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 3 & old_link== link_no &
                old_admission<= 1825, pmorbs5_3:=13]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 4 & old_link== link_no &
                old_admission<= 1825, pmorbs5_4:=4]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 5 & old_link== link_no &
                old_admission<= 1825, pmorbs5_5:=14]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 6 & old_link== link_no &
                old_admission<= 1825, pmorbs5_6:=3]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 7 & old_link== link_no &
                old_admission<= 1825, pmorbs5_7:=8]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 8 & old_link== link_no &
                old_admission<= 1825, pmorbs5_8:=9]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 9 & old_link== link_no &
                old_admission<= 1825, pmorbs5_9:=6]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 10 & old_link== link_no &
                old_admission<= 1825, pmorbs5_10:=4]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 11 & old_link== link_no &
                old_admission<= 1825, pmorbs5_11:=8]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 12 & old_link== link_no &
                old_admission<= 1825, pmorbs5_12:=-1]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 13 & old_link== link_no &
                old_admission<= 1825, pmorbs5_13:=1]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 14 & old_link== link_no &
                old_admission<= 1825, pmorbs5_14:=10]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 15 & old_link== link_no &
                old_admission<= 1825, pmorbs5_15:=14]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 16 & old_link== link_no &
                old_admission<= 1825, pmorbs5_16:=18]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 17 & old_link== link_no &
                old_admission<= 1825, pmorbs5_17:=2]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 1 & old_link== link_no &
                old_admission<= 365, pmorbs1_1:=5]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 2 & old_link== link_no &
                old_admission<= 365, pmorbs1_2:=11]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 3 & old_link== link_no &
                old_admission<= 365, pmorbs1_3:=13]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 4 & old_link== link_no &
                old_admission<= 365, pmorbs1_4:=4]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 5 & old_link== link_no &
                old_admission<= 365, pmorbs1_5:=14]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 6 & old_link== link_no &
                old_admission<= 365, pmorbs1_6:=3]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 7 & old_link== link_no &
                old_admission<= 365, pmorbs1_7:=8]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 8 & old_link== link_no &
                old_admission<= 365, pmorbs1_8:=9]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 9 & old_link== link_no &
                old_admission<= 365, pmorbs1_9:=6]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 10 & old_link== link_no &
                old_admission<= 365, pmorbs1_10:=4]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 11 & old_link== link_no &
                old_admission<= 365, pmorbs1_11:=8]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 12 & old_link== link_no &
                old_admission<= 365, pmorbs1_12:=-1]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 13 & old_link== link_no &
                old_admission<= 365, pmorbs1_13:=1]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 14 & old_link== link_no &
                old_admission<= 365, pmorbs1_14:=10]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 15 & old_link== link_no &
                old_admission<= 365, pmorbs1_15:=14]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 16 & old_link== link_no &
                old_admission<= 365, pmorbs1_16:=18]
  data_pmorbs[admission_date >= z_start_date_l & old_pmorbs== 17 & old_link== link_no &
                old_admission<= 365, pmorbs1_17:=2]
}

# Calculate the sum of the Charlson Index weightings for each CIS, for both 1
# and 5 years prior to admission
data_pmorbs[,`:=`(pmorbs1_sum = max(pmorbs1_1) + max(pmorbs1_2) + max(pmorbs1_3) + max(pmorbs1_4) + max(pmorbs1_5) + max(pmorbs1_6) + max(pmorbs1_7) + max(pmorbs1_8) + max(pmorbs1_9) +
                    max(pmorbs1_10) + max(pmorbs1_11) + max(pmorbs1_12) + max(pmorbs1_13) + max(pmorbs1_14) + max(pmorbs1_15) + max(pmorbs1_16) + max(pmorbs1_17),
                  pmorbs5_sum = max(pmorbs5_1) + max(pmorbs5_2) + max(pmorbs5_3) + max(pmorbs5_4) + max(pmorbs5_5) + max(pmorbs5_6) + max(pmorbs5_7) + max(pmorbs5_8) + max(pmorbs5_9) +
                    max(pmorbs5_10) + max(pmorbs5_11) + max(pmorbs5_12) + max(pmorbs5_13) + max(pmorbs5_14) + max(pmorbs5_15) + max(pmorbs5_16) + max(pmorbs5_17),
                  epinum = .I), by = .(link_no, cis_marker)]


# For every row in the pmorbs extract, look at each of the prior 50 rows and
# IF the previous episode belongs to the same person
# AND the time between the two episodes is 1 year
# AND the previous episode is an emergency admission
# THEN increase the number of emergency admissions by one in the n_emerg
# vector initiliased above.

for (i in 1:50) {
  # 1:50 because the 95th percentile of episode counts per patient was 51

  data_pmorbs <- data_pmorbs %>%
    mutate(n_emerg = if_else(!is.na(lag(link_no, i)), if_else(lag(old_smr1_tadm_code, i) >= 4 & link_no == lag(link_no, i) &
                                                                (admission_date - lag(admission_date, i)) <= 365, n_emerg + 1, n_emerg), n_emerg))
}

# Select required variables from data_pmorbs
data_pmorbs <- data_pmorbs %>%
  select(c("link_no", "cis_marker", "pmorbs1_sum", "pmorbs5_sum", "n_emerg"))

# Join data_pmorbs on to the main tibble
z_smr01 <- z_smr01 %>%
  left_join(data_pmorbs, by = c("link_no", "cis_marker"))

# Delete data_pmorbs as no longer required
rm(data_pmorbs);gc()


### 4 - SIMD ----

# Fix formatting of postcode variable (remove trailing spaces and any other
# unnecessary white space)
z_smr01$postcode <- sub("  ", " ", z_smr01$postcode)
z_smr01$postcode <- sub("   ", "  ", z_smr01$postcode)
z_smr01$postcode[which(regexpr(" ", z_smr01$postcode) == 5)] <- sub(" ", "", z_smr01$postcode[which(regexpr(" ", z_smr01$postcode) == 5)])

# Match SIMD 2016 onto years beyond 2014
names(z_simd_2016)                  <- c("postcode", "simd")
z_smr01$simd[which(z_smr01$year >= 2014)] <- z_simd_2016$simd[match(z_smr01$postcode, z_simd_2016$postcode)]

# Match SIMD 2012 onto years before 2014
names(z_simd_2012)                  <- c("postcode", "simd")
z_smr01$simd[which(z_smr01$year < 2014)]  <- z_simd_2012$simd[match(z_smr01$postcode, z_simd_2012$postcode)]


### 6 - Create patient level file

# Select first episode of final CIS for each patient
z_smr01 <- z_smr01 %>%
  group_by(link_no, quarter) %>%
  mutate(last_cis = max(cis_marker)) %>%
  ungroup() %>%
  filter(epinum == 1 & cis_marker == last_cis) %>%
  # Remove rows where SIMD, admfgrp and ipdc are missing as variables are required
  # for modelling/predicted values
  filter(!is.na(simd)) %>%
  filter(admfgrp %in% 1:6) %>%
  filter(ipdc %in% 1:2)

# If a patient dies within 30 days of admission in two subsequent quarters then
# remove the second record to avoid double counting deaths
z_cond  <- c(z_smr01$link_no == c(0, z_smr01$link_no[-length(z_smr01$link_no)]) &
               c(0, z_smr01$death30[-length(z_smr01$death30)]) == 1)

z_smr01 <- z_smr01[!z_cond, ]



### SECTION 4 - MODELLING ----

# Create subset of data for modelling
z_data_lr <- z_smr01 %>%

  # Select baseline period rows
  filter(quarter <= 12) %>%

  # Select required variables for model
  select(n_emerg, comorbs_sum, pmorbs1_sum, pmorbs5_sum, age_in_years, sex, surgmed,
         pdiag_grp, admfgrp, admgrp, ipdc, simd, death30) %>%

  # Calculate total number of deaths and total number of patients for each
  # combination of variables
  group_by(n_emerg, comorbs_sum, pmorbs1_sum, pmorbs5_sum, age_in_years, sex, surgmed,
           pdiag_grp, admfgrp, admgrp, ipdc, simd) %>%
  summarise(x = sum(death30), n = length(death30)) %>%
  ungroup()

# Run logistic regression
z_risk_model <- glm(cbind(x, n - x) ~ n_emerg + comorbs_sum + pmorbs1_sum +
                      pmorbs5_sum + age_in_years + factor(sex) + factor(surgmed) +
                      factor(pdiag_grp) + factor(admfgrp) + factor(admgrp) +
                      factor(ipdc) + factor(simd),
                    data = z_data_lr, family = "binomial", model = FALSE, y = FALSE)

# Delete unnecessary model information using bespoke function in order to retain
# special class of object for predicted probabilities below
z_risk_model <- clean_model(risk_model)

# Calculate predicted probabilities
z_smr01$pred_eq <- predict.glm(risk_model, z_smr01, type = "response")

# Remove rows with no probability calculated
z_smr01 <- z_smr01 %>%
  filter(!is.na(pred_eq))



### SECTION 5 - CREATE MINIMAL TIDY DATASET ----

### 1 - Create Scotland-level aggregation ----

z_hsmr_scot <- z_smr01 %>%
  group_by(quarter) %>%
  summarise(deaths = sum(death30),
            pred   = sum(pred_eq),
            pats   = length(death30)) %>%
  mutate(smr           = deaths/pred,
         crd_rate      = (deaths/pats) * 100,
         location_type = "Scotland",
         location      = "Scot",
         location_name = "Scotland")


### 2 - Create Hospital-level aggregation ----

z_hsmr_hosp <- z_smr01 %>%
  group_by(quarter, location) %>%
  summarise(deaths = sum(death30),
            pred   = sum(pred_eq),
            pats   = length(death30)) %>%
  mutate(smr           = deaths/pred,
         crd_rate      = (deaths/pats) * 100,
         location_type = "hospital") %>%
  # TO DO: NEED TO FILTER ON PUBLISHED HOSPITALS
  #filter(location %in% )


  ### 3 - Create HB-level aggregation ----

z_hsmr_hb <- z_smr01 %>%
  group_by(quarter, hbtreat_currentdate) %>%
  summarise(deaths = sum(death30),
            pred   = sum(pred_eq),
            pats   = length(death30)) %>%
  mutate(smr           = deaths/pred,
         crd_rate      = (deaths/pats) * 100,
         location_type = "NHS Board") %>%
  rename(location = hbtreat_currentdate)


### 4 - Merge dataframes and calculate regression line ----

# Merge data and match on location name
smr_data <- plyr::rbind.fill(z_hsmr_scot, z_hsmr_hosp, z_hsmr_hb) %>%
  join(z_hospitals, by = location)

# Create quarter variable used in linear model - every data point in the first year
# is considered to come from one time point (baseline period)
smr_data <- smr_data %>%
  mutate(quarter_reg = if_else(quarter <= 12, 0, quarter - 12))

# Run linear regression
z_reg_line <- lm(smr ~ quarter_reg * location_name, data = smr_data)

# Create reg variable of predicted values
smr_data$reg <- predict(reg_line, smr_data, type = "response")


### 5 - Save data ----

# Create data folder and save smr_data as an RDA file
devtools::use_data(smr_data)

# Tidy workspace
rm(smr_data)
rm(list = ls(pattern = "^z"))

### END OF SCRIPT ###
