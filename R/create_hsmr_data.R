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
library("odbc")          # Accessing SMRA
library("dplyr")         # For data manipulation in the "tidy" way
library("foreign")       # For reading in SPSS SAV Files
library("data.table")    # For efficient aggregation
library("haven")         # For reading in spss files


### 2 - Define the database connection with SMRA
suppressWarnings(SMRA_connect <- dbConnect(odbc(), dsn = "SMRA",
                                           uid = .rs.askForPassword("SMRA Username:"),
                                           pwd = .rs.askForPassword("SMRA Password:")))


### 3 - Extract dates ----
# Define the dates that the data are extracted from and to
z_start_date   <- c("'2017-01-01'")     # The beginning of baseline period
z_start_date_5 <- c("'2011-01-01'")     # Five years earlier for the five year look-back (pmorbs5)
z_start_date_l <- c("2017-01-01")       # Beginning of the baseline period (pmorbs)
z_end_date     <- c("'2017-03-31'")     # End date for the cut off for data


### 4 - Set filepaths ----
# Define lookups and output directory
z_lookups     <- "/conf/quality_indicators/hsmr/quarter_cycle/ref_files/"
z_base_file   <- "/conf/quality_indicators/hsmr/projects/R Adaptation/data/base_files/"


### 5 - Read in lookup files ----
# Primary Diagnosis Groupings
z_pdiag_grp_data <- as.data.frame(read.spss(paste(z_lookups, 'shmi_diag_grps_lookup.sav', sep = "")))
z_pdiag_grp_data <- z_pdiag_grp_data[ , c("diag1_4", "SHMI_DIAGNOSIS_GROUP")]

# ICD-10 codes, their Charlson Index Groupings and CIG weights
z_morbs          <- read.csv(paste(z_lookups, "morbs.csv", sep = ""))

# Postcode lookups for SIMD 2016 and 2012
z_simd_2016      <- read_spss("/conf/linkage/output/lookups/Unicode/Deprivation/postcode_2018_1.5_simd2016.sav")[ , c("pc7", "simd2016_sc_quintile")]
z_simd_2012      <- read_spss("/conf/linkage/output/lookups/Unicode/Deprivation/postcode_2016_1_simd2012.sav")[ , c("pc7", "simd2012_sc_quintile")]

# Read in hospital lookups
z_hospitals         <- read_csv(paste(z_lookups,"location_lookups.csv"))


### 6 - Source functions ----
source("R/data_prep_functions.R")


### SECTION 2 - DATA EXTRACTION----

### 1 - Data extraction ----
# Source SQL queries
source("R/sql_queries.R")


### 2 - Extract data ----
deaths <- as_tibble(dbGetQuery(SMRA_connect, z_query_gro))
data   <- as_tibble(dbGetQuery(SMRA_connect, z_query_smr01))


### SECTION 3 - DATA PREPARATION----

### 1 - Variable names to lower case ----
names(data)   <- tolower(names(data))
names(deaths) <- tolower(names(deaths))


### 2 - Deaths data ----
# Removing duplicate records on link_no as the deaths file is matched on to SMR01 by link_no
# link_no needs to be unique
deaths <- deaths %>%
  distinct(link_no, .keep_all = TRUE)

# Matching deaths data on to SMR01 data
data$date_of_death <- deaths$date_of_death[match(data$link_no,deaths$link_no)]

# Sorting data by link_no, cis_marker, adm_date and dis_date
data <- data %>%
  arrange(link_no, cis_marker, admission_date, discharge_date)

# Deleting unecessary dataframes
rm(deaths);gc()


### 3 - Basic SMR01 processing ----
# death_inhosp = 1 if the patient died in hospital during that episode of care
# dthdays      = the number of days from admission till death
# death30      = 1 if the patient died within 30 days of admission date
# quarter_name = quarter name in text form
# quarter      = quarter in number form (quarter 1 = Jan - Mar 2011)
# location     = recodes some hospital codes for combined sites
# diagx_4      = ICD-10 code to 4 digits
# diagx_3      = ICD-10 code to 3 digits
# pdiag_grp    = matches the primary diagnosis group on the 4-digit ICD-10 code
# wcomorbsx    = matches the charlson index weighting if the relevant ICD-10 codes
#                are present in any
#                of the five "other diagnosis" positions
# comorbs_sum  = sum of the wcomorbsx values across the episode
data <- data %>%
  mutate(death_inhosp = ifelse(discharge_type >= 40 & discharge_type <= 49, 1, 0),
         dthdays      = (date_of_death - admission_date)/60/60/24,
         death30      = ifelse(dthdays >= 0 & dthdays <= 30, 1, 0),
         quarter_name = paste(year, "Q", quarter, sep = ""),
         quarter      = as.numeric(as.factor(quarter_name)),
         location     = plyr::mapvalues(location,
                                        c("V102H","V201H","C206H","G207H","F805H","F705H","G306H","G516H"),
                                        c("V217H","V217H","C418H","G107H","F704H","F704H","G405H","G405H")),
         diag1_4      = substr(main_condition, 1, 4),
         diag2_4      = substr(other_condition_1, 1, 4),
         diag3_4      = substr(other_condition_2, 1, 4),
         diag4_4      = substr(other_condition_3, 1, 4),
         diag5_4      = substr(other_condition_4, 1, 4),
         diag6_4      = substr(other_condition_5, 1, 4),
         diag1_3      = substr(main_condition, 1, 3),
         diag2_3      = substr(other_condition_1, 1, 3),
         diag3_3      = substr(other_condition_2, 1, 3),
         diag4_3      = substr(other_condition_3, 1, 3),
         diag5_3      = substr(other_condition_4, 1, 3),
         diag6_3      = substr(other_condition_5, 1, 3),
         pdiag_grp    = z_pdiag_grp_data$SHMI_DIAGNOSIS_GROUP[match(diag1_4,z_pdiag_grp_data$diag1_4)],
         wcomorbs1    = ifelse(!is.na(z_morbs$morb[match(diag2_3, z_morbs$diag_3)]),z_morbs$morb[match(diag2_3, z_morbs$diag_3)],
                               ifelse(!is.na(z_morbs$morb[match(diag2_4, z_morbs$diag_4)]), z_morbs$morb[match(diag2_4, z_morbs$diag_4)], 0)),
         wcomorbs2    = ifelse(!is.na(z_morbs$morb[match(diag3_3, z_morbs$diag_3)]),z_morbs$morb[match(diag3_3, z_morbs$diag_3)],
                               ifelse(!is.na(z_morbs$morb[match(diag3_4, z_morbs$diag_4)]), z_morbs$morb[match(diag3_4, z_morbs$diag_4)], 0)),
         wcomorbs3    = ifelse(!is.na(z_morbs$morb[match(diag4_3, z_morbs$diag_3)]),z_morbs$morb[match(diag4_3, z_morbs$diag_3)],
                               ifelse(!is.na(z_morbs$morb[match(diag4_4, z_morbs$diag_4)]), z_morbs$morb[match(diag4_4, z_morbs$diag_4)], 0)),
         wcomorbs4    = ifelse(!is.na(z_morbs$morb[match(diag5_3, z_morbs$diag_3)]),z_morbs$morb[match(diag5_3, z_morbs$diag_3)],
                               ifelse(!is.na(z_morbs$morb[match(diag5_4, z_morbs$diag_4)]), z_morbs$morb[match(diag5_4, z_morbs$diag_4)], 0)),
         wcomorbs5    = ifelse(!is.na(z_morbs$morb[match(diag6_3, z_morbs$diag_3)]),z_morbs$morb[match(diag6_3, z_morbs$diag_3)],
                               ifelse(!is.na(z_morbs$morb[match(diag6_4, z_morbs$diag_4)]), z_morbs$morb[match(diag6_4, z_morbs$diag_4)], 0)),
         wcomorbs1     = z_morbs$wmorbs[match(wcomorbs1, z_morbs$morb)],
         wcomorbs2    = ifelse(!(wcomorbs2 %in% c(wcomorbs1)), z_morbs$wmorbs[match(wcomorbs2, z_morbs$morb)], 0),
         wcomorbs3    = ifelse(!(wcomorbs3 %in% c(wcomorbs1, wcomorbs2)), z_morbs$wmorbs[match(wcomorbs3, z_morbs$morb)], 0),
         wcomorbs4    = ifelse(!(wcomorbs4 %in% c(wcomorbs1, wcomorbs2, wcomorbs3)), z_morbs$wmorbs[match(wcomorbs4, z_morbs$morb)], 0),
         wcomorbs5    = ifelse(!(wcomorbs5 %in% c(wcomorbs1, wcomorbs2, wcomorbs3, wcomorbs4)), z_morbs$wmorbs[match(wcomorbs5, z_morbs$morb)], 0),
         comorbs_sum  = wcomorbs1 + wcomorbs2 + wcomorbs3 + wcomorbs4 + wcomorbs5) %>%

  # epinum           = the episode number for each individual episode within the CIS
  # death_inhosp_max = 1 if the patient died in hospital during any episode of the CIS

  group_by(link_no, cis_marker) %>%
  mutate(epinum           = row_number(),
         death_inhosp_max = max(death_inhosp)) %>%
  arrange(link_no, cis_marker, admission_date, discharge_date) %>%
  select(-one_of(c("main_condition", "other_condition_1", "other_condition_2", "other_condition_3", "other_condition_4",
                   "other_condition_5", "wcomorbs1", "wcomorbs2", "wcomorbs3", "wcomorbs4", "wcomorbs5", "quarter_name")))


### 4 - Prior morbidities within previous 1 & 5 years ----

data_pmorbs <- as_tibble(dbGetQuery(SMRA_connect, z_query_smr01_minus5))
names(data_pmorbs) <- tolower(names(data_pmorbs))

data_pmorbs <- data_pmorbs %>%
  ungroup() %>%
  mutate(diag1_4  = substr(main_condition, 1, 4),
         diag1_3  = substr(main_condition, 1, 3),
         pmorbs   = ifelse(!is.na(z_morbs$morb[match(diag1_3, z_morbs$diag_3)]), z_morbs$morb[match(diag1_3, z_morbs$diag_3)],
                           ifelse(!is.na(z_morbs$morb[match(diag1_4, z_morbs$diag_4)]), z_morbs$morb[match(diag1_4, z_morbs$diag_4)], 0)),
         pmorbs5_1  = 0,
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
         pmorbs1_17 = 0)


for(i in 1:74){
  print(i)

  data_pmorbs <- data_pmorbs %>%
    mutate(pmorbs5_1  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                 (admission_date - lag(admission_date, i)) <= 1825, 5, pmorbs5_1),

           pmorbs5_2  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                 (admission_date - lag(admission_date, i)) <= 1825, 11, pmorbs5_2),

           pmorbs5_3  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                 (admission_date - lag(admission_date, i)) <= 1825, 5,pmorbs5_3),

           pmorbs5_4  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                 (admission_date - lag(admission_date, i)) <= 1825, 5,pmorbs5_4),

           pmorbs5_5  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                 (admission_date - lag(admission_date, i)) <= 1825, 5,pmorbs5_5),

           pmorbs5_6  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                 (admission_date - lag(admission_date, i)) <= 1825, 5,pmorbs5_6),

           pmorbs5_7  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                 (admission_date - lag(admission_date, i)) <= 1825, 5,pmorbs5_7),

           pmorbs5_8  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                 (admission_date - lag(admission_date, i)) <= 1825, 5,pmorbs5_8),

           pmorbs5_9  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                 (admission_date - lag(admission_date, i)) <= 1825, 5,pmorbs5_9),

           pmorbs5_10  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                  (admission_date - lag(admission_date, i)) <= 1825, 5,pmorbs5_10),

           pmorbs5_11  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                  (admission_date - lag(admission_date, i)) <= 1825, 5,pmorbs5_11),

           pmorbs5_12  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                  (admission_date - lag(admission_date, i)) <= 1825, 5,pmorbs5_12),

           pmorbs5_13  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                  (admission_date - lag(admission_date, i)) <= 1825, 5,pmorbs5_13),

           pmorbs5_14  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                  (admission_date - lag(admission_date, i)) <= 1825, 5,pmorbs5_14),

           pmorbs5_15  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                  (admission_date - lag(admission_date, i)) <= 1825, 5,pmorbs5_15),

           pmorbs5_16  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                  (admission_date - lag(admission_date, i)) <= 1825, 5,pmorbs5_16),

           pmorbs5_17  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                  (admission_date - lag(admission_date, i)) <= 1825, 5,pmorbs5_17)m,

           pmorbs1_1  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                 (admission_date - lag(admission_date, i)) <= 365, 5, pmorbs1_1),

           pmorbs1_2  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                 (admission_date - lag(admission_date, i)) <= 365, 11, pmorbs1_2),

           pmorbs1_3  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                 (admission_date - lag(admission_date, i)) <= 365, 5,pmorbs1_3),

           pmorbs1_4  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                 (admission_date - lag(admission_date, i)) <= 365, 5,pmorbs1_4),

           pmorbs1_5  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                 (admission_date - lag(admission_date, i)) <= 365, 5,pmorbs1_5),

           pmorbs1_6  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                 (admission_date - lag(admission_date, i)) <= 365, 5,pmorbs1_6),

           pmorbs1_7  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                 (admission_date - lag(admission_date, i)) <= 365, 5,pmorbs1_7),

           pmorbs1_8  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                 (admission_date - lag(admission_date, i)) <= 365, 5,pmorbs1_8),

           pmorbs1_9  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                 (admission_date - lag(admission_date, i)) <= 365, 5,pmorbs1_9),

           pmorbs1_10  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                  (admission_date - lag(admission_date, i)) <= 365, 5,pmorbs1_10),

           pmorbs1_11  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                  (admission_date - lag(admission_date, i)) <= 365, 5,pmorbs1_11),

           pmorbs1_12  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                  (admission_date - lag(admission_date, i)) <= 365, 5,pmorbs1_12),

           pmorbs1_13  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                  (admission_date - lag(admission_date, i)) <= 365, 5,pmorbs1_13),

           pmorbs1_14  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                  (admission_date - lag(admission_date, i)) <= 365, 5,pmorbs1_14),

           pmorbs1_15  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                  (admission_date - lag(admission_date, i)) <= 365, 5,pmorbs1_15),

           pmorbs1_16  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                  (admission_date - lag(admission_date, i)) <= 365, 5,pmorbs1_16),

           pmorbs1_17  = ifelse(admission_date >= z_start_date_l & 1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                  (admission_date - lag(admission_date, i)) <= 365, 5,pmorbs1_17))

}

data_pmorbs <- data_pmorbs %>%
  group_by(link_no, cis_marker) %>%
  mutate(pmorbs1_sum = max(pmorbs1_1) + max(pmorbs1_2) + max(pmorbs1_3) + max(pmorbs1_4) + max(pmorbs1_5) + max(pmorbs1_6) + max(pmorbs1_7) + max(pmorbs1_8) + max(pmorbs1_9) +
           max(pmorbs1_10) + max(pmorbs1_11) + max(pmorbs1_12) + max(pmorbs1_13) + max(pmorbs1_14) + max(pmorbs1_15) + max(pmorbs1_16) + max(pmorbs1_17),
         pmorbs5_sum = max(pmorbs5_1) + max(pmorbs5_2) + max(pmorbs5_3) + max(pmorbs5_4) + max(pmorbs5_5) + max(pmorbs5_6) + max(pmorbs5_7) + max(pmorbs5_8) + max(pmorbs5_9) +
           max(pmorbs5_10) + max(pmorbs5_11) + max(pmorbs5_12) + max(pmorbs5_13) + max(pmorbs5_14) + max(pmorbs5_15) + max(pmorbs5_16) + max(pmorbs5_17),
         epinum      = row_number()) %>%
  filter(epinum == 1) %>%
  ungroup()

for (i in 1:54) {

  data_pmorbs <- data_pmorbs %>%
    mutate(n_emerg = ifelse(!is.na(lag(link_no, i)), ifelse(lag(old_smr1_tadm_code, i) >= 4 & link_no == lag(link_no, i) &
                                                              (admission_date - lag(admission_date, i)) <= 365, n_emerg + 1, n_emerg), n_emerg))
}


data_pmorbs <- data_pmorbs %>%
  select(c("link_no", "cis_marker", "pmorbs1_sum", "pmorbs5_sum"))

data <- data %>%
  left_join(data_pmorbs, by = c("link_no", "cis_marker"))

rm(data_pmorbs)


### 5 - SIMD ----

# Fix formatting of postcode variable (remove trailing spaces and any other
# unnecessary white space)
data$postcode <- sub("  ", " ", data$postcode)
data$postcode <- sub("   ", "  ", data$postcode)
data$postcode[which(regexpr(" ", data$postcode) == 5)] <- sub(" ", "", data$postcode[which(regexpr(" ", data$postcode) == 5)])

# Match SIMD 2016 onto years beyond 2014
names(z_simd_2016)                  <- c("postcode", "simd")
data$simd[which(data$year >= 2014)] <- z_simd_2016$simd[match(data$postcode, z_simd_2016$postcode)]

# Match SIMD 2012 onto years before 2014
names(z_simd_2012)                  <- c("postcode", "simd")
data$simd[which(data$year < 2014)]  <- z_simd_2012$simd[match(data$postcode, z_simd_2012$postcode)]


### 6 - Create patient level file

data <- data %>%
  group_by(link_no, quarter) %>%
  mutate(last_cis = max(cis)) %>%
  filter(epinum == 1 & cis_marker == last_cis)


### SECTION 4 - MODELLING ----

# Create subset of data for modelling
z_data_lr <- data %>%
  filter(quarter <= 12 | is.na(simd) | is.na(admfgrp)) %>%
  select(n_emerg, comorbs_sum, pmorbs1_sum, pmorbs5_sum, age, sex, surgmed,
         pdiag_grp, admfgrp, admgrp, ipdc, simd, death30) %>%
  group_by(n_emerg, comorbs_sum, pmorbs1_sum, pmorbs5_sum, age, sex, surgmed,
           pdiag_grp, admfgrp, admgrp, ipdc, simd) %>%
  summarise(x = sum(death30), n = length(death30))

# Run logistic regression
risk_model <- glm(cbind(x, n-x) ~ n_emerg + comorbs_sum + pmorbs1_sum + pmorbs5_sum +
                    age_in_years + factor(sex) + factor(surgmed) + factor(pdiag_grp) +
                    factor(admfgrp) + factor(admgrp) + factor(ipdc) + factor(simd),
                  data = z_data_lr, family = "binomial", model = FALSE, y = FALSE)

# Delete unnecessary model information using bespoke function
risk_model <- clean_model(risk_model)

# Create predicted probabilities
data$pred_eq <- predict.glm(risk_model, data, type = "response")

# Remove rows with no probability calculated
data <- data %>%
  filter(!is.na(pred_eq))


### SECTION 5 - CREATE MINIMAL TIDY DATASET ----

### 1 - Create Scotland-level aggregation ----
z_hsmr_scot <- data %>%
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
z_hsmr_hosp <- data %>%
  group_by(quarter, location) %>%
  summarise(deaths = sum(death30),
            pred   = sum(pred_eq),
            pats   = length(death30)) %>%
  mutate(smr      = deaths/pred,
         crd_rate = (deaths/pats) * 100,
         location_type = "hospital")


### 3 - Create HB-level aggregation ----
z_hsmr_hb <- data %>%
  group_by(quarter, hbtreat_new) %>%
  summarise(deaths = sum(death30),
            pred   = sum(pred_eq),
            pats   = length(death30)) %>%
  mutate(smr      = deaths/pred,
         crd_rate = (deaths/pats) * 100,
         location_type = "NHS Board") %>%
  rename(location = hbtreat_new)


### 4 - Merge dataframes and calculate regression line ----
# Merging data and matching on location name
hsmr <- rbind(z_hsmr_scot, z_hsmr_hosp, z_hsmr_hb) %>%
  join(z_hospitals, by = location)

hsmr <- hsmr %>%
  mutate(quarter_reg = ifelse(quarter <= 12, 0, quarter - 12))

reg_line <- lm(smr ~ quarter * location_name, data = hsmr)

hsmr$reg <- predict(reg_line, hsmr, type = "response")
