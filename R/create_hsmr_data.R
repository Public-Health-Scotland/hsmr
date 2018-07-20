#########################################################################
# Name of file - create_hsmr_data.R
# Data release - Quarterly HSMR publication
# Original Authors - David Caldwell
# Orginal Date - February 2018
#
# Type - Data extraction/preparation/modelling
# Written/run on - RStudio server
# Version of R - ?
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


# Define the database connection with SMRA
suppressWarnings(SMRA_connect <- dbConnect(odbc(), dsn="SMRA",
                                           uid=.rs.askForPassword("SMRA Username:"),
                                           pwd=.rs.askForPassword("SMRA Password:")))

### 2 - Extract dates ----
# Define the dates that the data are extracted from
start_date   <- c("'2017-01-01'")     # The beginning of baseline period
start_date_1 <- c("'2016-01-01'")     # One year earlier for the one year look-back (pmorbs1)
start_date_5 <- c("'2011-01-01'")     # Five years earlier for the five year look-back (pmorbs5)
start_date_l <- c("2017-01-01")       # Beginning of the baseline period (pmorbs)
end_date     <- c("'2017-03-31'")     # End date for the cut off for data

### 3 - Set filepaths ----
# Define lookups and output directory
lookups   <- "/conf/quality_indicators/hsmr/quarter_cycle/ref_files/"
base_file <- "/conf/quality_indicators/hsmr/projects/R Adaptation/data/base_files/"


### 4 - Read in lookup files
# Primary Diagnosis Groupings
pdiag_grp_data <- as.data.frame(read.spss(paste(lookups,'shmi_diag_grps_lookup.sav',sep="")))
pdiag_grp_data <- pdiag_grp_data[,c("diag1_4","SHMI_DIAGNOSIS_GROUP")]

# ICD-10 codes, their Charlson Index Groupings and CIG weights
morbs <- read.csv(paste(lookups,"morbs.csv", sep = ""))

# Postcode lookups for SIMD
simd <- data.frame(read.spss("/conf/linkage/output/lookups/deprivation/postcode_2017_2_simd2016.sav"))[,c("pc7","simd2016_sc_quintile")]
names(simd) <- c("POSTCODE", "simd")

### SECTION 2 - DATA EXTRACTION----

### 1 - Data extraction ----
# Define SQL queries
Query_SMR01 <- paste("select LINK_NO, ADMISSION_DATE, DISCHARGE_DATE, CIS_MARKER, SENDING_LOCATION, SEX,",
                     "CI_CHI_NUMBER, PATIENT_IDENTIFIER, LOCATION, POSTCODE,",
                     "SPECIALTY, SIGNIFICANT_FACILITY, CONSULTANT_HCP_RESPONSIBLE,",
                     "ADMISSION_TYPE, ADMISSION_TRANSFER_FROM, DISCHARGE_TYPE, DISCHARGE_TRANSFER_TO, MAIN_CONDITION,",
                     "OTHER_CONDITION_1, OTHER_CONDITION_2, OTHER_CONDITION_3, OTHER_CONDITION_4, OTHER_CONDITION_5,",
                     "MAIN_OPERATION, DATE_OF_MAIN_OPERATION, CLINICIAN_MAIN_OPERATION, OTHER_OPERATION_1,",
                     "DATE_OF_OTHER_OPERATION_1, CLINICIAN_OTHER_OPERATION_1, OTHER_OPERATION_2, DATE_OF_OTHER_OPERATION_2,",
                     "CLINICIAN_OTHER_OPERATION_2, OTHER_OPERATION_3, DATE_OF_OTHER_OPERATION_3, CLINICIAN_OTHER_OPERATION_3,",
                     "HBRES_CURRENTDATE, INPATIENT_DAYCASE_IDENTIFIER, OLD_SMR1_TADM_CODE, DATAZONE_2011,",
                     "CASE WHEN ADMISSION_TYPE BETWEEN 20 AND 48 OR ADMISSION_TYPE = 18 THEN '2'",
                     "WHEN ADMISSION_TYPE BETWEEN 10 AND 12 OR ADMISSION_TYPE = 19 THEN '1'",
                     "ELSE 'NULL' END admgrp,",
                     "CASE WHEN ADMISSION_TRANSFER_FROM IN ('20','21','22','23','24','25','26','27','28','29') THEN '1'",
                     "WHEN ADMISSION_TRANSFER_FROM IN ('60','61','62','63','64','65','66','67','68','69','70') THEN '2'",
                     "WHEN ADMISSION_TRANSFER_FROM IN ('10','11','12','13','14','15','16','17','18','19') THEN '3'",
                     "WHEN ADMISSION_TRANSFER_FROM IN ('30','31','32','33','34','35','36','37','38','39') THEN '4'",
                     "WHEN ADMISSION_TRANSFER_FROM IN ('50','51','52','53','54','55','56','57','58','59','5A','5B','5C', '5D', '5E', '5F', '5G') THEN '5'",
                     "WHEN ADMISSION_TRANSFER_FROM IN ('40','41','42','43','44','45','46','47','48','49','4A','4B','4C', '4D', '4E', '4F', '4G') THEN '6'",
                     "ELSE 'NULL' END admfgrp,",
                     "CASE WHEN SPECIALTY IN ('C1','C11','C12','C13','C4','C41','C42','C5','C6','C7','C8','C9','CA','CB','D3','D4','D5','D6','D8','F2') THEN '2'",
                     "ELSE '1' END surgmed,",
                     "CASE WHEN INPATIENT_DAYCASE_IDENTIFIER = 'I' THEN '1'",
                     "WHEN INPATIENT_DAYCASE_IDENTIFIER = 'D' THEN '2'",
                     "ELSE 'NULL' END ipdc,",
                     "CASE WHEN age_in_years BETWEEN 0 AND 9 THEN '1'",
                     "WHEN age_in_years BETWEEN 10 AND 19 THEN '2'",
                     "WHEN age_in_years BETWEEN 20 AND 29 THEN '3'",
                     "WHEN age_in_years BETWEEN 30 AND 39 THEN '4'",
                     "WHEN age_in_years BETWEEN 40 AND 49 THEN '5'",
                     "WHEN age_in_years BETWEEN 50 AND 59 THEN '6'",
                     "WHEN age_in_years BETWEEN 60 AND 69 THEN '7'",
                     "WHEN age_in_years BETWEEN 70 AND 79 THEN '8'",
                     "WHEN age_in_years BETWEEN 80 AND 89 THEN '9'",
                     "WHEN age_in_years >= 90 THEN '10'",
                     "ELSE 'NULL' END age_grp,to_char(admission_date,'Q') AS quarter, extract(year from admission_date) AS year,",
                     "HBTREAT_CURRENTDATE, AGE_IN_YEARS from SMR01_PI",
                     "where ADMISSION_DATE >= to_date(",start_date,",'yyyy-MM-dd') AND ADMISSION_DATE <= to_date(",end_date,",'yyyy-MM-dd')",
                     "ORDER BY LINK_NO, ADMISSION_DATE, RECORD_TYPE, DISCHARGE_DATE, ADMISSION, DISCHARGE, URI")

Query_SMR01_minus1 <- paste("select LINK_NO, ADMISSION_DATE, DISCHARGE_DATE, CIS_MARKER,",
                            "SPECIALTY, MAIN_CONDITION, OLD_SMR1_TADM_CODE from SMR01_PI",
                            "where ADMISSION_DATE >= to_date(",start_date_1,",'yyyy-MM-dd') AND ADMISSION_DATE <= to_date(",end_date,",'yyyy-MM-dd')",
                            "ORDER BY LINK_NO, ADMISSION_DATE, RECORD_TYPE, DISCHARGE_DATE, ADMISSION, DISCHARGE, URI")

Query_SMR01_minus5 <- paste("select LINK_NO, ADMISSION_DATE, DISCHARGE_DATE, CIS_MARKER,",
                            "SPECIALTY, MAIN_CONDITION from SMR01_PI",
                            "where ADMISSION_DATE >= to_date(",start_date_5,",'yyyy-MM-dd') AND ADMISSION_DATE <= to_date(",end_date,",'yyyy-MM-dd')",
                            "ORDER BY LINK_NO, ADMISSION_DATE, RECORD_TYPE, DISCHARGE_DATE, ADMISSION, DISCHARGE, URI")

Query_GRO <- paste("select LINK_NO, DATE_OF_DEATH",
                   "from ANALYSIS.GRO_DEATHS_C",
                   "where DATE_OF_DEATH >= to_date(",start_date,",'yyyy-MM-dd') AND DATE_OF_DEATH <= to_date('2017-09-30','yyyy-MM-dd')",
                   "ORDER BY LINK_NO")

### 2 - Extract Data ----
deaths <- as_tibble(dbGetQuery(SMRA_connect, Query_GRO))
data <- as_tibble(dbGetQuery(SMRA_connect, Query_SMR01))

### SECTION 3 - DATA PREPARATION----

### 1 - Variable names - to lower case ----
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
# wcomorbsx    = matches the charlson index weighting if the relevant ICD-10 codes are present in any
# of the five "other diagnosis" positions
# comorbs_sum  = sum of the wcomorbsx values across the episode
data <- data %>%
  mutate(death_inhosp = ifelse(discharge_type >= 40 & discharge_type <= 49, 1, 0),
         dthdays      = (date_of_death - admission_date)/60/60/24,
         death30      = ifelse(dthdays >= 0 & dthdays <= 30, 1, 0),
         quarter_name = paste(year, "Q", quarter, sep = ""),
         quarter      = as.numeric(as.factor(quarter_name)),
         location     = plyr::mapvalues(location, c("V102H","V201H","C206H","G207H","F805H","F705H","G306H","G516H"),
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
         pdiag_grp    = pdiag_grp_data$SHMI_DIAGNOSIS_GROUP[match(diag1_4,pdiag_grp_data$diag1_4)],
         wcomorbs1    = ifelse(!is.na(morbs$morb[match(diag2_3, morbs$diag_3)]),morbs$morb[match(diag2_3, morbs$diag_3)],
                               ifelse(!is.na(morbs$morb[match(diag2_4, morbs$diag_4)]), morbs$morb[match(diag2_4, morbs$diag_4)], 0)),
         wcomorbs2    = ifelse(!is.na(morbs$morb[match(diag3_3, morbs$diag_3)]),morbs$morb[match(diag3_3, morbs$diag_3)],
                               ifelse(!is.na(morbs$morb[match(diag3_4, morbs$diag_4)]), morbs$morb[match(diag3_4, morbs$diag_4)], 0)),
         wcomorbs3    = ifelse(!is.na(morbs$morb[match(diag4_3, morbs$diag_3)]),morbs$morb[match(diag4_3, morbs$diag_3)],
                               ifelse(!is.na(morbs$morb[match(diag4_4, morbs$diag_4)]), morbs$morb[match(diag4_4, morbs$diag_4)], 0)),
         wcomorbs4    = ifelse(!is.na(morbs$morb[match(diag5_3, morbs$diag_3)]),morbs$morb[match(diag5_3, morbs$diag_3)],
                               ifelse(!is.na(morbs$morb[match(diag5_4, morbs$diag_4)]), morbs$morb[match(diag5_4, morbs$diag_4)], 0)),
         wcomorbs5    = ifelse(!is.na(morbs$morb[match(diag6_3, morbs$diag_3)]),morbs$morb[match(diag6_3, morbs$diag_3)],
                               ifelse(!is.na(morbs$morb[match(diag6_4, morbs$diag_4)]), morbs$morb[match(diag6_4, morbs$diag_4)], 0)),
         wcomorbs1     = morbs$wmorbs[match(wcomorbs1, morbs$morb)],
         wcomorbs2    = ifelse(!(wcomorbs2 %in% c(wcomorbs1)), morbs$wmorbs[match(wcomorbs2, morbs$morb)], 0),
         wcomorbs3    = ifelse(!(wcomorbs3 %in% c(wcomorbs1, wcomorbs2)), morbs$wmorbs[match(wcomorbs3, morbs$morb)], 0),
         wcomorbs4    = ifelse(!(wcomorbs4 %in% c(wcomorbs1, wcomorbs2, wcomorbs3)), morbs$wmorbs[match(wcomorbs4, morbs$morb)], 0),
         wcomorbs5    = ifelse(!(wcomorbs5 %in% c(wcomorbs1, wcomorbs2, wcomorbs3, wcomorbs4)), morbs$wmorbs[match(wcomorbs5, morbs$morb)], 0),
         comorbs_sum  = wcomorbs1 + wcomorbs2 + wcomorbs3 + wcomorbs4 + wcomorbs5) %>%

  # epinum           = the episode number for each individual episode within the CIS
  # death_inhosp_max = 1 if the patient died in hospital during any episode of the CIS

  group_by(link_no, cis_marker) %>%
  mutate(epinum           = row_number(),
         death_inhosp_max = max(death_inhosp)) %>%
  arrange(link_no, cis_marker, admission_date, discharge_date) %>%
  select(-one_of(c("main_condition", "other_condition_1", "other_condition_2", "other_condition_3", "other_condition_4",
                   "other_condition_5", "wcomorbs1", "wcomorbs2", "wcomorbs3", "wcomorbs4", "wcomorbs5", "quarter_name")))

# Deleting unecessary dataframes
rm(pdiag_grp_data);gc()


save(data,file = paste(base_file,"QHSMR_SMR01_raw_basefile",".rda",sep=""))


### 4 - Prior morbidities within previous 1 year ----

data_pmorbs1 <- as_tibble(dbGetQuery(SMRA_connect, Query_SMR01_minus1))
names(data_pmorbs1) <- tolower(names(data_pmorbs1))

data_pmorbs1 <- data_pmorbs1 %>%
  ungroup() %>%
  mutate(diag1_4  = substr(main_condition, 1, 4),
         diag1_3  = substr(main_condition, 1, 3),
         pmorbs   = ifelse(!is.na(morbs$morb[match(diag1_3, morbs$diag_3)]), morbs$morb[match(diag1_3, morbs$diag_3)],
                           ifelse(!is.na(morbs$morb[match(diag1_4, morbs$diag_4)]), morbs$morb[match(diag1_4, morbs$diag_4)], 0)),
         pmorbs1  = 0,
         pmorbs2  = 0,
         pmorbs3  = 0,
         pmorbs4  = 0,
         pmorbs5  = 0,
         pmorbs6  = 0,
         pmorbs7  = 0,
         pmorbs8  = 0,
         pmorbs9  = 0,
         pmorbs10 = 0,
         pmorbs11 = 0,
         pmorbs12 = 0,
         pmorbs13 = 0,
         pmorbs14 = 0,
         pmorbs15 = 0,
         pmorbs16 = 0,
         pmorbs17 = 0)


for(i in 1:54){

  data_pmorbs1 <- data_pmorbs1 %>%
    mutate(pmorbs1  = ifelse(!is.na(lag(link_no, i)), ifelse(1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                               (admission_date - lag(admission_date, i)) <= 365, 5, pmorbs1), pmorbs1),

           pmorbs2  = ifelse(!is.na(lag(link_no, i)), ifelse(2 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                               (admission_date - lag(admission_date, i)) <= 365, 11, pmorbs2), pmorbs2),

           pmorbs3  = ifelse(!is.na(lag(link_no, i)), ifelse(3 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                               (admission_date - lag(admission_date, i)) <= 365, 13, pmorbs3), pmorbs3),

           pmorbs4  = ifelse(!is.na(lag(link_no, i)), ifelse(4 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                               (admission_date - lag(admission_date, i)) <= 365, 4, pmorbs4), pmorbs4),

           pmorbs5  = ifelse(!is.na(lag(link_no, i)), ifelse(5 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                               (admission_date - lag(admission_date, i)) <= 365, 14, pmorbs5), pmorbs5),

           pmorbs6  = ifelse(!is.na(lag(link_no, i)), ifelse(6 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                               (admission_date - lag(admission_date, i)) <= 365, 3, pmorbs6), pmorbs6),

           pmorbs7  = ifelse(!is.na(lag(link_no, i)), ifelse(7 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                               (admission_date - lag(admission_date, i)) <= 365, 8, pmorbs7), pmorbs7),

           pmorbs8  = ifelse(!is.na(lag(link_no, i)), ifelse(8 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                               (admission_date - lag(admission_date, i)) <= 365, 9, pmorbs8), pmorbs8),

           pmorbs9  = ifelse(!is.na(lag(link_no, i)), ifelse(9 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                               (admission_date - lag(admission_date, i)) <= 365, 6, pmorbs9), pmorbs9),

           pmorbs10 = ifelse(!is.na(lag(link_no, i)), ifelse(10 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                               (admission_date - lag(admission_date, i)) <= 365, 4, pmorbs10), pmorbs10),

           pmorbs11 = ifelse(!is.na(lag(link_no, i)), ifelse(11 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                               (admission_date - lag(admission_date, i)) <= 365, 8, pmorbs11), pmorbs11),

           pmorbs12 = ifelse(!is.na(lag(link_no, i)), ifelse(12 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                               (admission_date - lag(admission_date, i)) <= 365, -1, pmorbs12), pmorbs12),

           pmorbs13 = ifelse(!is.na(lag(link_no, i)), ifelse(13 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                               (admission_date - lag(admission_date, i)) <= 365, 1, pmorbs13), pmorbs13),

           pmorbs14 = ifelse(!is.na(lag(link_no, i)), ifelse(14 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                               (admission_date - lag(admission_date, i)) <= 365, 10, pmorbs14), pmorbs14),

           pmorbs15 = ifelse(!is.na(lag(link_no, i)), ifelse(15 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                               (admission_date - lag(admission_date, i)) <= 365, 14, pmorbs15), pmorbs15),

           pmorbs16 = ifelse(!is.na(lag(link_no, i)), ifelse(16 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                               (admission_date - lag(admission_date, i)) <= 365, 18, pmorbs16), pmorbs16),

           pmorbs17 = ifelse(!is.na(lag(link_no, i)), ifelse(17 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                               (admission_date - lag(admission_date, i)) <= 365, 2, pmorbs17), pmorbs17))

}


data_pmorbs1 <- data_pmorbs1 %>%
  group_by(link_no, cis_marker) %>%
  mutate(pmorbs1_sum = max(pmorbs1) + max(pmorbs2) + max(pmorbs3) + max(pmorbs4) + max(pmorbs5) + max(pmorbs6) + max(pmorbs7) + max(pmorbs8) + max(pmorbs9) +
           max(pmorbs10) + max(pmorbs11) + max(pmorbs12) + max(pmorbs13) + max(pmorbs14) + max(pmorbs15) + max(pmorbs16) + max(pmorbs17),
         epinum      = row_number(),
         n_emerg     = 0) %>%
  filter(epinum == 1) %>%
  ungroup()

for (i in 1:54) {

  data_pmorbs1 <- data_pmorbs1 %>%
    mutate(n_emerg = ifelse(!is.na(lag(link_no, i)), ifelse(lag(old_smr1_tadm_code, i) >= 4 & link_no == lag(link_no, i) &
                                                              (admission_date - lag(admission_date, i)) <= 365, n_emerg + 1, n_emerg), n_emerg))
}

data_pmorbs1 <- data_pmorbs1 %>%
  select(c("link_no", "cis_marker", "pmorbs1_sum", "n_emerg"))

data <- data %>%
  left_join(data_pmorbs1, by = c("link_no", "cis_marker"))

rm(data_pmorbs1)


save(data,file = paste(base_file,"QHSMR_SMR01_raw_basefile",".rda",sep=""))

###############
### PMORBS5 ###
###############


data_pmorbs5 <- as_tibble(dbGetQuery(SMRA_connect, Query_SMR01_minus5))
names(data_pmorbs5) <- tolower(names(data_pmorbs5))

data_pmorbs5 <- data_pmorbs5 %>%
  ungroup() %>%
  mutate(diag1_4  = substr(main_condition, 1, 4),
         diag1_3  = substr(main_condition, 1, 3),
         pmorbs   = ifelse(!is.na(morbs$morb[match(diag1_3, morbs$diag_3)]), morbs$morb[match(diag1_3, morbs$diag_3)],
                           ifelse(!is.na(morbs$morb[match(diag1_4, morbs$diag_4)]), morbs$morb[match(diag1_4, morbs$diag_4)], 0)),
         pmorbs1  = 0,
         pmorbs2  = 0,
         pmorbs3  = 0,
         pmorbs4  = 0,
         pmorbs5  = 0,
         pmorbs6  = 0,
         pmorbs7  = 0,
         pmorbs8  = 0,
         pmorbs9  = 0,
         pmorbs10 = 0,
         pmorbs11 = 0,
         pmorbs12 = 0,
         pmorbs13 = 0,
         pmorbs14 = 0,
         pmorbs15 = 0,
         pmorbs16 = 0,
         pmorbs17 = 0)


for(i in 1:74){

  data_pmorbs5 <- data_pmorbs5 %>%
    mutate(pmorbs1  = ifelse(!is.na(lag(link_no, i)) | admission_date >= start_date_l, ifelse(1 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                                                                (admission_date - lag(admission_date, i)) <= 1825, 5, pmorbs1), pmorbs1),

           pmorbs2  = ifelse(!is.na(lag(link_no, i)) | admission_date >= start_date_l, ifelse(2 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                                                                (admission_date - lag(admission_date, i)) <= 1825, 11, pmorbs2), pmorbs2),

           pmorbs3  = ifelse(!is.na(lag(link_no, i)) | admission_date >= start_date_l, ifelse(3 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                                                                (admission_date - lag(admission_date, i)) <= 1825, 13, pmorbs3), pmorbs3),

           pmorbs4  = ifelse(!is.na(lag(link_no, i)) | admission_date >= start_date_l, ifelse(4 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                                                                (admission_date - lag(admission_date, i)) <= 1825, 4, pmorbs4), pmorbs4),

           pmorbs5  = ifelse(!is.na(lag(link_no, i)) | admission_date >= start_date_l, ifelse(5 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                                                                (admission_date - lag(admission_date, i)) <= 1825, 14, pmorbs5), pmorbs5),

           pmorbs6  = ifelse(!is.na(lag(link_no, i)) | admission_date >= start_date_l, ifelse(6 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                                                                (admission_date - lag(admission_date, i)) <= 1825, 3, pmorbs6), pmorbs6),

           pmorbs7  = ifelse(!is.na(lag(link_no, i)) | admission_date >= start_date_l, ifelse(7 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                                                                (admission_date - lag(admission_date, i)) <= 1825, 8, pmorbs7), pmorbs7),

           pmorbs8  = ifelse(!is.na(lag(link_no, i)) | admission_date >= start_date_l, ifelse(8 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                                                                (admission_date - lag(admission_date, i)) <= 1825, 9, pmorbs8), pmorbs8),

           pmorbs9  = ifelse(!is.na(lag(link_no, i)) | admission_date >= start_date_l, ifelse(9 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                                                                (admission_date - lag(admission_date, i)) <= 1825, 6, pmorbs9), pmorbs9),

           pmorbs10 = ifelse(!is.na(lag(link_no, i)) | admission_date >= start_date_l, ifelse(10 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                                                                (admission_date - lag(admission_date, i)) <= 1825, 4, pmorbs10), pmorbs10),

           pmorbs11 = ifelse(!is.na(lag(link_no, i)) | admission_date >= start_date_l, ifelse(11 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                                                                (admission_date - lag(admission_date, i)) <= 1825, 8, pmorbs11), pmorbs11),

           pmorbs12 = ifelse(!is.na(lag(link_no, i)) | admission_date >= start_date_l, ifelse(12 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                                                                (admission_date - lag(admission_date, i)) <= 1825, -1, pmorbs12), pmorbs12),

           pmorbs13 = ifelse(!is.na(lag(link_no, i)) | admission_date >= start_date_l, ifelse(13 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                                                                (admission_date - lag(admission_date, i)) <= 1825, 1, pmorbs13), pmorbs13),

           pmorbs14 = ifelse(!is.na(lag(link_no, i)) | admission_date >= start_date_l, ifelse(14 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                                                                (admission_date - lag(admission_date, i)) <= 1825, 10, pmorbs14), pmorbs14),

           pmorbs15 = ifelse(!is.na(lag(link_no, i)) | admission_date >= start_date_l, ifelse(15 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                                                                (admission_date - lag(admission_date, i)) <= 1825, 14, pmorbs15), pmorbs15),

           pmorbs16 = ifelse(!is.na(lag(link_no, i)) | admission_date >= start_date_l, ifelse(16 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                                                                (admission_date - lag(admission_date, i)) <= 1825, 18, pmorbs16), pmorbs16),

           pmorbs17 = ifelse(!is.na(lag(link_no, i)) | admission_date >= start_date_l, ifelse(17 == lag(pmorbs, i) & link_no == lag(link_no, i) &
                                                                                                (admission_date - lag(admission_date, i)) <= 1825, 2, pmorbs17), pmorbs17))

}


data_pmorbs5 <- data_pmorbs5 %>%
  group_by(link_no, cis_marker) %>%
  mutate(pmorbs5_sum = max(pmorbs1) + max(pmorbs2) + max(pmorbs3) + max(pmorbs4) + max(pmorbs5) + max(pmorbs6) + max(pmorbs7) + max(pmorbs8) + max(pmorbs9) +
           max(pmorbs10) + max(pmorbs11) + max(pmorbs12) + max(pmorbs13) + max(pmorbs14) + max(pmorbs15) + max(pmorbs16) + max(pmorbs17),
         epinum      = row_number()) %>%
  filter(epinum == 1) %>%
  ungroup()

data_pmorbs5 <- data_pmorbs5 %>%
  select(c("link_no", "cis_marker", "pmorbs5_sum"))

data <- data %>%
  left_join(data_pmorbs5, by = c("link_no", "cis_marker"))

rm(data_pmorbs5)


############
### SIMD ###
############

data$POSTCODE <- sub("  "," ", data$POSTCODE)
data$POSTCODE <- sub("   ","  ", data$POSTCODE)
data$POSTCODE[which(regexpr(" ",data$POSTCODE) == 5)] <- sub(" ", "",data$POSTCODE[which(regexpr(" ",data$POSTCODE) == 5)])

data$simd <- simd$simd[match(data$POSTCODE,simd$POSTCODE)]
rm(simd)

###################
### SAVING DATA ###
###################
save(data,file = paste(base_file,"QHSMR_SMR01_raw_basefile",".rda",sep=""))
