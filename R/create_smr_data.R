#########################################################################
# Name of file - create_hsmr_data.R
# Data release - Quarterly HSMR publication
# Original Authors - David Caldwell
# Orginal Date - February 2018
#
# Type - Reproducible Analytical Pipeline
# Written/run on - RStudio server
# Version of R - 3.2.3
#
# Description - Extracts SMR01 & deaths data and carries out required
# manipulations and modelling to create the minimal tidy datasets for HSMR
#
# Approximate run time - xx minutes
#########################################################################


### SECTION 1 - HOUSE KEEPING ----

### 1 - Load packages ----
library(odbc)          # For accessing SMRA databases
library(data.table)    # For fast data manipulations
library(dplyr)         # For data manipulation in the "tidy" way
library(haven)         # For reading in SPSS files
library(readr)         # For reading in csv files
library(janitor)       # For 'cleaning' variable names
library(magrittr)      # For %<>% operator
library(lubridate)     # For dates
library(tidyr)         # For data manipulation in the "tidy" way
library(fuzzyjoin)     # For fuzzy joins
library(stringr)       # For string manipulation and matching

### 2 - Define Whether Running on Server or Locally ----
# Comment out as appropriate
#platform <- c("server")
platform <- c("locally")


# Define root directory for cl-out based on whether script is running locally or
# on server
plat_filepath <- ifelse(platform == "server",
                        '/conf/linkage/output/',
                        '//stats/cl-out/')


### 3 - Define the database connection with SMRA ----

z_smra_connect <- suppressWarnings(
  dbConnect(
    odbc(),
    dsn = "SMRA",
    uid = .rs.askForPassword("SMRA Username:"),
    pwd = .rs.askForPassword("SMRA Password:")))


### 4 - Extract dates ----


# Define the dates that the data are extracted from and to


# The beginning of baseline period
z_start_date   <- dmy(01012011)

# Five years earlier for the five year look-back (pmorbs5)
z_start_date_5 <- dmy(01012006)

# Beginning of the baseline period (pmorbs)
z_start_date_l <- dmy(01012011)

# End date for the cut off for data
z_end_date     <- dmy(31032018)


### 5 - Source scripts ----

# SQL queries
source("sql_queries_smr.R")

# Wrangling functions
source("R/smr_functions.R")


### 6 - Set filepaths ----

# Define lookups directory
z_lookups <- "R/reference_files/"


### 6 - Read in lookup files ----


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
# These files will be combined, so create a year variable in each one, to allow
# them to be differentiated from one another
z_simd_2016 <- read_spss(paste0(plat_filepath,
  "lookups/Unicode/Deprivation",
  "/postcode_2018_2_simd2016.sav")) %>%
  select(pc7, simd2016_sc_quintile) %>%
  rename(postcode = pc7,
         simd = simd2016_sc_quintile) %>%
  mutate(year = "simd_2016")

z_simd_2012 <- read_spss(paste0(plat_filepath,
  "lookups/Unicode/Deprivation/",
  "postcode_2016_1_simd2012.sav")) %>%
  select(pc7, simd2012_sc_quintile) %>%
  rename(postcode = pc7,
         simd = simd2012_sc_quintile) %>%
  mutate(year = "simd_2012")

# Combine postcode lookups into a single dataset
# Both lookups have labelled variables, and bind_rows() drops the labels
# This produces a warning message that vectorising labelled elements may not
# preserve their attributes, which can be ignored
z_simd_all <- bind_rows(z_simd_2016, z_simd_2012) %>%
  spread(year, simd)


# Hospital names
z_hospitals <- read_csv(paste0(z_lookups,
                               "location_lookups.csv"))


### SECTION 2 - DATA EXTRACTION AND MANIPULATION ----

### 1 - Extract data Extract deaths and SMR01 data from SMRA databases ----

# Deaths data
deaths  <- as_tibble(dbGetQuery(z_smra_connect, z_query_gro)) %>%
  clean_names()

# SMR01 data
z_smr01 <- as_tibble(dbGetQuery(z_smra_connect, z_query_smr01)) %>%
  clean_names()

# Prior morbidities within previous 1 & 5 years data
data_pmorbs <- as_tibble(dbGetQuery(z_smra_connect,
                                    z_query_smr01_minus5)) %>%
  clean_names()


# 2 - Pipeline ----
# SMR01    = The SMR01 extract used to produce SMR data. This should contain
#            ONLY the quarters being published
# GRO      = The deaths extract used to produce SMR data. This should contain
#            ALL data AFTER the start of the first publication quarter
# PDIAGS   = The primary diagnosis lookup dataframe/tibble
# POSTCODE = The postcode lookup dataframe for SIMD matching
#
# This function does most of the wrangling required for producing HSMR
z_smr01 <- smr_wrangling(smr01    = z_smr01,
                         gro      = deaths,
                         pdiags   = z_pdiag_grp_data,
                         postcode = z_simd_all,
                         morbs    = z_morbs)

# SMR01        = The output from function_1
# SMR01_MINUS5 = The SMR01 extract used to calculate the prior morbidities.
#                This should contain all publication quarters plus an extra
#                five years at the start
#
# This function does the final bits of wrangling required for HSMR. These
# are done separately from the rest because they are quite resource-heavy
# and prone to crashing
z_smr01 <- smr_pmorbs(smr01        = z_smr01,
                      smr01_minus5 = data_pmorbs,
                      morbs        = z_morbs)

# SMR01      = The output from function_2
# BASE_START = The beginning of the baseline period
# BASE_END   = The end of the baseline period
# INDEX      = Indicating whether the patient indexing is done quarterly
#              or annually
#
# This function runs the risk model and appends the probability of death on
# to the SMR01 extract
z_smr01 <- function_3(smr01      = z_smr01,
                      base_start = start_of_baseline,
                      base_end   = end_of_baseline,
                      index      = index)

# SMR01 = The output from function_3
# INDEX = Indicating whether the patient indexing is done quarterly
#         or annually
#
# This function aggregates the data down into quarterly/annual SMR figures
smr_data <- function_4(smr01 = z_smr01,
                       index = index)
