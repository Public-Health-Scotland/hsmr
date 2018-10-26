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


### 2 - Define the database connection with SMRA ----

smra_connect <- suppressWarnings(
  dbConnect(
    odbc(),
    dsn = "SMRA",
    uid = .rs.askForPassword("SMRA Username:"),
    pwd = .rs.askForPassword("SMRA Password:")))


### 3 - Extract dates ----


# Define the dates that the data are extracted from and to


# The beginning of baseline period (HSMR)
z_start_date   <- dmy(01012011)

# The beginning of baseline period (trends)
z_start_date   <- dmy(01012008)

# Five years earlier for the five year look-back (pmorbs5)
z_start_date_5 <- dmy(01012006)

# Beginning of the baseline period (pmorbs)
z_start_date_l <- dmy(01012011)

# End date for the cut off for data (HSMR)
z_end_date     <- dmy(31032018)

# End date for the cut off for data (trends)
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


z_simd_2016 <- read_spss(paste0(
  "/conf/linkage/output/lookups/Unicode/Deprivation",
  "/postcode_2018_2_simd2016.sav")) %>%
  select(pc7, simd2016_sc_quintile) %>%
  rename(postcode = pc7,
         simd = simd2016_sc_quintile) %>%
  mutate(year = "simd_2016")

z_simd_2012 <- read_spss(paste0(
  "/conf/linkage/output/lookups/Unicode/Deprivation/",
  "postcode_2016_1_simd2012.sav")) %>%
  select(pc7, simd2012_sc_quintile) %>%
  rename(postcode = pc7,
         simd = simd2012_sc_quintile) %>%
  mutate(year = "simd_2012")

z_simd_2009 <- read_spss(paste0(
  "/conf/linkage/output/lookups/Unicode/Deprivation/",
  "postcode_2012_2_simd2009v2.sav")) %>%
  select(PC7, simd2009v2_sc_quintile) %>%
  rename(postcode = PC7,
         simd = simd2009v2_sc_quintile) %>%
  mutate(year = "simd_2009")

# Combine postcode lookups into a single dataset
# Ignore warning messages about vectorising labelled elements
z_simd_all <- bind_rows(z_simd_2016, z_simd_2012, z_simd_2009) %>%
  spread(year, simd)


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
  bind_rows(z_pop %>%
              group_by(year) %>%
              summarise(pop = sum(pop)) %>%
              ungroup() %>%
              mutate(hb2014 = "Scotland"))


# Hospital names
z_hospitals <- read_csv(paste0(z_lookups,
                               "location_lookups.csv"))


### SECTION 2 - SMR DATA PIPELINE ----

# 1 - Source SQL Query ----
source("sql_queries_smr.R")


# 2 - Extract deaths and SMR01 data from SMRA databases ----
deaths  <- as_tibble(dbGetQuery(z_smra_connect, z_query_gro)) %>%
  clean_names()

z_smr01 <- as_tibble(dbGetQuery(z_smra_connect, z_query_smr01)) %>%
  clean_names()

data_pmorbs <- as_tibble(dbGetQuery(z_smra_connect,
                                    z_query_smr01_minus5)) %>%
  clean_names()


# 3 - Pipeline ----
# SMR01    = The SMR01 extract used to produce SMR data. This should contain
#            ONLY the quarters being published
# GRO      = The deaths extract used to produce SMR data. This should contain
#            ALL data AFTER the start of the first publication quarter
# PDIAGS   = The primary diagnosis lookup dataframe/tibble
# POSTCODE = The postcode lookup dataframe for SIMD matching
#
# This function does most of the wrangling required for producing HSMR
z_smr01 <- function_1(SMR01    = z_smr01,
                      GRO      = deaths,
                      PDIAGS   = z_pdiag_grp_data,
                      POSTCODE = z_simd_all)

# SMR01        = The output from function_1
# SMR01_MINUS5 = The SMR01 extract used to calculate the prior morbidities.
#                This should contain all publication quarters plus an extra
#                five years at the start
#
# This function does the final bits of wrangling required for HSMR. These
# are done separately from the rest because they are quite resource-heavy
# and prone to crashing
z_smr01 <- function_2(SMR01        = z_smr01,
                      SMR01_MINUS5 = data_pmorbs)

# SMR01      = The output from function_2
# BASE_START = The beginning of the baseline period
# BASE_END   = The end of the baseline period
# INDEX      = Indicating whether the patient indexing is done quarterly
#              or annually
#
# This function runs the risk model and appends the probability of death on
# to the SMR01 extract
z_smr01 <- function_3(SMR01      = z_smr01,
                      BASE_START = start_of_baseline,
                      BASE_END   = end_of_baseline,
                      INDEX      = index)

# SMR01 = The output from function_3
# INDEX = Indicating whether the patient indexing is done quarterly
#         or annually
#
# This function aggregates the data down into quarterly/annual SMR figures
smr_data <- function_4(SMR01 = z_smr01,
                       INDEX = index)


### SECTION 3 - TRENDS PIPELINE

## 1 - Source SQL Query ----
source("sql_queries_trends.R")


### 2 - Extract data ----
z_gro     <- as_tibble(dbGetQuery(smra_connect, z_query_gro)) %>%
  clean_names()

z_smr01   <- as_tibble(dbGetQuery(smra_connect, z_query_smr01_ltt)) %>%
  clean_names()


### 3 - Pipeline ----
# SMR01    = The SMR01 extract used to produce crude rate data.
#            This should contain the quarters being published
#            PLUS one extra quarter at the beginning
# GRO      = The deaths extract used to produce CR data. This should contain
#            ALL data AFTER the start of the first publication quarter
# pop      = The population lookup file
# dep      = The postcode lookup dataframe for SIMD matching
#
# This function does most of the wrangling required for producing HSMR
trends_data <- create_trends(smr01    = z_smr01,
                             gro      = z_gro,
                             pop      = z_pop,
                             dep      = z_simd_all)
