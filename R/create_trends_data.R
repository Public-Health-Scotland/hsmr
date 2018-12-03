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
library(tidyr)         # For data manipulation in the "tidy" way
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
z_start_date_trends <- dmy(01012008)     # The beginning of the ten year trend
z_end_date          <- dmy(31032018)     # End date for the cut off for z_smr01


### 4 - Source scripts ----

# SQL queries
source("R/sql_queries_trends.R")

# Wrangling function
source("R/trends_functions.R")


### 5 - Read in lookup files ----

# Postcode lookups for SIMD 2016, 2012 and 2009
# These files will be combined, so create a year variable in each one, to allow
# them to be differentiated from one another
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
# All lookups have labelled variables, and bind_rows() drops the labels
# This produces a warning message that vectorising labelled elements may not
# preserve their attributes, which can be ignored
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
# Both lookups have labelled variables, and bind_rows() drops the labels
# This produces a warning message that vectorising labelled elements may not
# preserve their attributes, which can be ignored
z_pop <- bind_rows(z_pop_est, z_pop_proj)

# Aggregate lookup to get Scotland population and append to bottom
z_pop %<>%
  bind_rows(z_pop %>%
              group_by(year) %>%
              summarise(pop = sum(pop)) %>%
              ungroup() %>%
              mutate(hb2014 = "Scotland"))


### SECTION 2 - DATA EXTRACTION AND MANIPULATION----

### 1 - Extract data ----

# Deaths data
z_gro     <- as_tibble(dbGetQuery(smra_connect, z_query_gro)) %>%
  clean_names()

# SMR01 data
z_smr01   <- as_tibble(dbGetQuery(smra_connect, z_query_smr01_ltt)) %>%
  clean_names()


### 2 - Pipeline ----
#
# SMR01    = The SMR01 extract used to produce crude rate data.
#            This should contain the quarters being published
#            PLUS one extra quarter at the beginning
# GRO      = The deaths extract used to produce CR data. This should contain
#            ALL data AFTER the start of the first publication quarter
# POP      = The population lookup file
# POSTCODE = The postcode lookup dataframe for SIMD matching
#
# This function does most of the wrangling required for producing HSMR
trends_data <- create_trends(smr01    = z_smr01,
                             gro      = z_gro,
                             pop      = z_pop,
                             dep      = z_simd_all)


### 3 - Save data ----
# temp step - find better way of saving (in new folder?)
readr::write_csv(trends_data[[1]], path = 'mtd_trends.csv')
