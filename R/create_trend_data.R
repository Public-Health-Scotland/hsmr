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
library("odbc")          # Accessing SMRA
library("dplyr")         # For data manipulation in the "tidy" way
library("foreign")       # For reading in SPSS SAV Files
library("haven")         # For reading in spss files


### 2 - Define the database connection with SMRA
suppressWarnings(SMRA_connect <- dbConnect(odbc(), dsn = "SMRA",
                                           uid = .rs.askForPassword("SMRA Username:"),
                                           pwd = .rs.askForPassword("SMRA Password:")))


### 3 - Extract dates ----
# Define the dates that the data are extracted from and to
z_start_date   <- c("'2008-01-01'")     # The beginning of the ten year trend
z_end_date     <- c("'2018-03-31'")     # End date for the cut off for z_smr01


# Postcode lookups for SIMD 2016 and 2012
z_simd_2016      <- read_spss("/conf/linkage/output/lookups/Unicode/Deprivation/postcode_2018_1.5_simd2016.sav")[ , c("pc7", "simd2016_sc_quintile")]
z_simd_2012      <- read_spss("/conf/linkage/output/lookups/Unicode/Deprivation/postcode_2016_1_simd2012.sav")[ , c("pc7", "simd2012_sc_quintile")]
z_simd_2009      <- read_spss("/conf/linkage/output/lookups/Unicode/Deprivation/postcode_2012_2_simd2009v2.sav")[ , c("pc7", "simd2009v2_sc_quintile")]


### SECTION 2 - DATA EXTRACTION----

### 1 - data extraction ----
# Source SQL queries
source("R/sql_queries_trends.R")


### 2 - Extract data ----
z_gro     <- as_tibble(dbGetQuery(SMRA_connect, z_query_gro))
z_smr01   <- as_tibble(dbGetQuery(SMRA_connect, z_query_smr01_ltt))

### SECTION 3 - DATA PREPARATION----

### 1 - Variable names to lower case ----
names(z_smr01)   <- tolower(names(z_smr01))
names(z_gro) <- tolower(names(z_gro))


### 2 - Deaths Data ----
# Removing duplicate records on link_no as the deaths file is matched on to SMR01 by link_no
# link_no needs to be unique
z_gro <- z_gro %>%
  distinct(link_no, .keep_all = TRUE)

# Matching deaths data on to SMR01 data
z_smr01$date_of_death <- z_gro$date_of_death[match(z_smr01$link_no,z_gro$link_no)]

# Sorting data by link_no, cis_marker, adm_date and dis_date
z_smr01 <- z_smr01 %>%
  arrange(link_no, cis_marker, admission_date, discharge_date)

# Deleting unecessary dataframes
rm(z_gro);gc()


### 3 - SIMD ---

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

### 4 - Manipulations

z_smr01 <- z_smr01 %>%
  mutate(death_inhosp = ifelse(discharge_type >= 40 & discharge_type <= 49, 1, 0),
         dthdays      = (date_of_death - admission_date)/60/60/24,
         death30      = 0,
         death30      = ifelse(dthdays >= 0 & dthdays <= 30, 1, 0),
         quarter_name = paste(year, "Q", quarter, sep = ""),
         quarter      = as.numeric(as.factor(quarter_name))) %>%
  group_by(link_no, cis_marker) %>%
  mutate(epinum           = row_number(),
         death_inhosp_max = max(death_inhosp)) %>%
  arrange(link_no, cis_marker, admission_date, discharge_date)
