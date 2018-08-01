#########################################################################
# Name of file - create_hsmr_data.R
# Data release - Quarterly HSMR publication
# Original Authors - David Caldwell & Anna Price
# Orginal Date - August 2018
#
# Type - Data extraction/preparation/modelling
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
z_end_date     <- c("'2018-03-31'")     # End date for the cut off for data
