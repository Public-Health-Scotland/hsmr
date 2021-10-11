#########################################################################
# Name of file - setup_environment.R
# Data release - Quarterly HSMR publication
# Original Authors - David Caldwell
# Orginal Date - April 2019
#
# Type - Reproducible Analytical Pipeline
# Written/run on - RStudio server
# Version of R - 3.5.1
#
# Description - Sets up environment required for running reproducible analytical
# pipeline process. This is the only file which should require updating every
# time the RAP process is run
#
# Approximate run time - xx minutes
#########################################################################

### 1 - Load packages ----
library(odbc)          # For accessing SMRA databases
library(dplyr)         # For data manipulation in the "tidy" way
library(haven)         # For reading in SPSS files
library(readr)         # For reading in csv files
library(janitor)       # For 'cleaning' variable names
library(magrittr)      # For %<>% operator
library(lubridate)     # For dates
library(tidylog)       # For printing results of some dplyr functions
library(tidyr)         # For data manipulation in the "tidy" way
library(stringr)       # For string manipulation and matching
library(ggplot2)       # For ggplot objects
library(scales)        # For use with ggplot2
library(ggrepel)       # For funnel plot labels
library(here)          # For the here() function
library(openxlsx)      # For manipulating Excel files
library(devtools)      # For automatically building HSMR package

# Install HSMR package, without creating a bundle and without checking/upgrading
# dependencies. Ensures latest changes are used.
#
# Note this will not update the documentation.
# Ctrl+Shift+D > install package > restart R to do that.
# (Help returns error until R is restarted => cannot do programmatically.)
devtools::install(build = FALSE, dependencies = FALSE, upgrade = "never")

library(hsmr)          # For HSMR functions

### 2 - Define Whether Running on Server or Locally ----
# Covers both the old server and the pro one
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)",
                                  "x86_64-pc-linux-gnu (64-bit)")) {
  platform <- "server"
  } else {
  platform <- "locally"
}


# Define root directory for cl-out based on whether script is running locally or
# on server
plat_filepath <- dplyr::if_else(platform == "server",
                                '/conf/linkage/output/',
                                '//stats/cl-out/')

data_folder <- dplyr::if_else(platform == "server",
                                '/conf/quality_indicators/hsmr/quarter_cycle/data/',
                                '//stats/quality_indicators/hsmr/quarter_cycle/data/')

# Setting file permissions to anyone to allow writing/overwriting of project files
Sys.umask("006")

### 3 - Extract dates ----


# Define the dates that the data are extracted from and to
# Dates are in ddmmyyyy format


# The beginning of baseline period/extract window
start_date        <- lubridate::dmy(01072018)

# The beginning of baseline period/extract window for trend data
start_date_trends <- lubridate::dmy(01072016)

# The end of the baseline period (for model)
base_end          <- lubridate::dmy(31062021)

# Five years earlier for the five year look-back (pmorbs5)
start_date_5      <- lubridate::dmy(01072013)

# First day of latest quarter in current publication
qtr_start         <- lubridate::dmy(01042021)

# End date for the cut off for data
end_date          <- lubridate::dmy(31062021)

# Publication date
pub_day <- pub_date(end_date = end_date, pub = "current")
previous_pub <- pub_date(end_date, "previous")

## 4 - Create data folder structure ----
#  If the folder doesn't exist creates it and subfolders too
if (dir.exists(paste0(data_folder, pub_day)) == FALSE) {
  dir.create(paste0(data_folder, pub_day))
  dir.create(paste0(data_folder, pub_day, "/base_files"))
  dir.create(paste0(data_folder, pub_day, "/tde"))
  dir.create(paste0(data_folder, pub_day, "/output"))
  dir.create(paste0(data_folder, pub_day, "/open_data"))
}

## 5 - Load common lookups to all scripts ----
# Hospital names
hospitals <- bind_rows(read_csv(paste0(
  plat_filepath,
  "lookups/Unicode/National Reference Files/",
  "location.csv")) %>%
    select(Location, Locname) %>%
    rename(location      = Location,
           location_name = Locname),
  read_spss(paste0(plat_filepath,
                   "lookups/Unicode/National Reference Files/",
                   "Health_Board_Identifiers.sav")) %>%
    select(description, HB_Area_2014) %>%
    rename(location      = HB_Area_2014,
           location_name = description),
  tibble(location = "Scot", location_name = "Scotland"),
  tibble(location = "S08000029", location_name = "NHS Fife"),
  tibble(location = "S08000030", location_name = "NHS Tayside"),
  tibble(location = "S08000031", location_name = "NHS Greater Glasgow & Clyde"),
  tibble(location = "S08000032", location_name = "NHS Lanarkshire"))

# Specialty Groupings lookup
specialty_group <- readRDS(here("reference_files", "discovery_spec_grps.rds"))

## 6 - Select locations to be included in excel tables/dashboard files ----

# List of locations included in the Excel tables/dashboard files/markdown
hosp_filter = c('A101H', 'A111H', 'A210H', 'B120H', 'D102H', 'F704H',
                'G107H', 'C313H', 'G405H', 'C418H', 'H212H', 'H103H', 'C121H',
                'H202H', 'L302H', 'L106H', 'L308H', 'N101H', 'N411H', 'R103H',
                'S314H', 'S308H', 'S116H', 'T101H', 'T202H', 'T312H', 'V217H',
                'W107H', 'Y146H', 'Y144H', 'Z102H')

board_filter = c("S08000015", "S08000016", "S08000017", "S08000029",
                 "S08000019", "S08000020", "S08000031", "S08000022",
                 "S08000032", "S08000024", "S08000025", "S08000026",
                 "S08000030", "S08000028", "S08100001")

scot_filter = "Scot"

locations_filter = c(hosp_filter, board_filter, scot_filter)


### END OF SCRIPT ###
