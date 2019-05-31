#########################################################################
# Name of file - setup_environment.R
# Data release - Quarterly HSMR publication
# Original Authors - David Caldwell
# Orginal Date - April 2019
#
# Type - Reproducible Analytical Pipeline
# Written/run on - RStudio server
# Version of R - 3.2.3
#
# Description - Sets up environment required for running reproducible analytical
# pipeline process. This is the only file which should require updating every
# time the RAP process is run
#
# Approximate run time - xx minutes
#########################################################################


### 1 - Define Whether Running on Server or Locally ----
# Comment out as appropriate
platform <- c("server")
#platform <- c("locally")


# Define root directory for cl-out based on whether script is running locally or
# on server
plat_filepath <- dplyr::if_else(platform == "server",
                                '/conf/linkage/output/',
                                '//stats/cl-out/')


### 2 - Extract dates ----


# Define the dates that the data are extracted from and to
# Dates are in ddmmyyyy format


# The beginning of baseline period/extract window
start_date        <- lubridate::dmy(01012016)

# The beginning of baseline period/extract window for trend data
start_date_trends <- lubridate::dmy(01012014)

# The end of the baseline period (for model)
base_end          <- lubridate::dmy(31122018)

# Five years earlier for the five year look-back (pmorbs5)
start_date_5      <- lubridate::dmy(01012011)

# First day of latest quarter in current publication
first_day         <- lubridate::dmy(01012019)

# End date for the cut off for data
end_date          <- lubridate::dmy(31122018)


### END OF SCRIPT ###
