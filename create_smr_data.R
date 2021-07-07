#########################################################################
# Name of file - create_hsmr_data.R
# Data release - Quarterly HSMR publication
# Original Authors - David Caldwell
# Orginal Date - February 2018
#
# Type - Reproducible Analytical Pipeline
# Written/run on - RStudio server
# Version of R - 3.5.1
#
# Description - Extracts SMR01 & deaths data and carries out required
# manipulations and modelling to create the minimal tidy datasets for HSMR
#
# Approximate run time - xx minutes
#########################################################################


### SECTION 1 - HOUSE KEEPING ----

### 1 - Load environment file ----
source("setup_environment.R")

# Define the database connection with SMRA 
smra_connect  <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                            uid=.rs.askForPassword("SMRA Username:"),
                                            pwd=.rs.askForPassword("SMRA Password:")))

### 2 - Read in lookup files ----

# Primary Diagnosis Groupings
pdiag_grp_data <- read_csv(here("reference_files",
                                 "diag_grps_lookup_updated.csv")) %>%
  select(diag1_4, DIAGNOSIS_GROUP) %>%
  clean_names()


# ICD-10 codes, their Charlson Index Groupings and CIG weights
morbs <- read_csv(here("reference_files", "morbs.csv")) %>%

  # Gather ICD codes into a single column
  gather(code, diag, diag_3:diag_4) %>%
  select(-code) %>%

  # Remove all NAs from the ICD-10 column
  filter(!is.na(diag))


# Specialty Groupings lookup
specialty_group <- read_spss(here("reference_files", "discovery_spec_grps.sav"))


# Postcode lookups for SIMD 2020, 2016 and 2012
# These files will be combined, so create a year variable in each one, to allow
# them to be differentiated from one another
simd_2020 <- read_spss(paste0(plat_filepath,
                              "lookups/Unicode/Deprivation",
                              "/postcode_2020_2_simd2020v2.sav")) %>%
  select(pc7, simd2020v2_sc_quintile) %>%
  rename(postcode = pc7,
         simd = simd2020v2_sc_quintile) %>%
  mutate(year = "simd_2020")

simd_2020[] <- lapply(simd_2020, c) #drop attributes to allow binding

simd_2016 <- read_spss(paste0(plat_filepath,
                              "lookups/Unicode/Deprivation",
                              "/postcode_2019_2_simd2016.sav")) %>%
  select(pc7, simd2016_sc_quintile) %>%
  rename(postcode = pc7,
         simd = simd2016_sc_quintile) %>%
  mutate(year = "simd_2016")

simd_2016[] <- lapply(simd_2016, c) #drop attributes to allow binding

simd_2012 <- read_spss(paste0(plat_filepath,
                              "lookups/Unicode/Deprivation/",
                              "postcode_2016_1_simd2012.sav")) %>%
  select(pc7, simd2012_sc_quintile) %>%
  rename(postcode = pc7,
         simd = simd2012_sc_quintile) %>%
  mutate(year = "simd_2012") 

simd_2012[] <- lapply(simd_2012, c) #drop attributes to allow binding

# Combine postcode lookups into a single dataset
# Both lookups have labelled variables, and bind_rows() drops the labels
# This produces a warning message that vectorising labelled elements may not
# preserve their attributes, which can be ignored
simd_all <- bind_rows(simd_2020, simd_2016, simd_2012) %>%
  pivot_wider(names_from = year, values_from = simd)

rm(simd_2020, simd_2016, simd_2012) # saving a bit of space

### SECTION 2 - DATA EXTRACTION AND MANIPULATION ----

### 1 - Extract data Extract deaths and SMR01 data from SMRA databases ----

# Deaths data
deaths  <- as_tibble(dbGetQuery(smra_connect,
                                query_gro_smr(extract_start = start_date))) %>%
  clean_names()

# SMR01 data
smr01 <- as_tibble(dbGetQuery(smra_connect,
                              query_smr01(extract_start = start_date,
                                          extract_end = end_date))) %>%
  clean_names()

# Prior morbidities within previous 1 & 5 years data
data_pmorbs <- as_tibble(dbGetQuery(smra_connect,
                                    query_smr01_minus5(
                                      extract_start = start_date_5,
                                      extract_end = end_date))) %>%
  clean_names()

# Save basefiles
save_file(deaths, "GRO_deaths", "base_files", "rds")
save_file(smr01, "SMR01_basefile", "base_files", "rds")
save_file(data_pmorbs, "SMR01_minus_5_basefile", "base_files", "rds")


# 2 - Pipeline ----
# smr01    = The SMR01 extract used to produce SMR data. This should contain
#            ONLY the quarters being published
# gro      = The deaths extract used to produce SMR data. This should contain
#            ALL data AFTER the start of the first publication quarter
# pdiags   = The primary diagnosis lookup dataframe/tibble
# postcode = The postcode lookup dataframe for SIMD matching
# morbs    = The lookup tibble for morbidity groupings
# spec     = The specialty grouping lookup tibble
#
# This function does most of the wrangling required for producing HSMR
smr01 <- smr_wrangling(smr01    = smr01,
                       gro      = deaths,
                       pdiags   = pdiag_grp_data,
                       postcode = simd_all,
                       morbs    = morbs,
                       spec     = specialty_group)

# smr01        = The output from smr_wrangling()
# smr01_minus5 = The SMR01 extract used to calculate the prior morbidities.
#                This should contain all publication quarters plus an extra
#                five years at the start
# morbs        = The lookup tibble for morbidity groupings
#
# This function does the final bits of wrangling required for HSMR. These
# are done separately from the rest because they are quite resource-heavy
# and prone to crashing
smr01 <- smr_pmorbs(smr01        = smr01,
                    smr01_minus5 = data_pmorbs,
                    morbs        = morbs)

# smr01      = The output from smr_pmorbs()
# base_start = The beginning of the baseline period
# base_end   = The end of the baseline period
# index      = Indicating whether the patient indexing is done quarterly
#              or annually
#
# This function runs the risk model and appends the probability of death on
# to the SMR01 extract
smr01 <- smr_model(smr01      = smr01,
                   base_start = start_date,
                   base_end   = base_end,
                   index      = "Y")

# smr01 = The output from smr_model()
# index = Indicating whether the patient indexing is done quarterly
#         or annually
#
# This function aggregates the data down into quarterly/annual SMR figures
smr_data <- smr_data(smr01 = smr01,
                     index = "Y",
                     hospital_lookup = hospitals)


### 3 - Save data ----
# This is the level 3 caselisting file
save_file(smr01 %>% filter(admission_date >= start_date + years(2)), 
          "SMR-with-predprob", "base_files", "csv")

save_file(smr_data, "SMR-data", "output", "csv", dev = F, overwrite = F)

# File for dashboard, bringing previous publication data and adding new period
smr_data_dash <- readr::read_csv(paste0(data_folder, previous_pub,
                                 "/output/", previous_pub, "-SMR-data_dashboard.csv")) %>% 
                mutate(completeness_date = paste0(substr(completeness_date,7,10),
                                                  "-", substr(completeness_date,4,5),
                                                  "-", substr(completeness_date,1,2)))

smr_data_dash <- rbind(smr_data, smr_data_dash) %>% 
  change_hbcodes(version_to = "14") # Tableau uses 2014 codes, but code produces 2019

# Create TDE files
# yyyy-mm-dd_SMR-data_dashboard.csv – Discovery HSMR Level 1 SMR & Discovery HSMR Level 1 SMR Live 
save_file(smr_data_dash, "Discovery HSMR Level 1 SMR", out_folder = "tde", 
          type = "xlsx", dev = F, overwrite = F)
save_file(smr_data_dash, "Discovery HSMR Level 1 SMR Live", out_folder = "tde", 
          type = "xlsx", dev = F, overwrite = F)

### END OF SCRIPT ###
