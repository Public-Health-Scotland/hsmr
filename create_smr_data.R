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

# Postcode lookups for SIMD 2020, 2016 and 2012
# These files will be combined, so create a year variable in each one, to allow
# them to be differentiated from one another
simd_2020 <- readRDS(paste0(plat_filepath,
                              "lookups/Unicode/Deprivation",
                              "/postcode_2021_2_simd2020v2.rds")) %>%
  select(pc7, simd2020v2_sc_quintile) %>%
  rename(postcode = pc7,
         simd = simd2020v2_sc_quintile) %>%
  mutate(year = "simd_2020")

simd_2016 <- readRDS(paste0(plat_filepath,
                              "lookups/Unicode/Deprivation",
                              "/postcode_2019_2_simd2016.rds")) %>%
  select(pc7, simd2016_sc_quintile) %>%
  rename(postcode = pc7,
         simd = simd2016_sc_quintile) %>%
  mutate(year = "simd_2016")

simd_2012 <- readRDS(paste0(plat_filepath,
                              "lookups/Unicode/Deprivation/",
                              "postcode_2016_1_simd2012.rds")) %>%
  select(pc7, simd2012_sc_quintile) %>%
  rename(postcode = pc7,
         simd = simd2012_sc_quintile) %>%
  mutate(year = "simd_2012")

# Combine postcode lookups into a single dataset
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
save_file(deaths, "GRO_deaths", "base_files", "rds",  dev = F, overwrite = F)
save_file(smr01, "SMR01_basefile", "base_files", "rds",  dev = F, overwrite = F)
save_file(data_pmorbs, "SMR01_minus_5_basefile", "base_files", "rds",
          dev = F, overwrite = F)


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
save_file(smr01 %>% filter(admission_date >= start_date + years(2)) %>%
                    change_hbcodes(version_to = "14", code_cols = "hbtreat_currentdate"),
          "SMR-with-predprob", "base_files", "csv", dev = F, overwrite = F)

save_file(smr_data, "SMR-data", "output", "csv", dev = F, overwrite = F)


# File for dashboard, bringing previous publication data and adding new period
smr_data_dash <- readr::read_csv(paste0(data_folder, previous_pub,
                                 "/output/", previous_pub, "_SMR-data_dashboard.csv")) %>%
                mutate(completeness_date = paste0(substr(completeness_date,7,10),
                                                  "-", substr(completeness_date,4,5),
                                                  "-", substr(completeness_date,1,2)))

smr_data_dash <- rbind(smr_data, smr_data_dash) %>%
  # Required locations specified in setup_environment
  filter(location %in% locations_filter) %>%
  change_hbcodes(version_to = "14") # Tableau uses 2014 codes, but code produces 2019

# Used for the offline dashboard
save_file(smr_data_dash, "SMR-data_dashboard", "output", "csv", dev = F, overwrite = F)

# Create TDE files
# yyyy-mm-dd_SMR-data_dashboard.csv – Discovery HSMR Level 1 SMR & Discovery HSMR Level 1 SMR Live
save_file(smr_data_dash, "Discovery HSMR Level 1 SMR", out_folder = "tde",
          type = "xlsx", dev = F, overwrite = F)
save_file(smr_data_dash, "Discovery HSMR Level 1 SMR Live", out_folder = "tde",
          type = "xlsx", dev = F, overwrite = F)


# Create file for RShiny public dashboard
# Update the HB codes back to the 2019 codes
public_dash <- smr_data_dash %>%
  change_hbcodes(version_to = "19") %>%
  # Create a variable that is used to sort time periods
  mutate(year = stringr::word(period_label, 2, 2),
         month = sprintf("%02d", match(stringr::word(period_label, 1, 1), month.name)),
         order_var = paste0(year, "-", month))

# Create a Scotland row to add after funnel limits have been calculated
public_dash_scot <- public_dash %>%
  filter(location == 'Scot' & period == 3)

# Create warning and control confidence limits for funnel plot
public_dash_hosps <- public_dash %>%
  filter(period == 3 & location %in% c(hosp_filter)) %>%
  mutate(st_err = round_half_up(sqrt(1/round_half_up(pred, 8)), 8),
         z = if_else(location_type == "hospital",
                     round_half_up(((round_half_up(smr, 8) - 1)/round_half_up(st_err,8)), 8),
                     0)) %>%
  mutate(z_max = max(z),
         z_min = min(z),
         z_flag = case_when(z == z_max ~ 1,
                            z == z_min ~ -1,
                            TRUE ~ 0),
         z = if_else(z == z_max | z == z_min, 0, z),
         z_max = max(z),
         z_min = min(z),
         z = case_when(z_flag == 1 ~ z_max,
                       z_flag == -1 ~ z_min,
                       TRUE ~ z),
         z_flag = if_else(z != 0, 1, 0),
         w_score = round_half_up(sqrt(sum(round_half_up(z * z, 8))/sum(z_flag)),8)) %>%
  # Calculate funnel limits for funnel plot
  mutate(uwl = 1 + 1.96 * round_half_up(st_err * w_score,8),
         ucl = 1 + 3.09 * round_half_up(st_err * w_score,8),
         lwl = 1 - 1.96 * round_half_up(st_err * w_score,8),
         lcl = 1 - 3.09 * round_half_up(st_err * w_score,8)) %>%

  # Create flag for where hospital sits on funnel plot
  mutate(flag = case_when(smr > ucl ~ "1",
                          smr < lcl ~ "2",
                          smr > uwl & smr <= ucl ~ "3",
                          smr <lwl & smr >= lcl ~ "4",
                          TRUE ~ "0"))

# Keep only variables that are required for dashboard
public_dash_all <- bind_rows(public_dash_scot, public_dash_hosps) %>%
  select(hb, location, location_name, order_var, period_label, deaths, pred,
                 pats, smr, crd_rate, smr_scot, death_scot, pats_scot,
                 uwl, ucl, lwl, lcl, flag) %>%
  arrange(order_var, location_name)


# Save into output folder
save_file(public_dash_all, "SMR_data_public_dashboard", "output", "rds", dev = F, overwrite = F)


### END OF SCRIPT ###
