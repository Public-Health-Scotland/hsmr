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


### 2 - Define the database connection with SMRA ----
source("odbc_connect.R")


### 3 - Read in lookup files ----

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
                              "/postcode_2020_1_simd2020v2.sav")) %>%
  select(pc7, simd2020v2_sc_quintile) %>%
  rename(postcode = pc7,
         simd = simd2020v2_sc_quintile) %>%
  mutate(year = "simd_2020")

simd_2016 <- read_spss(paste0(plat_filepath,
                              "lookups/Unicode/Deprivation",
                              "/postcode_2019_2_simd2016.sav")) %>%
  select(pc7, simd2016_sc_quintile) %>%
  rename(postcode = pc7,
         simd = simd2016_sc_quintile) %>%
  mutate(year = "simd_2016")

simd_2012 <- read_spss(paste0(plat_filepath,
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
simd_all <- bind_rows(simd_2020, simd_2016, simd_2012) %>%
  spread(year, simd)


# Hospital names
hospitals <- bind_rows(read_spss(paste0(
  plat_filepath,
  "lookups/Unicode/National Reference Files/",
  "location.sav")) %>%
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

saveRDS(deaths, here("data",
                     "base_files",
                     paste0(pub_date(end_date = end_date, pub = "current"),
                            "_GRO_deaths.rds")))

saveRDS(smr01, here("data",
                     "base_files",
                     paste0(pub_date(end_date = end_date, pub = "current"),
                            "_SMR01_basefile.rds")))

saveRDS(data_pmorbs, here("data",
                     "base_files",
                     paste0(pub_date(end_date = end_date, pub = "current"),
                            "_SMR01_minus_5_basefile.rds")))


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
write_csv(smr01 %>%
            filter(admission_date >= start_date + years(2)),
            here("data",
                      "base_files",
                      paste0(pub_date(end_date = end_date, pub = "current"),
                             "_SMR-with-predprob.csv")))

write_csv(smr_data, here("data",
                         "output",
                         paste0(pub_date(end_date = end_date, pub = "current"),
                                "_SMR-data.csv")))


### END OF SCRIPT ###
