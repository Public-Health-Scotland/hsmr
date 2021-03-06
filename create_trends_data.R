#########################################################################
# Name of file - create_hsmr_data.R
# data release - Quarterly HSMR publication
# Original Authors - David Caldwell & Anna Price
# Orginal Date - August 2018
#
# Type - data extraction/preparation/modelling
# Written/run on - RStudio server
# Version of R - 3.5.1
#
# Description - Extracts SMR01 & deaths data and carries out required
# manipulations to create minimal tidy dataset for long term trends for HSMR
#
# Approximate run time - 60 minutes
#########################################################################


### SECTION 1 - HOUSE KEEPING ----

### 1 - Load environment file ----
source("setup_environment.R")

# Define the database connection with SMRA 
smra_connect  <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                            uid=.rs.askForPassword("SMRA Username:"),
                                            pwd=.rs.askForPassword("SMRA Password:")))

### 2 - Read in lookup files ----

# Postcode lookups for SIMD 2020, 2016, 2012 and 2009
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

simd_2009 <- read_spss(paste0(plat_filepath,
  "lookups/Unicode/Deprivation/",
  "postcode_2012_2_simd2009v2.sav")) %>%
  select(PC7, simd2009v2_sc_quintile) %>%
  rename(postcode = PC7,
         simd = simd2009v2_sc_quintile) %>%
  mutate(year = "simd_2009")

simd_2009[] <- lapply(simd_2009, c) #drop attributes to allow binding

# Combine postcode lookups into a single dataset
# All lookups have labelled variables, and bind_rows() drops the labels
# This produces a warning message that vectorising labelled elements may not
# preserve their attributes, which can be ignored
simd_all <- bind_rows(simd_2020, simd_2016, simd_2012, simd_2009) %>%
  mutate(simd = as.numeric(simd)) %>%
  pivot_wider(names_from = year, values_from = simd)

# Specialty Groupings lookup
spec <- read_spss(here("reference_files",
                       "discovery_spec_grps.sav"))

# Population lookups for 2017
pop_est  <- read_spss(paste0(plat_filepath,
  "lookups/Unicode/Populations/Estimates/",
  "HB2019_pop_est_1981_2019.sav")) %>%
  clean_names() %>%
  group_by(year, hb2019) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  mutate(hb2019 = as.character(hb2019) ) %>%
  rename(hb2014 = hb2019)

pop_est[] <- lapply(pop_est, c) #drop attributes to allow binding

pop_proj <- read_spss(paste0(plat_filepath,
  "lookups/Unicode/Populations/Projections/",
  "HB2019_pop_proj_2018_2043.sav")) %>%
  clean_names() %>%
  filter(year >= 2020) %>%
  group_by(year, hb2019) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%  rename(hb2014 =hb2019)

pop_proj[] <- lapply(pop_proj, c) #drop attributes to allow binding

# Combine population lookups into one lookup
# Both lookups have labelled variables, and bind_rows() drops the labels
# This produces a warning message that vectorising labelled elements may not
# preserve their attributes, which can be ignored
pop <- bind_rows(pop_est, pop_proj)

# Aggregate lookup to get Scotland population and append to bottom
pop %<>%
  bind_rows(pop %>%
              group_by(year) %>%
              summarise(pop = sum(pop)) %>%
              ungroup() %>%
              mutate(hb2014 = "Scotland"))


### SECTION 2 - DATA EXTRACTION AND MANIPULATION----

### 1 - Extract data ----

# Deaths data
gro     <- as_tibble(dbGetQuery(smra_connect, query_gro_ltt(
  extract_start = start_date_trends))) %>%
  clean_names()

# SMR01 data
smr01   <- as_tibble(dbGetQuery(smra_connect, query_smr01_ltt(
  extract_start = start_date_trends,
  extract_end = end_date))) %>%
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
# This function does most of the wrangling required for producing long term
# trends data
trends_data <- create_trends(smr01           = smr01,
                             gro             = gro,
                             pop             = pop,
                             dep             = simd_all,
                             spec            = spec,
                             hospital_lookup = hospitals)


### 3 - Save data ----

trends_data_lvl1 <- trends_data %>%
  filter((sub_grp == "All Admissions" &
            (agg_label == "Hospital" | agg_label == "Board")) |
           (agg_label == "Scotland" &
              (sub_grp != "Depth of Coding" & sub_grp != "Symptom Coding")) |
           (agg_label == "Board" &
              (sub_grp == "Discharge" | sub_grp == "Population")))

# File used for the Excel tables
save_file(trends_data_lvl1, "trends-data-level1", "output", "csv", dev = F, overwrite = F)

# Create TDE files
# yyyy-mm-dd_trend-data-level1.csv – Discovery HSMR Level 1 Trends & Discovery HSMR Level 1 Trends Live
save_file(trends_data_lvl1, "Discovery HSMR Level 1 Trends", out_folder = "tde", 
          type = "xlsx", dev = F, overwrite = F)
save_file(trends_data_lvl1, "Discovery HSMR Level 1 Trends Live", out_folder = "tde", 
          type = "xlsx", dev = F, overwrite = F)

# yyyy-mm-dd_trend-data-level2.csv – Discovery HSMR Level 2 Trends & Discovery HSMR Level 2 Trends Live
save_file(trends_data, "Discovery HSMR Level 2 Trends", out_folder = "tde", 
          type = "xlsx", dev = F, overwrite = F)
save_file(trends_data, "Discovery HSMR Level 2 Trends Live", out_folder = "tde", 
          type = "xlsx", dev = F, overwrite = F)

### END OF SCRIPT ###
