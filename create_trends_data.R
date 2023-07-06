#########################################################################.
# Name of file - create_hsmr_data.R
# data release - Quarterly HSMR publication
#
# Description - Extracts SMR01 & deaths data and carries out required
# manipulations to create minimal tidy dataset for long term trends for HSMR
#
# Approximate run time - 60 minutes
#########################################################################.

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
simd_2020 <- readRDS(paste0(plat_filepath,
                              "lookups/Unicode/Deprivation",
                              "/postcode_2023_1_simd2020v2.rds")) %>%
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

simd_2009 <- readRDS(paste0(plat_filepath,
  "lookups/Unicode/Deprivation/",
  "postcode_2012_2_simd2009v2.rds")) %>%
  select(pc7, simd2009v2_sc_quintile) %>%
  rename(postcode = pc7,
         simd = simd2009v2_sc_quintile) %>%
  mutate(year = "simd_2009")

# Combine postcode lookups into a single dataset
simd_all <- bind_rows(simd_2020, simd_2016, simd_2012, simd_2009) %>%
  mutate(simd = as.numeric(simd)) %>%
  pivot_wider(names_from = year, values_from = simd)

# Population lookups, combining both estimations and projections
pop_est  <- readRDS(paste0(plat_filepath,
  "lookups/Unicode/Populations/Estimates/",
  "HB2019_pop_est_1981_2021.rds")) %>%
  clean_names() %>%
  group_by(year, hb2019) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  mutate(hb2019 = as.character(hb2019) ) %>%
  rename(hb2014 = hb2019)

pop_proj <- readRDS(paste0(plat_filepath,
  "lookups/Unicode/Populations/Projections/",
  "HB2019_pop_proj_2018_2043.rds")) %>%
  clean_names() %>%
  filter(year >= 2022) %>%
  group_by(year, hb2019) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%  rename(hb2014 =hb2019)

# Combine population lookups into one lookup
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
  extract_start = start_date_trends_buffer,
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
                             spec            = specialty_group,
                             hospital_lookup = hospitals)

trends_data %<>%
  # Tableau uses 2014 codes, but code produces 2019
  change_hbcodes(version_to = "14")


### 3 - Save data ----

trends_data_lvl1_all_loc <- trends_data %>%
  filter((sub_grp == "All admissions" &
            (agg_label == "Hospital" | agg_label == "Board")) |
           (agg_label == "Scotland" &
              (sub_grp != "Depth of coding" & sub_grp != "Symptom coding")) |
           (agg_label == "Board" &
              (sub_grp == "Discharge" | sub_grp == "Population")))

# File used for community hospital IR
save_file(trends_data_lvl1_all_loc, "trends-data-all-loc-level1", "output", "csv",
          dev = F, overwrite = T)

# Remaining outputs should contain only the locations in locations_filter
# (specified in setup_environment using 2019 codes - but trends_data uses 2014)
locations_filter_hb14 =
  change_hbcodes(as.data.frame(locations_filter, stringsAsFactors = FALSE),
                 version_to = "14",
                 code_cols = "locations_filter") %>%
  pull(locations_filter)
trends_data_lvl1 = filter(trends_data_lvl1_all_loc,
                          location %in% locations_filter_hb14)
trends_data = filter(trends_data, location %in% locations_filter_hb14)

# File used for the Excel tables and offline dashboard
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
# Used for the offline dashboard and markdown script
save_file(trends_data, "trends-data-level2", out_folder = "output",
          type = "csv", dev = F, overwrite = F)


# Create file for RShiny public dashboard
# Reformat variables
public_dash_trends <- trends_data_lvl1 %>%
  mutate(label_short = case_when(time_period == "Quarter" ~ quarter_short,
                                 time_period == "Month" ~ month_label),
         mth_qtr = case_when(time_period == "Quarter" ~ quarter,
                             time_period == "Month" ~ month))

# Calculate the Scotland crude rate
public_dash_trends %<>%
  mutate(scot_crd_rate = case_when(sub_grp == "Place of death" ~ crd_rate,
                                   sub_grp == "Population" ~ (scot_deaths/scot_pats)*1000,
                                   TRUE ~ (scot_deaths/scot_pats)*100)) %>%
  select(hb, location, location_name, agg_label, time_period, mth_qtr, label_short,
         sub_grp, label, deaths, pats, crd_rate, scot_deaths, scot_pats,
         scot_crd_rate)

# Save into output folder
save_file(public_dash_trends, "trend_data_public_dashboard", "output", "rds", dev = F, overwrite = F)


### END OF SCRIPT ###
