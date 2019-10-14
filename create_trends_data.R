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
# Approximate run time - xx minutes
#########################################################################


### SECTION 1 - HOUSE KEEPING ----

### 1 - Load environment file ----
source("setup_environment.R")


### 2 - Define the database connection with SMRA ----
source("odbc_connect.R")


### 3 - Read in lookup files ----

# Postcode lookups for SIMD 2016, 2012 and 2009
# These files will be combined, so create a year variable in each one, to allow
# them to be differentiated from one another
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

simd_2009 <- read_spss(paste0(plat_filepath,
  "lookups/Unicode/Deprivation/",
  "postcode_2012_2_simd2009v2.sav")) %>%
  select(PC7, simd2009v2_sc_quintile) %>%
  rename(postcode = PC7,
         simd = simd2009v2_sc_quintile) %>%
  mutate(year = "simd_2009")

# Combine postcode lookups into a single dataset
# All lookups have labelled variables, and bind_rows() drops the labels
# This produces a warning message that vectorising labelled elements may not
# preserve their attributes, which can be ignored
simd_all <- bind_rows(simd_2016, simd_2012, simd_2009) %>%
  spread(year, simd)

# Specialty Groupings lookup
spec <- read_spss(here("reference_files",
                       "discovery_spec_grps.sav"))

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

# Population lookups for 2017
pop_est  <- read_spss(paste0(plat_filepath,
  "lookups/Unicode/Populations/Estimates/",
  "HB2019_pop_est_1981_2018.sav")) %>%
  clean_names() %>%
  group_by(year, hb2019) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  mutate(hb2019 = as.character(hb2019),
         hb2019 = case_when(hb2019 == "S08000029" ~ "S08000018",
                            hb2019 == "S08000030" ~ "S08000027",
                            hb2019 == "S08000031" ~ "S08000021",
                            hb2019 == "S08000032" ~ "S08000023",
                            TRUE ~ hb2019)) %>%
  rename(hb2014 = hb2019)

pop_proj <- read_spss(paste0(plat_filepath,
  "lookups/Unicode/Populations/Projections/",
  "HB2018_pop_proj_2016_2041.sav")) %>%
  clean_names() %>%
  filter(year >= 2019) %>%
  group_by(year, hb2014) %>%
  summarise(pop = sum(pop)) %>%
  ungroup()

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


# write CSVs
write_csv(trends_data, here("data",
                                    "output",
                                    paste0(pub_date(end_date = end_date,
                                                    pub = "current"),
                                           "_trends-data-level2.csv")))

write_csv(trends_data %>%
            filter((sub_grp == "All Admissions" &
                      (agg_label == "Hospital" | agg_label == "Board")) |
                     (agg_label == "Scotland" &
                        (sub_grp != "Depth of Coding" & sub_grp != "Symptom Coding")) |
                     (agg_label == "Board" &
                        (sub_grp == "Discharge" | sub_grp == "Population")))
          ,
          here("data",
               "output",
               paste0(pub_date(end_date = end_date,
                               pub = "current"),
                      "_trends-data-level1.csv")))


### END OF SCRIPT ###
