#########################################################################.
# Name of file - create_excel_tables.R
# data release - Quarterly HSMR publication
# Original Authors - David Caldwell
# Orginal Date - June 2019
#
# Type - data extraction/preparation/modelling
# Written/run on - RStudio server
# Version of R - 3.5.1
#
# Description - Populates Excel Tables for quarterly HSMR publication
#
# Approximate run time - 1 minute
#########################################################################.


### SECTION 1 - HOUSE KEEPING ----
source("setup_environment.R")

# Read in SMR data, filtered on latest period/reported hospitals
smr_data <- read_csv(paste0(data_folder, pub_day, "/output/",
                            pub_day, "_SMR-data.csv")) %>%
  change_hbcodes(version_to = "19") %>%  # Making sure using latest codes, maybe not needed
  # Required locations specified in setup_environment
  filter(location %in% locations_filter & period == 3) %>%
  # Calculate funnel limits for funnel plot
  mutate(st_err = round_half_up(sqrt(1/round_half_up(pred, 8)), 8),
         z = if_else(location_type == "hospital",
                     round_half_up(((round_half_up(smr, 8) - 1)/round_half_up(st_err,8)), 8),
                     0)) %>%
  group_by(period) %>%
  mutate(
         z_max = max(z),
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
         w_score = round_half_up(sqrt(sum(round_half_up(z * z, 8))/sum(z_flag)),
                          8)) %>%
  ungroup() %>%
  mutate(
         uwl = 1 + 1.96 * round_half_up(st_err * w_score,8),
         ucl = 1 + 3.09 * round_half_up(st_err * w_score,8),
         lwl = 1 - 1.96 * round_half_up(st_err * w_score,8),
         lcl = 1 - 3.09 * round_half_up(st_err * w_score,8)) %>%
  select(period, deaths, pred, pats, smr, crd_rate, location_type,
         hb, location, location_name, completeness_date, death_scot,
         pred_scot, pats_scot, smr_scot, st_err,  uwl, ucl, lwl,
         lcl, period_label)

# Read in trend data
trend_data <- read_csv(paste0(data_folder, pub_day, "/output/",
                              pub_day, "_trends-data-level1.csv"),
                       col_types = cols(
                         quarter = col_double(),
                         quarter_short = col_character(),
                         quarter_full = col_character()
                       )) %>%
    change_hbcodes(version_to = "19") %>%  # using latest HB codes
  # Required locations specified in setup_environment
  filter(location %in% locations_filter & time_period == "Quarter") %>%
  select(hb, location, location_name, agg_label, quarter, quarter_short,
         quarter_full, sub_grp, label, scot_deaths,	scot_pats,
         completeness_date,	deaths,	pats,	crd_rate)

### SECTION 2 - CREATE TABLES ----

# Load Table 1 template
table1 <- loadWorkbook(here("reference_files",
                            "Table1-HSMR.xlsx"))

# Write data to data tab in Table 1
writeData(table1, "Raw Data", smr_data, startCol = 2)

# Hide lookup sheets
sheetVisibility(table1)[4:6] = FALSE

# Output Table 1
save_file(table1, "Table1-HSMR", "output", "xlsx", dev = F, overwrite = F)

# Load in Table 2 template
table2 <- loadWorkbook(here("reference_files",
                            "Table2-Crude-Mortality-subgroups.xlsx"))

# Write data to data tab in Table 2
writeData(table2, "Raw Data", 
          trend_data %>% filter(!(sub_grp %in% c("Discharge", "Population"))),
          startCol = 2)

#Preparing location lookup and writing it in table
locations <- read_csv("https://www.opendata.nhs.scot/dataset/cbd1802e-0e04-4282-88eb-d7bdcfb120f0/resource/c698f450-eeed-41a0-88f7-c1e40a568acc/download/current-hospital_flagged20211216.csv") %>% 
  janitor::clean_names() %>% 
  select(location, location_name, hb) %>% 
  mutate(hbname = phsmethods::match_area(hb),
         hbname = case_when(location == 'D102H' ~ "Golden Jubilee",
                            T ~ paste0("NHS ", hbname)),
         hb = case_when(location == 'D102H' ~ "S08100001",
                        T ~ paste0(hb)),
         hbname = gsub(" and ", " & ", hbname))

hb_list <- locations %>% #list of hbs
  mutate (location = hb, location_name = hbname) %>% distinct()

locations <- rbind(hb_list, locations) %>% arrange (hb) %>% 
  select(hbname, location_name, location) %>% 
  filter(location %in% c(hosp_filter, board_filter)) # only locations in publication

writeData(table2, "location_lookup", locations, colNames = FALSE)

# Hide lookup sheets
sheetVisibility(table2)[9:11] = FALSE

# Output Table 2
save_file(table2, "Table2-Crude-Mortality-subgroups", "output", "xlsx",
          dev = F, overwrite = F)

# Load in Table 3 template
table3 <- loadWorkbook(here("reference_files",
                            paste0("Table3-Crude-Mortality-population-based",
                                   "-and-30-day-from-discharge.xlsx")))

# Write data to data tab in Table 3
writeData(table3, "Raw Data", trend_data %>%
            filter(sub_grp %in% c("Discharge", "Population")),
          startCol = 2)

# Hide lookup sheets
sheetVisibility(table3)[4] = FALSE

# Output Table 3
save_file(table3, "Table3-Crude-Mortality-population-based-and-30-day-from-discharge",
          "output", "xlsx", dev = F, overwrite = F)

# Load in Hopsital Intelligence Dashboard file
hid_data <- hsmr_hid(smr_data, trend_data, end_date)

save_file(hid_data, "QHSMR_HID", "output", "csv", dev = F, overwrite = T)

### SECTION 3 - CREATE OPEN DATA ----

od_dev <- FALSE
od_over <- TRUE

# SMR data - Scotland & HB and Hospital
create_open_data(smr_data, measure = "smr", location = "hb",
                 filename = "smr_open_data_hb",
                 dev = od_dev, overwrite = od_over)

create_open_data(smr_data, measure = "smr", location = "hosp",
                 filename = "smr_open_data_hosp",
                 dev = od_dev, overwrite = od_over)

# All admissions - Scotland & HB and Hospital
create_open_data(measure = "crude", location = "hb",
                 filename = "all_admissions_open_data_hb",
                 dev = od_dev, overwrite = od_over)

create_open_data(measure = "crude", location = "hosp",
                 filename = "all_admissions_open_data_hosp",
                 dev = od_dev, overwrite = od_over)

# Admission Type
create_open_data(measure = "crude", split = "Admission type",
                 filename = "admissions_type_open_data", label_var = "AdmissionType",
                 dev = od_dev, overwrite = od_over)

# Age Group

create_open_data(measure = "crude", split = "Age group",
                 filename = "age_group_open_data", label_var = "AgeGroup",
                 dev = od_dev, overwrite = od_over)

# Deprivation
create_open_data(measure = "crude", split = "Deprivation",
                 filename = "simd_open_data",
                 dev = od_dev, overwrite = od_over)

# Discharge
create_open_data(measure = "crude", split = "Discharge",
                 filename = "discharge_open_data",
                 dev = od_dev, overwrite = od_over)

# Population
create_open_data(measure = "crude", split = "Population",
                 filename = "pop_open_data",
                 dev = od_dev, overwrite = od_over)

# Place of Death
create_open_data(measure = "crude", split = "Place of Death",
                 filename = "place_open_data",
                 dev = od_dev, overwrite = od_over)

# Sex
create_open_data(measure = "crude", split = "Sex",
                 filename = "sex_open_data", label_var = "Sex",
                 dev = od_dev, overwrite = od_over)

# Specialty
create_open_data(measure = "crude", split = "Specialty",
                 filename = "spec_open_data", label_var = "Specialty",
                 dev = od_dev, overwrite = od_over)

# Update metadata template for this publication
metadata <- read_csv(here("reference_files/metadata.csv")) %>%
  select(Label, `Completed fields`)

metadata %<>%
  mutate(`Completed fields` =
           case_when(Label == "Time frame of data and timeliness" ~ paste0("Hospital Standardised Mortality Ratios (HSMR) for the latest 12 month period from ", yr(end_date),
                                                                           ". Quarterly crude mortality trends from July to September 2016 to ", qtr(qtr_start) , "."),
                     Label == "Coverage" ~ paste0("Scotland, July 2016 - ", format(end_date, "%B %Y")),
                     Label == "Completeness" ~ paste0("Approximately ", completeness(quarter = "current", level = "scotland", first_day = qtr_start),
                                                      " for the latest quarter. "),
                     Label == "Description" ~ paste0("Release of HSMR at Scotland, NHS Board and Hospital levels for the period ", yr(end_date),
                                                     ". Also includes analyses of crude mortality trends over the longer term from July to September 2016 to ", qtr(qtr_start), "."),
                     TRUE ~ `Completed fields`))

save_file(metadata, "metadata", "open_data", "csv", dev = F, overwrite = F)


### END OF SCRIPT ###

