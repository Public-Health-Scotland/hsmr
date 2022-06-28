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
writeData(table2, "Raw Data", trend_data %>%
            filter(!(sub_grp %in% c("Discharge", "Population"))),
          startCol = 2)

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

save_file(hid_data, "QHSMR_HID", "output", "csv", dev = F, overwrite = F)

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
create_open_data(measure = "crude", split = "Admission Type",
                 filename = "admissions_type_open_data", label_var = "AdmissionType",
                 dev = od_dev, overwrite = od_over)

# Age Group

create_open_data(measure = "crude", split = "Age Group",
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

### END OF SCRIPT ###

# Load metadata template
metadata <- loadWorkbook(here("reference_files",
                            "metadata.xlsx"))
