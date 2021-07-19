#########################################################################
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
#########################################################################


### SECTION 1 - HOUSE KEEPING ----
source("setup_environment.R")

# List of locations included in the Excel tables
locations_excel <- c('A101H', 'A111H', 'A210H', 'B120H', 'D102H', 'F704H',
                     'G107H', 'C313H', 'G405H', 'C418H', 'H212H', 'H103H', 'C121H',
                     'H202H', 'L302H', 'L106H', 'L308H', 'N101H', 'N411H', 'R103H',
                     'S314H', 'S308H', 'S116H', 'T101H', 'T202H', 'T312H', 'V217H',
                     'W107H', 'Y146H', 'Y144H', 'Z102H',
                     "S08000015", "S08000016", "S08000017", "S08000029",
                     "S08000019", "S08000020", "S08000031", "S08000022",
                     "S08000032", "S08000024", "S08000025", "S08000026",
                     "S08000030", "S08000028", "S08100001",
                     "Scot")

# Read in SMR data, filtered on latest period/reported hospitals
smr_data <- read_csv(paste0(data_folder, pub_day, "/output/",
                            pub_day, "_SMR-data.csv")) %>%
  change_hbcodes(version_to = "19") %>%  # Making sure using latest codes, maybe not needed
  filter(location %in% locations_excel & period == 3) %>%
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

### SECTION 2 - CREATE TABLES ----

# Read in trend data
trend_data <- read_csv(paste0(data_folder, pub_day, "/output/",
                              pub_day, "_trends-data-level1.csv"),
                       col_types = cols(
                         quarter = col_double(),
                         quarter_short = col_character(),
                         quarter_full = col_character()
                       )) %>%
    change_hbcodes(version_to = "19") %>%  # using latest HB codes
  filter(location %in% locations_excel & time_period == "Quarter") %>%
  select(hb, location, location_name, agg_label, quarter, quarter_short,
         quarter_full, sub_grp, label, scot_deaths,	scot_pats,
         completeness_date,	deaths,	pats,	crd_rate)

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

# Open data

# SMR data - Scotland and HB & Hospital
save_file(create_open_data(smr_data,
                           trend_data,
                           type = "smr",
                           location = "hb") %>%
          dplyr::rename(HBT = LocationCode),
          "smr_open_data_hb", out_folder = "open_data", "csv", dev = F,
          overwrite = F)

save_file(create_open_data(smr_data,
                           trend_data,
                           type = "smr",
                           location = "hosp"),
          "smr_open_data_hosp", out_folder = "open_data", "csv", dev = F,
          overwrite = F)

# All admissions - Scotland and HB & Hospital
save_file(create_open_data(smr_data,
                           trend_data,
                           type = "crude",
                           split = "All Admissions",
                           location = "hb") %>%
            dplyr::rename(Subgroup = Label,
                          HBT = LocationCode),
          "all_admissions_open_data_hb", out_folder = "open_data", "csv", dev = F,
          overwrite = F)

save_file(create_open_data(smr_data,
                           trend_data,
                           type = "crude",
                           split = "All Admissions",
                           location = "hosp") %>%
            dplyr::rename(Subgroup = Label),
          "all_admissions_open_data_hosp", out_folder = "open_data", "csv", dev = F,
          overwrite = F)

# Admission Type
save_file((create_open_data(smr_data,
                            trend_data,
                            type = "crude",
                            split = "Admission Type") %>%
             dplyr::rename(Country = LocationCode,
                           AdmissionType = Label)),
          "admissions_type_open_data", out_folder = "open_data", "csv", dev = F,
          overwrite = F)

# Age Group
save_file((create_open_data(smr_data,
                            trend_data,
                            type = "crude",
                            split = "Age Group")%>%
             dplyr::rename(Country = LocationCode,
                           AgeGroup = Label)),
          "age_group_open_data", out_folder = "open_data", "csv", dev = F,
          overwrite = F)

# Deprivation
save_file((create_open_data(smr_data,
                            trend_data,
                            type = "crude",
                            split = "Deprivation") %>%
             dplyr::rename(Country = LocationCode,
                           SIMDQuintile = Label) %>%
             dplyr::mutate(SIMDQuintile =
                             case_when(SIMDQuintile == "1 - Most Deprived" ~ "1",
                                       SIMDQuintile == "5 - Least Deprived" ~ "5",
                                       SIMDQuintile == "Unknown" ~ "",
                                       TRUE ~ SIMDQuintile),
                           SIMDQuintileQF =
                             case_when(SIMDQuintile == "" ~ ":",
                                       TRUE ~ "")) %>%
             dplyr::select("TimePeriod", "Country", "SIMDQuintile",
                           "SIMDQuintileQF", "NumberOfDeaths",	"NumberOfDeathsQF",
                           "NumberOfPatients",	"NumberOfPatientsQF",	"CrudeRate")),
          "simd_open_data", out_folder = "open_data", "csv", dev = F,
          overwrite = F)

# Discharge
save_file(create_open_data(smr_data,
                            trend_data,
                            type = "crude",
                            split = "Discharge") %>%
            dplyr::rename(Subgroup = Label),
          "discharge_open_data", out_folder = "open_data", "csv", dev = F,
          overwrite = F)

# Population
save_file(create_open_data(smr_data,
                            trend_data,
                            type = "crude",
                            split = "Population") %>%
            dplyr::rename(Subgroup = Label),
          "pop_open_data", out_folder = "open_data", "csv", dev = F,
          overwrite = F)

# Sex
save_file((create_open_data(smr_data,
                            trend_data,
                            type = "crude",
                            split = "Sex") %>%
             dplyr::rename(Country = LocationCode,
                           Sex = Label)),
          "sex_open_data", out_folder = "open_data", "csv", dev = F,
          overwrite = F)

# Specialty
save_file((create_open_data(smr_data,
                            trend_data,
                            type = "crude",
                            split = "Specialty") %>%
             dplyr::rename(Country = LocationCode,
                           Specialty = Label)),
          "spec_open_data", out_folder = "open_data", "csv", dev = F,
          overwrite = F)

### END OF SCRIPT ###
