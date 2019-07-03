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


# Read in SMR data, filtered on latest period/reported hospitals
smr_data          <- read_csv(here("data",
                                   "output",
                                   paste0(pub_date(end_date = end_date,
                                                   pub = "current"),
                                          "_SMR-data.csv"))) %>%
  mutate(location      = case_when(
    location == "S08000018" ~ "S08000029",
    location == "S08000027" ~ "S08000030",
    location == "S08000021" ~ "S08000031",
    location == "S08000023" ~ "S08000032",
    TRUE                    ~ location),
    hb = case_when(
      hb == "S08000018" ~ "S08000029",
      hb == "S08000027" ~ "S08000030",
      hb == "S08000021" ~ "S08000031",
      hb == "S08000023" ~ "S08000032",
      TRUE ~ hbtreat_currentdate)) %>%
  filter(location %in%
           c('A101H', 'A111H', 'A210H', 'B120H', 'D102H', 'F805H', 'F704H',
             'G107H', 'C313H', 'G405H', 'C418H', 'H212H', 'H103H', 'C121H',
             'H202H', 'L302H', 'L106H', 'L308H', 'N101H', 'N411H', 'R101H',
             'S314H', 'S308H', 'S116H', 'T101H', 'T202H', 'T312H', 'V217H',
             'W107H', 'Y146H', 'Y144H', 'Z102H',
             "S08000015", "S08000016", "S08000017", "S08000029",
             "S08000019", "S08000020", "S08000031", "S08000022",
             "S08000032", "S08000024", "S08000025", "S08000026",
             "S08000030", "S08000028", "S08100001",
             "Scot") & period == 3) %>%

  # Calculate funnel limits for funnel plot
  mutate(st_err = sqrt(1/pred),
         z = if_else(location_type == "hospital",
                             ((smr - 1)/st_err),
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
         w_score = sqrt(sum(z * z)/sum(z_flag))) %>%
  ungroup() %>%
  mutate(
         uwl = 1 + 1.96 * st_err * w_score,
         ucl = 1 + 3.09 * st_err * w_score,
         lwl = 1 - 1.96 * st_err * w_score,
         lcl = 1 - 3.09 * st_err * w_score) %>%
  select("period", "deaths", "pred", "pats", "smr", "crd_rate", "location_type",
         "hb", "location", "location_name", "completeness_date", "death_scot",
         "pred_scot", "pats_scot", "smr_scot", "st_err",  "uwl", "ucl", "lwl",
         "lcl")

### SECTION 2 - CREATE TABLES ----

# Read in trend data
trend_data <- read_csv(here("data",
                            "output",
                            paste0(pub_date(end_date = end_date,
                                            pub = "current"),
                                   "_trends-data.csv"))) %>%
  mutate(hb = case_when(
    hb == "S08000018" ~ "S08000029",
    hb == "S08000027" ~ "S08000030",
    hb == "S08000021" ~ "S08000031",
    hb == "S08000023" ~ "S08000032",
    TRUE ~ hb
  ),
  location = case_when(
    location == "S08000018" ~ "S08000029",
    location == "S08000027" ~ "S08000030",
    location == "S08000021" ~ "S08000031",
    location == "S08000023" ~ "S08000032",
    TRUE ~ location
  )) %>%
  filter(location %in%
           c('A101H', 'A111H', 'A210H', 'B120H', 'D102H', 'F805H', 'F704H',
             'G107H', 'C313H', 'G405H', 'C418H', 'H212H', 'H103H', 'C121H',
             'H202H', 'L302H', 'L106H', 'L308H', 'N101H', 'N411H', 'R101H',
             'S314H', 'S308H', 'S116H', 'T101H', 'T202H', 'T312H', 'V217H',
             'W107H', 'Y146H', 'Y144H', 'Z102H',
             "S08000015", "S08000016", "S08000017", "S08000029",
             "S08000019", "S08000020", "S08000031", "S08000022",
             "S08000032", "S08000024", "S08000025", "S08000026",
             "S08000030", "S08000028", "S08100001",
             "Scot"))

# Load Table 1 template
table1 <- loadWorkbook(here("reference_files",
                      "Table1-HSMR.xlsx"))

# Write data to data tab in Table 1
writeData(table1, "funnel_data", smr_data, startCol = 2)

# Output Table 1
saveWorkbook(table1,
             here("data",
                  "output",
                  paste0(pub_date(end_date, pub = "current"),"Table-1HSMR.xlsx")
                  ), overwrite = TRUE)

# Load in Table 2 template
table2 <- loadWorkbook(here("reference_files",
                            "Table2-Crude-Mortality-subgroups.xlsx"))

# Write data to data tab in Table 2
writeData(table2, "table_data", trend_data %>%
            filter(!(sub_grp %in% c("Discharge", "Population",
                                    "Symptom Coding", "Depth of Coding"))),
          startCol = 2)

# Output Table 2
saveWorkbook(table2,
             here("data",
                  "output",
                  paste0(pub_date(end_date,
                                  pub = "current"),
                         "Table2-Crude-Mortality-subgroups.xlsx")),
             overwrite = TRUE)

# Load in Table 3 template
table3 <- loadWorkbook(here("reference_files",
                            paste0("Table3-Crude-Mortality-population-based",
                                   "-and-30-day-from-discharge.xlsx")))

# Write data to data tab in Table 3
writeData(table3, "data", trend_data %>%
            filter(sub_grp %in% c("Discharge", "Population")), startCol = 2)

# Output Table 3
saveWorkbook(table3,
             here("data",
                  "output",
                  paste0(pub_date(end_date,
                                  pub = "current"),
                              paste0("Table3-Crude-Mortality-population-based",
                                   "-and-30-day-from-discharge.xlsx"))),
             overwrite = TRUE)

### END OF SCRIPT ###
