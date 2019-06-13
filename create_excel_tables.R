#########################################################################
# Name of file - create_excel_tables.R
# data release - Quarterly HSMR publication
# Original Authors - David Caldwell
# Orginal Date - June 2019
#
# Type - data extraction/preparation/modelling
# Written/run on - RStudio server
# Version of R - 3.2.3
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
  filter(location %in%
           c("C206H", "C418H", "Y104H", "Y146H", "N101H", "A101H", "R101H",
             "H212H", "B120H", "H103H", "N411H", "Y146H", "V217H", "Y144H",
             "Z102H", "G107H", "D102H", "C313H", "C121H", "G306H", "T101H",
             "T202H", "G405H", "F805H", "H202H", "C418H", "S314H", "S308H",
             "G207H", "T312H", "A210H", "A111H", "L302H", "L106H", "L308H",
             "F704H", "S116H", "W107H", "G516H",
             "S08000015", "S08000016", "S08000017", "S08000018",
             "S08000019", "S08000020", "S08000021", "S08000022",
             "S08000023", "S08000024", "S08000025", "S08000026",
             "S08000027", "S08000028", "S08100001",
             "Scot")) %>%

  # Calculate funnel limits for funnel plot
  mutate(st_err = sqrt(1/pred),
         uwl = 1 + 1.96 * st_err,
         ucl = 1 + 2.99 * st_err,
         lwl = 1 - 1.96 * st_err,
         lcl = 1 - 2.99 * st_err)

### SECTION 2 - CREATE TABLES ----

# Read in trend data
trend_data <- read_csv(here("data",
                            "output",
                            paste0(pub_date(end_date = end_date,
                                            pub = "current"),
                                   "_trends-data.csv")))

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
writeData(table2, "table_data", trend_data, startCol = 2)

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
writeData(table3, "data", trend_data, startCol = 2)

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
