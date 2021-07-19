#########################################################################
# Name of file - create_covid_data.R
# Data release - Quarterly HSMR publication
# Original Authors - Lucinda Lawrie
# Original Date - October 2020
#
# Type - Reproducible Analytical Pipeline
# Written/run on - RStudio server
# Version of R - 3.5.1
#
# Description - Uses SMR01 extract to calculate the proportion of hospital stays
# with a diagnosis of COVID-19 at HB and Scot level
#
# Approximate run time - xx minutes
#########################################################################


### SECTION 1 - HOUSE KEEPING ----

### 1 - Load environment file ----
source("setup_environment.R")

### 2 - COVID codes ----
covid_diag_codes <- c("U071", "U072", "U073", "U074", "U075", "U076", "U077")

### SECTION 2 - DATA MANIPULATION ----

### 1 - Read in SMR01 basefile ----
smr01 <- readRDS(paste0(data_folder, pub_day, "/base_files/",
                        pub_day, "_SMR01_basefile.rds")) %>%
  tidylog::mutate(quarter_name = paste0(year, "Q", quarter),
                  quarter = as.numeric(as.factor(quarter_name)),
                  month = lubridate::month(admission_date),
                  month_name = lubridate::month(admission_date, label = T, abbr = T),
                  month_label = paste0(month_name, " ", year),
                  diag1_4 = substr(main_condition, 1, 4),
                  diag2_4 = substr(other_condition_1, 1, 4),
                  diag3_4 = substr(other_condition_2, 1, 4),
                  diag4_4 = substr(other_condition_3, 1, 4),
                  diag5_4 = substr(other_condition_4, 1, 4),
                  diag6_4 = substr(other_condition_5, 1, 4)) %>%
  tidylog::group_by(link_no, cis_marker) %>%
  tidylog::mutate(epinum = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  tidylog::filter(year >= 2020)

### 2 - COVID extract ----
covid_extract <- smr01 %>%

  # Flag all covid episodes
  tidylog::mutate(covid = case_when(diag1_4 %in% covid_diag_codes ~ 1,
                                    diag2_4 %in% covid_diag_codes ~ 1,
                                    diag3_4 %in% covid_diag_codes ~ 1,
                                    diag4_4 %in% covid_diag_codes ~ 1,
                                    diag5_4 %in% covid_diag_codes ~ 1,
                                    diag6_4 %in% covid_diag_codes ~ 1,
                                    TRUE ~ 0)) %>%

  # Aggregate condition flag for all records per patient
  tidylog::group_by(link_no) %>%
  tidylog::mutate(covid_pat = max(covid)) %>%
  dplyr::ungroup() %>%

  # Create CIS start and end dates
  dplyr::arrange(link_no, cis_marker, admission_date, discharge_date) %>%
  tidylog::group_by(link_no, cis_marker) %>%
  tidylog::mutate(cisdoa = min(admission_date),
                  cisdodis = max(discharge_date)) %>%
  dplyr::ungroup() %>%
  tidylog::mutate(quarter_label =
                    phsmethods::qtr(lubridate::as_date(admission_date),
                                    format = "short")) %>%
  tidylog::filter(cisdoa >= dmy(01032020))%>%

  # Recode hospitals for new codes and combinations
  tidylog::mutate(location = case_when(location == "C206H" ~ "C418H",
                                       location == "G207H" ~ "G107H",
                                       location == "G306H" ~ "G405H",
                                       location == "G516H" ~ "G405H",
                                       location == "Y104H" ~ "Y146H",
                                       location == "R101H" ~ "R103H",
                                       TRUE ~ as.character(location))) %>%

  # Match on hospital names
  tidylog::left_join(hospitals, by = "location") %>%
  tidylog::mutate(location_name = case_when(location == "C418H" ~
                                              "Royal Alexandra/Vale of Leven",
                                            location == "D102H" ~
                                              "Golden Jubilee National Hospital",
                                            location == "R103H" ~
                                              "The Balfour",
                                            TRUE ~ location_name)) %>%
  rename(hosp = location, hosp_name = location_name,
         location = hbtreat_currentdate) %>%

  # Match on health board names
  tidylog::left_join(hospitals, by = "location") %>%
  rename(hb = location, hb_name = location_name) %>%

  # All hospital stays - Scotland total
  dplyr::group_by(month_label) %>%
  tidylog::mutate(scot_total = n()) %>%
  dplyr::ungroup() %>%

  # All hospital stays - HB total
  dplyr::group_by(month_label, hb) %>%
  tidylog::mutate(hb_total = n()) %>%
  dplyr::ungroup() %>%

  # All hospital stays - Hosp total
  dplyr::group_by(month_label, hb, hosp) %>%
  tidylog::mutate(hosp_total = n()) %>%
  dplyr::ungroup() %>%

  # Select only covid patients
  tidylog::filter(covid_pat == 1) %>%

  # Index event - Select the first episode with a covid diagnosis in the CIS
  tidylog::filter(covid == 1) %>%
  dplyr::group_by(link_no, cis_marker) %>%
  tidylog::mutate(covid_episode = min(epinum)) %>%
  tidylog::mutate(index_event = case_when(covid_episode == epinum ~ 1,
                                          TRUE ~ 0)) %>%
  dplyr::ungroup() %>%
  tidylog::filter(index_event == 1)

### 3 - Monthly ----

### Create Scotland-level aggregation ----

covid_scot_m <- covid_extract %>%
  tidylog::group_by(month, month_label) %>%
  tidylog::summarise(scot_covid_stays = sum(covid),
                     scot_hospital_stays = max(scot_total)) %>%
  tidylog::mutate(covid_stays = scot_covid_stays,
                  hosp_stays = scot_hospital_stays,
                  scot_crd_rate = (covid_stays/hosp_stays) * 100,
                  crd_rate = scot_crd_rate,
                  hb_name = "Scotland",
                  hosp_name = "Scotland",
                  hb_code_9 = "Scot",
                  hosp = "Scot") %>%
  dplyr::ungroup() %>%
  tidylog::select(hb_name, hosp_name, hosp, month, month_label, covid_stays, hosp_stays,
                  crd_rate, scot_covid_stays, scot_hospital_stays, scot_crd_rate, hb_code_9) %>%
  dplyr::arrange(month)

### Create HB-level aggregation ----

covid_hb_m <- covid_extract %>%
  tidylog::group_by(month, month_label, hb_name, hb) %>%
  tidylog::summarise(covid_stays = sum(covid),
                     hosp_stays = max(hb_total)) %>%
  tidylog::mutate(scot_covid_stays = 0,
                  scot_hospital_stays = 0,
                  scot_crd_rate = 0,
                  crd_rate = (covid_stays/hosp_stays) * 100,
                  hosp_name = hb_name,
                  hb_code_9 = hb,
                  hosp = hb) %>%
  dplyr::ungroup() %>%
  tidylog::select(hb_name, hosp_name, hosp, month, month_label, covid_stays, hosp_stays,
                  crd_rate, scot_covid_stays, scot_hospital_stays, scot_crd_rate, hb_code_9) %>%
  dplyr::arrange(month)

### Create hosp-level aggregation ----

covid_hosp_m <- covid_extract %>%
  tidylog::group_by(month, month_label, hb_name, hb, hosp_name, hosp) %>%
  tidylog::summarise(covid_stays = sum(covid),
                     hosp_stays = max(hosp_total)) %>%
  tidylog::mutate(scot_covid_stays = 0,
                  scot_hospital_stays = 0,
                  scot_crd_rate = 0,
                  crd_rate = (covid_stays/hosp_stays) * 100,
                  hb_code_9 = hb) %>%
  dplyr::ungroup() %>%
  tidylog::select(hb_name, hosp_name, hosp, month, month_label, covid_stays, hosp_stays,
                  crd_rate, scot_covid_stays, scot_hospital_stays, scot_crd_rate, hb_code_9) %>%
  dplyr::arrange(month)

### Combine Scot and HB data ----

covid <- covid_scot_m %>%
  rbind(covid_hb_m) %>%
  rbind(covid_hosp_m) %>%
  dplyr::group_by(month) %>%
  tidylog::mutate(scot_crd_rate = max(scot_crd_rate),
                  scot_hospital_stays = max(scot_hospital_stays),
                  scot_covid_stays = max(scot_covid_stays)) %>%
  ungroup() %>%
  tidylog::select(-month) %>%
  tidylog::rename(time_period = month_label)

### 4 - Combine with Location Template ----

# Create template so null values are 0
# Data frame with a row for each time_period
time_period_template <- data.frame(time_period = covid$time_period) %>%
  distinct(.keep_all = TRUE)

# Data frame with a row for each location
location_template <- data.frame(hb_name = covid$hb_name,
                                hb_code_9 = covid$hb_code_9,
                                hosp_name = covid$hosp_name) %>%
  distinct(.keep_all =TRUE)

# Combines above in to a data frame with a row for each combination of location
# and time_period
covid <- merge(location_template, time_period_template) %>%
  left_join(covid) %>%
  replace_na(list(covid_stays=0, hosp_stays=0, crd_rate=0)) %>%
  dplyr::group_by(time_period) %>%
  tidylog::mutate(scot_covid_stays=max(scot_covid_stays, na.rm = TRUE),
                  scot_hospital_stays=max(scot_hospital_stays, na.rm = TRUE),
                  scot_crd_rate=max(scot_crd_rate, na.rm = TRUE)) %>%
  ungroup() %>%
  na.omit() %>%
  # Required locations specified in setup_environment
  filter(hosp %in% locations_filter) %>%
  select(-hosp) %>%
  # Tableau uses 2014 codes, but code produces 2019
  change_hbcodes(version_to = "14", code_cols = "hb_code_9")

### SECTION 3 - SAVE DATA ----
# Create TDE files
# yyyy-mm-dd__COVID-data.csv – Discovery HSMR Level 2 COVID & Discovery HSMR Level 2 COVID Live
save_file(covid, "Discovery HSMR Level 2 COVID", out_folder = "tde",
          type = "xlsx", dev = F, overwrite = F)
save_file(covid, "Discovery HSMR Level 2 COVID Live", out_folder = "tde",
          type = "xlsx", dev = F, overwrite = F)

### END OF SCRIPT ###
