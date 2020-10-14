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
covid_diag_codes <- c("U071", "U072")

### 3 - Hospital names ----
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
  tibble(location = "S08000032", location_name = "NHS Lanarkshire"),
  tibble(location = "S08100001", location_name = "Golden Jubilee"))

### SECTION 2 - DATA MANIPULATION ----

### 1 - Read in SMR01 basefile ----
smr01 <- readRDS(here("data",
                    "base_files",
                    paste0(pub_date(end_date = end_date, pub = "current"),
                           "_SMR01_basefile.rds"))) %>%
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

  # Recode DGRI and Balfour
  tidylog::mutate(location = case_when(location == "R101H" ~ "R103H",
                                       location == "Y104H" ~ "Y146H",
                                       TRUE ~ as.character(location))) %>%

  # Match on hospital names
  tidylog::left_join(hospitals, by = "location") %>%
  rename(hosp = location, hosp_name = location_name,
         location = hbtreat_currentdate) %>%

  # Match on health board names
  tidylog::left_join(hospitals, by = "location") %>%
  rename(hb = location, hb_name = location_name) %>%

  # All hospital stays - Scotland total
  dplyr::group_by(quarter_label) %>%
  tidylog::mutate(scot_total = n()) %>%
  dplyr::ungroup() %>%

  # All hospital stays - HB total
  dplyr::group_by(quarter_label, hb) %>%
  tidylog::mutate(hb_total = n()) %>%
  dplyr::ungroup() %>%

  # All hospital stays - HB total
  dplyr::group_by(quarter_label, hb, hosp) %>%
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

### 3 - Quarterly ----

### Create Scotland-level aggregation ----

covid_scot <- covid_extract %>%
  tidylog::group_by(quarter,  quarter_label) %>%
  tidylog::summarise(scot_covid_stays = sum(covid),
                     scot_hospital_stays = max(scot_total)) %>%
  tidylog::mutate(covid_stays = scot_covid_stays,
                  hosp_stays = scot_hospital_stays,
                  scot_crd_rate = (covid_stays/hosp_stays) * 100,
                  crd_rate = scot_crd_rate,
                  hb_name = "Scotland") %>%
  dplyr::ungroup() %>%
  tidylog::select(hb_name, quarter, quarter_label, covid_stays, hosp_stays,
                  crd_rate, scot_covid_stays, scot_hospital_stays, scot_crd_rate) %>%
  dplyr::arrange(quarter) %>%
  dplyr::rename(location = hb_name)

### Create HB-level aggregation ----

covid_hb <- covid_extract %>%
  tidylog::group_by(quarter, quarter_label, hb_name) %>%
  tidylog::summarise(covid_stays = sum(covid),
                     hosp_stays = max(hb_total)) %>%
  tidylog::mutate(scot_covid_stays = 0,
                  scot_hospital_stays = 0,
                  scot_crd_rate = 0,
                  crd_rate = (covid_stays/hosp_stays) * 100) %>%
  dplyr::ungroup() %>%
  tidylog::select(hb_name, quarter, quarter_label, covid_stays, hosp_stays,
                  crd_rate, scot_covid_stays, scot_hospital_stays, scot_crd_rate) %>%
  dplyr::arrange(quarter) %>%
  dplyr::rename(location = hb_name)

### Create hosp-level aggregation ----

covid_hosp<- covid_extract %>%
  tidylog::group_by(quarter, quarter_label, hb_name, hosp_name) %>%
  tidylog::summarise(covid_stays = sum(covid),
                     hosp_stays = max(hosp_total)) %>%
  tidylog::mutate(scot_covid_stays = 0,
                  scot_hospital_stays = 0,
                  scot_crd_rate = 0,
                  crd_rate = (covid_stays/hosp_stays) * 100) %>%
  dplyr::ungroup() %>%
  tidylog::select(hosp_name, quarter, quarter_label, covid_stays, hosp_stays,
                  crd_rate, scot_covid_stays, scot_hospital_stays, scot_crd_rate) %>%
  dplyr::arrange(quarter) %>%
  dplyr::rename(location = hosp_name)

### Combine Scot and HB data ----

covid <- covid_scot %>%
  rbind(covid_hb) %>%
  rbind(covid_hosp) %>%
  dplyr::group_by(quarter) %>%
  tidylog::mutate(scot_crd_rate = max(scot_crd_rate),
                  scot_hospital_stays = max(scot_hospital_stays),
                  scot_covid_stays = max(scot_covid_stays)) %>%
  ungroup() %>%
  tidylog::select(-quarter) %>%
  tidylog::rename(time_period = quarter_label)

### 4 - Monthly ----

### Create Scotland-level aggregation ----

covid_scot_m <- covid_extract %>%
  tidylog::group_by(month, month_label) %>%
  tidylog::summarise(scot_covid_stays = sum(covid),
                     scot_hospital_stays = max(scot_total)) %>%
  tidylog::mutate(covid_stays = scot_covid_stays,
                  hosp_stays = scot_hospital_stays,
                  scot_crd_rate = (covid_stays/hosp_stays) * 100,
                  crd_rate = scot_crd_rate,
                  hb_name = "Scotland") %>%
  dplyr::ungroup() %>%
  tidylog::select(hb_name, month, month_label, covid_stays, hosp_stays,
                  crd_rate, scot_covid_stays, scot_hospital_stays, scot_crd_rate) %>%
  dplyr::arrange(month) %>%
  dplyr::rename(location = hb_name)

### Create HB-level aggregation ----

covid_hb_m <- covid_extract %>%
  tidylog::group_by(month, month_label, hb_name) %>%
  tidylog::summarise(covid_stays = sum(covid),
                     hosp_stays = max(hb_total)) %>%
  tidylog::mutate(scot_covid_stays = 0,
                  scot_hospital_stays = 0,
                  scot_crd_rate = 0,
                  crd_rate = (covid_stays/hosp_stays) * 100) %>%
  dplyr::ungroup() %>%
  tidylog::select(hb_name, month, month_label, covid_stays, hosp_stays,
                  crd_rate, scot_covid_stays, scot_hospital_stays, scot_crd_rate) %>%
  dplyr::arrange(month) %>%
  dplyr::rename(location = hb_name)

### Create hosp-level aggregation ----

covid_hosp_m <- covid_extract %>%
  tidylog::group_by(month, month_label, hb_name, hosp_name) %>%
  tidylog::summarise(covid_stays = sum(covid),
                     hosp_stays = max(hosp_total)) %>%
  tidylog::mutate(scot_covid_stays = 0,
                  scot_hospital_stays = 0,
                  scot_crd_rate = 0,
                  crd_rate = (covid_stays/hosp_stays) * 100) %>%
  dplyr::ungroup() %>%
  tidylog::select(hosp_name, month, month_label, covid_stays, hosp_stays,
                  crd_rate, scot_covid_stays, scot_hospital_stays, scot_crd_rate) %>%
  dplyr::arrange(month) %>%
  dplyr::rename(location = hosp_name)

### Combine Scot and HB data ----

covid_m <- covid_scot_m %>%
  rbind(covid_hb_m) %>%
  rbind(covid_hosp_m) %>%
  dplyr::group_by(month) %>%
  tidylog::mutate(scot_crd_rate = max(scot_crd_rate),
                  scot_hospital_stays = max(scot_hospital_stays),
                  scot_covid_stays = max(scot_covid_stays)) %>%
  ungroup() %>%
  tidylog::select(-month) %>%
  tidylog::rename(time_period = month_label)

### 5 - Combine Quarterly and Monthly ----

covid %<>% rbind(covid_m)

### SECTION 3 - WRITE XLSX ----

write_csv(covid, here("data",
                         "output",
                         paste0(pub_date(end_date = end_date, pub = "current"),
                                "_COVID-data.csv")))

### END OF SCRIPT ###
