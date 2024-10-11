# Name of file - c_statistics.R
#
# Description - Runs expensive ROC/AUC and c-statistic calculations
# for the model_assessment markdown.
#
# Note - Requires a large session (80k+) due to size of temporary objects
#
# Approximate run time - 10 mins

# 1. Housekeeping --------------------------------------------------------------

# Environment file
source(here::here("setup_environment.R"))

# Base file with predprob
data <- read.csv(paste0(data_folder,
                        pub_day,
                        "/base_files/", 
                        pub_day, 
                        "_SMR-with-predprob.csv"))

# Primary diagnosis lookup
pdg <- read.csv(paste0(here::here(),
                       "/reference_files/",
                       "diag_grps_lookup_updated.csv")) %>%
  select(pdiag_grp = DIAGNOSIS_GROUP,
         primary_diagnosis = label) %>%
  distinct()

# VIF for collinearity
vif <- readRDS(paste0(data_folder,
                      pub_day,
                      "/diagnostics/", 
                      "vif_result.rds"))

# Cooks distance
cooks <- readRDS(paste0(data_folder,
                        pub_day,
                        "/diagnostics/",
                        "cooks_result.rds"))

## 1.1 Joining lookups ---------------------------------------------------------

# Diagnosis
data <- data %>%
  left_join(pdg, by = "pdiag_grp")

# Location
data <- data %>%
  mutate(location = case_when(
    location == "C206H" ~ "C418H",
    location == "G207H" ~ "G107H",
    location == "G306H" ~ "G405H",
    location == "G516H" ~ "G405H",
    location == "Y104H" ~ "Y146H",
    location == "R101H" ~ "R103H",
    TRUE ~ location)) %>%
  left_join(hospitals) %>%
  mutate(location_name = case_when(location == "C418H" ~
                                     "Royal Alexandra/Vale of Leven",
                                   location == "S08100001" ~
                                     "Golden Jubilee",
                                   location == "D102H" ~
                                     "Golden Jubilee National Hospital",
                                   location == "R103H" ~
                                     "The Balfour",
                                   TRUE ~ location_name))

## 1.2 C-Statistic ranking lookup ----------------------------------------------

c_statistics <- data.frame(low_bound = c(0., 0.5, 0.6, 0.7, 0.8, 0.9),
                           discrimination = c("Very Poor", "Poor", "Fair",
                                              "Very Good", "Good", "Excellent"))

## 1.3 Clean up env (for space) ------------------------------------------------
rm(pdg, hospitals, specialty_group)

# 2. C-Statistic and ROC -------------------------------------------------------

## 2.1 C-Statistic by diagnosis --------------------------------------------------

# Get a dataframe of diagnosis names and codes
# with a column for % patients in this diagnosis group
# and a dummy column for C-statistic

c_diagnosis <- data %>%
  group_by(pdiag_grp, primary_diagnosis) %>%
  summarise(pats = n()) %>%
  ungroup() %>% 
  mutate(c_stat = NA,
         pcent_pats = round(100*pats/sum(pats), 3))

# Loop over all diagnosis codes
# Filter for code i
# Make sure both outcomes (1 and 0) are represented
# Get the C-statistic and save in the dummy column

for(i in 1:nrow(c_diagnosis)){                          
  x <- data %>% filter(pdiag_grp == i)              
  if(length(unique(x$death30)) == 2){               
    auc_x <- as.numeric(auc(x$death30, x$pred_eq))  
    c_diagnosis[i, 4] <- auc_x                          
  }
}

rm(auc_x)

### 2.1.1 Join to ranking ------------------------------------------------------

# Use trunc() to extract (NOT round) the 1st dec place
# and treat as the lower bound
# eg. trunc(0.87) -> 0.8

c_diagnosis <- c_diagnosis %>%
  mutate(c_trunc = trunc(c_stat*10)/10) %>%
  left_join(c_statistics,
            by = c("c_trunc" = "low_bound")) %>%
  select(-c_trunc)

## 2.2 C-Statistic by hospital ------------------------------------------------

# Get a dataframe of hospital names
# with a column for % patients at this hospital
# and a dummy column for C-statistic
# (filter for named hospitals in the publication)

c_hospital <- data %>%
  filter(location %in% hosp_filter) %>% 
  group_by(location, location_name) %>%
  summarise(pats = n()) %>%
  ungroup() %>% 
  mutate(c_stat = NA,
         pcent_pats = round(100*pats/sum(pats), 3))

# Loop over all hospitals
# Filter for hospital i
# Make sure both outcomes (1 and 0) are represented
# Get the C-statistic and save in the dummy column

for(i in 1:nrow(c_hospital)){                          
  x <- data %>% filter(location == hosp_filter[i])              
  if(length(unique(x$death30)) == 2){               
    auc_x <- as.numeric(auc(x$death30, x$pred_eq))  
    c_hospital[i, 4] <- auc_x                          
  }
}

rm(auc_x)

### 2.2.1 Join to ranking ------------------------------------------------------

# Use trunc() to extract (NOT round) the 1st dec place
# and treat as the lower bound
# eg. trunc(0.87) -> 0.8

c_hospital <- c_hospital %>%
  mutate(c_trunc = trunc(c_stat*10)/10) %>%
  left_join(c_statistics,
            by = c("c_trunc" = "low_bound")) %>%
  select(-c_trunc)

## 2.3 Whole-Scotland result ---------------------------------------------------

### 2.3.1 ROC plot -------------------------------------------------------------
roc_scot <- roc(data$death30, data$pred_eq)
auc_scot <- auc(roc_scot)

ggroc(roc_scot, 
      legacy.axes = TRUE,
      colour = '#3f3685') +
  theme_bw() +
  geom_abline(slope = 1,
              intercept = 0,
              colour = 'grey',
              linetype = 'dashed') +
  labs(title = "Overall ROC Curve for Logistic Regression Model",
       subtitle = paste0(pub_day, " Publication")) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  annotate("label", 
           label=paste0("AUC (C-Statistic) = ", round(auc_scot, 3)),
           x = 0.8, 
           y= 0.1)

ggsave(paste0(data_folder, pub_day, "/diagnostics/", "roc.png"),
       last_plot(),
       width = 7,
       height = 5)

### 2.3.2 C-statistic and ranking ----------------------------------------------

c_scot <- data.frame("location" = "Scotland",
                     "c_stat" = as.numeric(auc_scot)) %>%
          mutate("c_trunc" = trunc(c_stat*10)/10) %>%
          left_join(c_statistics,
                    by = c("c_trunc" = "low_bound")) %>%
          select(-c_trunc)

## 2.4 Save out ----------------------------------------------------------------

wb <- createWorkbook()
pdg <- addWorksheet(wb, "Diagnosis")
loc <- addWorksheet(wb, "Location")
scot <- addWorksheet(wb, "Scotland")
writeData(wb, pdg, c_diagnosis)
writeData(wb, loc, c_hospital)
writeData(wb, scot, c_scot)
saveWorkbook(wb,
             paste0(data_folder, pub_day, "/diagnostics/", "c_stats.xlsx"),
             overwrite = TRUE)

## 2.5 Clean up env (for space) ------------------------------------------------

rm(c_diagnosis, c_hospital, c_scot, c_statistics, roc_scot, x, auc_scot)

# 3. Collinearity --------------------------------------------------------------



# 4. Cooks Distance ------------------------------------------------------------
