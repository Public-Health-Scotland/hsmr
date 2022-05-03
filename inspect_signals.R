# Code to look at shifts, trends and outliers in the level 2 data

#TODO:
# Would be good to exclude ones reported in last quarter, could be done comparing
# files from previous publication

###############################################.
## Packages, functions, setup ----
###############################################.
source("setup_environment.R")

###############################################.
## Bringing data and creating required variables ----
###############################################.
signals_data <- read.csv(paste0(data_folder, pub_day, "/output/", pub_day, 
                        "_trends-data-level2.csv"))

signals_data %<>% 
  filter(is.na(month)) %>% #selecting quarters
  filter(!(sub_grp %in% c("Depth of Coding", "Symptom Coding"))) %>%  # we don't report on these
  select(-c(scot_deaths, scot_pats, completeness_date, month, month_label, time_period))

 # Creating mean and limits
# Following this: https://r-bar.net/xmr-control-chart-tutorial-examples/
signals_data %<>% 
  group_by(location, sub_grp, label) %>% 
  mutate(mean = mean(crd_rate), # mean - centreline
         mr = abs(crd_rate - lag(crd_rate)), # moving range - absolute difference
         mean_mr = mean(mr, na.rm = T), # mean moving range
         seq_std_dev = mean_mr/1.128, # sequential std deviation divided by constant
         low_limit = mean - seq_std_dev * 3,
         upper_limit = mean + seq_std_dev * 3) %>% 
  ungroup

###############################################.
## Identifying signals in data ----
###############################################.
# Shift - 8 or more consecutive observations above/below baseline
# Trend - 6 or more observations all increasing or decreasing
# Outlier - outside control limits

signals_data %<>% 
  group_by(location, sub_grp, label) %>%
  mutate(shift_i = case_when((crd_rate > mean & lag(crd_rate, 1) > mean &
                                lag(crd_rate, 2) > mean & lag(crd_rate, 3) > mean &
                                lag(crd_rate, 4) > mean & lag(crd_rate, 5) > mean &
                                lag(crd_rate, 6) > mean & lag(crd_rate, 7) > mean)
                             | (crd_rate < mean & lag(crd_rate, 1) < mean &
                                  lag(crd_rate, 2) < mean & lag(crd_rate, 3) < mean &
                                  lag(crd_rate, 4) < mean & lag(crd_rate, 5) < mean &
                                  lag(crd_rate, 6) < mean & lag(crd_rate, 7) < mean) 
                             ~ T , T ~ F),
         shift = case_when(shift_i == T | lead(shift_i, 1) == T | lead(shift_i, 2) == T
                           | lead(shift_i, 3) == T | lead(shift_i, 4) == T
                           | lead(shift_i, 5) == T | lead(shift_i, 6) == T
                           | lead(shift_i, 7) == T  ~ T, T ~ F),
         trend_i = case_when((crd_rate > lag(crd_rate ,1) & lag(crd_rate, 1) > lag(crd_rate, 2)
                              & lag(crd_rate, 2) > lag(crd_rate, 3)  & lag(crd_rate, 3) > lag(crd_rate, 4) &
                                lag(crd_rate, 4) > lag(crd_rate, 5)) |
                               (crd_rate < lag(crd_rate ,1) & lag(crd_rate, 1) < lag(crd_rate, 2)
                                & lag(crd_rate, 2) < lag(crd_rate, 3)  & lag(crd_rate, 3) < lag(crd_rate, 4) &
                                lag(crd_rate, 4) < lag(crd_rate, 5))
                             ~ T , T ~ F),
         trend = case_when(trend_i == T | lead(trend_i, 1) == T | lead(trend_i, 2) == T
                           | lead(trend_i, 3) == T | lead(trend_i, 4) == T | lead(trend_i, 5) == T
                           ~ T, T ~ F),
         outlier = case_when(crd_rate < low_limit | crd_rate > upper_limit ~ T, T ~ F)) %>%
  select(-shift_i, -trend_i, -mr, -mean_mr, -seq_std_dev)
  
###############################################.
## Selecting signals in last quarter ----
###############################################.

#If any signal is true in last quarter select 
signals_data %<>% 
  filter((shift == TRUE | trend == TRUE | outlier == TRUE) & quarter == 20) %>% 
  arrange(deaths) # most are 0 trends so can generally be ignored

View(signals_data)

save_file(signals_data, "XMR_signals", "output", "csv", dev = F, overwrite = F)

## END
