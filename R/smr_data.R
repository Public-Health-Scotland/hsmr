


smr_data <- function(smr01, index){

  ### SECTION 5 - CREATE MINIMAL TIDY DATASET ----

  ### 1 - Create Scotland-level aggregation ----

  z_hsmr_scot <- smr01 %>%
    group_by(quarter) %>%
    summarise(deaths = sum(death30),
              pred   = sum(pred_eq),
              pats   = length(death30)) %>%
    mutate(smr           = deaths/pred,
           crd_rate      = (deaths/pats) * 100,
           location_type = "Scotland",
           location      = "Scot") %>%
    ungroup()


  ### 2 - Create Hospital-level aggregation ----

  z_hsmr_hosp <- smr01 %>%
    group_by(quarter, location) %>%
    summarise(deaths = sum(death30),
              pred   = sum(pred_eq),
              pats   = length(death30)) %>%
    mutate(smr           = deaths/pred,
           crd_rate      = (deaths/pats) * 100,
           location_type = "hospital") %>%
    ungroup() #%>%

  # TO DO: NEED TO FILTER ON PUBLISHED HOSPITALS
  # filter(location %in% )


  ### 3 - Create HB-level aggregation ----

  z_hsmr_hb <- smr01 %>%
    group_by(quarter, hbtreat_currentdate) %>%
    summarise(deaths = sum(death30),
              pred   = sum(pred_eq),
              pats   = length(death30)) %>%
    mutate(smr           = deaths/pred,
           crd_rate      = (deaths/pats) * 100,
           location_type = "NHS Board") %>%
    ungroup() %>%
    rename(location = hbtreat_currentdate)


  ### 4 - Merge dataframes and calculate regression line ----

  # Merge data and match on location name
  smr_data <- bind_rows(z_hsmr_scot, z_hsmr_hosp, z_hsmr_hb) %>%
    left_join(z_hospitals, by = "location") %>%
    drop_na(location_name) %>%

    # Create quarter variable used in linear model - every data point in the first
    # year is considered to come from one time point (baseline period)
    mutate(quarter_reg = if_else(quarter <= 12, 0, quarter - 12))

  # Run linear regression
  z_reg_line <- lm(smr ~ quarter_reg * location_name, data = smr_data)

  # Create reg variable of predicted values
  smr_data %<>%
    mutate(reg = predict(z_reg_line, ., type = "response"))

}










