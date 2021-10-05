#' @title Open Data
#'
#' @description Creates csv files used by the open data team to populate  
#' open data website.
#'
#' @details \code{create_open_data} expects a \code{tibble} produced by the 
#' \code{create_smr_data} script and a \code{tibble} produced by the
#' \code{create_trends_data} script.
#' 
#' @param smr Input tibble for HSMR data.
#' @param measure A character string specifying whether the data used is to be
#' HSMR or crude trend data. Valid options are 'smr' and 'crude'.
#' @param split A character string specifying the subgroup to be used to split 
#' the crude data files. Default value is NULL.
#' @param location A character string specifying whether the resulting tibble
#' should include data for hospitals or Health Boards & Scotland. Default value
#' is NULL. If not NULL, valid options are 'hb' and 'hosp'.
#' @param label_var New name of the label variable used in a few files.
#' @param save If true it will save the file. If false it will return the object
#' @param filename dev and overwrite. They come from the save_file function
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @export


create_open_data <- function(smr = NULL,
                             measure = c("smr", "crude"),
                             split = NULL,
                             location = NULL,
                             label_var = NULL,
                             save = TRUE,
                             filename = NULL, # from the save_file function
                             dev = NULL, # from the save_file function
                             overwrite = NULL ){ # from the save_file function
  
##### SMR data ----
  
  if (measure == "smr") {
  
  smr %<>% 
    dplyr::filter(period == 3) %>% 
    dplyr::rename(ObservedNumberOfDeaths  = deaths,
                  PredictedNumberOfDeaths = pred,
                  NumberOfPatients        = pats,
                  SMR = smr,
                  CrudeRate = crd_rate,
                  LocationCode = location,
                  LocationType = location_type) %>% 
    dplyr::mutate(LocationCode = case_when(LocationCode == "Scot" ~ "S92000003",
                                           LocationCode == "S08100001" ~ "SB0801",
                                           TRUE ~ LocationCode),
                  ObservedNumberOfDeathsQF = case_when(LocationCode ==
                                                         "S92000003" ~ "d",
                                                       TRUE ~ ""),
                  PredictedNumberOfDeathsQF = case_when(LocationCode ==
                                                          "S92000003" ~ "d",
                                                        TRUE ~ ""),
                  NumberOfPatientsQF = case_when(LocationCode ==
                                                   "S92000003" ~ "d",
                                                 TRUE ~ ""),
                  T1 = stringr::str_split(period_label,
                                          " ")[[1]][1],
                  T2 = stringr::str_split(period_label,
                                          " ")[[1]][2],
                  T3 = stringr::str_split(period_label,
                                          " ")[[1]][4],
                  T4 = stringr::str_split(period_label,
                                          " ")[[1]][5],
                  T1 = recode(T1,
                              "October" = "Q4",
                              "January" = "Q1",
                              "April"   = "Q2",
                              "July"    = "Q3"),
                  T3 = recode(T3,
                              "December" = "Q4",
                              "March" = "Q1",
                              "June"   = "Q2",
                              "September"    = "Q3"),
                  TimePeriod = paste0(T2, T1, "-", T4, T3)) %>%
    select("TimePeriod", "LocationCode", "ObservedNumberOfDeaths",
           "ObservedNumberOfDeathsQF", "PredictedNumberOfDeaths",
           "PredictedNumberOfDeathsQF", "NumberOfPatients",
           "NumberOfPatientsQF",	"SMR",	"CrudeRate", "LocationType")
    
    
  # SMR data - Scotland & HB

    if (location == "hb" && !(is.null(location))){

      smr %<>%
        dplyr::filter(LocationType %in% c("NHS Board", "Scotland")) %>%
        dplyr::rename(HBT = LocationCode) %>% 
        select("TimePeriod", "HBT", "ObservedNumberOfDeaths",
               "ObservedNumberOfDeathsQF", "PredictedNumberOfDeaths",
               "PredictedNumberOfDeathsQF", "NumberOfPatients",
               "NumberOfPatientsQF",	"SMR",	"CrudeRate")
        
  # SMR data - Hospital

  } else if (location == "hosp"){
    
    smr %<>%
      dplyr::filter(LocationType == "hospital") %>% 
      select("TimePeriod", "LocationCode", "ObservedNumberOfDeaths",
             "ObservedNumberOfDeathsQF", "PredictedNumberOfDeaths",
             "PredictedNumberOfDeathsQF", "NumberOfPatients",
             "NumberOfPatientsQF",	"SMR",	"CrudeRate")
    
    
    
  } 
    
    if (save == T) {
      
      save_file(dataset = smr, filename = filename, out_folder = "open_data", 
                type = "csv", dev = od_dev, overwrite = od_over)
    } else {
      return(smr)
    }
  }

#### Crude data ----
  
  if (measure == "crude") {
  
  trend <- trend_data %>%
    dplyr::rename(NumberOfDeaths   = deaths,
                  NumberOfPatients = pats,
                  CrudeRate = crd_rate,
                  LocationCode = location,
                  Label = label) %>% 
    dplyr::mutate(LocationCode = case_when(LocationCode == "Scot" ~ "S92000003",
                                           LocationCode == "S08100001" ~ "SB0801",
                                  TRUE ~ LocationCode),
                  NumberOfDeathsQF = case_when(LocationCode ==
                                                 "S92000003" ~ "d",
                                               TRUE ~ ""),
                  NumberOfPatientsQF = case_when(LocationCode ==
                                                   "S92000003" ~ "d",
                                                 TRUE ~ ""),
                  T1 = stringr::str_split(quarter_full,
                                          " "
                                          , simplify = TRUE)[,1],
                  T2 = stringr::str_split(quarter_full,
                                          " "
                                          , simplify = TRUE)[,4],
                  T1 = recode(T1,
                              "October" = "Q4",
                              "January" = "Q1",
                              "April"   = "Q2",
                              "July"    = "Q3"),
                  TimePeriod = paste0(T2, T1)) %>%
    dplyr::select(TimePeriod, LocationCode, Label,
                  NumberOfDeaths,	NumberOfDeathsQF,
                  NumberOfPatients,	NumberOfPatientsQF,	CrudeRate,
                  agg_label, sub_grp)
    
  
  # Crude - All admissions - Scotland & HB
    
  if(location == "hb" && !(is.null(location))){

    trend %<>%
      filter(sub_grp == "All Admissions",
             agg_label %in% c("Board", "Scotland")) %>%
      dplyr::select(TimePeriod, HBT = LocationCode, SubGroup = Label,
                    NumberOfDeaths,	NumberOfDeathsQF,
                    NumberOfPatients,	NumberOfPatientsQF,	CrudeRate)

    # Crude - All admissions - Hospital
    
  } else if(location == "hosp" && !(is.null(location))){
    
    trend %<>%
      filter(sub_grp == "All Admissions",
             agg_label == "Hospital") %>%
      dplyr::select(TimePeriod, LocationCode, SubGroup = Label,
                    NumberOfDeaths,	NumberOfDeathsQF,
                    NumberOfPatients,	NumberOfPatientsQF,	CrudeRate)
    
  # Crude - Place of Death
    
  } else if (split == "Place of Death"){
    
    trend %<>%
      filter(sub_grp == split) %>%
      dplyr::group_by(TimePeriod, LocationCode) %>%
      dplyr::mutate(TotalNumberOfDeaths = sum(NumberOfDeaths)) %>% 
      ungroup() %>% 
      dplyr::mutate(CrudeRate = (NumberOfDeaths/TotalNumberOfDeaths)*100,
                    NumberOfDeathsQF = "",
                    TotalNumberOfDeathsQF = "") %>%
      dplyr::select(TimePeriod, Country = LocationCode, PlaceOfDeath = Label,
                    NumberOfDeaths,	NumberOfDeathsQF,
                    TotalNumberOfDeaths,	TotalNumberOfDeathsQF,	CrudeRate)
    
  # Crude - Deprivation
    
  } else if (split == "Deprivation"){
    
    trend %<>%
      filter(sub_grp == split) %>%
      dplyr::rename(SIMDQuintile = Label) %>%
      dplyr::mutate(SIMDQuintile =
                      case_when(SIMDQuintile == "1 - Most Deprived" ~ "1",
                                SIMDQuintile == "5 - Least Deprived" ~ "5",
                                SIMDQuintile == "Unknown" ~ "",
                                TRUE ~ SIMDQuintile),
                    SIMDQuintileQF =
                      case_when(SIMDQuintile == "" ~ ":",
                                TRUE ~ ""),
                    NumberOfDeathsQF = "",
                    NumberOfPatientsQF = "") %>% 
      dplyr::select(TimePeriod, Country = LocationCode, SIMDQuintile,
                    SIMDQuintileQF, NumberOfDeaths,	NumberOfDeathsQF,
                    NumberOfPatients,	NumberOfPatientsQF,	CrudeRate)
    
  # Crude - Admission Type, Age, Sex & Specialty
    
  } else if (split != "Discharge" && split != "Population"){
    
    trend %<>%
      filter(sub_grp == split) %>%
      dplyr::mutate(NumberOfDeathsQF = "",
                    NumberOfPatientsQF = "") %>% 
      dplyr::select(TimePeriod, Country = LocationCode, Label,
                    NumberOfDeaths,	NumberOfDeathsQF,
                    NumberOfPatients,	NumberOfPatientsQF,	CrudeRate)
    
    colnames(trend)[colnames(trend) == "Label"] <- label_var
  
  # Crude - Discharge & Population
    
  } else {
    
    trend %<>%
      filter(sub_grp == split) %>%
      dplyr::select(TimePeriod, HBT = LocationCode, SugGroup = Label,
                    NumberOfDeaths,	NumberOfDeathsQF,
                    NumberOfPatients,	NumberOfPatientsQF,	CrudeRate)
    
  }
  
  # Bringing previous file for open data and keeping only periods not included
  # in the current publication
  old_data <- read_csv(paste0(data_folder, previous_pub, "/open_data/",
                              previous_pub, "_", filename, ".csv")) %>%
    filter(!(TimePeriod %in% trend$TimePeriod))
  
  trend <- rbind(old_data, trend)
  
  if (save == T) {
    
  save_file(dataset = trend, filename = filename, out_folder = "open_data", 
            type = "csv", dev = od_dev, overwrite = od_over)
  } else {
    return(trend)
  }
  
  }
  
  

} #end of function bracket
