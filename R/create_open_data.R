#' @title Open Data
#'
#' @description Creates csv files used by the open data team to populate  
#' open data website.
#'
#' @details \code{create_open_data} expects a \code{tibble} produced by the 
#' \code{create_smr_data} script and a \code{tibble} produced by the
#' \code{create_trends_data} script.
#'
#' @param type A character string specifying whether the data used is to be
#' HSMR or crude trend data. Valid options are 'smr' and 'crude'.
#' @param split A character string specifying the subgroup to be used to split 
#' the crude data files. Default value is NULL.
#' @param location A character string specifying whether the resulting tibble
#' should include data for hospitals or Health Boards & Scotland. Default value
#' is NULL. If not NULL, valid options are 'hb' and 'hosp'.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @export


create_open_data <- function(smr = NULL,
                             type = c("smr", "crude"),
                             split = NULL,
                             location = NULL){
  
##### SMR data ----
  
  if (type == "smr") {
  
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
        
    return(smr)
      
  # SMR data - Hospital

  } else if (location == "hosp"){
    
    smr %<>%
      dplyr::filter(LocationType == "hospital") %>% 
      select("TimePeriod", "LocationCode", "ObservedNumberOfDeaths",
             "ObservedNumberOfDeathsQF", "PredictedNumberOfDeaths",
             "PredictedNumberOfDeathsQF", "NumberOfPatients",
             "NumberOfPatientsQF",	"SMR",	"CrudeRate")
    
    return(smr)
    
  } 
  
  }

#### Crude data ----
  
  if (type == "crude") {
  
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
    dplyr::select("TimePeriod", "LocationCode", "Label",
                  "NumberOfDeaths",	"NumberOfDeathsQF",
                  "NumberOfPatients",	"NumberOfPatientsQF",	"CrudeRate",
                  "agg_label", "sub_grp")
    
  
  # Crude - All admissions - Scotland & HB
    
  if(location == "hb" && !(is.null(location))){

    trend %<>%
      filter(sub_grp == "All Admissions",
             agg_label %in% c("Board", "Scotland")) %>%
      dplyr::rename(HBT = LocationCode,
                    SubGroup = Label) %>%
      dplyr::select("TimePeriod", "HBT", "SubGroup",
                    "NumberOfDeaths",	"NumberOfDeathsQF",
                    "NumberOfPatients",	"NumberOfPatientsQF",	"CrudeRate")

    return(trend)

    # Crude - All admissions - Hospital
    
  } else if(location == "hosp" && !(is.null(location))){
    
    trend %<>%
      filter(sub_grp == "All Admissions",
             agg_label == "Hospital") %>%
      dplyr::rename(SubGroup = Label) %>%
      dplyr::select("TimePeriod", "LocationCode", "SubGroup",
                    "NumberOfDeaths",	"NumberOfDeathsQF",
                    "NumberOfPatients",	"NumberOfPatientsQF",	"CrudeRate")
    
    return(trend)
    
  # Crude - Place of Death
    
  } else if (split == "Place of Death"){
    
    trend %<>%
      filter(sub_grp == split) %>%
      dplyr::rename(Country = LocationCode,
                    PlaceOfDeath = Label) %>%
      dplyr::group_by(TimePeriod, Country) %>%
      dplyr::mutate(TotalNumberOfDeaths = sum(NumberOfDeaths)) %>% 
      ungroup() %>% 
      dplyr::mutate(CrudeRate = (NumberOfDeaths/TotalNumberOfDeaths)*100,
                    NumberOfDeathsQF = "",
                    TotalNumberOfDeathsQF = "") %>%
      dplyr::select("TimePeriod", "Country", "PlaceOfDeath",
                    "NumberOfDeaths",	"NumberOfDeathsQF",
                    "TotalNumberOfDeaths",	"TotalNumberOfDeathsQF",	"CrudeRate")
    
    return(trend)  
    
  # Crude - Deprivation
    
  } else if (split == "Deprivation"){
    
    trend %<>%
      filter(sub_grp == split) %>%
      dplyr::rename(Country = LocationCode,
                    SIMDQuintile = Label) %>%
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
      dplyr::select("TimePeriod", "Country", "SIMDQuintile",
                    "SIMDQuintileQF", "NumberOfDeaths",	"NumberOfDeathsQF",
                    "NumberOfPatients",	"NumberOfPatientsQF",	"CrudeRate")
    
    return(trend)
    
  # Crude - Admission Type, Age, Sex & Specialty
    
  } else if (split != "Discharge" && split != "Population"){
    
    trend %<>%
      filter(sub_grp == split) %>%
      dplyr::rename(Country = LocationCode) %>%
      dplyr::mutate(NumberOfDeathsQF = "",
                    NumberOfPatientsQF = "") %>% 
      dplyr::select("TimePeriod", "Country", "Label",
                    "NumberOfDeaths",	"NumberOfDeathsQF",
                    "NumberOfPatients",	"NumberOfPatientsQF",	"CrudeRate")
    
    return(trend)
  
  # Crude - Discharge & Population
    
  } else {
    
    trend %<>%
      filter(sub_grp == split) %>%
      dplyr::rename(HBT = LocationCode) %>%
      dplyr::select("TimePeriod", "HBT", "Label",
                    "NumberOfDeaths",	"NumberOfDeathsQF",
                    "NumberOfPatients",	"NumberOfPatientsQF",	"CrudeRate")
    
    return(trend)
    
  }
  
  }

}
