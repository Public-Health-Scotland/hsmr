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
  
  # SMR data - Scotland & HB and Hospital

    if (type == "smr" && location == "hb" && !(is.null(location))){

      smr %<>%
      dplyr::filter(period == 3,
                    location_type %in% c("NHS Board", "Scotland")) %>%
      dplyr::rename(ObservedNumberOfDeaths  = deaths,
                    PredictedNumberOfDeaths = pred,
                    NumberOfPatients        = pats,
                    SMR = smr,
                    CrudeRate = crd_rate,
                    HBT = location) %>%
      dplyr::mutate(HBT = case_when(HBT == "Scot" ~ "S92000003",
                                    HBT == "S08100001" ~ "SB0801",
                                             TRUE ~ HBT),
                    ObservedNumberOfDeathsQF = case_when(HBT ==
                                                           "S92000003" ~ "d",
                                                         TRUE ~ ""),
                    PredictedNumberOfDeathsQF = case_when(HBT ==
                                                           "S92000003" ~ "d",
                                                         TRUE ~ ""),
                    NumberOfPatientsQF = case_when(HBT ==
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
      select("TimePeriod", "HBT", "ObservedNumberOfDeaths",
             "ObservedNumberOfDeathsQF", "PredictedNumberOfDeaths",
             "PredictedNumberOfDeathsQF", "NumberOfPatients",
             "NumberOfPatientsQF",	"SMR",	"CrudeRate")

    return(smr)

  } else if (type == "smr" && location == "hosp"){
    
    smr %<>%
      dplyr::filter(period == 3,
                    location_type == "hospital") %>%
      dplyr::rename(ObservedNumberOfDeaths  = deaths,
                    PredictedNumberOfDeaths = pred,
                    NumberOfPatients        = pats,
                    SMR = smr,
                    CrudeRate = crd_rate,
                    LocationCode = location) %>%
      dplyr::mutate(ObservedNumberOfDeathsQF = case_when(LocationCode ==
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
             "NumberOfPatientsQF",	"SMR",	"CrudeRate")
    
    return(smr)
    
  } 
  
  # Crude - All admissions - Scotland & HB and Hospital
    
  if(type == "crude" && location == "hb" && !(is.null(location))){

    trend_data %<>%
      filter(sub_grp == "All Admissions",
             agg_label %in% c("Board", "Scotland")) %>%
      dplyr::rename(NumberOfDeaths   = deaths,
                    NumberOfPatients = pats,
                    CrudeRate = crd_rate,
                    HBT = location,
                    SubGroup = label) %>%
      dplyr::mutate(HBT = case_when(HBT == "Scot" ~ "S92000003",
                                             HBT == "S08100001" ~ "SB0801",
                                             TRUE ~ HBT),
                    NumberOfDeathsQF = case_when(HBT ==
                                                           "S92000003" ~ "d",
                                                         TRUE ~ ""),
                    NumberOfPatientsQF = case_when(HBT ==
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
      dplyr::select("TimePeriod", "HBT", "SubGroup",
                    "NumberOfDeaths",	"NumberOfDeathsQF",
                    "NumberOfPatients",	"NumberOfPatientsQF",	"CrudeRate")

    return(trend_data)

  } else if(type == "crude" && location == "hosp" && !(is.null(location))){
    
    trend_data %<>%
      filter(sub_grp == "All Admissions",
             agg_label == "Hospital") %>%
      dplyr::rename(NumberOfDeaths   = deaths,
                    NumberOfPatients = pats,
                    CrudeRate = crd_rate,
                    LocationCode = location,
                    SubGroup = label) %>%
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
      dplyr::select("TimePeriod", "LocationCode", "SubGroup",
                    "NumberOfDeaths",	"NumberOfDeathsQF",
                    "NumberOfPatients",	"NumberOfPatientsQF",	"CrudeRate")
    
    return(trend_data)
    
  # Crude - Place of Death
    
  } else if (type == "crude" && split == "Place of Death"){
    
    trend_data %<>%
      filter(sub_grp == split) %>%
      dplyr::rename(NumberOfDeaths   = deaths,
                    Country = location,
                    PlaceOfDeath = label) %>%
      dplyr::group_by(quarter, Country) %>%
      dplyr::mutate(TotalNumberOfDeaths = sum(NumberOfDeaths)) %>% 
      ungroup() %>% 
      dplyr::mutate(Country = case_when(Country == "Scot" ~ "S92000003",
                                        Country == "S08100001" ~ "SB0801",
                                             TRUE ~ Country),
                    CrudeRate = (NumberOfDeaths/TotalNumberOfDeaths)*100,
                    NumberOfDeathsQF = "",
                    TotalNumberOfDeathsQF = "",
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
      dplyr::select("TimePeriod", "Country", "PlaceOfDeath",
                    "NumberOfDeaths",	"NumberOfDeathsQF",
                    "TotalNumberOfDeaths",	"TotalNumberOfDeathsQF",	"CrudeRate")
    
    return(trend_data)  
    
  # Crude - Deprivation
    
  } else if (type == "crude" && split == "Deprivation"){
    
    trend_data %<>%
      filter(sub_grp == split) %>%
      dplyr::rename(NumberOfDeaths   = deaths,
                    NumberOfPatients = pats,
                    CrudeRate = crd_rate,
                    Country = location,
                    SIMDQuintile = label) %>%
      dplyr::mutate(Country = case_when(Country == "Scot" ~ "S92000003",
                                        Country == "S08100001" ~ "SB0801",
                                        TRUE ~ Country),
                    NumberOfDeathsQF = "",
                    NumberOfPatientsQF = "",
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
                    TimePeriod = paste0(T2, T1),
                    SIMDQuintile =
                      case_when(SIMDQuintile == "1 - Most Deprived" ~ "1",
                                SIMDQuintile == "5 - Least Deprived" ~ "5",
                                SIMDQuintile == "Unknown" ~ "",
                                TRUE ~ SIMDQuintile),
                    SIMDQuintileQF =
                      case_when(SIMDQuintile == "" ~ ":",
                                TRUE ~ "")) %>%
      dplyr::select("TimePeriod", "Country", "SIMDQuintile",
                    "SIMDQuintileQF", "NumberOfDeaths",	"NumberOfDeathsQF",
                    "NumberOfPatients",	"NumberOfPatientsQF",	"CrudeRate")
    
    return(trend_data)
    
  # Crude - Admission Type, Age, Sex & Specialty
    
  } else if (type == "crude" && split != "Discharge" && split != "Population"){
    
    trend_data %<>%
      filter(sub_grp == split) %>%
      dplyr::rename(NumberOfDeaths   = deaths,
                    NumberOfPatients = pats,
                    CrudeRate = crd_rate,
                    Country = location,
                    Label = label) %>%
      dplyr::mutate(Country = case_when(Country == "Scot" ~ "S92000003",
                                        Country == "S08100001" ~ "SB0801",
                                             TRUE ~ Country),
                    NumberOfDeathsQF = "",
                    NumberOfPatientsQF = "",
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
      dplyr::select("TimePeriod", "Country", "Label",
                    "NumberOfDeaths",	"NumberOfDeathsQF",
                    "NumberOfPatients",	"NumberOfPatientsQF",	"CrudeRate")
    
    return(trend_data)
  
  # Crude - Discharge & Population
    
  } else if (type == "crude"){
    
    trend_data %<>%
      filter(sub_grp == split) %>%
      dplyr::rename(NumberOfDeaths   = deaths,
                    NumberOfPatients = pats,
                    CrudeRate = crd_rate,
                    Label = label,
                    HBT = hb) %>%
      dplyr::mutate(HBT = case_when(HBT == "Scotland" ~ "S92000003",
                                    HBT == "S08100001" ~ "SB0801",
                                    TRUE ~ HBT),
                    NumberOfDeathsQF = case_when(HBT ==
                                                   "S92000003" ~ "d",
                                                 TRUE ~ ""),
                    NumberOfPatientsQF = case_when(HBT ==
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
      dplyr::select("TimePeriod", "HBT", "Label",
                    "NumberOfDeaths",	"NumberOfDeathsQF",
                    "NumberOfPatients",	"NumberOfPatientsQF",	"CrudeRate")
    
    return(trend_data)
    
  }
  
  

}
