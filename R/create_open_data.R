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
#' @param trends Input tibble for crude trend data.
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


create_open_data <- function(smr,
                             trends,
                             type = c("smr", "crude"),
                             split = NULL,
                             location = NULL){
  
  locations_HB <- c("S08000015", "S08000016", "S08000017", "S08000029",
                       "S08000019", "S08000020", "S08000031", "S08000022",
                       "S08000032", "S08000024", "S08000025", "S08000026",
                       "S08000030", "S08000028", "S08100001",
                       "Scot")
  
  locations_hosp <- c('A101H', 'A111H', 'A210H', 'B120H', 'D102H', 'F704H',
                        'G107H', 'C313H', 'G405H', 'C418H', 'H212H', 'H103H', 'C121H',
                        'H202H', 'L302H', 'L106H', 'L308H', 'N101H', 'N411H', 'R103H',
                        'S314H', 'S308H', 'S116H', 'T101H', 'T202H', 'T312H', 'V217H',
                        'W107H', 'Y146H', 'Y144H', 'Z102H')

    if (type == "smr" && location == "hb" && !(is.null(location))){

    smr %<>%
      dplyr::filter(period == 3,
                    location %in% locations_HB) %>%
      dplyr::rename(ObservedNumberOfDeaths  = deaths,
                    PredictedNumberOfDeaths = pred,
                    NumberOfPatients        = pats,
                    SMR = smr,
                    CrudeRate = crd_rate,
                    LocationCode = location) %>%
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
             "NumberOfPatientsQF",	"SMR",	"CrudeRate")

    return(smr)

  } else if (type == "smr" && location == "hosp"){
    
    smr %<>%
      dplyr::filter(period == 3,
                    location %in% locations_hosp) %>%
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
    
  if(type == "crude" && location == "hb" && !(is.null(location))){

    trends %<>%
      filter(sub_grp == split,
             location %in% locations_HB) %>%
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
                    "NumberOfPatients",	"NumberOfPatientsQF",	"CrudeRate")

    return(trends)

  } else if(type == "crude" && location == "hosp" && !(is.null(location))){
    
    trends %<>%
      filter(sub_grp == split,
             location %in% locations_hosp) %>%
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
                    "NumberOfPatients",	"NumberOfPatientsQF",	"CrudeRate")
    
    return(trends)
    
  } else if (type == "crude" && split != "Discharge" && split != "Population"){
    
    trends %<>%
      filter(sub_grp == split) %>%
      dplyr::rename(NumberOfDeaths   = deaths,
                    NumberOfPatients = pats,
                    CrudeRate = crd_rate,
                    LocationCode = location,
                    Label = label) %>%
      dplyr::mutate(LocationCode = case_when(LocationCode == "Scot" ~ "S92000003",
                                             LocationCode == "S08100001" ~ "SB0801",
                                             TRUE ~ LocationCode),
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
      dplyr::select("TimePeriod", "LocationCode", "Label",
                    "NumberOfDeaths",	"NumberOfDeathsQF",
                    "NumberOfPatients",	"NumberOfPatientsQF",	"CrudeRate")
    
    return(trends)
    
  } else if (type == "crude"){
    
    trends %<>%
      filter(sub_grp == split) %>%
      dplyr::rename(NumberOfDeaths   = deaths,
                    NumberOfPatients = pats,
                    CrudeRate = crd_rate,
                    LocationCode = location,
                    Label = label,
                    HBT = hb) %>%
      dplyr::mutate(LocationCode = case_when(LocationCode == "Scot" ~ "S92000003",
                                             LocationCode == "S08100001" ~ "SB0801",
                                             TRUE ~ LocationCode),
                    HBT = case_when(HBT == "Scotland" ~ "S92000003",
                                    HBT == "S08100001" ~ "SB0801",
                                    TRUE ~ HBT),
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
      dplyr::select("TimePeriod", "HBT", "LocationCode", "Label",
                    "NumberOfDeaths",	"NumberOfDeathsQF",
                    "NumberOfPatients",	"NumberOfPatientsQF",	"CrudeRate")
    
    return(trends)
    
  }
  
  

}
