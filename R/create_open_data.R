#' @title Open Data
#'
#' @description tba.
#'
#' @details tba.
#'
#' @param data tba.
#'
#' @return tba.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @export

create_open_data <- function(smr,
                             trends,
                             type = c("smr", "crude"),
                             split = NULL,
                             time = NULL){

  if (type == "smr"){

    smr %<>%
      dplyr::filter(period == 3) %>%
      dplyr::rename(ObservedNumberOfDeaths  = deaths,
                    PredictedNumberOfDeaths = pred,
                    NumberOfPatients        = pats,
                    SMR = smr,
                    CrudeRate = crd_rate,
                    LocationCode = location,
                    HBT = hb) %>%
      dplyr::mutate(LocationCode = case_when(LocationCode == "Scot" ~ "S92000003",
                                             LocationCode == "S08100001" ~ "SB0801",
                                             TRUE ~ LocationCode),
                    HBT     = case_when(HBT == "Scotland" ~ "S92000003",
                                        HBT == "S08100001" ~ "SB0801",
                                             TRUE ~ HBT),
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
      select("TimePeriod", "HBT", "LocationCode", "ObservedNumberOfDeaths",
             "ObservedNumberOfDeathsQF", "PredictedNumberOfDeaths",
             "PredictedNumberOfDeathsQF", "NumberOfPatients",
             "NumberOfPatientsQF",	"SMR",	"CrudeRate")

    return(smr)

  }

  if(type == "crude"){

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
                    HBT     = case_when(HBT == "Scotland" ~ "S92000003",
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
