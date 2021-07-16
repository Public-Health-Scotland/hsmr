#' @title Hospital Intelligence Dashboard.
#'
#' @description \code{hsmr_hid} reformats the output from \code{smr_data}
#' and \code{trend_data} in order to fit the template agreed for HID.
#' It adds variables \code{scot_ratio}, \code{Topic}, \code{Indicator}
#' and \code{Frequency}; and reformats \code{location}, \code{hb}.
#' Dropping all other variables except the following which are renamed:
#' \code{hb}, \code{location_name}, \code{deaths}, \code{pred}, \code{pats},
#' \code{smr}, \code{crd_rate}, \code{death_scot}, \code{scot_deaths},
#' \code{pred_scot}, \code{scot_pats} and \code{st_err}.
#'
#' @details \href{https://www.isdscotland.org/Health-Topics/Quality-Indicators/
#' Hospital-Scorecard//}{Hospital Intelligence Dashboard} is a management
#' information system to be interpreted and used nationally by the Scottish
#' Government and locally by NHS Boards and clinicians. It allows comparison
#' of each NHS Boards' performance against the national average, and provides
#' trend analysis to help monitor variation over time for each indicator where
#' appropriate. The data are presented by NHS Board and hospital of treatment.
#'
#' @param data The input \code{tibble} which is the output from \code{smr_data}
#' or \code{create_trends}.
#'
#' @return If the class is not initiated correctly, nothing is returned.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @export

hsmr_hid <- function(smr_data, trend_data, end_date){

  ### 1 - Error handling ----
  if (class(end_date) != "Date") {
    stop("The extract end date must be provided in date format")
  }

  if(!(format(end_date, "%d %B") %in% c("31 March",
                                        "30 June",
                                        "30 September",
                                        "31 December"))) {
    stop("The extract end date must be the final day of either March, June, ",
         "September or December")
  }

  ### 2 - Reformat SMR Data ----
  if(!all(c("deaths", "pred", "pats", "smr", "crd_rate",
              "location", "hb", "location_name",
              "period_label", "death_scot", "pred_scot",
              "pats_scot", "smr_scot") %in% names(smr_data))) {
      stop(paste0("For HID data for HSMR, input_data object must be ",
                  "output from smr_data()"))}

    hsmr_data<-smr_data %<>%
      tidylog::filter(period == 3 & location %in%
               c("C206H", "C418H", "Y104H", "Y146H", "N101H", "A101H", "R101H",
                 "H212H", "B120H", "H103H", "N411H", "Y146H", "V217H", "Y144H",
                 "Z102H", "G107H", "D102H", "C313H", "C121H", "G306H", "T101H",
                 "T202H", "G405H", "F805H", "H202H", "C418H", "S314H", "S308H",
                 "G207H", "T312H", "A210H", "A111H", "L302H", "L106H", "L308H",
                 "F704H", "S116H", "W107H", "G516H",
                 "S08000015", "S08000016", "S08000017", "S08000029",
                 "S08000019", "S08000020", "S08000031", "S08000022",
                 "S08000032", "S08000024", "S08000025", "S08000026",
                 "S08000030", "S08000028", "S08100001",
                 "Scot")) %>%
      tidylog::mutate(
            hosp_code = dplyr::case_when(
                location == "S08000015" ~ "A",
                location == "S08000016" ~ "B",
                location == "S08000017" ~ "Y",
                location == "S08000029" ~ "F",
                location == "S08000019" ~ "V",
                location == "S08000020" ~ "N",
                location == "S08000031" ~ "G",
                location == "S08000022" ~ "H",
                location == "S08000032" ~ "L",
                location == "S08000024" ~ "S",
                location == "S08000025" ~ "R",
                location == "S08000026" ~ "Z",
                location == "S08000030" ~ "T",
                location == "S08000028" ~ "W",
                location == "S08100001" ~ "D",
                TRUE ~ as.character(location)),
            HBName = dplyr::case_when(
                 hb == "S08000015" ~ "Ayrshire & Arran",
                 hb == "S08000016" ~ "Borders",
                 hb == "S08000017" ~ "Dumfries & Galloway",
                 hb == "S08000029" ~ "Fife",
                 hb == "S08000019" ~ "Forth Valley",
                 hb == "S08000020" ~ "Grampian",
                 hb == "S08000031" ~ "Greater Glasgow & Clyde",
                 hb == "S08000022" ~ "Highland",
                 hb == "S08000032" ~ "Lanarkshire",
                 hb == "S08000024" ~ "Lothian",
                 hb == "S08000025" ~ "Orkney",
                 hb == "S08000026" ~ "Shetland",
                 hb == "S08000030" ~ "Tayside",
                 hb == "S08000028" ~ "Western Isles",
                 hb == "S08100001" ~ "Golden Jubilee",
                 hb == "Scotland" ~ "Scotland"),
            HealthBoard = dplyr::case_when(
                 hb == "S08000015" ~ "NHS Ayrshire & Arran",
                 hb == "S08000016" ~ "NHS Borders",
                 hb == "S08000017" ~ "NHS Dumfries & Galloway",
                 hb == "S08000029" ~ "NHS Fife",
                 hb == "S08000019" ~ "NHS Forth Valley",
                 hb == "S08000020" ~ "NHS Grampian",
                 hb == "S08000031" ~ "NHS Greater Glasgow & Clyde",
                 hb == "S08000022" ~ "NHS Highland",
                 hb == "S08000032" ~ "NHS Lanarkshire",
                 hb == "S08000024" ~ "NHS Lothian",
                 hb == "S08000025" ~ "NHS Orkney",
                 hb == "S08000026" ~ "NHS Shetland",
                 hb == "S08000030" ~ "NHS Tayside",
                 hb == "S08000028" ~ "NHS Western Isles",
                 hb == "S08100001" ~ "Golden Jubilee",
                 hb == "Scotland" ~ "Scotland"),
             scot_ratio  = 1,
             stdev = as.character(round(st_err, digits=8)),
             Topic       = "HSMR",
             Indicator   = "HSMR",
            # Dealing with cases when month is one or two digits
             date_label = case_when(lubridate::month(end_date) - 2 == 1 ~
                                     paste("01", paste0("0", lubridate::month(end_date) - 2),
                                           lubridate::year(end_date), sep = "/"),
                                   T ~ paste("01", lubridate::month(end_date) - 2,
                                             lubridate::year(end_date), sep = "/")),
             Frequency = "Quarterly") %>%
      tidylog::select(hosp_code, hb, location_name, HBName, HealthBoard,
             deaths, pred, smr, death_scot, pred_scot,
             scot_ratio, stdev, Topic, Indicator, date_label, Frequency)%>%
      dplyr::rename(hb_code=hb, Location=location_name, numerator=deaths,
             denominator=pred,ratio=smr,scot_num=death_scot,
             scot_denom=pred_scot)

 ### 3 - Reformat Crude data ----
 if(!all(c("hb", "location", "location_name", "agg_label", "quarter",
                  "quarter_short", "quarter_full", "sub_grp", "label",
                  "scot_deaths", "scot_pats", "deaths", "pats",
                  "crd_rate") %in% names(trend_data))) {
    stop(paste0("For NHS Performs data for Crude Rates, input_data object ",
                      "must be output from create_trends()"))}

    crude_data<-trend_data %<>%
        tidylog::filter(sub_grp == "All Admissions") %>%
        tidylog::mutate(
            hosp_code = dplyr::case_when(
              location == "S08000015" ~ "A",
              location == "S08000016" ~ "B",
              location == "S08000017" ~ "Y",
              location == "S08000029" ~ "F",
              location == "S08000019" ~ "V",
              location == "S08000020" ~ "N",
              location == "S08000031" ~ "G",
              location == "S08000022" ~ "H",
              location == "S08000032" ~ "L",
              location == "S08000024" ~ "S",
              location == "S08000025" ~ "R",
              location == "S08000026" ~ "Z",
              location == "S08000030" ~ "T",
              location == "S08000028" ~ "W",
              location == "S08100001" ~ "D",
              TRUE ~ as.character(location)),
            HBName = dplyr::case_when(
              hb == "S08000015" ~ "Ayrshire & Arran",
              hb == "S08000016" ~ "Borders",
              hb == "S08000017" ~ "Dumfries & Galloway",
              hb == "S08000029" ~ "Fife",
              hb == "S08000019" ~ "Forth Valley",
              hb == "S08000020" ~ "Grampian",
              hb == "S08000031" ~ "Greater Glasgow & Clyde",
              hb == "S08000022" ~ "Highland",
              hb == "S08000032" ~ "Lanarkshire",
              hb == "S08000024" ~ "Lothian",
              hb == "S08000025" ~ "Orkney",
              hb == "S08000026" ~ "Shetland",
              hb == "S08000030" ~ "Tayside",
              hb == "S08000028" ~ "Western Isles",
              hb == "S08100001" ~ "Golden Jubilee",
              hb == "Scotland" ~ "Scotland"),
            HealthBoard = dplyr::case_when(
              hb == "S08000015" ~ "NHS Ayrshire & Arran",
              hb == "S08000016" ~ "NHS Borders",
              hb == "S08000017" ~ "NHS Dumfries & Galloway",
              hb == "S08000029" ~ "NHS Fife",
              hb == "S08000019" ~ "NHS Forth Valley",
              hb == "S08000020" ~ "NHS Grampian",
              hb == "S08000031" ~ "NHS Greater Glasgow & Clyde",
              hb == "S08000022" ~ "NHS Highland",
              hb == "S08000032" ~ "NHS Lanarkshire",
              hb == "S08000024" ~ "NHS Lothian",
              hb == "S08000025" ~ "NHS Orkney",
              hb == "S08000026" ~ "NHS Shetland",
              hb == "S08000030" ~ "NHS Tayside",
              hb == "S08000028" ~ "NHS Western Isles",
              hb == "S08100001" ~ "Golden Jubilee",
              hb == "Scotland" ~ "Scotland"),
            scot_ratio  = (scot_deaths/scot_pats)*100,
            stdev = " ",
            Topic       = "HSMR",
            Indicator   = "Crude Mortality rates within 30-days",
            date_label = dplyr::case_when(substr(quarter_short, 1, 3) == "Apr" ~
                                     paste0("01/04/", substr(quarter_short,
                                                             9, 12)),
                                   substr(quarter_short, 1, 3) == "Jul" ~
                                     paste0("01/07/", substr(quarter_short,
                                                             9, 12)),
                                   substr(quarter_short, 1, 3) == "Oct" ~
                                     paste0("01/10/", substr(quarter_short,
                                                             9, 12)),
                                   substr(quarter_short, 1, 3) == "Jan" ~
                                     paste0("01/01/", substr(quarter_short,
                                                             9, 12))),
            Frequency = "Quarterly") %>%
          tidylog::select(hosp_code, hb, location_name, HBName, HealthBoard,
                 deaths, pats, crd_rate, scot_deaths, scot_pats,
                 scot_ratio, stdev, Topic, Indicator, date_label, Frequency)%>%
          dplyr::rename(hb_code=hb, Location=location_name, numerator=deaths,
                denominator=pats,ratio=crd_rate,scot_num=scot_deaths,
                scot_denom=scot_pats)

    #4 - Combine smr and crude data into one file
    hid_data<-dplyr::bind_rows(hsmr_data, crude_data)

    return(hid_data)
}

