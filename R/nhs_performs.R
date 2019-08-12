#' @title NHS Performs
#'
#' @description \code{nhs_performs} reformats the output from \code{smr_data}
#' and \code{trend_data} in order to fit the template agreed with the team in
#' charge of NHS Performs. It adds variables \code{topic} and \code{indicator}
#' and reformats \code{period}, dropping all other variables except
#' \code{location_name} and \code{smr}/\code{crd_rat}.
#'
#' @details \href{https://www.nhsperforms.scot/}{NHS Performs} is a companion
#' website to the ISD website. It contains a collection of indicators which
#' provide information on how a selection of hospitals and NHS boards are
#' performing. HSMR and crude mortality are two indicators featured on the
#' website.
#'
#' @param data The input \code{tibble} which is the output from \code{smr_data}
#' or \code{create_trends}.
#' @param indicator controls whether the function reformats data for the trends
#' information tab or the smr information tab. Valid options are `hsmr` and
#' `crude`.
#'
#' @examples
#' nhs_performs(data = smr_data, end_date = lubridate::dmy(31032019),
#'              indicator = "hsmr")
#' nhs_performs(data = trend_data, end_date = lubridate::dmy(31032019),
#'              indicator = "crude")
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @export

nhs_performs <- function(input_data, end_date, indicator = c("hsmr", "crude")){

  match.arg(indicator)

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


  if(indicator == "hsmr"){
    if(!all(c("period", "deaths", "pred", "pats", "smr", "crd_rate",
              "location_type", "location", "hb", "location_name",
              "completeness_date", "period_label", "death_scot", "pred_scot",
              "pats_scot", "smr_scot") %in% names(input_data))) {
      stop(paste0("For NHS Performs data for HSMR, input_data object must be ",
                  "output from smr_data()"))}

    input_data %<>%
      filter(period == 3 & location %in%
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
      mutate(topic       = "HSMR",
             indicator   = "SMR",
             time_period = end_date - years(1) + days(1)) %>%
      select(topic, indicator, time_period, location, location_name, smr,
             completeness_date, period_label)

    return(input_data)


  }

  if(indicator == "crude"){
    if(!all(c("hb", "location", "location_name", "agg_label", "quarter",
              "quarter_short", "quarter_full", "sub_grp", "label",
              "scot_deaths", "scot_pats", "completeness_date", "deaths", "pats",
              "crd_rate") %in% names(input_data))) {
      stop(paste0("For NHS Performs data for Crude Rates, input_data object ",
                  "must be output from create_trends()"))}

    input_data %<>%
      filter(sub_grp == "All Admissions") %>%
      mutate(topic       = "HSMR",
             indicator   = "Crude Mortality",
             time_period = case_when(substr(quarter_short, 1, 3) == "Apr" ~
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
                                                               9, 12)))) %>%
      select(topic, indicator, quarter, location, location_name, crd_rate,
             completeness_date, quarter_short)

    return(input_data)


  }
}
