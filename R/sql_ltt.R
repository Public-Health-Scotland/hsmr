#' @title Extract Scottish Morbidity Record (SMR) data for long term trends
#' using SQL
#'
#' @description
#'
#' The long term trend query functions take an extract start date and, in the
#' case of \code{query_smr01_ltt}, an extract end date. They define the SQL
#' queries used to extract data for long term trends as part of the HSMR
#' publication.
#'
#' \itemize{
#' \item \code{query_smr01_ltt} extracts the relevant data from the SMR01
#' database from the beginning of the baseline period for the long term trend
#' data until the end date cut off for the current publication.
#'
#' \item \code{query_gro_ltt} extracts data from the SMR Deaths database from
#' the beginning of the baseline period for the long term trend data.
#' }
#'
#' @param extract_start The extract start date, supplied with \code{Date}
#' class. Must be the first day of either January, April, July or October.
#' @param extract_end The extract end date, supplied with \code{Date} class.
#' Must be the final day of either March, June, September or December.


#' @export
#' @rdname sql_ltt
query_smr01_ltt <- function(extract_start, extract_end) {

  if (class(extract_start) != "Date" | class(extract_end) != "Date") {
    stop("The extract start and end dates must both be provided in date format")
  }

  if(!(format(extract_end, "%d %B") %in% c("31 March",
                                           "30 June",
                                           "30 September",
                                           "31 December"))) {
    stop("The extract end date must be the final day of either March, June, ",
         "September or December")
  }

  paste("select LINK_NO, ADMISSION_DATE, DISCHARGE_DATE, CIS_MARKER, POSTCODE,",
        "SPECIALTY, DISCHARGE_TYPE, HBTREAT_CURRENTDATE, SEX, LOCATION,",
        "MAIN_CONDITION, OTHER_CONDITION_1, OTHER_CONDITION_2,",
        "OTHER_CONDITION_3, OTHER_CONDITION_4, OTHER_CONDITION_5,",
        "CASE WHEN ADMISSION_TYPE BETWEEN 20 AND 48 OR ADMISSION_TYPE = 18",
        "THEN '2'",
        "WHEN ADMISSION_TYPE BETWEEN 10 AND 12 OR ADMISSION_TYPE = 19",
        "THEN '1'",
        "ELSE 'NULL'",
        "END admgrp,",
        "CASE WHEN ADMISSION_TRANSFER_FROM IN",
        "('20','21','22','23','24','25','26','27','28', '29')",
        "THEN '1'",
        "WHEN ADMISSION_TRANSFER_FROM IN",
        "('60','61','62','63','64','65','66','67','68', '69', '70')",
        "THEN '2'",
        "WHEN ADMISSION_TRANSFER_FROM IN",
        "('10','11','12','13','14','15','16','17','18', '19')",
        "THEN '3'",
        "WHEN ADMISSION_TRANSFER_FROM IN",
        "('30','31','32','33','34','35','36','37','38', '39')",
        "THEN '4'",
        "WHEN ADMISSION_TRANSFER_FROM IN",
        "('50','51','52','53','54','55','56','57','58',",
        "'59','5A','5B','5C', '5D', '5E', '5F', '5G')",
        "THEN '5'",
        "WHEN ADMISSION_TRANSFER_FROM IN",
        "('40','41','42','43','44','45','46','47','48', '49','4A','4B','4C',",
        "'4D', '4E', '4F', '4G')",
        "THEN '6'",
        "ELSE 'NULL'",
        "END admfgrp,",
        "CASE WHEN SPECIALTY IN",
        "('C1','C11','C12','C13','C4','C41','C42','C5', 'C6','C7','C8','C9',",
        "'CA','CB','D3','D4','D5', 'D6','D8','F2')",
        "THEN '2'",
        "ELSE '1'",
        "END surgmed,",
        "CASE WHEN INPATIENT_DAYCASE_IDENTIFIER = 'I'",
        "THEN '1'",
        "WHEN INPATIENT_DAYCASE_IDENTIFIER = 'D'",
        "THEN '2'",
        "ELSE 'NULL'",
        "END ipdc,",
        "CASE WHEN age_in_years BETWEEN 0 AND 19 THEN '1'",
        "WHEN age_in_years BETWEEN 20 AND 39 THEN '2'",
        "WHEN age_in_years BETWEEN 40 AND 59 THEN '3'",
        "WHEN age_in_years BETWEEN 60 AND 79 THEN '4'",
        "WHEN age_in_years >= 80 THEN '5'",
        "ELSE 'NULL'",
        "END age_grp,",
        "to_char(admission_date,'Q') AS quarter,",
        "to_char(admission_date,'MM') AS month,",
        "extract(year from admission_date) AS year",
        "from SMR01_PI",
        "where ADMISSION_DATE BETWEEN",
        "TO_DATE(", shQuote(extract_start, type = "sh"),", 'yyyy-mm-dd')",
        "AND TO_DATE(", shQuote(extract_end, type = "sh"),", 'yyyy-mm-dd')",
        "ORDER BY LINK_NO, ADMISSION_DATE, RECORD_TYPE, DISCHARGE_DATE,",
        "ADMISSION, DISCHARGE, URI")
}


#' @export
#' @rdname sql_ltt
query_gro_ltt <- function(extract_start) {

  if (class(extract_start) != "Date") {
    stop("The extract start date must be provided in date format")
  }

  if(!(format(extract_start, "%d %B") %in% c("01 January",
                                             "01 April",
                                             "01 July",
                                             "01 October"))) {
    stop("The extract start date must be the first day of either January, ",
         "April, July or October")
  }

  paste("select LINK_NO, DATE_OF_DEATH, HBRES_CURRENTDATE,",
        "to_char(DATE_OF_DEATH,'Q') AS quarter,",
        "to_char(DATE_OF_DEATH,'MM') AS month,",
        "extract(year from DATE_OF_DEATH) AS year",
        "from ANALYSIS.GRO_DEATHS_C",
        "where DATE_OF_DEATH >=",
        "TO_DATE(", shQuote(extract_start, type = "sh"),", 'yyyy-mm-dd')",
        "ORDER BY LINK_NO")
}
