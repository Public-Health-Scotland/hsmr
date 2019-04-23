#' @title Extract SMR data for long term trends using SQL
#'
#' @description
#'
#' The long term trend query functions take an extract start date and, in the
#' case of \code{query_smr01_ltt}, an extract end date. They define the SQL
#' queries used to extract data for long term trends as part of the HSMR
#' publication.
#'
#' \itemize{
#' \item \code{query_smr01_ltt} extracts the relevant data from SMR01 from the
#' beginning of the baseline period for the long term trend data until the end
#' date cut off for the current publication.
#'
#' \item \code{query_gro_ltt} extracts data from SMR Deaths from the beginning
#' of the baseline period for the long term trend data.
#' }
#'
#' @param extract_start The extract start date, supplied with \code{Date}
#' class. Must be the first day of either January, April, July or October.
#' @param extract_end The extract end date, supplied with \code{Date} class.
#' Must be the final day of either March, June, September or December.
