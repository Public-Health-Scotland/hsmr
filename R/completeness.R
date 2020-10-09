#' @title SMR01 completeness
#'
#' @description \code{completeness} takes the first day of the latest quarter
#' of data which are included in the current HSMR publication. It extracts
#' completeness information for the SMR01 dataset from the
#' \href{https://www.isdscotland.org/Products-and-Services/Data-Support-and-Monitoring/SMR-Completeness/_docs/SMR_Estimates.xlsx}{SMR Completeness Estimates spreadsheet}
#' published on the
#' \href{https://www.isdscotland.org/}{ISD website}. It extracts the
#' completeness percentages for the two most recent quarters for all health
#' boards and Scotland.
#'
#' @details The \code{completeness} function assumes that the list of health
#' board names (including Scotland) is contained in cells B31:B47 in the
#' spreadsheet, where B29 is `NHS Board` and B47 is `All NHS Boards`. Should
#' this change, \code{completeness} should be edited accordingly.
#'
#' The URL of the spreadsheet is occasionally modified. Should this happen,
#' \code{completeness} should again be edited accordingly.
#'
#' If the ISD website is down, \code{completeness} will return an error saying
#' it has timed out. The \code{hsmr} package will also fail to build when the
#' website is down, and the unit tests relating to \code{completeness} will
#' fail, as access to the spreadsheet is required. Nothing can be done in this
#' instance other than waiting for the website to return to normal.
#'
#' @param quarter A \code{character} string specifying the quarter for which
#' completeness data should be returned. Valid options are `previous` and
#' `current`.
#' @param level A \code{character} string specifying the geographic level for
#' which completeness data should be returned. Valid options are `board` and
#' `scotland`.
#' @param first_day The first day of the latest quarter, supplied with
#' \code{Date} class. Must be the first day of either January, April, July or
#' October. Only one value of \code{first_day} is valid at any one time. When
#' the spreadsheet is updated with a new quarter's data, the previously valid
#' value of \code{first_day} will now return an error, and a new value (the
#' previous value + 3 months) should be supplied instead.
#'
#' @return When \code{level} is set equal to \code{board}, \code{completeness}
#' returns a sentence listing the boards (if any) which had an SMR01
#' completeness level of less than 95 percent for the specified \code{quarter}.
#' When \code{level} is set equal to \code{scotland}, \code{completeness}
#' returns Scotland's SMR01 completeness level for the specified \code{quarter}
#' as a percentage.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @export

completeness <- function(quarter = c("previous", "current"),
                         level = c("board", "scotland"),
                         first_day) {

  if (class(first_day) != "Date") {
    stop("The first day of the quarter must be provided in date format")
  }

  if(!(format(first_day, "%d %B") %in% c("01 January",
                                         "01 April",
                                         "01 July",
                                         "01 October"))) {
    stop("The beginning of a quarter must be the first day of either January, ",
         "April, September or December")
  }

  quarter <- match.arg(quarter)
  level <- match.arg(level)

  tmp <- tempfile(fileext = ".xlsx")

  # Note that this file can occasionally change when a new quarter's data is
  # added, which will cause the function to error if not updated
  httr::GET(url = paste0("https://www.isdscotland.org/products-and-Services/",
                         "Data-Support-and-Monitoring/SMR-Completeness/_docs/",
                         "SMR_Estimates.xlsx"),
            httr::write_disk(tmp))

  # Note that the range is based on B29 being "NHS Board" and B47 being "All
  # NHS Boards" - if these shift up or down slightly when the file is modified
  # then the range will need to be updated accordingly for the function to work
  # as intended
  comp <- suppressMessages(
    readxl::read_xlsx(tmp, range = "B29:AF47", col_names = FALSE)) %>%
    janitor::clean_names() %>%
    tibble::as_tibble()

  # The above step parses the file with the dataset names in the first row,
  # however it only adds the dataset name to the column containing the first
  # quarter of data, and gives all subsuquent quarters NA
  #
  # This step replaces those NAs in the first row with the nearest non-NA value
  # to the left, which should be the name of the relevant dataset
  comp[1,] <- tibble::as_tibble(t(dplyr::select(
    tidyr::fill(
      tidyr::gather(
        dplyr::slice(
          comp, 1)),
      value), 2)))[1,]


  # Set the dataset names as the column names
  comp %<>%
    setNames(., unlist(dplyr::slice(., 1), use.names = FALSE)) %>%
    janitor::clean_names() %>%

    # Subsequently select only the columns pertaining to the name of the board
    # and the SMR01 dataset
    # The regex is needed to differentiate SMR01 from SMR01 GLS
    dplyr::select(nhs_board, smr01, dplyr::matches("^smr01_[0-9]$")) %>%
    dplyr::slice(-1)


  # Now set the column names (with the exception of the one pertaining to the
  # name of the board) to the relevant quarter
  colnames(comp)[-1] <- unlist(dplyr::slice(dplyr::select(comp, -nhs_board), 1),
                               use.names = FALSE)

  comp %<>%
    dplyr::slice(-1) %>%
    janitor::clean_names() %>%

    # Select only the two most recent quarters
    dplyr::select(nhs_board, tail(names(.), 2)) %>%
    dplyr::mutate(nhs_board = replace(nhs_board,
                                      nhs_board == "All NHS Boards",
                                      "Scotland"))

  # Calculate the final month in the most recent quarter, to ensure that the
  # function can only be used on the two most recent quarters
  last_month <- colnames(comp)[2:3]
  last_month <- unlist(stringr::str_split(last_month, "_"))
  last_month <- format(zoo::as.yearmon(last_month, "%b%y"), "%B %Y")
  last_month <- dplyr::last(last_month)

  if (hsmr::qtr_end(first_day = first_day,
                    quarter = "current") != last_month) {
    stop("Only the first day of the current quarter can be supplied")
  }

  if (quarter == "previous") {

    comp %<>%
      dplyr::select(nhs_board, dplyr::last_col(offset = 1))
  }

  if (quarter == "current") {

    comp %<>%
      dplyr::select(nhs_board, dplyr::last_col())
  }

  if (level == "board") {

    comp %<>%

      # Remove the row pertaining to Scotland level data
      dplyr::slice(-dplyr::n()) %>%

      # Filter boards which had a completeness level < 95%
      dplyr::filter_at(dplyr::vars(dplyr::last_col()),
                       dplyr::any_vars(. < 0.95)) %>%

      # Present SMR01 completeness as a percentage
      dplyr::mutate_at(dplyr::vars(dplyr::last_col()),
                       function(x) paste0("(",
                                          scales::percent(as.numeric(x),
                                                          accuracy = 1),
                                          ")")) %>%
      tidyr::unite(var, sep = " ") %>%
      dplyr::pull()

    if (length(comp) == 0) {

      # If all boards have >= 95% completeness, return a sentence saying that
      return(capture.output(
        cat("All NHS Board HSMRs are based on completeness levels of 95% and",
            "above for",
            dplyr::if_else(quarter == "previous",
                           hsmr::qtr_prev(first_day = first_day),
                           hsmr::qtr(first_day = first_day,
                                     format = "long")))))
    } else {

      # If >= 1 board has < 95% completeness, return a sentence which lists
      # those boards accompanied by their completeness percentage in brackets
      return(capture.output(
        cat("All NHS Board HSMRs are based on completeness levels of 95% and",
            "above for",
            dplyr::if_else(quarter == "previous",
                           hsmr::qtr_prev(first_day = first_day),
                           hsmr::qtr(first_day = first_day,
                                     format = "long")),
            "with the exception of",
            glue::glue_collapse(sort(comp), sep = ", ", last = " and "))))
    }
  }

  if (level == "scotland") {

    comp %<>%

      # Select only the row pertaining to Scotland level data
      dplyr::slice(dplyr::n()) %>%

      # Present Scotland's SMR01 completeness as a percentage
      dplyr::mutate_at(dplyr::vars(dplyr::last_col()),
                       function(x) scales::percent(as.numeric(x),
                                                   accuracy = 1)) %>%
      dplyr::pull(-1)

    return(comp)
  }
}
