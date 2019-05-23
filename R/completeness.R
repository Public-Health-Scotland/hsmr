#' @importFrom dplyr %>%
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
    janitor::clean_names()

  # The above step parses the file with the dataset names in the first row,
  # however it only adds the dataset name to the column containing the first
  # quarter of data, and gives all subsuquent quarters NA
  #
  # This step replace those NAs in the first row with the nearest non-NA value
  # to the left, which should be the name of the relevant dataset
  comp[1,] <- t(dplyr::select(
    tidyr::fill(
      tidyr::gather(
        dplyr::slice(
          comp, 1)),
      value), 2))

  # Set the dataset names as the column names
  comp %<>%
    setNames(., unlist(dplyr::slice(., 1), use.names = FALSE)) %>%
    dplyr::slice(-1) %>%
    janitor::clean_names() %>%

    # Subsequently select only the columns pertaining to the name of the board
    # and the SMR01 dataset
    # The regex is needed to differentiate SMR01 from SMR01 GLS
    dplyr::select(nhs_board, smr01, dplyr::matches("^smr01_[0-9]*$"))

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
            stringi::stri_replace_last_fixed(
              stringr::str_c(sort(comp), collapse = ", "),
              ", ",
              " and "))))
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
