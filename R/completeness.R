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

  httr::GET(url = paste0("https://www.isdscotland.org/products-and-Services/",
                         "Data-Support-and-Monitoring/SMR-Completeness/_docs/",
                         "SMR-Estimates.xlsx"),
            httr::write_disk(tmp))

  comp <- readxl::read_xlsx(tmp, range = "B29:AF47", col_names = FALSE) %>%
    janitor::clean_names()

  comp[1,] <- t(dplyr::select(
    tidyr::fill(
      tidyr::gather(
        dplyr::slice(
          comp, 1)),
      value), 2))

  comp %<>%
    setNames(., unlist(dplyr::slice(., 1), use.names = FALSE)) %>%
    dplyr::slice(-1) %>%
    janitor::clean_names() %>%
    dplyr::select(nhs_board, smr01, dplyr::matches("^smr01_[0-9]*$"))

  colnames(comp)[-1] <- unlist(dplyr::slice(dplyr::select(comp, -nhs_board), 1),
                               use.names = FALSE)

  comp %<>%
    dplyr::slice(-1) %>%
    janitor::clean_names() %>%
    dplyr::select(nhs_board, tail(names(.), 2)) %>%
    dplyr::mutate(nhs_board = replace(nhs_board,
                                      nhs_board == "All NHS Boards",
                                      "Scotland"))

  last_month <- colnames(comp)[2:3]

  last_month <- unlist(stringr::str_split(last_month, "_"))

  last_month <- format(zoo::as.yearmon(last_month, "%b%y"), "%B %Y")

  last_month <- dplyr::last(last_month)

  if (!(hsmr::qtr_end(first_day = first_day,
                      quarter = "current") == last_month)) {
    stop("Only the first day of the current quarter can be supplied")
  }

  if (quarter == "previous") {

    h <- comp %>%
      dplyr::select(nhs_board, dplyr::last_col(offset = 1))
  }

  if (quarter == "current") {

    h <- comp %>%
      dplyr::select(nhs_board, dplyr::last_col())
  }

  if (level == "board") {

    b <- h %>%
      dplyr::slice(-dplyr::n()) %>%
      dplyr::filter_at(dplyr::vars(dplyr::last_col()),
                       dplyr::any_vars(. < 0.95)) %>%
      dplyr::mutate_at(dplyr::vars(dplyr::last_col()),
                       function(x) paste0("(",
                                          scales::percent(as.numeric(x),
                                                          accuracy = 1),
                                          ")")) %>%
      tidyr::unite(var, sep = " ") %>%
      dplyr::pull()


    if (length(b) == 0) {

      return(capture.output(
        cat("All NHS Board HSMRs are based on completeness levels of 95% and",
            "above for",
            dplyr::if_else(quarter == "previous",
                           hsmr::qtr_prev(first_day = first_day),
                           hsmr::qtr(first_day = first_day,
                                     format = "long")))))
    } else {

      return(capture.output(
        cat("All NHS Board HSMRs are based on completeness levels of 95% and",
            "above for",
            dplyr::if_else(quarter == "previous",
                           hsmr::qtr_prev(first_day = first_day),
                           hsmr::qtr(first_day = first_day,
                                     format = "long")),
            "with the exception of",
            stringi::stri_replace_last_fixed(
              stringr::str_c(sort(b), collapse = ", "),
              ", ",
              " and "))))
    }

  }

  if (level == "scotland") {

    b <- h %>%
      dplyr::slice(dplyr::n()) %>%
      dplyr::mutate_at(dplyr::vars(dplyr::last_col()),
                       function(x) scales::percent(as.numeric(x),
                                                   accuracy = 1)) %>%
      dplyr::pull(-1)

    return(b)
  }
}
