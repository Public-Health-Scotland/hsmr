library(magrittr)

completeness <- function(quarter = c("previous", "current"),
                         level = c("board", "scotland")) {

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

  if(quarter == "previous") {

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
                       dplyr::any_vars(. < 0.99)) %>%
      dplyr::mutate_at(dplyr::vars(dplyr::last_col()),
                       function(x) paste0("(",
                                          scales::percent(as.numeric(x),
                                                          accuracy = 1),
                                          ")")) %>%
      tidyr::unite(var, sep = " ") %>%
      dplyr::pull()

    if(length(b) == 0) {

      return(paste0("All NHS Board HSMRs are based on completeness levels of",
                    "95% and above for October-December2018"))
    }

    return(pander::pandoc.list(b, style = 'bullet', add.end.of.list = FALSE))
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
