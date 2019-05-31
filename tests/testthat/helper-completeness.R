tmp <- tempfile(fileext = ".xlsx")
httr::GET(url = paste0("https://www.isdscotland.org/products-and-Services/",
                       "Data-Support-and-Monitoring/SMR-Completeness/_docs/",
                       "SMR_Estimates.xlsx"),
          httr::write_disk(tmp))

comp <- suppressMessages(
  readxl::read_xlsx(tmp, range = "B29:AF47", col_names = FALSE))
comp <- janitor::clean_names(comp)

comp[1,] <- t(dplyr::select(
  tidyr::fill(
    tidyr::gather(
      dplyr::slice(
        comp, 1)),
    value), 2))

comp <- setNames(comp, unlist(dplyr::slice(comp, 1), use.names = FALSE))
comp <- dplyr::slice(comp, -1)
comp <- janitor::clean_names(comp)
comp <- dplyr::select(comp, nhs_board, smr01, dplyr::matches("^smr01_[0-9]*$"))

colnames(comp)[-1] <- unlist(dplyr::slice(dplyr::select(comp, -nhs_board), 1),
                             use.names = FALSE)

comp <- dplyr::slice(comp, -1)
comp <- janitor::clean_names(comp)
comp <- dplyr::select(comp, nhs_board, tail(names(comp), 2))
comp <- dplyr::mutate(comp, nhs_board = replace(nhs_board,
                                                nhs_board == "All NHS Boards",
                                                "Scotland"))

last_month <- colnames(comp)[2:3]
last_month <- unlist(stringr::str_split(last_month, "_"))
last_month <- format(zoo::as.yearmon(last_month, "%b%y"), "%B %Y")
last_month <- last_month[3]

first_day <- lubridate::as_date(zoo::as.yearmon(last_month))
