file_sizes <- function(filepath = here::here("data", "output")) {

  if (!file.exists(filepath)) {
    stop(paste0("A valid filepath to the folder containing the HSMR Excel ",
                "tables must be supplied"))
  }

  x <- dir(path = filepath,
           pattern = "^\\d{4}-\\d{2}-\\d{2}(T|t)able[a-zA-Z0-9\\_\\-]*\\.xlsx$")

  if (length(x) != 3) {
    stop(paste0("Exactly three HSMR Excel tables should be present in the ",
                "folder: Tables 1, 2 and 3"))
  }

  x %>%
    purrr::map(~file.info(paste0(filepath, "/", .))$size) %>%
    unlist() %>%
    gdata::humanReadable(standard = "IEC", digits = 0) %>%
    gsub("i", "", .) %>%
    trimws() %>%
    paste0("Excel ", .)
}
