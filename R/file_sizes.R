file_sizes <- function(filepath = here::here("data", "output")) {

  x <- dir(path = filepath,
           pattern = "^\\d{4}-\\d{2}-\\d{2}(T|t)able[a-zA-Z0-9\\_\\-]*\\.xlsx$")

  x %>%
    purrr::map(~file.info(paste0(filepath, "/", .))$size) %>%
    unlist() %>%
    gdata::humanReadable(standard = "IEC", digits = 0) %>%
    gsub("i", "", .) %>%
    trimws() %>%
    paste0("Excel ", .)
}



