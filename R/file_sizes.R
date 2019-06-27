#' @title Extract file sizes for HSMR Excel tables
#'
#' @description \code{file_sizes} takes the filepath to the location of the
#' HSMR Excel tables. It returns a \code{character} vector containing the size
#' of each file.
#'
#' @details There are three HSMR Excel tables: tables 1, 2 and 3. If fewer or
#' more Excel tables which follow the same naming conventions as tables 1, 2
#' and 3 are found in the filepath, the function will return an error.
#'
#' File sizes are returned in Kilobytes (KB) or Megabytes (MB). A Kilobyte is
#' taken to be 1024 bytes, and a Megabyte to be 1024 Kilobytes.
#'
#' @param filepath A \code{character} string containing the filepath to the
#' location of the HSMR Excel tables. Errors if not provided with a valid
#' filepath. Defaults to \code{here::here("data", "output")}.
#'
#' @return A \code{character} vector.
#'
#' @examples
#' file_sizes()
#' file_sizes(getwd())[1]
#'
#' @export

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
