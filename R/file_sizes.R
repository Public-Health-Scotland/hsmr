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
#' File sizes are returned in kilobytes (KB) or megabytes (MB). A kilobyte is
#' taken to be 1,024 bytes, and a megabyte to be 1,024 kilobytes.
#'
#' @param filepath A \code{character} string containing the filepath to the
#' location of the HSMR Excel tables. Errors if not provided with a valid
#' filepath. Defaults to \code{here::here("data", "output")}.
#'
#' @return A \code{character} vector.
#'
#' @examples
#' file_sizes()
#' file_sizes(filepath = getwd())[1]
#'
#' @importFrom dplyr %>%
#'
#' @export

file_sizes <- function(filepath = here::here("data", "output")) {

  if (!file.exists(filepath)) {
    stop(paste0("A valid filepath to the folder containing the HSMR Excel ",
                "tables must be supplied"))
  }

  # Identify the HSMR Excel files in the filepath
  # Regex works as follows:
  # ^ = starts with
  # \\ = escape special behaviour (to match a literal `\`, write `\\\\`)
  # \\d{4}-\\d{2}-\\d{2} = yyyy-mm-dd
  # (T|t)able = Table or table
  # * = zero or more pattern matches
  # [a-zA-Z0-9\\_\\-]* = zero or more lower case, uppercase, numeric,
  #                      underscore or dash characters
  # $ = ends with
  # \\.xlsx$ = has .xlsx file extension
  x <- dir(path = filepath,
           pattern = "^\\d{4}-\\d{2}-\\d{2}(T|t)able[a-zA-Z0-9\\_\\-]*\\.xlsx$")

  if (length(x) != 3) {
    stop(paste0("Exactly three HSMR Excel tables should be present in the ",
                "folder: Tables 1, 2 and 3"))
  }

  x %>%

    # Calculate the size of each HSMR Excel table in bytes
    purrr::map(~file.info(paste0(filepath, "/", .))$size) %>%
    unlist() %>%

    # The gdata package defines a kilobyte as 1,000 bytes, and a kibibyte as
    # 1,024 bytes
    # We wish to take a kilobyte as 1,024 bytes
    # As a workaround, calculate file sizes in kibibytes or mebibytes, then
    # drop the `i` from the output, so the final result is displayed as KB or MB
    gdata::humanReadable(standard = "IEC", digits = 0) %>%
    gsub("i", "", .) %>%
    trimws() %>%
    paste0("Excel ", .)
}
