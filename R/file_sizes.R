#' @title Extract file sizes for HSMR Excel tables
#'
#' @description \code{file_sizes} takes the final date for which SMR data are
#' included in the current publication and the filepath to the location of the
#' HSMR Excel tables. It returns a \code{character} vector containing the size
#' of each table.
#'
#' @details There are three HSMR Excel tables: tables 1, 2 and 3. If fewer or
#' more Excel tables which follow the same naming conventions as tables 1, 2
#' and 3 are found in the filepath, the function will return an error.
#'
#' File sizes are returned in kilobytes (KB) or megabytes (MB). A kilobyte is
#' taken to be 1,024 bytes, and a megabyte to be 1,024 kilobytes.
#'
#' @param end_date The cut-off date for data to be included in the current HSMR
#' publication, supplied with \code{Date} class. Must be the final day of
#' either March, June, September or December. Must not be prior to
#' \code{2019-03-31}, as this is the extract end date for the \code{2019-08-13}
#' publication. The current format of the HSMR Excel tables was first created
#' for this publication.
#' @param filepath A \code{character} string containing the filepath to the
#' location of the HSMR Excel tables. Errors if not provided with a valid
#' filepath. Defaults to \code{here::here("data", "output")}.
#'
#' @return A \code{character} vector.
#'
#' @seealso The code which creates this function uses regular expressions. For
#' more information on using regular expressions, see this
#' \href{https://www.jumpingrivers.com/blog/regular-expressions-every-r-programmer-should-know/}{Jumping Rivers blog post}
#' and this
#' \href{https://stringr.tidyverse.org/articles/regular-expressions.html}{vignette}
#' from the \href{https://stringr.tidyverse.org/}{stringr} package.
#'
#' @examples
#' extract_end_date <- lubridate::dmy(31032019)
#'
#' file_sizes(end_date = extract_end_date)
#' file_sizes(end_date = extract_end_date, filepath = getwd())[1]
#'
#' @importFrom dplyr %>%
#'
#' @export

file_sizes <- function(end_date, filepath = here::here("data", "output")) {

  if (class(end_date) != "Date") {
    stop("The extract end date must be provided in date format")
  }

  if(!(format(end_date, "%d %B") %in% c("31 March",
                                        "30 June",
                                        "30 September",
                                        "31 December"))) {
    stop("The extract end date must be the final day of either March, June, ",
         "September or December")
  }

  if(end_date < lubridate::dmy(31032019)) {
    stop(paste0("The extract end date must not be prior to 2019-03-31, as ",
                "the current format of the HSMR Excel tables files was first ",
                "created for the 2019-08-13 publication"))
  }

  if (!file.exists(filepath)) {
    stop(paste0("A valid filepath to the folder containing the HSMR Excel ",
                "tables must be supplied"))
  }

  # Identify the HSMR Excel files in the filepath
  # Regex works as follows:
  # ^ = starts with
  # \\ = escape special behaviour (to match a literal `\`, write `\\\\`)
  # (T|t)able = Table or table
  # * = zero or more pattern matches
  # [a-zA-Z0-9\\_\\-]* = zero or more lower case, uppercase, numeric,
  #                      underscore or dash characters
  # $ = ends with
  # \\.xlsx$ = has .xlsx file extension
  x <- dir(path = filepath,
           pattern = paste0("^",
                            hsmr::pub_date(end_date, "current"),
                            "(T|t)able[a-zA-Z0-9\\_\\-]*\\.xlsx$"))

  if (length(x) != 3) {
    stop(paste0("Exactly three HSMR Excel tables for the current publication ",
                "should be present in the folder: tables 1, 2 and 3"))
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
