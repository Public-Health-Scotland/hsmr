# This script calculates the first day of the latest quarter in the SMR
# completeness spreadsheet
# It should be accurate at any given moment in time
# This value is used in unit testing of the completeness function

# Load SMR completeness spreadsheet
# Note that this file can occasionally change when a new quarter's data is
# added, which will cause the function to error if not updated
tmp <- tempfile(fileext = ".xlsx")
httr::GET(url = paste0("https://www.isdscotland.org/products-and-Services/",
                       "Data-Support-and-Monitoring/SMR-Completeness/_docs/",
                       "SMR_Estimates.xlsx"),
          httr::write_disk(tmp))

# Note that the range is based on B29 being "NHS Board" and B47 being "All
# NHS Boards" - if these shift up or down slightly when the file is modified
# then the range will need to be updated accordingly for the function to work
# as intended
comp <- suppressMessages(
  readxl::read_xlsx(tmp, range = "B29:AF47", col_names = FALSE))
comp <- janitor::clean_names(comp)

# The above step parses the file with the dataset names in the first row,
# however it only adds the dataset name to the column containing the first
# quarter of data, and gives all subsuquent quarters NA
#
# This step replaces those NAs in the first row with the nearest non-NA value
# to the left, which should be the name of the relevant dataset
comp[1,] <- t(dplyr::select(
  tidyr::fill(
    tidyr::gather(
      dplyr::slice(
        comp, 1)),
    value), 2))

# Set the dataset names as the column names
comp <- setNames(comp, unlist(dplyr::slice(comp, 1), use.names = FALSE))
comp <- dplyr::slice(comp, -1)
comp <- janitor::clean_names(comp)

# Subsequently select only the columns pertaining to the name of the board
# and the SMR01 dataset
# The regex is needed to differentiate SMR01 from SMR01 GLS
comp <- dplyr::select(comp, nhs_board, smr01, dplyr::matches("^smr01_[0-9]$"))

# Now set the column names (with the exception of the one pertaining to the
# name of the board) to the relevant quarter
colnames(comp)[-1] <- unlist(dplyr::slice(dplyr::select(comp, -nhs_board), 1),
                             use.names = FALSE)

comp <- dplyr::slice(comp, -1)
comp <- janitor::clean_names(comp)

# Select only the two most recent quarters
comp <- dplyr::select(comp, nhs_board, tail(names(comp), 2))
comp <- dplyr::mutate(comp, nhs_board = replace(nhs_board,
                                                nhs_board == "All NHS Boards",
                                                "Scotland"))

# Calculate the first month in the most recent quarter, to allow the first day
# of said month to be calculated subsequently
first_month <- colnames(comp)[2:3]
first_month <- unlist(stringr::str_split(first_month, "_"))
first_month <- format(zoo::as.yearmon(first_month, "%b%y"), "%B %Y")
first_month <- first_month[3]

# Calculate the start date of the most recent quarter
qtr_start <- lubridate::as_date(zoo::as.yearmon(first_month))
