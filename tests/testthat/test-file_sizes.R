library(hsmr)
context("file_sizes")


extract_end_date <- lubridate::dmy(31032019)

test_that("Output returns vector of length three", {
  expect_length(file_sizes(extract_end_date), 3)
})

test_that("File sizes are returned in KB or MB", {
  expect_true(all(stringr::str_detect(file_sizes(extract_end_date), "(K|M)B$")))
})

test_that("Errors if extract end date is not in date format", {
  expect_error(file_sizes(end_date = "2019-03-31"))
  expect_error(file_sizes(end_date = as.factor("2019-03-31")))
  expect_error(file_sizes(end_date = as.numeric(lubridate::dmy(31032019))))
  expect_error(file_sizes(end_date = as.integer(lubridate::dmy(31032019))))
})

