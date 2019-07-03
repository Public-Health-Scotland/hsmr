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
  expect_error(file_sizes(end_date = as.numeric(extract_end_date)))
  expect_error(file_sizes(end_date = as.integer(extract_end_date)))
})

test_that("Errors if extract end date is not final day of a quarter", {
  expect_error(file_sizes(end_date = lubridate::dmy(01072019)))
  expect_error(file_sizes(end_date = lubridate::dmy(15071998)))
  expect_error(file_sizes(end_date = lubridate::dmy(30041989)))
})

test_that("Errors if extract end date is before 2019-03-31", {
  expect_error(file_sizes(end_date = lubridate::dmy(31122018)))
  expect_error(file_sizes(end_date = lubridate::dmy(31032018)))
  expect_error(file_sizes(end_date = lubridate::dmy(30061997)))
})
