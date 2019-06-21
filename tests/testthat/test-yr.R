library(hsmr)
context("yr")

test_that("Years are correct for previous, current and next publications", {
  expect_equal("January 2018 to December 2018",
               yr(end_date = lubridate::dmy(31122018)))
  expect_equal("April 2018 to March 2019",
               yr(end_date = lubridate::dmy(31032019)))
  expect_equal("July 2018 to June 2019",
               yr(end_date = lubridate::dmy(30062019)))
})

test_that("Errors if extract end date is not in date format", {
  expect_error(yr(end_date = "2019-09-30"))
  expect_error(yr(end_date = as.factor("2019-09-30")))
  expect_error(yr(end_date = as.numeric(lubridate::dmy(31032018))))
  expect_error(yr(end_date = as.integer(lubridate::dmy(31032018))))
  expect_error(yr(end_date = as.character(lubridate::dmy(31032019))))
})
