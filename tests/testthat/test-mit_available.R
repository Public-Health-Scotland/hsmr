library(hsmr)
context("mit_available")

test_that("Returns date of last Friday in the month before publication", {
  expect_equal(lubridate::dmy(25012019),
               mit_available(end_date = lubridate::dmy(30092018)))
  expect_equal(lubridate::dmy(26042019),
               mit_available(end_date = lubridate::dmy(31122018)))
  expect_equal(lubridate::dmy(26072019),
               mit_available(end_date = lubridate::dmy(31032019)))
  expect_equal(lubridate::dmy(25102019),
               mit_available(end_date = lubridate::dmy(30062019)))
})

test_that("Errors if extract end date is not in date format", {
  expect_error(mit_available(end_date = "2019-09-30"))
  expect_error(mit_available(end_date = as.factor("2019-09-30")))
  expect_error(mit_available(end_date = as.numeric(lubridate::dmy(31032018))))
  expect_error(mit_available(end_date = as.integer(lubridate::dmy(31032018))))
})
