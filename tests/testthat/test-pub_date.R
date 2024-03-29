library(hsmr)
context("pub_date")


extract_end_date <- lubridate::dmy(30092018)

test_that("Publication dates are correct for previous, current and next", {
  expect_equal(lubridate::dmy(13112018),
               pub_date(end_date = extract_end_date, pub = "previous"))
  expect_equal(lubridate::dmy(12022019),
               pub_date(end_date = extract_end_date, pub = "current"))
  expect_equal(lubridate::dmy(14052019),
               pub_date(end_date = extract_end_date, pub = "next"))
})

test_that("Errors if extract end date is not in date format", {
  expect_error(pub_date(end_date = "2019-09-30", pub = "current"))
  expect_error(pub_date(end_date = as.factor("2019-09-30"), pub = "next"))
  expect_error(pub_date(end_date = as.numeric(lubridate::dmy(31032018)),
                        pub = "current"))
  expect_error(pub_date(end_date = as.integer(lubridate::dmy(31032018)),
                        pub = "next"))
})

test_that("Errors if extract end date is not final day of a quarter", {
  expect_error(pub_date(end_date = lubridate::dmy(01072018), pub = "previous"))
  expect_error(pub_date(end_date = lubridate::dmy(15092018), pub = "current"))
  expect_error(pub_date(end_date = lubridate::dmy(30122018), pub = "next"))
})
