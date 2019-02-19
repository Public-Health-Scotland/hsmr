# Tests for hsmr publication date function

library(hsmr)
context("pub_date")


extract_end_date <- lubridate::dmy(30092018)

test_that("Publication dates are correct for current, previous and next", {
  expect_equal(lubridate::dmy(12022019),
               pub_date(end_date = extract_end_date, pub = "current"))
  expect_equal(lubridate::dmy(13112018),
               pub_date(end_date = extract_end_date, pub = "previous"))
  expect_equal(lubridate::dmy(14052019),
               pub_date(end_date = extract_end_date, pub = "next"))
  })
