library(hsmr)
context("qtr")


test_that("Returns correct quarter in correct format", {
  expect_equal("Jan-Mar 2018",
               qtr(first_day = lubridate::dmy(01012018), format = "short"))
  expect_equal("January to March 2018",
               qtr(first_day = lubridate::dmy(01012018), format = "long"))
  expect_equal("Oct-Dec 1995",
               qtr(first_day = lubridate::dmy(01101995), format = "short"))
  expect_equal("October to December 1995",
               qtr(first_day = lubridate::dmy(01101995), format = "long"))
  expect_equal("September 2018",
               qtr_end(first_day = lubridate::dmy(01072018),
                       quarter = "current"))
  expect_equal("December 2018",
               qtr_end(first_day = lubridate::dmy(01072018), quarter = "next"))
  expect_equal("December 1995",
               qtr_end(first_day = lubridate::dmy(01101995),
                       quarter = "current"))
  expect_equal("March 1996",
               qtr_end(first_day = lubridate::dmy(01101995), quarter = "next"))
  expect_equal("April to June 2018",
               qtr_prev(first_day = lubridate::dmy(01072018)))
  expect_equal("October to December 1995",
               qtr_prev(first_day = lubridate::dmy(01011996)))
})

test_that("Errors if first_day is not in date format", {
  expect_error(qtr(first_day = "2018-01-01", format = "short"))
  expect_error(qtr(first_day = as.factor("1995-10-01"), format = "long"))
  expect_error(qtr_end(first_day = c("2018-07-01"), quarter = "current"))
  expect_error(qtr_end(first_day = as.numeric(lubridate::dmy(01101995)),
                       quarter = "next"))
  expect_error(qtr_prev(first_day = as.integer(lubridate::dmy(01072018))))
})

test_that("Errors if first_day is not the first day of a quarter", {
  expect_error(qtr(first_day = lubridate::dmy(02012018), format = "short"))
  expect_error(qtr(first_day = lubridate::dmy(31122017), format = "long"))
  expect_error(qtr_end(first_day = lubridate::dmy(15072018),
                       quarter = "current"))
  expect_error(qtr_end(first_day = lubridate::dmy(01111995), quarter = "next"))
  expect_error(qtr_prev(first_day = lubridate::dmy(01033096)))
})
