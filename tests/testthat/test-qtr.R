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
