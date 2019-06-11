library(hsmr)
context("completeness")

# qtr_start is calculated in the helper script

test_that("Output returns boards only with SMR01 completeness < 95%", {

  # Extract all percentages from the returned output for both the current and
  # previous quarters
  # Remove the first one as this should always be 95%
  numbers_current <- readr::parse_number(
    tail(
      unlist(
        stringr::str_match_all(
          completeness(quarter = "current",
                       level = "board",
                       first_day = qtr_start),
          "\\d+\\%")), -1))

  numbers_prev <- readr::parse_number(
    tail(
      unlist(
        stringr::str_match_all(
          completeness(quarter = "previous",
                       level = "board",
                       first_day = qtr_start),
          "\\d+\\%")), -1))

  # If no boards have SMR01 completeness < 95%, set equal to zero, so the tests
  # pass
  numbers_current <- replace(numbers_current, length(numbers_current) == 0, 0)
  numbers_prev <- replace(numbers_prev, length(numbers_prev) == 0, 0)

  # Check that every returned percentage is < 95
  expect_true(all(numbers_current < 95))
  expect_true(all(numbers_prev < 95))
})

test_that("SMR01 completeness for Scotland is displayed as percentage", {
  expect_match(completeness(quarter = "current",
                            level = "scotland",
                            first_day = qtr_start),
               "%")
  expect_match(completeness(quarter = "previous",
                            level = "scotland",
                            first_day = qtr_start),
               "%")
})

test_that("SMR01 completeness for Scotland never exceeds 100%", {
  expect_lte(readr::parse_number(completeness(quarter = "current",
                                              level = "scotland",
                                              first_day = qtr_start)),
             100)
  expect_lte(readr::parse_number(completeness(quarter = "previous",
                                              level = "scotland",
                                              first_day = qtr_start)),
             100)
})

test_that("Errors if first day of latest quarter is not in date format", {
  expect_error(completeness(quarter = "current",
                            level = "board",
                            first_day = "2019-01-01"))
  expect_error(completeness(quarter = "current",
                            level = "scotland",
                            first_day = as.factor("2017-10-01")))
  expect_error(completeness(quarter = "previous",
                            level = "board",
                            first_day = as.numeric(lubridate::dmy(01042018))))
  expect_error(completeness(quarter = "previous",
                            level = "scotland",
                            first_day = as.integer(lubridate::dmy(01072012))))
})

test_that("Errors if supplied with date that isn't first day of a quarter", {
  expect_error(completeness(quarter = "current",
                            level = "board",
                            first_day = lubridate::dmy(01112018)))
  expect_error(completeness(quarter = "current",
                            level = "scotland",
                            first_day = lubridate::dmy(13122000)))
  expect_error(completeness(quarter = "previous",
                            level = "board",
                            first_day = lubridate::dmy(02012016)))
  expect_error(completeness(quarter = "previous",
                            level = "scotland",
                            first_day = lubridate::dmy(30062013)))
})

test_that("Errors if supplied with first day of a past quarter", {

  # These tests were written for the 2019-08-13 publication, at which point
  # 2019-01-01 was the first day of the most recent quarter of data included in
  # the publication
  # All dates prior to this should produce an error, including those which were
  # the first day of a quarter

  expect_error(completeness(quarter = "current",
                            level = "board",
                            first_day = lubridate::dmy(01102018)))
  expect_error(completeness(quarter = "current",
                            level = "scotland",
                            first_day = lubridate::dmy(01072018)))
  expect_error(completeness(quarter = "previous",
                            level = "board",
                            first_day = lubridate::dmy(01012010)))
  expect_error(completeness(quarter = "previous",
                            level = "scotland",
                            first_day = lubridate::dmy(01041996)))
})
