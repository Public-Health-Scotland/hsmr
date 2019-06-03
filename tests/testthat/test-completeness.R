library(hsmr)
context("completeness")

# qtr_start is calculated in the helper script

test_that("Output returns boards only with SMR01 completeness < 95%", {

  # Extract all percentages from the returned output for both the current and
  # previous quarters
  # Remove the first one as this should always be 95%
  numbers_current <- as.numeric(
    tail(
      unlist(
        stringr::str_match_all(
          completeness("current",
                       "board",
                       qtr_start),
          "\\d+\\%")), -1))

  numbers_prev <- as.numeric(
    tail(
      unlist(
        stringr::str_match_all(
          completeness("previous",
                       "board",
                       qtr_start),
          "\\d+\\%")), -1))

  # If no boards have SMR01 completeness < 95%, set equal to zero, so the tests
  # pass
  numbers_current <- replace(numbers_current, length(numbers_current) == 0, 0)
  numbers_prev <- replace(numbers_prev, length(numbers_prev) == 0, 0)

  # Check that every returned percentage is < 95
  for (i in 1:length(numbers_current)) {
    expect_lt(numbers_current[i], 95)
  }

  for (j in 1:length(numbers_prev)) {
    expect_lt(numbers_prev[j], 95)
  }
})

test_that("SMR01 completeness for Scotland is displayed as percentage", {
  expect_match(completeness("current", "scotland", qtr_start), "%")
  expect_match(completeness("previous", "scotland", qtr_start), "%")
})

test_that("SMR01 completeness for Scotland never exceeds 100%", {
  expect_lte(readr::parse_number(completeness("current",
                                              "scotland",
                                              qtr_start)),
             100)
  expect_lte(readr::parse_number(completeness("previous",
                                              "scotland",
                                              qtr_start)),
             100)
})

test_that("Errors if first day of latest quarter is not in date format", {
  expect_error(completeness("current", "board", "2019-01-01"))
  expect_error(completeness("current", "scotland", as.factor("2017-10-01")))
  expect_error(completeness("previous", "board", as.numeric("2018-04-01")))
  expect_error(completeness("previous", "scotland", as.integer("2012-07-01")))
})
