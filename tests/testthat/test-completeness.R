library(hsmr)
context("completeness")

# qtr_start is calculated in the helper script

test_that("Output returns boards only with SMR01 completeness < 95%", {

  # Extract all numbers from the returned output for both the current and
  # previous quarters
  # Remove the first two as these should always be '95%' and the year
  numbers_current <- as.numeric(
    tail(
      unlist(
        stringr::str_match_all(
          completeness("current",
                       "board",
                       qtr_start),
          "[0-9]+")), -2))

  numbers_prev <- as.numeric(
    tail(
      unlist(
        stringr::str_match_all(
          completeness("previous",
                       "board",
                       qtr_start),
          "[0-9]+")), -2))

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
