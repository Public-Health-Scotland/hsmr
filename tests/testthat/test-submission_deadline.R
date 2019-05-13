library(hsmr)
context("submission_deadline")


test_that("Submission deadlines are correct for Nov 18 + Feb and May 19 pubs", {
  expect_equal(lubridate::dmy(12102018),
               submission_deadline(end_date = lubridate::dmy(30062018)))
  expect_equal(lubridate::dmy(11012019),
               submission_deadline(end_date = lubridate::dmy(30092018)))
  expect_equal(lubridate::dmy(11042019),
               submission_deadline(end_date = lubridate::dmy(31122018)))
})

test_that("Errors if extract end date is not in date format", {
  expect_error(submission_deadline("2019-09-30"))
  expect_error(submission_deadline(as.factor("2019-09-30")))
  expect_error(submission_deadline(as.numeric(lubridate::dmy(31032018))))
  expect_error(submission_deadline(as.integer(lubridate::dmy(31032018))))
})

test_that("Errors if extract end date is not final day of a quarter", {
  expect_error(submission_deadline(lubridate::dmy(01072018)))
  expect_error(submission_deadline(lubridate::dmy(31092018)))
  expect_error(submission_deadline(lubridate::dmy(30122018)))
})
