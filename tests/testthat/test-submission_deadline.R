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
