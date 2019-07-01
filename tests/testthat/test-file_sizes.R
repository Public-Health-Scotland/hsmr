library(hsmr)
context("file_sizes")


test_that("Output returns vector of length three", {
  expect_length(file_sizes(), 3)
})

test_that("File sizes are returned in KB or MB", {
  expect_true(all(stringr::str_detect(file_sizes(), "(K|M)B$")))
})



