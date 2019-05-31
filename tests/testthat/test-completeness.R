library(hsmr)
context("completeness")

test_that("Less than 95%", {
  j <- as.numeric(
    tail(
      unlist(
        stringr::str_match_all(
          hsmr::completeness("current",
                             "board",
                             first_day),
          "[0-9]+")), -2))

  k <- dplyr::case_when(
    length(j) > 0 ~ j,
    length(j) == 0 ~ 0
  )

  for (i in 1:length(k)) {
    expect_lt(k[i], 95)
  }
})

