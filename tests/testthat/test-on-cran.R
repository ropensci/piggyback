context("on-cran")


testthat::test_that(
  "no guarentees of anything on CRAN, better check", {
  expect_that(1+1, 2)

  }
)
