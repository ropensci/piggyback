context("on-cran")


testthat::test_that(
  "no guarentees of anything on CRAN, better check", {
  expect_true(1+1 == 2)

  }
)
