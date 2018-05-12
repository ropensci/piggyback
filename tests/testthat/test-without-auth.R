testthat::context("Without Authentication")

testthat::test_that(
  "we can download all files from the latest release", {
  pb_download("cboettig/piggyback")

  ## v0.0.4 has 1 file

  testthat::expect_true(file.exists("mtcars.tsv.gz"))
  cars <- readr::read_tsv("mtcars.tsv.gz")
  testthat::expect_equivalent(cars, mtcars)
  unlink("mtcars.tsv.gz")
})


testthat::test_that(
  "we can download all files from the requested release", {
    pb_download("cboettig/piggyback", tag="v0.0.1")

    ## v0.0.1 has 2 files
    testthat::expect_true(file.exists("mtcars.tsv.gz"))
    testthat::expect_true(file.exists("iris.tsv.xz"))

    unlink("mtcars.tsv.gz")
    unlink("iris.tsv.xz")
  })

testthat::test_that(
  "we can download a requested file from the latest release", {
    pb_download("cboettig/piggyback", "mtcars.tsv.gz")

    testthat::expect_true(file.exists("mtcars.tsv.gz"))
    cars <- readr::read_tsv("mtcars.tsv.gz")
    testthat::expect_equivalent(cars, mtcars)
    unlink("mtcars.tsv.gz")
})


testthat::test_that(
  "we can download a requested file from the requested release", {
    pb_download("cboettig/piggyback", "mtcars.tsv.gz", tag = "v0.0.1")

    testthat::expect_true(file.exists("mtcars.tsv.gz"))
    cars <- readr::read_tsv("mtcars.tsv.gz")
    testthat::expect_equivalent(cars, mtcars)
    unlink("mtcars.tsv.gz")
  })
