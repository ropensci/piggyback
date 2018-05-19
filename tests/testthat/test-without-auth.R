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

testthat::test_that(
  "we can download a requested file to requested subdirectory", {
    dir.create("test_data/")
    pb_download("cboettig/piggyback", "mtcars.tsv.gz", tag = "v0.0.1", dest = "test_data/")

    testthat::expect_true(file.exists("test_data/mtcars.tsv.gz"))
    cars <- readr::read_tsv("test_data/mtcars.tsv.gz")
    testthat::expect_equivalent(cars, mtcars)
    unlink("test_data", TRUE)
  })


#######  We need to be in an active project to track something

testthat::test_that("we can track data with manifest", {
  cur <- getwd()
  tmp <- tempdir()
  proj_dir <- file.path(tmp, "piggyback-test")
  suppressMessages(usethis::create_project(proj_dir,
                          open=FALSE))
  setwd(proj_dir)

  pb_track("*.tsv")
  out <- pb_pull(.repo = "cboettig/piggyback")
  testthat::expect_true(out)

  setwd(cur)
})


