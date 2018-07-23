testthat::context("Without Authentication")

## Even though authentication is not required for these tests,
## they do call the GH API and are subject to tight rate-limiting
## when no Token is available.  It is preferable / advisable to have
## set GITHUB_TOKEN env var for any testing, as we do on Appveyor
## and Travis
testthat::test_that(
  "we can download all files from the latest release", {

    testthat::skip_on_cran()
    pb_download(repo = "cboettig/piggyback", dest = tempdir())

    f <- fs::path(tempdir(), "data/mtcars.tsv.gz")
    testthat::expect_true(file.exists(f))
    cars <- readr::read_tsv(f)
    testthat::expect_equivalent(cars, mtcars)

    unlink(f)
    unlink(fs::path(tempdir(), "data/iris.tsv.gz"))
    unlink(fs::path(tempdir(), "data"))
  })


testthat::test_that(
  "we can omit dest dir", {

  testthat::skip_on_cran()

  ## Works interactively but check will fail when this tries to write to "."
  pb_download(repo = "cboettig/piggyback")

  testthat::expect_true(file.exists("data/mtcars.tsv.gz"))
  cars <- readr::read_tsv("data/mtcars.tsv.gz")
  testthat::expect_equivalent(cars, mtcars)


  pb_download(repo = "cboettig/piggyback")


  unlink("data/mtcars.tsv.gz")
  unlink("data/iris.tsv.gz")
  unlink("data")
})


testthat::test_that(
  "we can list files", {
    testthat::skip_on_cran()
    testthat::expect_output(cat(
      pb_list(repo = "cboettig/piggyback")), "iris.tsv.gz")
  })



testthat::test_that(
  "we can list multiple ignore files, including non-existent ones", {
    testthat::skip_on_cran()
    pb_download(
      repo = "cboettig/piggyback",
      ignore = c("manifest.json", "big_data_file.csv"),
      dest = tempdir())
    testthat::expect_true(TRUE)
  })

testthat::test_that(
  "we can download all files from the requested release", {

    testthat::skip_on_cran()


    pb_download(repo = "cboettig/piggyback", tag="v0.0.1")

    ## v0.0.1 has 2 files
    testthat::expect_true(file.exists("mtcars.tsv.gz"))
    testthat::expect_true(file.exists("iris.tsv.xz"))

    unlink("mtcars.tsv.gz")
    unlink("iris.tsv.xz")
    unlink("iris.tsv.gz")

  })


testthat::test_that(
  "we can download a requested file from the requested release", {

    testthat::skip_on_cran()


    pb_download( file = "mtcars.tsv.gz",
                 repo = "cboettig/piggyback",
                 tag = "v0.0.1")

    testthat::expect_true(file.exists("mtcars.tsv.gz"))
    cars <- readr::read_tsv("mtcars.tsv.gz")
    testthat::expect_equivalent(cars, mtcars)
    unlink("mtcars.tsv.gz")
  })

testthat::test_that(
  "we can download a requested file to requested subdirectory", {

    testthat::skip_on_cran()


    dir.create("test_data/")
    pb_download( file = "mtcars.tsv.gz",
                 dest = "test_data/",
                 repo = "cboettig/piggyback",
                 tag = "v0.0.1")

    testthat::expect_true(file.exists("test_data/mtcars.tsv.gz"))
    cars <- readr::read_tsv("test_data/mtcars.tsv.gz")
    testthat::expect_equivalent(cars, mtcars)
    unlink("test_data", TRUE)
  })


#######  We need to be in an active project to track something

testthat::test_that("we can track data with manifest", {

  testthat::skip_on_cran()

  cur <- getwd()
  tmp <- tempdir()
  proj_dir <- file.path(tmp, "piggyback-test")
  suppressMessages(usethis::create_project(proj_dir,
                          open=FALSE))
  setwd(proj_dir)

  pb_track("*.tsv")
  out <- pb_pull(repo = "cboettig/piggyback")
  testthat::expect_true(out)

    setwd(cur)
    unlink(tmp)

})


