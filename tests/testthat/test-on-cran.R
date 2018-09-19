context("on-cran")

testthat::test_that(
  "we can download a requested file from the latest release", {

    tmp <- tempdir()


    pb_download(
      file = "data/mtcars.tsv.gz",
      repo = "cboettig/piggyback-tests",
      tag = "v0.0.1",
      dest = tmp,
      show_progress = FALSE
    )

    f <- file.path(tmp, "data/mtcars.tsv.gz")
    testthat::expect_true(file.exists(f))
    unlink(f)
  }
)

