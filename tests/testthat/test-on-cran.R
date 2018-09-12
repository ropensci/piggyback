
testthat::test_that(
  "we can download a requested file from the latest release", {
    pb_download(
      file = "data/mtcars.tsv.gz",
      repo = "cboettig/piggyback",
      dest = tempdir(),
      show_progress = FALSE
    )

    f <- fs::path(tempdir(), "data/mtcars.tsv.gz")
    testthat::expect_true(file.exists(f))
    unlink(f)
  }
)
