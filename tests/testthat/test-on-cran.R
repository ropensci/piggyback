context("on-cran")


testthat::test_that(
  "we can list files even from CRAN", {

    x <- pb_list(
      repo = "cboettig/piggyback-tests",
      tag = "v0.0.1"
    )
    testthat::expect_true("data/iris.tsv.xz" %in% x$file_name)
  }
)
