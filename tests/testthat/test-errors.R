testthat::context("Error Handling")


testthat::test_that(
  "Attempt upload without authentication",
  {

    readr::write_tsv(datasets::iris, "iris3.tsv.gz")
    testthat::expect_error(
      out <- pb_upload(repo = "cboettig/piggyback",
                       file = "iris3.tsv.gz",
                       tag = "v0.0.1",
                       overwrite = TRUE,
                       .token = "not_valid_token"),
      "GITHUB_TOKEN"

    )
    unlink("iris.tsv.gz")

  })




testthat::test_that(
  "Attempt overwrite on upload when overwrite is FALSE",
  {
    testthat::skip_on_cran()

    data <- readr::write_tsv(datasets::iris, "iris2.tsv.gz")
    testthat::expect_warning(
      out <- pb_upload(repo = "cboettig/piggyback",
                       file = "iris2.tsv.gz",
                       tag = "v0.0.1",
                       overwrite = FALSE),
      "Skipping upload of iris2.tsv.gz as file exists"
    )
    unlink("iris2.tsv.gz")
  })



testthat::test_that(
  "Attempt upload non-existent file",
  {

    testthat::skip_on_cran()
    testthat::expect_warning(
      out <- pb_upload(repo = "cboettig/piggyback",
                       file = "not-a-file",
                       tag = "v0.0.1",
                       use_timestamps = FALSE),
      "not-a-file does not exist"

    )
    unlink("iris.tsv.gz")

  })


testthat::test_that(
  "Attempt download non-existent file",
  {

    testthat::skip_on_cran()
    testthat::expect_warning(
      out <- pb_download(repo = "cboettig/piggyback",
                       file = "not-a-file",
                       tag = "v0.0.1"),
      "not found"

    )
    unlink("iris.tsv.gz")

  })

testthat::test_that(
  "Attempt upload to non-existent tag",
  {

    testthat::skip_on_cran()
    data <- readr::write_tsv(datasets::iris, "iris.tsv.gz")

    testthat::expect_error(
      out <- pb_upload(repo = "cboettig/piggyback",
                       file = "iris.tsv.gz",
                       tag = "not-a-tag",
                       overwrite = TRUE),
      "Cannot access release"

    )
    unlink("iris.tsv.gz")

  })




testthat::test_that(
  "Attempt overwrite on download when overwrite is FALSE",
  {

    testthat::skip_on_cran()

    data <- readr::write_tsv(datasets::iris, "iris.tsv.gz")
    testthat::expect_warning(
      out <- pb_download(repo = "cboettig/piggyback",
                       file = "iris.tsv.gz",
                       tag = "v0.0.1",
                       overwrite = FALSE,
                       use_timestamps = FALSE),
      "exists"
    )
    unlink("iris.tsv.gz")

  })

## FIXME should message
testthat::test_that(
  "Attempt to delete non-existent file",
  {
pb_delete(repo = "cboettig/piggyback",
          file = "mtcars2.tsv.gz",
          tag = "v0.0.1")
  })
