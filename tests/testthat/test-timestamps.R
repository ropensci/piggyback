

testthat::test_that(
  "upload obeys timestamp only when newer",
  {
    testthat::skip_on_cran()

    readr::write_tsv(datasets::mtcars, "mtcars2.tsv.gz")
    testthat::expect_silent(
      out <- pb_upload(file = "mtcars2.tsv.gz",
                       repo = "cboettig/piggyback",
                       tag = "v0.0.1",
                       overwrite = TRUE,
                       show_progress = FALSE))
    testthat::expect_message(
      out <- pb_upload(file = "mtcars2.tsv.gz",
                       repo = "cboettig/piggyback",
                       tag = "v0.0.1",
                       overwrite = TRUE),
      "more recent version of"
    )

    pb_delete(repo = "cboettig/piggyback",
              file = "mtcars2.tsv.gz",
              tag = "v0.0.1")

    unlink("mtcars2.tsv.gz")
  })

