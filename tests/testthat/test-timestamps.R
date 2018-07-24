

testthat::test_that(
  "upload obeys timestamp only when newer",
  {

    testthat::skip_on_travis() # No idea...

    testthat::skip_if(piggyback:::get_token() == "")
    testthat::skip_if_not(as.logical(Sys.getenv("CBOETTIG_TOKEN", FALSE)))

    pb_delete(repo = "cboettig/piggyback",
              file = "mtcars2.tsv.gz",
              tag = "v0.0.1")

    readr::write_tsv(datasets::mtcars, "mtcars2.tsv.gz")

    testthat::expect_silent(
      out <- pb_upload(file = "mtcars2.tsv.gz",
                       repo = "cboettig/piggyback",
                       tag = "v0.0.1",
                       overwrite = FALSE,
                       show_progress = FALSE,
                       use_timestamps = TRUE))
    testthat::expect_message(
      out <- pb_upload(file = "mtcars2.tsv.gz",
                       repo = "cboettig/piggyback",
                       tag = "v0.0.1",
                       overwrite = TRUE,
                       use_timestamps = TRUE),
      "more recent version of"
    )

    pb_delete(repo = "cboettig/piggyback",
              file = "mtcars2.tsv.gz",
              tag = "v0.0.1")

    unlink("mtcars2.tsv.gz")
  })

