

testthat::test_that(
  "upload obeys timestamp only when newer",
  {

    testthat::skip_if(piggyback:::get_token() == "")
    testthat::skip_if(piggyback:::get_token() == "b2b7441daeeb010b1df26f1f60a7f1edc485e443")
    testthat::skip_if_not(as.logical(Sys.getenv("CBOETTIG_TOKEN", FALSE)))

    pb_delete(repo = "cboettig/piggyback",
              file = "mtcars2.tsv.gz",
              tag = "v0.0.1")

    tmp <- tempdir()

    readr::write_tsv(datasets::mtcars, fs::path(tmp, "mtcars2.tsv.gz"))

    testthat::expect_silent(
      out <- pb_upload(file = fs::path(tmp, "mtcars2.tsv.gz"),
                       repo = "cboettig/piggyback",
                       tag = "v0.0.1",
                       overwrite = FALSE,
                       show_progress = FALSE,
                       use_timestamps = TRUE,
                       dir = tmp))

    testthat::expect_message(
      out <- pb_upload(file = fs::path(tmp, "mtcars2.tsv.gz"),
                       repo = "cboettig/piggyback",
                       tag = "v0.0.1",
                       overwrite = TRUE,
                       use_timestamps = TRUE,
                       dir = tmp),
      "more recent version of"
    )

    pb_delete(repo = "cboettig/piggyback",
              file = "mtcars2.tsv.gz",
              tag = "v0.0.1")

    unlink(fs::path(tmp, "mtcars2.tsv.gz"))
  })

