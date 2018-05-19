testthat::context("Requiring Authentication")

## Setup: create test repo
#repo <- "cboettig/piggyback-test"


testthat::test_that("We can upload data",{
  testthat::skip_if(piggyback:::get_token() == "")
  data <- readr::write_tsv(datasets::iris, "iris.tsv.gz")
  pb_upload("cboettig/piggyback", file = "iris.tsv.gz", tag = "v0.0.1", overwrite = TRUE)
})


testthat::test_that("We can push and pull data",{
  testthat::skip_if(piggyback:::get_token() == "")

  cur <- getwd()
  tmp <- tempdir()
  proj_dir <- file.path(tmp, "piggyback-test")
  suppressMessages(usethis::create_project(proj_dir,
                                           open=FALSE))
  setwd(proj_dir)

  pb_track("*.tsv")
  out <- pb_pull(.repo = "cboettig/piggyback")
  testthat::expect_true(out)

  pb_push(.repo = "cboettig/piggyback")
  setwd(cur)

  })
## Create and delete a new test repository?

#repo <- "cboettig/piggyback-test"
#' gh_new_release(test_repo, "v0.0.5")

#   pb_upload("cboettig/piggyback", file = "data/mtcars.tsv.gz", tag = "v0.0.1", overwrite = TRUE)
## And again, overwriting!
#  pb_upload("cboettig/piggyback", file = "data/mtcars.tsv.gz", tag = "v0.0.1", overwrite = TRUE)
