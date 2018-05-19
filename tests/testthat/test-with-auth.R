testthat::context("Requiring Authentication")

## Setup: create test repo
#repo <- "cboettig/piggyback-test"


testthat::test_that("We can upload data",{
  testthat::skip_if(piggyback:::get_token() == "")
  data <- readr::write_tsv(datasets::iris, "iris.tsv.gz")
  pb_upload("cboettig/piggyback", file = "iris.tsv.gz", tag = "v0.0.1", overwrite = TRUE)
})
## Create and delete a new test repository?

#repo <- "cboettig/piggyback-test"
#' gh_new_release(test_repo, "v0.0.5")

#   pb_upload("cboettig/piggyback", file = "data/mtcars.tsv.gz", tag = "v0.0.1", overwrite = TRUE)
## And again, overwriting!
#  pb_upload("cboettig/piggyback", file = "data/mtcars.tsv.gz", tag = "v0.0.1", overwrite = TRUE)
