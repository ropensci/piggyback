testthat::context("Requiring Authentication")

## Setup: create test repo
#repo <- "cboettig/piggyback-test"


testthat::test_that("We can upload data",{
  testthat::skip_if(piggyback:::get_token() == "")
  data <- readr::write_tsv(datasets::iris, "iris.tsv.gz")
  out <- pb_upload("cboettig/piggyback", file = "iris.tsv.gz",
            tag = "v0.0.1", overwrite = TRUE)
  testthat::expect_is(out, "response")

  unlink("iris.tsv.gz")
  unlink("manifest.json")

})


testthat::test_that("We can push and pull data",{
  testthat::skip_if(piggyback:::get_token() == "")

  ##  Setup
  cur <- getwd()
  tmp <- fs::path_temp("piggyback-test")
  fs::dir_create(tmp)
  setwd(tmp)
  if(packageVersion("git2r") <= "0.21.0"){
    testthat::skip_on_os("mac")
  }

  suppressMessages(
  usethis::create_from_github("cboettig/piggyback",
                              destdir = ".",
                              open = FALSE,
                              protocol = "https"))
  setwd("piggyback")


  pb_track("*.tsv")

  testthat::expect_true(pb_pull(.repo = "cboettig/piggyback"))
  testthat::expect_true(pb_push(.repo = "cboettig/piggyback"))
  testthat::expect_true(pb_pull())
  testthat::expect_true(pb_push())

  testthat::expect_true(pb_pull(tag="v0.0.1"))
  testthat::expect_true(pb_push(tag="v0.0.1"))

  ## Should error if tag already exists
  testthat::expect_error(
    gh_new_release("cboettig/piggyback", tag = "v0.0.1"))

   ## tare down
  setwd(cur)
  fs::dir_delete(tmp)

  })

