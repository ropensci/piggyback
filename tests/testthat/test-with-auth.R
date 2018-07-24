testthat::context("Requiring Authentication")

## Setup: create test repo
#repo <- "cboettig/piggyback-test"


testthat::test_that("We can upload data",{
  testthat::skip_if(piggyback:::get_token() == "")
  testthat::skip_if_not(as.logical(Sys.getenv("CBOETTIG_TOKEN", FALSE)))

  data <- readr::write_tsv(datasets::iris, "iris.tsv.gz")
  out <- pb_upload(repo = "cboettig/piggyback",
                   file = "iris.tsv.gz",
                   tag = "v0.0.1",
                   overwrite = TRUE,
                   show_progress = FALSE)
  testthat::expect_is(out, "response")

  unlink("iris.tsv.gz")
  unlink("manifest.json")

})


testthat::test_that("We can push and pull data",{

  testthat::skip_if(piggyback:::get_token() == "")
  testthat::skip_if_not(as.logical(Sys.getenv("CBOETTIG_TOKEN", FALSE)))

  ##  Setup
  cur <- getwd()
  tmp <- fs::path(tempfile(), "pb_test")
  fs::dir_create(tmp)
  setwd(tmp)
  if(packageVersion("git2r") <= "0.21.0"){
    testthat::skip_on_os("mac")
  }

  suppressMessages(
  usethis::create_from_github(repo = "cboettig/piggyback",
                              destdir = ".",
                              open = FALSE,
                              protocol = "https"))
  setwd("piggyback")

  fs::dir_create("data")
  readr::write_tsv(mtcars, "mtcars.tsv.gz")
  readr::write_tsv(mtcars, "data/mtcars.tsv.xz")

  pb_track(c("*.tsv.gz", "data/*"))
  testthat::expect_true(pb_push(repo = "cboettig/piggyback"))
  testthat::expect_true(pb_pull(repo = "cboettig/piggyback"))

  testthat::expect_true(pb_pull())
  testthat::expect_true(pb_push())
  pb_push()
  pb_pull()

  testthat::expect_true(pb_pull(tag="v0.0.1"))

  ## Should error if tag already exists
  testthat::expect_error(
    gh_new_release(repo = "cboettig/piggyback", tag = "v0.0.1"))

   ## tare down
  setwd(cur)
  fs::dir_delete(tmp)

  })



testthat::test_that("we can get a download url", {

  testthat::skip_if(piggyback:::get_token() == "")
  testthat::skip_if_not(as.logical(Sys.getenv("CBOETTIG_TOKEN", FALSE)))

  x <- pb_download_url("data/iris.tsv.gz",
                       repo = "cboettig/piggyback",
                       tag = "v0.0.1",
                       .token = piggyback:::get_token() )
  testthat::expect_is(x, "character")
})

testthat::test_that(
  "we error when creating a release on non-existant repo", {
    testthat::skip_on_cran()
    testthat::expect_error(
      pb_new_release("cboettig/not_a_repo", "v2.0"), "404")
  })

