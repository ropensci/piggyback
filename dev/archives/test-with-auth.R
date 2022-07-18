context("Get Release/Asset Info With Auth")

## Setup: create test repo
# repo <- "cboettig/piggyback-test"

test_that("We can upload data", {
  skip_if(Sys.getenv("CBOETTIG_TOKEN") == "", "CBOETTIG_TOKEN not found")

  data <- readr::write_tsv(datasets::iris, "iris.tsv.gz")

  out <- pb_upload(
    repo = "cboettig/piggyback-tests",
    file = "iris.tsv.gz",
    tag = "v0.0.1",
    overwrite = TRUE,
    show_progress = FALSE
  )

  expect_is(out, "list")

  unlink("iris.tsv.gz")
})


test_that("working from git repo", {

  ##  Setup
  cur <- getwd()
  tmp <- fs::path(tempfile(), "pb_test")
  fs::dir_create(tmp)
  setwd(tmp)


  sink(tempfile())
  usethis::create_from_github(
    repo = "cboettig/piggyback-tests",
    destdir = tmp,
    open = FALSE,
    protocol = "https"
  )

  setwd("piggyback-tests")

  fs::dir_create("data")
  readr::write_tsv(datasets::mtcars, "mtcars.tsv.gz")
  readr::write_tsv(datasets::iris, "iris.tsv.xz")

  sink() # avoid verbose messages in test log. usethis msgs cannot turn off(?)


  ## Test guessing repo
  info <- pb_list()
  expect_is(info, "data.frame")
  # pb_push
  library(magrittr)
  fs::dir_ls("data") %>%
    pb_upload(repo = "cboettig/piggyback-tests", tag = "v0.0.1",
              show_progress = FALSE, overwrite = TRUE)

  # pb_pull
  pb_download(repo = "cboettig/piggyback-tests", tag = "v0.0.1",
              show_progress = FALSE)

  expect_true(TRUE)

  ## Should error if tag already exists
  expect_error(
    gh_new_release(repo = "cboettig/piggyback-tests", tag = "v0.0.1")
  )

  ## tare down
  setwd(cur)
  fs::dir_delete(tmp)


})

test_that("we can get a download url", {

  # public pat
  skip_if(gh::gh_token() == "b2b7441daeeb010b1df26f1f60a7f1edc485e443")
  skip_if(Sys.getenv("CBOETTIG_TOKEN") == "")

  x <- pb_download_url("iris.tsv.gz",
                       repo = "cboettig/piggyback-tests",
                       tag = "v0.0.1",
                       .token = gh::gh_token()
  )
  expect_is(x, "character")
})

test_that(
  "we error when creating a release on non-existant repo", {
    skip_on_cran()
    expect_error(
      pb_new_release("cboettig/not_a_repo", "v2.0"),
      "Cannot access release data"
    )
  }
)

testthat::test_that(
  "test delete", {

    testthat::skip_if(gh::gh_token() == "")
    testthat::skip_if(gh::gh_token() ==
                        "b2b7441daeeb010b1df26f1f60a7f1edc485e443")
    skip_if(Sys.getenv("CBOETTIG_TOKEN") == "")


    tmp <- tempdir()

    readr::write_tsv(datasets::mtcars, fs::path(tmp, "mtcars2.tsv.gz"))

    testthat::expect_silent(
      out <- pb_upload(
        file = fs::path(tmp, "mtcars2.tsv.gz"),
        repo = "cboettig/piggyback-tests",
        tag = "v0.0.1",
        show_progress = FALSE,
        dir = tmp
      )
    )

    pb_delete(
      repo = "cboettig/piggyback-tests",
      file = "mtcars2.tsv.gz",
      tag = "v0.0.1"
    )

    x <- pb_list(repo = "cboettig/piggyback-tests",
                 tag = "v0.0.1")
    expect_false("mtcars2.tsv.gz" %in% x$file_name)
    unlink(fs::path(tmp, "mtcars2.tsv.gz"))
  }
)


