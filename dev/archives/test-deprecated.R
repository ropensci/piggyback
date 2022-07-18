testthat::context("push/pull (deprecated)")


testthat::test_that("We can push and pull data", {

  # public pat
  testthat::skip_if(piggyback:::get_token() ==
                      "b2b7441daeeb010b1df26f1f60a7f1edc485e443")
  testthat::skip_if_not(as.logical(
    Sys.getenv("CBOETTIG_TOKEN", FALSE)))

  ##  Setup
  cur <- getwd()
  tmp <- fs::path(tempfile(), "pb_test")
  fs::dir_create(tmp)
  setwd(tmp)
  if (packageVersion("git2r") <= "0.21.0") {
    testthat::skip_on_os("mac")
  }

  sink(tempfile())
  usethis::create_from_github(
    repo = "cboettig/piggyback",
    destdir = ".",
    open = FALSE,
    protocol = "https"
  )

  setwd("piggyback")

  fs::dir_create("data")
  readr::write_tsv(datasets::mtcars, "data/mtcars.tsv.gz")
  readr::write_tsv(datasets::iris, "data/iris.tsv.xz")
  x <- pb_track("data/*")

  sink() # avoid verbose messages in test log. usethis msgs cannot turn off(?)


  testthat::expect_true(
    pb_push(repo = "cboettig/piggyback", tag = "v0.0.3", show_progress = FALSE)
  )
  testthat::expect_true(
    pb_pull(repo = "cboettig/piggyback", tag = "v0.0.3", show_progress = FALSE)
  )

  testthat::expect_true(pb_pull(show_progress = FALSE))
  testthat::expect_true(pb_push(show_progress = FALSE))
  pb_push(show_progress = FALSE)
  pb_pull(show_progress = FALSE)

  testthat::expect_true(pb_pull(tag = "v0.0.3", show_progress = FALSE))

  ## Should error if tag already exists
  testthat::expect_error(
    gh_new_release(repo = "cboettig/piggyback", tag = "v0.0.3")
  )

  ## tare down
  setwd(cur)
  fs::dir_delete(tmp)
})
