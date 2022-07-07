context("utilities")

test_that("compact works",{
  expect_equal(
    compact(list("a",NULL,"b")),
    list("a","b")
  )
})

test_that("guess_repo works",{
  skip_if_offline("api.github.com")
  skip_if_not_installed("usethis")

  temp_dir <- tempdir(check = TRUE)

  usethis::create_from_github(
    repo_spec = "cboettig/piggyback-tests",
    destdir = temp_dir,
    fork = FALSE,
    rstudio = FALSE,
    open = FALSE,
    protocol = "https"
  )

  repo_path <- file.path(temp_dir,"piggyback-tests")
  on.exit(unlink(repo_path, recursive = TRUE))

  expect_equal(
    guess_repo(repo_path),
    "cboettig/piggyback-tests"
  )

})

test_that("parse_repo works",{
  expect_equal(
    parse_repo("cboettig/piggyback-tests"),
    c("cboettig","piggyback-tests")
  )
  expect_error(
    parse_repo("cboettig-piggyback-tests"),
    "Could not parse"
  )
})
