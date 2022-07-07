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

  repo_path <- file.path(tempdir(check = TRUE),"piggyback-tests")
  fs::dir_create(repo_path)

  gert::git_clone(url = "https://github.com/cboettig/piggyback-tests",
                  path = repo_path)

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
