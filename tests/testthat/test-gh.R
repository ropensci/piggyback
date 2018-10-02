context("utilities")

test_that("get token", {

  gh_token <- Sys.getenv("GITHUB_TOKEN")
  gh_pat <- Sys.getenv("GITHUB_PAT")
  Sys.unsetenv("GITHUB_PAT")
  Sys.unsetenv("GITHUB_TOKEN")

  expect_warning(piggyback:::get_token())

  # clean up. Setting to "" is not unsetting!
  if(gh_pat != "")
    Sys.setenv("GITHUB_PAT" = gh_pat)
  if(gh_token != "")
    Sys.setenv("GITHUB_TOKEN" = gh_token)

})



test_that("maybe", {

  piggyback:::maybe(1)
  expect_message(piggyback:::maybe(stop(),
                                   TRUE,
                                   quiet = FALSE),
                 "Error")


})
