#' Check Test PAT
#'
#' We use a fine-grained GitHub Personal Access Token in our auth testing. This
#' function is memoised and checks that the token has the correct permissions
#' to the various test repositories.
#'
#' It should have the following token permissions:
#' - Contents (read/write)
#' - Metadata (read)
#'
#' on the following repositories
#' - tanho63/piggyback
#' - tanho63/piggyback-tests
#' - tanho63/piggyback-private (a private repository)
#'
#' @param test_token github token, typically stored at TAN_GH_TOKEN
#' @param test_repos repos used in testing
#'
#' @keywords internal
#' @return named vector of TRUE or FALSE as to whether the token is configured
#' and can access the test repos.
.check_test_token <- function(){
  test_token <-  Sys.getenv("TAN_GH_TOKEN", unset = "")
  test_repos <-  c("tanho63/piggyback",
                   "tanho63/piggyback-tests",
                   "tanho63/piggyback-private")

  if (test_token == "") {
    out <- setNames(FALSE, "envvar TAN_GH_TOKEN is not set")
    return(out)
  }

  checks <- sapply(
    test_repos,
    function(repo) {
      r <- httr::GET(
        paste0(.gh_api_url(),"/repos/",repo),
        httr::add_headers(Authorization = paste("token", test_token))
      )

      if (httr::http_error(r)) {
        msg <- paste(httr::status_code(r), "Could not access repo",repo, "with token TAN_GH_TOKEN")
        return(setNames(FALSE, msg))
      }

      if (!isTRUE(httr::content(r)$permissions$push)) {
        msg <- paste("Token TAN_GH_TOKEN does not have push permissions to", repo)
        return(setNames(FALSE, msg))
      }

      return(TRUE)
    },
    USE.NAMES = FALSE)

  return(checks)
}
