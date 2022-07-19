#' Remove null elements of a list
#' @keywords internal
#' @noRd
compact <- function (l) Filter(Negate(is.null), l)

#' Parses repository spec and errors if it fails
#' @keywords internal
#' @noRd
parse_repo <- function(repo){
  r <- strsplit(repo, "/")[[1]]

  if (length(r) != 2) {
    cli::cli_abort(
      c("Could not parse {.val {repo}} as a GitHub repository.",
        "Make sure you have used the format: {.val owner/repo}")
    )
  }

  return(r)
}

#' Guesses GH repo based on git remote info for current git directory
#' @keywords internal
#' @noRd
guess_repo <- function(path = ".") {
  paste(gh::gh_tree_remote(path), collapse = "/")
}
