#' Remove null elements of a list
#' @keywords internal
#' @noRd
compact <- function (l) Filter(Negate(is.null), l)

#' trycatch/try wrapper
#' @keywords internal
#' @noRd
maybe <- function(expr, otherwise, quiet = TRUE) {
  if (missing(otherwise)) {
    try(expr, silent = quiet)
  } else {
    tryCatch(expr,
             error = function(e) {
               if (!quiet) {
                 message("Error: ", e$message)
               }
               otherwise
             }
    )
  }
}

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

  exists <- requireNamespace("gert", quietly = TRUE)
  if (!exists) stop(paste(
    "Install package 'gert' to let piggyback discover the",
    "current repo, or provide your repo name explicitly"))

  repo <- gert::git_find(path)
  remotes <- gert::git_remote_list(repo)
  remotes_names <- remotes[["name"]]

  # When there are more than 1 remote, we prefer "upstream"
  #   then "origin." If neither exists, we error to avoid
  #   ambiguity.
  remote <- if (length(remotes_names) > 1) {
    if ("upstream" %in% remotes_names) {
      "upstream"
    } else if ("origin" %in% remotes_names) {
      "origin"
    } else
      stop("Cannot infer repo, please provide `repo` explicitly.",
           call. = FALSE)
  } else {
    remotes_names
  }

  addr <- remotes$url[remotes$name == remote]

  out <- gsub(".*[:|/]([^/]+/[^/]+)(?:\\.git$)?", "\\1", addr)
  gsub("\\.git$", "", out)
}
