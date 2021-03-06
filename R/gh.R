
## Map local paths to valid names for GitHub assets
asset_filename <- function(x, start = ".") {
  x <- fs::path_rel(x, start)
  x <- gsub("^\\.", "", x)
  # name relative to repo
  ## Cannot use %2f as html escape for slash
  gsub(.Platform$file.sep, ".2f", x)
}

local_filename <- function(x) {
  # x <- gsub("^manifest.json$", ".manifest.json", x)

  gsub("\\.2f", .Platform$file.sep, x)
}


##################### Generic helpers ##################
api_error_msg <- function(r) {
  paste0(
    "Cannot access release data for repository ",
    crayon::blue$bold(paste0(r[[1]], "/", r[[2]])),
    ".",
    " Check that you have set a GITHUB_TOKEN and",
    " that at least one release on your GitHub repository page."
  )
}





get_token <- function(warn = TRUE) {

  ## Not safe: doing Sys.setenv(GITHUB_PAT="") prevents default.
  ## Sys.unsetenv("GITHUB_PAT")
  #pat <- Sys.getenv("GITHUB_PAT", Sys.getenv("GITHUB_TOKEN"))


  pat <- Sys.getenv("GITHUB_PAT")
  if (pat == "") pat <- Sys.getenv("GITHUB_TOKEN")
  if (pat == "") {
    pat <- paste0(
      "b2b7441d", "aeeb010b", "1df26f1f6",
      "0a7f1ed", "c485e443"
    )
    if (warn) warning("Using default public GITHUB_TOKEN.
                     Please set your own token")
  }
  pat
}
#####################################################



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


#' @importFrom gert git_find git_info git_remote_list
guess_repo <- function(path = ".") {

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

  addr <- remotes[remotes[["name"]] == remote, "url"][["url"]]

  out <- gsub(".*[:|/]([^/]+/[^/]+)(?:\\.git$)?", "\\1", addr)
  gsub("\\.git$", "", out)
}
