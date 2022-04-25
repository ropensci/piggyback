
asset_filename <- function(x, start = ".") {
  ## piggyback will no longer embed file structure in filename
  ## Asset uploading is simply flat storage
  x
}

local_filename <- function(x) {
  # x <- gsub("^manifest.json$", ".manifest.json", x)

  gsub("\\.2f", .Platform$file.sep, x)
}


##################### Generic helpers ##################
api_error_msg <- function(r) {
  cli::cli_warn(
    c("!"="Cannot access release data for repo {.val {paste0(r[[1]], "/", r[[2]])}}.",
      "Check that you have provided a {code .token} and that there is at least one release on your repo"
  ))
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


guess_repo <- function(path = ".") {

  exists <- requireNamespace("gert", quietly = TRUE)
  if(!exists) stop(paste(
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

  addr <- remotes[remotes[["name"]] == remote, "url"][["url"]]

  out <- gsub(".*[:|/]([^/]+/[^/]+)(?:\\.git$)?", "\\1", addr)
  gsub("\\.git$", "", out)
}
