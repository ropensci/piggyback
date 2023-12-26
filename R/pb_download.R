#' Download data from an existing release
#'
#' @param file name or vector of names of files to be downloaded. If `NULL`,
#' all assets attached to the release will be downloaded.
#' @param dest name of vector of names of where file should be downloaded.
#' Can be a directory or a list of filenames the same length as `file`
#' vector. Any directories in the path provided must already exist.
#' @param overwrite Should any local files of the same name be overwritten?
#'  default `TRUE`.
#' @param ignore a list of files to ignore (if downloading "all" because
#'  `file=NULL`).
#' @inheritParams pb_upload
#'
#' @export
#' @examples \dontrun{
#'  ## Download a specific file.
#'  ## (dest can be omitted when run inside and R project)
#'  piggyback::pb_download("iris.tsv.gz",
#'                         repo = "cboettig/piggyback-tests",
#'                         dest = tempdir())
#' }
#' \dontrun{
#'  ## Download all files
#'  piggyback::pb_download(repo = "cboettig/piggyback-tests",
#'                         dest = tempdir())
#'
#' }
pb_download <- function(file = NULL,
                        dest = ".",
                        repo = guess_repo(),
                        tag = "latest",
                        overwrite = TRUE,
                        ignore = "manifest.json",
                        use_timestamps = TRUE,
                        show_progress = getOption("piggyback.verbose", default = interactive()),
                        .token = gh::gh_token()) {

  progress <- httr::progress("down")

  if (!show_progress) progress <- NULL

  df <- pb_info(repo, tag, .token)

  ## drop failed upload states from list
  df <- df[df$state != "starter",]

  if (!is.null(file)) {
    i <- which(df$file_name %in% file)
    if (length(i) < 1) {
      cli::cli_warn("file(s) {.file {file}} not found in repo {.val {repo}}")
    }

    df <- df[i, ]
  } else {
    i <- which(df$file_name %in% ignore)
    if (length(i) >= 1) {
      df <- df[-i, ]
    }
    file <- df$file_name
  }


  ## if dest paths are not provided, we will write all files to dest dir
  # User is responsible for making sure dest dir exists!
  if (length(dest) == 1) {
    i <- which(df$file_name %in% file)
    dest <- file.path(dest, df$file_name[i])
  }
  # dest should now be of length df
  df$dest <- dest


  if (use_timestamps) {
    local_timestamp <- fs::file_info(dest)$modification_time
    update <- df$timestamp > local_timestamp
    update[is.na(update)] <- TRUE # we'll download if missing locally
    df <- df[update, ]

    if (dim(df)[[1]] < 1) {
      cli::cli_alert_info("All local files already up-to-date!")
      return(invisible(NULL))
    }
  }

  resp <- lapply(seq_along(df$id), function(i)
    gh_download_asset(
      download_url = df$browser_download_url[i],
      destfile = df$dest[i],
      owner = df$owner[1],
      repo = df$repo[1],
      id = df$id[i],
      overwrite = overwrite,
      .token = .token,
      progress = progress
    ))
  return(invisible(resp))
}

## gh() fails on this, so we do with httr. See https://github.com/r-lib/gh/issues/57
## Consider option to suppress progress bar?
gh_download_asset <- function(download_url,
                              destfile,
                              owner,
                              repo,
                              id,
                              overwrite = TRUE,
                              .token = gh::gh_token(),
                              progress = httr::progress("down")) {
  if (fs::file_exists(destfile) && !overwrite) {
    cli::cli_warn(
      c("!"="{.val {destfile}} already exists, skipping download.",
        "Set {.code overwrite = TRUE} to overwrite files.")
    )
    return(NULL)
  }

  if (!is.null(progress)) {
    cli::cli_alert_info(
      "Downloading {.val {basename(destfile)}}..."
    )
  }

  auth_token <- if(!is.null(.token) && .token != "") {
    httr::add_headers(Authorization = paste("token", .token))
  }

  # Attempt download via browser download URL to avoid ratelimiting
  resp <- httr::RETRY(
    verb = "GET",
    url = download_url,
    httr::add_headers(Accept = "application/octet-stream"),
    auth_token,
    httr::write_disk(destfile, overwrite = overwrite),
    progress
  )

  # Fall back to api.github.com download if browser url returns http error
  if (httr::http_error(resp)){
    resp <- httr::RETRY(
      verb = "GET",
      url = paste0(
        "https://",
        "api.github.com/repos/", owner, "/",
        repo, "/", "releases/assets/", id
      ),
      httr::add_headers(Accept = "application/octet-stream"),
      auth_token,
      httr::write_disk(destfile, overwrite = overwrite),
      progress
    )

    # Try to use the redirection URL instead in case of "bad request"
    # See https://gist.github.com/josh-padnick/fdae42c07e648c798fc27dec2367da21
    if (resp$status_code == 400) {
      resp <- httr::RETRY(
        verb = "GET",
        url = resp$url,
        httr::add_headers(Accept = "application/octet-stream"),
        auth_token,
        httr::write_disk(destfile, overwrite = overwrite),
        progress
      )
    }
  }


  # warn if response not found
  if(getOption("piggyback.verbose", default = TRUE)) httr::warn_for_status(resp)

  invisible(resp)
}
