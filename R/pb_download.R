#' Download data from an existing release
#'
#' @param file character: vector of names of files to be downloaded. If `NULL`,
#' all assets attached to the release will be downloaded.
#' @param dest character: path to destination directory (if length one) or
#' vector of destination filepaths the same length as `file`.
#' Any directories in the path provided must already exist.
#' @param overwrite boolean: should any local files of the same name be overwritten? default `TRUE`
#' @param ignore character: vector of files to ignore (used if downloading "all" via `file=NULL`)
#' @inheritParams pb_upload
#'
#' @export
#' @examples \donttest{
#'  try({ # this try block is to avoid errors on CRAN, not needed for normal use
#'    ## Download a specific file.
#'    ## (if dest is omitted, will write to current directory)
#'    dest <- tempdir()
#'    piggyback::pb_download(
#'      "iris.tsv.gz",
#'      repo = "cboettig/piggyback-tests",
#'      tag = "v0.0.1",
#'      dest = dest
#'    )
#'    list.files(dest)
#'    ## Download all files
#'    piggyback::pb_download(
#'      repo = "cboettig/piggyback-tests",
#'      tag = "v0.0.1",
#'      dest = dest
#'    )
#'    list.files(dest)
#'  })
#'  \dontshow{
#'    try(unlink(list.files(dest, full.names = TRUE)))
#'  }
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
  stopifnot(
    # length of dest should be either be one (dest directory) or same length as provided files
    is.character(dest) && (length(dest) == 1 || length(dest) == length(file))
  )

  progress <- httr::progress("down")
  if (!show_progress) progress <- NULL

  df <- pb_info(repo, tag, .token)

  ## drop failed upload states from list
  df <- df[df$state != "starter",]

  # if file is provided, filter pb_info to only those files
  if (!is.null(file)) {
    df <- df[which(df$file_name %in% file),]
    missing_files <- setdiff(file, df$file_name)
    if (length(missing_files) > 0) {
      cli::cli_warn("file(s) {.file {missing_files}} not found in repo {.val {repo}}")
    }
  }

  # if file is not provided, filter out ignored files from df
  if (is.null(file) && length(ignore) > 0) {
    df <- df[!df$file_name %in% ignore,]
  }

  # If dest == 1 we assume it is a destination directory and write all files
  # to this directory. Otherwise, if the length of dest is not the same as
  # files to download we will raise an error.
  #
  # User is responsible for making sure dest dir exists!
  if (length(dest) == 1) {
    dest <- .map_chr(df$file_name, function(file_name) file.path(dest,file_name))
  }

  # Set dest for each file
  df$dest <- dest

  if (use_timestamps) {
    local_timestamp <- fs::file_info(dest)$modification_time
    update <- df$timestamp > local_timestamp
    update[is.na(update)] <- TRUE # we'll download if missing locally
    df <- df[update, ]

    if (nrow(df) < 1) {
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
