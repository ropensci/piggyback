#' Download data from an existing release
#'
#' @param repo Repository name in format "owner/repo". Will guess
#' the current repo if not specified.
#' @param file name or vector of names of files to be downloaded. If `NULL`,
#' all assets attached to the release will be downloaded.
#' @param dest name of vector of names of where file should be downloaded.
#' Should be a directory or a list of filenames the same length as `file`
#' vector. Can include paths to files, but any directories in that path
#' must already exist.
#' @param tag tag for the GitHub release to which this data is attached
#' @param overwrite Should any local files of the same name be overwritten?
#'  default `TRUE`.
#' @param ignore a list of files to ignore (if downloading "all" because
#'  `file=NULL`).
#' @param use_timestamps If `TRUE`, then files will only be downloaded
#' if timestamp on GitHub is newer than the local timestamp (if
#' `overwrite=TRUE`).  Defaults to `TRUE`.
#' @param show_progress logical, should we show progress bar for download?
#' Defaults to `TRUE`.
#' @param .token GitHub authentication token. Typically set from an
#' environmental variable, e.g. in a `.Renviron` file or with
#' `Sys.setenv(GITHUB_TOKEN = "xxxxx")`, which helps prevent
#' accidental disclosure of a secret token when sharing scripts.
#' @importFrom httr GET add_headers write_disk
#' @importFrom gh gh
#' @importFrom fs dir_create
#' @export
#' @examples \donttest{
#'  ## Download a specific file.
#'  ## (dest can be omitted when run inside and R project)
#'  piggyback::pb_download("data/iris.tsv.gz",
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
                        dest = usethis::proj_get(),
                        repo = guess_repo(),
                        tag = "latest",
                        overwrite = TRUE,
                        ignore = "manifest.json",
                        use_timestamps = TRUE,
                        show_progress = TRUE,
                        .token = get_token()) {
  progress <- httr::progress("down")
  if (!show_progress) {
    progress <- NULL
  }


  df <- pb_info(repo, tag, .token)

  ## drop failed upload states from list
  df <- df[df$state != "starter",]

  if (!is.null(file)) {
    i <- which(df$file_name %in% file)
    if (length(i) < 1) {
      warning(paste(
        "file(s)",
        paste(crayon::blue(file), collapse = " "),
        "not found in repo",
        crayon::blue(repo)
      ))
    }

    df <- df[i, ]
  } else {
    i <- which(df$file_name %in% ignore)
    if (length(i) >= 1) {
      df <- df[-i, ]
    }
    file <- df$file_name
  }


  ## if dest not provided, we will write
  if (length(dest) == 1) {
    i <- which(df$file_name %in% file)
    ## Make sure dest dir exists!
    dest <- fs::path_rel(file.path(dest, df$file_name[i]))
    fs::dir_create(fs::path_dir(dest))
  }
  # dest should now be of length df
  df$dest <- dest


  if (use_timestamps) {
    local_timestamp <- fs::file_info(dest)$modification_time
    update <- df$timestamp > local_timestamp
    update[is.na(update)] <- TRUE # we'll download if missing locally
    df <- df[update, ]

    if (dim(df)[[1]] < 1) {
      message(paste("All files up-to-date already\n"))
    }
  }

  resp <- lapply(seq_along(df$id), function(i)
    gh_download_asset(df$owner[[1]],
                      df$repo[[1]],
                      id = df$id[i],
                      destfile = dest[i],
                      overwrite = overwrite,
                      progress = progress
    ))
  invisible(resp)
}




## gh() fails on this, so we do with httr. See https://github.com/r-lib/gh/issues/57
## Consider option to supress progress bar?
gh_download_asset <- function(owner,
                              repo,
                              id,
                              destfile,
                              overwrite = TRUE,
                              .token = get_token(),
                              progress = httr::progress("down")) {
  if (fs::file_exists(destfile) && !overwrite) {
    warning(paste(
      destfile, "already exists, skipping download.",
      "Set overwrite = TRUE to overwrite files."
    ))
    return(NULL)
  }

  if (!is.null(progress)) {
    message(paste("downloading", basename(destfile), "..."))
  }

  resp <- httr::GET(
    paste0(
      "https://",
      "api.github.com/repos/", owner, "/",
      repo, "/", "releases/assets/", id#,
      #"?access_token=", .token
    ),
    httr::add_headers(Authorization = paste0("token ",.token), Accept = "application/octet-stream"),
    httr::write_disk(destfile, overwrite = overwrite),
    progress
  )
  ## handle error cases? resp not found
  httr::stop_for_status(resp)
  invisible(resp)
}
