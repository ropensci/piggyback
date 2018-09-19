#' Upload data to an existing release
#'
#' NOTE: you must first create a release if one does not already exists.
#' @param file path to file to be uploaded
#' @param repo Repository name in format "owner/repo". Will guess the current
#' repo if not specified.
#' @param tag  tag for the GitHub release to which this data should be attached.
#' @param name name for uploaded file. If not provided will use the basename of
#' `file` (i.e. filename without directory)
#' @param overwrite overwrite any existing file with the same name already
#'  attached to the on release?
#' @param use_timestamps logical, if `TRUE`, then files will only be downloaded
#' if timestamp on GitHub is newer than the local timestamp (if
#' `overwrite=TRUE`).  Defaults to `TRUE`.
#' @param show_progress logical, show a progress bar be shown for uploading?
#' Defaults to `TRUE`.
#' @param .token GitHub authentication token. Typically set from an
#'  environmental variable, e.g. in a `.Renviron` file or with
#'  `Sys.setenv(GITHUB_TOKEN = "xxxxx")`, which helps prevent accidental
#'   disclosure of a secret token when sharing scripts.
#' @param dir directory relative to which file names should be based.
#' @examples
#' \donttest{
#' # Needs your real token to run
#'
#' readr::write_tsv(mtcars,"mtcars.tsv.xz")
#' pb_upload("mtcars.tsv.xz", "cboettig/piggyback",
#'           "v0.0.3", overwrite = TRUE)
#' }
#' @importFrom httr progress upload_file POST stop_for_status
#' @export
#'
pb_upload <- function(file,
                      repo = guess_repo(),
                      tag = "latest",
                      name = NULL,
                      overwrite = FALSE,
                      use_timestamps = TRUE,
                      show_progress = TRUE,
                      .token = get_token(),
                      dir = ".") {
  out <- lapply(file, function(f)
    pb_upload_file(
      f,
      repo,
      tag,
      name,
      overwrite,
      use_timestamps,
      show_progress,
      .token,
      dir
    ))
  invisible(out)
}

pb_upload_file <- function(file,
                           repo = guess_repo(),
                           tag = "latest",
                           name = NULL,
                           overwrite = FALSE,
                           use_timestamps = TRUE,
                           show_progress = TRUE,
                           .token = get_token(),
                           dir = ".") {
  if (!file.exists(file)) {
    warning("file ", file, " does not exist")
    return(NULL)
  }
  progress <- httr::progress("up")
  if (!show_progress) {
    progress <- NULL
  }

  if (is.null(name)) {
    ## name is name on GitHub, technically need not be name of local file
    name <- fs::path_rel(file, start = dir)
  }

  df <- pb_info(repo, tag, .token)


  i <- which(df$file_name == name)

  if (length(i) > 0) { # File of same name is on GitHub

    if (use_timestamps) {
      local_timestamp <- fs::file_info(file)$modification_time

      no_update <- local_timestamp <= df[i, "timestamp"]
      if (no_update) {
        message(paste(
          "matching or more recent version of",
          file, "found on GitHub, not uploading"
        ))
        return(NULL)
      }
    }

    if (overwrite) {
      ## If we find matching id, Delete file from release.
      gh::gh("DELETE /repos/:owner/:repo/releases/assets/:id",
             owner = df$owner[[1]],
             repo = df$repo[[1]],
             id = df$id[i],
             .token = .token
      )
    } else {
      warning(paste(
        "Skipping upload of", df$file_name[i],
        "as file exists on GitHub",
        repo, "and overwrite = FALSE"
      ))
      return(NULL)
    }
  }


  r <- httr::POST(sub("\\{.+$", "", df$upload_url[[1]]),
                  query = list(name = asset_filename(name)),
                  body = httr::upload_file(file),
                  progress,
                  httr::authenticate(.token, "x-oauth-basic", "basic")
  )

  cat("\n")
  httr::stop_for_status(r)

  ## Release info changed, so break cache
  memoise::forget(memoised_pb_info)
  invisible(r)
}
