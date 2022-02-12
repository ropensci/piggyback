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
#'  attached to the on release? Default behavior is based on timestamps,
#'  only overwriting those files which are older.
#' @param use_timestamps DEPRECATED.
#' @param show_progress logical, show a progress bar be shown for uploading?
#' Defaults to `TRUE`.
#' @param .token GitHub authentication token, see `[gh::gh_token()]`
#' @param dir directory relative to which file names should be based, defaults to NULL for current working directory.
#' @examples
#' \dontrun{
#' # Needs your real token to run
#'
#' readr::write_tsv(mtcars,"mtcars.tsv.xz")
#' pb_upload("mtcars.tsv.xz", "cboettig/piggyback-tests")
#' }
#' @importFrom httr progress upload_file POST stop_for_status
#' @export
#'
pb_upload <- function(file,
                      repo = guess_repo(),
                      tag = "latest",
                      name = NULL,
                      overwrite = "use_timestamps",
                      use_timestamps = NULL,
                      show_progress = TRUE,
                      .token = get_token(),
                      dir = NULL) {

  ## start fresh
  memoise::forget(memoised_pb_info)

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

  ## break cache when done
  memoise::forget(memoised_pb_info)
  invisible(out)
}

pb_upload_file <- function(file,
                           repo = guess_repo(),
                           tag = "latest",
                           name = NULL,
                           overwrite = "use_timestamps",
                           use_timestamps = NULL,
                           show_progress = TRUE,
                           .token = get_token(),
                           dir = NULL) {

  file_path <- do.call(file.path, compact(list(dir,file)))
  ## Uses NULL as default dir, drops it with compact, then
  ## does the file.path call with what's left
  ##
  ## This is better than using "." as default dir because if you try to pass an
  ## absolute path with "." e.g. file.path(".","C:/Users/Tan") it will
  ## return "./C:/Users/Tan" which is not desired.

  if (!file.exists(file_path)) {
    warning("file ", file_path, " does not exist")
    return(NULL)
  }

  if(!is.null(use_timestamps)){
    warning(paste("use_timestamps argument is deprecated",
                  "please set overwrite='use_timestamps'",
                  "instead."))
  }


  ## Yeah, having two separate arguments was clearly a mistake!
  ## Code has been partially refactored now so that user just
  ## sets `overwrite` and we handle the twisted logic internally here:
  use_timestamps <- switch (as.character(overwrite),
    "TRUE" = FALSE,
    "FALSE" = FALSE,
    "use_timestamps" = TRUE
  )
  overwrite <- switch (as.character(overwrite),
    "TRUE" = TRUE,
    "FALSE" = FALSE,
    "use_timestamps" = TRUE
  )

  progress <- httr::progress("up")
  if (!show_progress) {
    progress <- NULL
  }

  if (is.null(name)) {
    ## name is name on GitHub, technically need not be name of local file
    name <- basename(file_path)
  }

  ## memoised for piggyback_cache_duration
  df <- pb_info(repo, tag, .token)

  i <- which(df$file_name == name)

  if (length(i) > 0) { # File of same name is on GitHub

    if (use_timestamps) {
      local_timestamp <- fs::file_info(file_path)$modification_time

      no_update <- local_timestamp <= df[i, "timestamp"]
      if (no_update) {
        message(paste(
          "matching or more recent version of",
          file_path, "found on GitHub, not uploading"
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

  if (!is.null(progress)) {
    message(paste("uploading", name, "..."))
  }

  r <- httr::POST(sub("\\{.+$", "", df$upload_url[[1]]),
                  query = list(name = asset_filename(name)),
                  httr::add_headers(Authorization = paste("token", .token)),
                  body = httr::upload_file(file_path),
                  progress
  )

  cat("\n")
  if(getOption("verbose")) httr::warn_for_status(r)

  ## Release info changed, so break cache
  # memoise::forget(memoised_pb_info)
  invisible(r)
}
