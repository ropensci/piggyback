






#' @importFrom fs dir_ls
#' @importFrom jsonlite write_json
#' @importFrom magrittr %>%
create_manifest <- function(manifest = "manifest.json") {
  proj_dir <- usethis::proj_get()
  files <- pb_track()


  hashes <- lapply(files, tools::md5sum)
  names(hashes) <- files

  json <- jsonlite::write_json(hashes,
                               file.path(proj_dir, manifest),
                               auto_unbox = TRUE,
                               pretty = TRUE
  )
  invisible(hashes)
}


#' Pull data from GitHub
#'
#' Download any tracked datasets piggybacking on GitHub. Files identical on
#' local and remote versions will not be transferred.  Otherwise, **assumes
#' GitHub version should overwrite local versions.**
#'
#' @param repo Name of the repo on GitHub (`owner/repo`, i.e.
#' `cboettig/piggyback`). By default will guess the current repository's
#'  GitHub `origin`.
#' @param tag name of release/tag on GitHub to which data assets will be
#' attached. Default is to use the latest available release.
#' @param overwrite should existing files be overwritten when hashes do
#'  not match? default `TRUE`.
#' @param manifest name of the local manifest file. Note: A leading dot
#'  (i.e. indicating a hidden file) in the manifest name will be removed
#'  from the name used on the GitHub asset list.
#' @param use_timestamps If `TRUE`, then files will only be uploaded/downloaded
#' if timestamp of target is newer than the existing version.  Default is `FALSE`,
#' see details.
#' @param show_progress logical, should we show progress bar for download?
#' Defaults to `TRUE`.
#' @details
#'  [pb_pull()] Will only download tracked files, as identified by the manifest
#'  attached to the requested release on GitHub. Add files to tracking with
#'  [pb_track()] first and push to GitHub with [pb_push()].
#'
#'  By default, use_timestamps is false in [pb_pull()] and [pb_push()] since
#'  these methods are designed to use the manifest, which relies on hashes
#'  rather than timestamps to decide if a file has changed.  You can use
#'  timestamps and hashes together, but note that timestamps may not be
#'  as reliable, particularly if your files are being moved or copied
#'  some other way without actually being updated.
#'
#'  Hash comparisons can be more reliable, but unlike timestamps, are not
#'  directional -- we cannot tell which file is most recent.  Checking
#'  hashes therefore only ensures we not bother uploading or downloading
#'  a file identical to one we already have.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pb_pull()
#' }
pb_pull <- function(repo = guess_repo(),
                    tag = "latest",
                    overwrite = TRUE,
                    manifest = "manifest.json",
                    use_timestamps = FALSE,
                    show_progress = TRUE) {

  # Get hashes of all tracked files
  create_manifest(manifest)
  proj_dir <- usethis::proj_get()

  ## List files that will be newly pulled
  files <- new_data(
    mode = "pull",
    tag = tag,
    manifest = manifest,
    repo = repo
  )
  if (is.null(files)) {
    message("Already up to date")
    return(invisible(TRUE))
  }
  if (length(files) == 0) {
    message("Already up to date")
    return(invisible(TRUE))
  }
  pb_download(
    repo = repo,
    tag = tag,
    file = files,
    dest = proj_dir,
    overwrite = overwrite,
    use_timestamps = use_timestamps,
    show_progress = show_progress
  )

  unlink(manifest)

  invisible(TRUE)
}

#' Push data to GitHub
#'
#' Push all currently tracked data files to GitHub.  Only files identical
#' to those already on GitHub (by md5sum hash) will not be transferred.
#' Otherwise, **assumes local version should overwrite existing GitHub
#' version.** Create a new release if you do not want to overwrite previous
#' GitHub versions when pushing.
#'
#' @inheritParams pb_pull
#' @details Will only upload tracked files, as identified by the local
#' manifest.  Add files to tracking with \code{\link{pb_track}} first.
#'
#'  By default, use_timestamps is false in [pb_pull()] and [pb_push()] since
#'  these methods are designed to use the manifest, which relies on hashes
#'  rather than timestamps to decide if a file has changed.  You can use
#'  timestamps and hashes together, but note that timestamps may not be
#'  as reliable, particularly if your files are being moved or copied
#'  some other way without actually being updated.
#'
#'  Hash comparisons can be more reliable, but unlike timestamps, are not
#'  directional -- we cannot tell which file is most recent.  Checking
#'  hashes therefore only ensures we not bother uploading or downloading
#'  a file identical to one we already have.'
#' @export
#'
#' @examples
#' \dontrun{
#' pb_push()
#' }
pb_push <- function(repo = guess_repo(),
                    tag = "latest",
                    overwrite = TRUE,
                    manifest = "manifest.json",
                    use_timestamps = FALSE,
                    show_progress = TRUE) {
  create_manifest(manifest)
  dir <- usethis::proj_get()
  files <- new_data(
    mode = "push",
    tag = tag,
    manifest = manifest,
    repo = repo
  )

  lapply(files, function(f) {
    ## print file name being uploaded?
    message(paste(f))
    pb_upload(
      repo = repo,
      file = f,
      tag = tag,
      overwrite = overwrite,
      use_timestamps = use_timestamps,
      dir = dir
    )
  })

  ## Merge local manifest with GitHub manifest first.
  m <- file.path(usethis::proj_get(), basename(manifest))
  pb_upload(
    repo = repo,
    file = m,
    tag = tag,
    use_timestamps = FALSE,
    overwrite = TRUE,
    show_progress = show_progress,
    dir = dir
  )

  unlink(manifest)
  invisible(TRUE)
}



## Identify data that we do not need to sync because it has not changed.
new_data <- function(mode = c("push", "pull"),
                     repo = guess_repo(),
                     tag = "latest",
                     manifest = "manifest.json") {

  ## github name for files (i.e. manifest) cannot start with `.`
  mode <- match.arg(mode)
  id <- gh_file_id(
    repo = repo,
    file = manifest,
    tag = tag
  )

  ## If no manifest yet on GitHub, then nothing to exclude
  if (is.na(id)) {
    github_manifest <- NULL
  } else {
    ## Read in the online manifest, silently and cleanly!
    tmp <- tempdir()
    pb_download(
      repo = repo,
      file = manifest,
      dest = tmp,
      tag = tag,
      ignore = "",
      use_timestamps = FALSE,
      show_progress = FALSE
    )
    github_manifest <- jsonlite::read_json(
      file.path(tmp, manifest)
    )
    ## Tidy up
    unlink(file.path(tmp, manifest))
  }
  ## Read in local manifest
  m <- file.path(usethis::proj_get(), manifest)

  local_manifest <- jsonlite::read_json(m)

  if (mode == "push") {

    ## Return filenames of anything in local whose hash is not on github
    files <- names(local_manifest[ !c(local_manifest %in% github_manifest) ])

    ## Now update merge the manifests so the uploaded copy has both:
    gh_only <- github_manifest[!c(github_manifest %in% local_manifest)]
    merged <- c(local_manifest, gh_only)
    proj_dir <- usethis::proj_get()
    json <- jsonlite::write_json(merged,
                                 file.path(proj_dir, manifest),
                                 auto_unbox = TRUE,
                                 pretty = TRUE
    )
  } else if (mode == "pull") {

    ## Files whose hashes are on GitHub manifest but local manifest
    files <- names(github_manifest[!c(github_manifest %in% local_manifest)])
  }

  files
}


## Helper routine:
## get the id of a file, or NA if file is not found in release assets
gh_file_id <- function(repo,
                       file,
                       tag = "latest",
                       name = NULL,
                       .token = get_token()) {
  df <- pb_info(repo, tag, .token)

  df[df$file_name %in% file, "id"]
}
