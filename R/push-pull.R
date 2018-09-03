## FIXME? Note that md5sums will only tell us that GitHub and local versions
## of a file with the same name are not identical.  It does not tell us which
## one is more recent.

## FIXME?: this approach assumes that manifest.json is not version managed,
## but is treated like another data asset attached only to the release.
## Alternately, this file could be committed to GitHub.  We could potentially
## then use `git` logic to decide if the local manifeset or GitHub manifest
## was more recent?
## Downside is that uploading any data would then also require a git commit.


#' Track data files of a given pattern or location
#'
#' @details Note: tracked patterns are simply written to `.pbattributes`
#' (analogous to `.gitattributes` in `git-lfs`.)  You can also edit this
#' file manually.  You will probably want to check in `.psattributes` to
#' as to version control., with `git add .psattributes`.  Note that
#' tracked file patterns will also be added to `.gitignore`.
#' @param glob vector of file names and/or glob pattern (e.g. `*.csv`, `data/*.csv`)
#' which will be tracked by piggyback.  Omit (default `NULL`) to just return
#' a list of files currently tracked.
#' @param repo_root repository root, will be guessed by `usethis` otherwise.
#' @importFrom usethis use_git_ignore proj_get
#' @importFrom fs path_join
#' @return list of tracked files (invisibly)
#' @export
#' @examples
#' \dontrun{
#' ## Track all .csv and .tsv files
#' pb_track(c("*.tsv", "*.tsv.gz"))
#'
#' }
pb_track <- function(glob = NULL, repo_root = usethis::proj_get()){

  if(!is.null(glob)){
    write_union(usethis::proj_get(),
                ".pbattributes",
                glob)
    usethis::use_build_ignore(c(".pbattributes", "manifest.json"))
    if(!is.null(git2r::discover_repository("."))){
      usethis::use_git_ignore("manifest.json")
      usethis::use_git_ignore(glob)
    }
  }


  pbattrs <- fs::path_rel(".pbattributes", repo_root)
  if (file.exists(pbattrs)) {
    glob <- readLines(pbattrs, warn = FALSE)
  } else {
    glob <- character(0L)
  }
  invisible(match_globs(glob, repo_root))
}

#' @importFrom fs dir_ls
#' @importFrom jsonlite write_json
#' @importFrom magrittr %>%
create_manifest <- function(manifest = "manifest.json"){

  proj_dir <- usethis::proj_get()
  files <- pb_track()


  hashes <- lapply(files, tools::md5sum)
  names(hashes) <- files

  json <- jsonlite::write_json(hashes,
                               file.path(proj_dir, manifest),
                               auto_unbox = TRUE,
                               pretty = TRUE)
  invisible(hashes)
}

## Helper function
#' @importFrom fs path_rel path_filter
match_globs <- function(globs, proj_dir = usethis::proj_get()){
  unique(
  unname(unlist(lapply(globs, function(g){
  ## only match files (i.e. things we can hash)
  fs::dir_ls(path = proj_dir, recursive = TRUE, type = "file") %>%
    fs::path_rel(proj_dir) %>%
    fs::path_filter(glob = g)
  }))))
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
                    show_progress = TRUE)
                    {

  # Get hashes of all tracked files
  create_manifest(manifest)
  proj_dir <- usethis::proj_get()

  ## List files that will be newly pulled
  files <- new_data(mode = "pull",
                    tag = tag,
                    manifest = manifest,
                    repo = repo)
  if(is.null(files)){
    message("Already up to date")
    return(invisible(TRUE))
  }
  if(length(files)==0){
    message("Already up to date")
    return(invisible(TRUE))
  }
  pb_download(repo = repo,
              tag = tag,
              file = files,
              dest = proj_dir,
              overwrite = overwrite,
              use_timestamps = use_timestamps,
              show_progress = show_progress)

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
                    show_progress = TRUE){

  create_manifest(manifest)
  dir <- usethis::proj_get()
  files <- new_data(mode = "push",
                    tag = tag,
                    manifest = manifest,
                    repo = repo)

  lapply(files, function(f){
    ## print file name being uploaded?
    message(paste(f))
    pb_upload(repo = repo,
              file = f,
              tag = tag,
              overwrite = overwrite,
              use_timestamps = use_timestamps,
              dir = dir)
  })

  ## Merge local manifest with GitHub manifest first.
  m <- file.path(usethis::proj_get(), basename(manifest))
  pb_upload(repo = repo,
            file = m,
            tag = tag,
            use_timestamps = FALSE,
            overwrite = TRUE,
            show_progress = show_progress,
            dir = dir)

  unlink(manifest)
  invisible(TRUE)
}



## Identify data that we do not need to sync because it has not changed.
new_data <- function(mode = c("push", "pull"),
                     repo = guess_repo(),
                     tag = "latest",
                     manifest = "manifest.json"){

  ## github name for files (i.e. manifest) cannot start with `.`
  mode <- match.arg(mode)
  id <- gh_file_id(repo = repo,
                   file = manifest,
                   tag = tag)

  ## If no manifest yet on GitHub, then nothing to exclude
  if(is.na(id)){
    github_manifest <- NULL
  } else {
    ## Read in the online manifest, silently and cleanly!
    tmp <- tempdir()
    pb_download(repo = repo,
                file = manifest,
                dest = tmp,
                tag = tag,
                ignore = "",
                use_timestamps = FALSE,
                show_progress = FALSE)
    github_manifest <- jsonlite::read_json(
      file.path(tmp, manifest))
    ## Tidy up
    unlink(file.path(tmp, manifest))
  }
  ## Read in local manifest
  m <- file.path(usethis::proj_get(), manifest)

  local_manifest <- jsonlite::read_json(m)

  if(mode == "push"){

    ## Return filenames of anything in local whose hash is not on github
    files <- names(local_manifest[ !c(local_manifest %in% github_manifest) ])

    ## Now update merge the manifests so the uploaded copy has both:
    gh_only <- github_manifest[!c(github_manifest %in% local_manifest)]
    merged <- c(local_manifest, gh_only)
    proj_dir <- usethis::proj_get()
    json <- jsonlite::write_json(merged,
                                 file.path(proj_dir, manifest),
                                 auto_unbox = TRUE,
                                 pretty = TRUE)

  } else if (mode == "pull"){

    ## Files whose hashes are on GitHub manifest but local manifest
    files <- names(github_manifest[!c(github_manifest %in% local_manifest)])
  }

  files

}


## Helper routine:
## get the id of a file, or NA if file is not found in release assets
gh_file_id <- function(repo, file, tag = "latest", name = NULL, .token = get_token()){
  if(is.null(name)){
    name <- asset_filename(file)
  }

  x <- release_info(repo, tag, .token)

  filenames <- vapply(x$assets, `[[`, character(1), "name")
  ids <- vapply(x$assets, `[[`, integer(1), "id")
  if(name %in% filenames){
    i <- which(filenames == name)
    ids[i]
  } else {
    NA
  }

}


