## FIXME? Note that md5sums will only tell us that GitHub and local versions
## of a file with the same name are not identical.  It does not tell us which
## one is more recent.

## FIXME?: this approach assumes that .manifest.json is not version managed,
## but is treated like another data asset attached only to the release.
## Alternately, this file could be committed to GitHub.  We could potentially
## then use `git` logic to decide if the local manifeset or GitHub manifest
## was more recent?
## Downside is that uploading any data would then also require a git commit.


#' Track data files of a given pattern or location
#'
#' @details Tracked files will be added to .gitignore
#' @inheritParams fs::dir_ls
#' @inheritParams pb_pull
#' @importFrom fs dir_ls
#' @importFrom usethis use_git_ignore proj_get
#' @importFrom jsonlite write_json
#' @return hashes list (invisibly)
#' @export
#' @examples
#' \donttest{
#' ## Track all .csv files
#' pb_track("*.csv")
#' }
pb_track <- function(glob, path = ".", all = TRUE, recursive = TRUE,
                     type = "any", invert = FALSE, regexp = NULL,
                     manifest = ".manifest.json",
                     ...){

  ## Update .gitignore list
  files <- fs::dir_ls(path = path, glob = glob, all = all,
                      recursive = recursive, type = type,
                      regexp = regexp, invert = invert, ...)


  if(!is.null(git2r::discover_repository("."))){
    usethis::use_git_ignore(manifest)
    usethis::use_git_ignore(files)
  }
  hashes <- lapply(files, tools::md5sum)

  ## Append to any existing manifest.  Update existing keys (files)
  previous <- NULL
  m <- file.path(usethis::proj_get(), basename(manifest))
  if(file.exists(m)){
    previous <- jsonlite::read_json(m, simplifyVector = FALSE)
  }

  hashes <- merge_hashes(hashes, previous)
  json <- jsonlite::write_json(hashes,
                               file.path(m),
                               auto_unbox = TRUE)
  invisible(hashes)
}



#' Pull data from GitHub
#'
#' Download any tracked datasets piggybacking on GitHub. Files identical on local
#' and remote versions will not be transfered.  Otherwise, **assumes
#' GitHub version should overwrite local versions.**
#'
#'
#' @param tag name of release/tag on GitHub to which data assets will be attached.
#' Default is to use the latest available release.
#' @param overwrite should existing files be overwritten when hashes do not match? default `TRUE`
#' @param .repo Name of the repo on GitHub (`owner/repo`, i.e. `cboettig/piggyback`).
#' By default will guess the current repository's GitHub `origin`.
#' @param manifest name of the local manifest file. Note: A leading dot (i.e. indicating a hidden file)
#' in the manifest name will be removed from the name used on the GitHub asset list.
#' @details Will only download tracked files, as identified by the manifest attached to the
#' requested release on GitHub. Add files to tracking with \code{\link{pb_track}} first and
#' push to GitHub with \code{\link{pb_push}}.
#'
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pb_pull()
#' }
pb_pull <- function(tag = "latest",
                    overwrite = TRUE,
                    .repo = guess_repo(),
                    manifest = ".manifest.json")
                    {

  # Make sure hashes reflect current files
  update_hashes(manifest)
  files <- new_data("pull", tag = tag, manifest = manifest, .repo = .repo)
  if(!is.null(files))
    pb_download(.repo, tag = tag, file = basename(files),
                dest = files, overwrite = overwrite)

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
#' @export
#'
#' @examples
#' \dontrun{
#' pb_push()
#' }
pb_push <- function(tag = "latest",
                    overwrite = TRUE,
                    manifest = ".manifest.json",
                    .repo = guess_repo()){


  update_hashes(manifest)

  files <- new_data("push", tag = tag, manifest = manifest, .repo = .repo)
  lapply(files, function(f){
    ## print file name being uploaded?
    message(paste(f))
    pb_upload(.repo, file = f, tag = tag, overwrite = overwrite)
  })

  ## Upload the manifest, quietly

  s <- tempfile()
  sink(s)
  ## manifest name cannot start with . in upload
  m <- file.path(usethis::proj_get(), basename(manifest))
  pb_upload(.repo, file = m,
            name = gsub("^\\.", "", manifest),
            tag = tag, overwrite = TRUE)

  ## tidy up
  unlink(s)
  sink()

  invisible(TRUE)
}









## Re-compute hashes for all files in manifest
update_hashes <- function(manifest = ".manifest.json"){

  m <- file.path(usethis::proj_get(), basename(manifest))

  if(!file.exists(m)){
    stop("No tracked files found. use pb_track()
         to indicate what data to piggyback")
  }

  local <- names(jsonlite::read_json(m))
  ## drop any files that no longer exist from the local manifest
  exists <- vapply(local, file.exists, logical(1))
  hashes <- lapply(local[exists], tools::md5sum)


  jsonlite::write_json(hashes,
                       m,
                       auto_unbox = TRUE)

}


## Identify data that we do not need to sync because it has not changed.
new_data <- function(mode = c("push", "pull"), tag = "latest",
                     manifest = ".manifest.json",
                     .repo = guess_repo()){
  ## github name for files (i.e. manifest) cannot start with `.`
  mode <- match.arg(mode)
  id <- gh_file_id(repo = .repo,
                   file = gsub("^\\.", "", manifest),
                   tag = tag)

  ## If no manifest yet on GitHub, then nothing to exclude
  if(is.na(id)){
    github_manifest <- NULL
  } else {
    ## Read in the online manifest
    tmp <- tempdir()
    ## do so silently!
    s <- tempfile()
    sink(s)

    ## here we go
    pb_download(repo = .repo,
                file = gsub("^\\.", "", manifest),
                dest = tmp, tag = tag)
    github_manifest <- jsonlite::read_json(
      file.path(tmp, gsub("^\\.", "", manifest)))

    ## Tidy up
    unlink(tmp)

    sink()
    unlink(s)
  }
  ## Read in local manifest
  m <- file.path(usethis::proj_get(), basename(manifest))

  local_manifest <- jsonlite::read_json(m)

  ## Return filenames of anything in local whose hash is not on github
  upload <- names(local_manifest[ !c(local_manifest %in% github_manifest) ])

  ## Return filenames of anything on github whose hash is not in local
  download <- names(github_manifest[!c(github_manifest %in% local_manifest)])

  switch(mode,
         "push" = upload,
         "pull" = download)

}




merge_hashes <- function(new, old){
  new_files <- names(new)
  old_files <- names(old)
  ## keep only old files not in new file list.
  c(new, old[!(old_files %in% new_files)])
}

#' @importFrom git2r remote_url repository discover_repository
guess_repo <- function(path = "."){
  addr <-
    git2r::remote_url(
      git2r::repository(
        git2r::discover_repository(path)))
  out <- gsub(".*[:|/]([^/]+/[^/]+)(?:\\.git$)?", "\\1", addr)
  gsub("\\.git$", "", out)
}

