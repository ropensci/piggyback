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
#' @details Note: tracked patterns are simply written to `.pbattributes`
#' (analogous to `.gitattributes` in `git-lfs`.)  You can also edit this
#' file manually.  You will probably want to check in `.psattributes` to
#' as to version control., with `git add .psattributes`.  Note that
#' tracked file patterns will also be added to `.gitignore`.
#' @param glob vector of file names and/or glob pattern (e.g. `*.csv`, `data/*.csv`)
#' which will be tracked by piggyback.
#' @param repo_root repository root, will be guessed by `usethis` otherwise.
#' @importFrom usethis use_git_ignore proj_get
#' @importFrom fs path_join
#' @return input `globpath` (invisibly)
#' @export
#' @examples
#' \dontrun{
#' ## Track all .csv and .tsv files
#' pb_track(c("*.tsv", "*.tsv.gz"))
#'
#' }
pb_track <- function(glob, repo_root = usethis::proj_get()){

  write_union(usethis::proj_get(),
              ".pbattributes",
              glob)
  usethis::use_build_ignore(c(".pbattributes", ".manifest.json"))
  if(!is.null(git2r::discover_repository("."))){
    usethis::use_git_ignore(".manifest.json")
    usethis::use_git_ignore(glob)
  }
  invisible(glob)
}

#' @importFrom fs dir_ls
#' @importFrom jsonlite write_json
create_manifest <- function(manifest = ".manifest.json"){

  proj_dir <- usethis::proj_get()
  full_path <- fs::path_rel(file.path(proj_dir, ".pbattributes"))
  if (file.exists(full_path)) {
    glob <- readLines(full_path, warn = FALSE)
  } else {
    glob <- character()
  }

  list_globs <- function(g, start = "."){
    path <- fs::path_join(c(start, fs::path_rel(fs::path_dir(g))))
    glob <- fs::path_file(g)
    if(fs::dir_exists(path)){
    fs::path_rel(
      fs::dir_ls(path = path, glob = glob, all = TRUE,
               recursive = TRUE, type = "file"),
      start)
    } else {
      NULL
    }
  }

  files <- unname(unlist(lapply(glob, list_globs, proj_dir)))

  hashes <- lapply(files, tools::md5sum)
  names(hashes) <- files

  json <- jsonlite::write_json(hashes,
                               file.path(proj_dir, manifest),
                               auto_unbox = TRUE,
                               pretty = TRUE)
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
#' @details Will only download tracked files, as identified by the manifest
#'  attached to the requested release on GitHub. Add files to tracking with
#'  \code{\link{pb_track}} first and push to GitHub with \code{\link{pb_push}}.
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
                    manifest = ".manifest.json")
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
              overwrite = overwrite)

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
#' @export
#'
#' @examples
#' \dontrun{
#' pb_push()
#' }
pb_push <- function(repo = guess_repo(),
                    tag = "latest",
                    overwrite = TRUE,
                    manifest = ".manifest.json"){

  create_manifest(manifest)

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
              overwrite = overwrite)
  })

  ## Merge local manifest with GitHub manifest first.

  ## Upload the manifest, quietly

  s <- tempfile()
  sink(s)
  ## manifest name cannot start with . in upload
  m <- file.path(usethis::proj_get(), basename(manifest))
  pb_upload(repo = repo,
            file = m,
            name = gsub("^\\.", "", manifest),
            tag = tag,
            overwrite = TRUE)

  ## tidy up
  unlink(s)
  sink()

  unlink(manifest)
  invisible(TRUE)
}



## Identify data that we do not need to sync because it has not changed.
new_data <- function(mode = c("push", "pull"),
                     repo = guess_repo(),
                     tag = "latest",
                     manifest = ".manifest.json"){

  ## github name for files (i.e. manifest) cannot start with `.`
  mode <- match.arg(mode)
  id <- gh_file_id(repo = repo,
                   file = gsub("^\\.", "", manifest),
                   tag = tag)

  ## If no manifest yet on GitHub, then nothing to exclude
  if(is.na(id)){
    github_manifest <- NULL
  } else {
    ## Read in the online manifest, silently and cleanly!
    tmp <- tempdir()
    s <- tempfile()
    sink(s)
    ## here we go
    pb_download(repo = repo,
                file = gsub("^\\.", "", manifest),
                dest = tmp,
                tag = tag)
    github_manifest <- jsonlite::read_json(
      file.path(tmp, gsub("^\\.", "", manifest)))

    ## Tidy up
    unlink(tmp)
    sink()
    unlink(s)
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




#' @importFrom git2r remote_url repository discover_repository
guess_repo <- function(path = "."){
  addr <-
    git2r::remote_url(
      git2r::repository(
        git2r::discover_repository(path)))
  out <- gsub(".*[:|/]([^/]+/[^/]+)(?:\\.git$)?", "\\1", addr)
  gsub("\\.git$", "", out)
}

