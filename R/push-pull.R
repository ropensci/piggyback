


pb_pull <- function(tag = "latest",  manifest = ".manifest.json",
                    .repo = guess_repo()){

  # Make sure hashes reflect current files
  update_hashes(manifest)

  files <- new_data("pull", tag = tag, manifest = manifest, .repo = .repo)
  pb_download(.repo, tag = tag, file = basename(files), dest = files, overwrite = TRUE)

  ## Download the manifest.
  ## Check hashes against `datadir` hashes
  ## Download un-matched files
  ## remove manifest from local dir.

}


pb_push <- function(tag = "latest",  manifest = ".manifest.json",
                    .repo = guess_repo()){

  update_hashes(manifest)

  files <- new_data("push", tag = tag, manifest = manifest, .repo = .repo)
  lapply(files, function(f){
    pb_upload(.repo, file = f, tag = tag, overwrite = TRUE)
  })

  ## manifest name cannot start with . in upload
  pb_upload(.repo, file = gsub("^\\.", "", manifest),
            tag = tag, overwrite = TRUE)
}


## Re-compute hashes for all files in manifest
update_hashes <- function(manifest = ".manifest.json"){

  if(!file.exists(manifest)){
    stop("No tracked files found. use pb_track() to indicate what data to piggyback")
  }

  local <- names(jsonlite::read_json(manifest))
  ## drop any files that no longer exist from the local manifest
  exists <- vapply(local, file.exists, logical(1))
  hashes <- lapply(local[exists], tools::md5sum)
  jsonlite::write_json(hashes, manifest, auto_unbox = TRUE)

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
    pb_download(repo = .repo,
                file = gsub("^\\.", "", manifest),
                dest = tmp, tag = tag)
    github_manifest <- jsonlite::read_json(
      file.path(tmp, gsub("^\\.", "", manifest)))
    unlink(tmp)
  }
  ## Read in local manifest
  local_manifest <- jsonlite::read_json(manifest)

  ## Return filenames of anything in local whose hash is not on github
  upload <- names(local_manifest[ !c(local_manifest %in% github_manifest) ])

  ## Return filenames of anything on github whose hash is not in local
  download <- names(github_manifest[!c(github_manifest %in% local_manifest)])

  switch(mode,
         "push" = upload,
         "pull" = download)

}


#' @importFrom fs dir_ls
#' @importFrom usethis use_git_ignore
#' @importFrom jsonlite write_json
pb_track <- function(glob, path = ".", all = TRUE, recursive = TRUE,
                     type = "any", invert = FALSE, regexp = NULL,
                     manifest = ".manifest.json",
                     ...){

  ## Update .gitignore list
  files <- fs::dir_ls(path = path, glob = glob, all = all,
                      recursive = recursive, type = type,
                      regexp = regexp, invert = invert, ...)

  #usethis::use_build_ignore(manifest) might not want to create Rbuildignore if none exists
  usethis::use_git_ignore(manifest)
  usethis::use_git_ignore(files)

  hashes <- lapply(files, tools::md5sum)

  ## Append to any existing manifest.  Update existing keys (files)
  previous <- NULL
  if(file.exists(manifest)){
    previous <- jsonlite::read_json(manifest, simplifyVector = FALSE)
  }

  hashes <- merge_hashes(hashes, previous)
  json <- jsonlite::write_json(hashes, manifest, auto_unbox = TRUE)
  invisible(hashes)
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
  gsub(".*[:|/](\\w+/\\w+)(?:\\.git$)?", "\\1", addr)
}


## Alternately:: use updated_at date instead?
## Not as robust though

## A manifest is a small metadata file providing hashes
## This helps us avoid uploading or downloading possibly
## large files that have not changed.
write_manifest <- function(datadir){


}
