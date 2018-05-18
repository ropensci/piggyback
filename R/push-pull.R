
pb_pull <- function(datadir, tag = "latest", .repo = guess_repo()){
  pb_download(.repo, tag = tag, dest = datadir, overwrite = TRUE)

  ## Download the manifest.
  ## Check hashes against `datadir` hashes
  ## Download un-matched files
  ## remove manifest from local dir.

}


pb_push <- function(..., tag = "latest",  .repo = guess_repo()){

  ## nope, don't call unless `...` length > 0.
  # hashes <- pb_track(...)
  ## Fixme needs to handle nicely if manifest doesn't yet exist

  ## Download to a tmp dir
  tmp <- tmpdir()
  pb_download(repo = .repo, file = ".piggyback_manifest.json",
              dest = tmp, tag = tag)
  jsonlite::read_json(file.path(tmp, ".piggyback_manifest.json"))

  ## Compare hashes


  lapply(files, function(f){
    pb_upload(.repo, file = f, tag = tag, overwrite = TRUE)
  })
  ## write manifest of datadir
  ## upload data
  ## upload manifest
  ## delete manifest
}

#' @importFrom fs dir_ls
#' @importFrom usetthis use_git_ignore
#' @importFrom jsonlite write_json
pb_track <- function(glob, path = ".", all = TRUE, recursive = TRUE,
                     type = "any", invert = FALSE, regexp = NULL, ...){

  ## Update .gitignore list
  files <- fs::dir_ls(path = path, glob = glob, all = all,
                      recursive = recursive, type = type,
                      regexp = regexp, invert = invert, ...)

  usethis::use_git_ignore(files)

  hashes <- lapply(files, tools::md5sum)

  ## Append to any existing manifest.  Update existing keys (files)
  ## using new hashes.
  current <- jsonlite::read_json(".piggyback_manifest.json", simplifyVector = FALSE)

  json <- jsonlite::write_json(hashes, ".piggyback_manifest.json", auto_unbox = TRUE)
  invisible(hashes)
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
