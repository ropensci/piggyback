
pb_pull <- function(datadir, tag = "latest", .repo = guess_repo()){
  pb_download(.repo, tag = tag, dest = datadir, overwrite = TRUE)

  ## Download the manifest.
  ## Check hashes against `datadir` hashes
  ## Download un-matched files
  ## remove manifest from local dir.

}


pb_push <- function(datadir = ".", pattern = NULL, tag = "latest",  .repo = guess_repo()){
  files <- list.files(datadir,
                      pattern = pattern,
                      all.files = TRUE,
                      full.names = TRUE,
                      recursive = TRUE)
  lapply(files, function(f){
    pb_upload(.repo, file = f, tag = tag, overwrite = TRUE)
  })
  ## write manifest of datadir
  ## upload data
  ## upload manifest
  ## delete manifest
}

#' @importFrom fs dir_ls
pb_track <- function(grob, path, all = TRUE, recursive = TRUE,
                     type = "any", invert = FALSE, regexp = NULL, ...){

  ## Update .gitignore list
  fs::dir_ls()
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
