
pull <- function(datadir, tag = "latest"){

  ## Download the manifest.
  ## Check hashes against `datadir` hashes
  ## Download un-matched files
  ## remove manifest from local dir.

}

push <- function(datadir, tag = "latest"){
  ## write manifest of datadir
  ## upload data
  ## upload manifest
  ## delete manifest
}

## Alternately:: use updated_at date instead?

## A manifest is a small metadata file providing hashes
## This helps us avoid uploading or downloading possibly
## large files that have not changed.
write_manifest <- function(datadir){


}
