## FIXME consider supporting tag as part of repo string: user/repo@tag


#' download_data("cboettig/ghdata")
#' @param repo Repository name in format "owner/repo".
#' @param file name or vector of names of files to be downloaded. If `NULL`,
#' all assets attached to the release will be downloaded.
#' @param dest name of vector of names of where file should be downloaded.
#' Should be a directory or a list of filenames the same length as `file` vector.
#' Can include paths to files, but any directories in that path must already exist.
#' @param tag tag for the GitHub release to which this data is attached
#' @param overwrite default `TRUE`, should any local files of the same name be overwritten?
#' @importFrom httr GET add_headers write_disk
#' @importFrom gh gh
#' @export
pb_download <- function(repo, file = NULL, dest = ".",
                        tag = "latest", overwrite = TRUE){

  x <- release_info(repo, tag)
  id <- vapply(x$assets, `[[`, integer(1), "id")
  file_names <-  vapply(x$assets, `[[`, character(1), "name")


  if(!is.null(file)){
    i <- which(file_names %in% file)
    id <- id[i]
  } else {
    file <- file_names
  }
  ## if dest not provided, we will write
  if(length(dest) <= 1){
    i <- which(file_names %in% file)
    ## FIXME make sure dest dir exists!
    dest <- file.path(dest, file_names[i])
  }

  for(i in seq_along(id)){
    resp <- gh_download_asset(x$owner,
                              x$repo,
                              id=id[i],
                              destfile = dest[i],
                              overwrite = overwrite)

    httr::stop_for_status(resp)
  }
  invisible(resp)
}

## gh() fails on this, so we do with httr. See https://github.com/r-lib/gh/issues/57
## Consider option to supress progress bar?
gh_download_asset <- function(owner, repo, id, destfile, overwrite=TRUE,
                              .token = get_token()
                              ){
  resp <- httr::GET(paste0("https://api.github.com/repos/", owner,"/",
                    repo, "/", "releases/assets/", id,
                    "?access_token=", .token),
                    httr::add_headers(Accept = "application/octet-stream"),
                    httr::write_disk(destfile, overwrite = overwrite),
                    httr::progress("down"))
  httr::stop_for_status(resp)
  invisible(resp)
}

##################### Generic helpers ##################
release_info <- function(repo, tag="latest"){
  r <- strsplit(repo, "/")[[1]]
  if(tag == "latest"){
    out <- gh("/repos/:owner/:repo/releases/latest",
       owner = r[[1]], repo = r[[2]])
  } else {
    out <- gh("/repos/:owner/:repo/releases/tags/:tag",
       owner = r[[1]], repo = r[[2]], tag = tag)
  }
  out$owner <- r[[1]]
  out$repo <-  r[[2]]
  out$tag <- tag
  out
}
get_token <- function(){
  Sys.getenv("GITHUB_PAT", Sys.getenv("GITHUB_TOKEN"))
}
#####################################################



#' Create a new release on GitHub repo
#'
#' @param repo Repository name in format "owner/repo".
#' @param tag tag to create for this release
#' @param commit Specifies the commitish value that
#'  determines where the Git tag is created from.
#'  Can be any branch or commit SHA. Unused if the
#'  Git tag already exists. Default: the repository's
#'  default branch (usually `master`).
#' @param name The name of the release. Defaults to tag.
#' @param body Text describing the contents of the tag.
#'  default text is "Data release".
#' @param draft default `FALSE`. Set to `TRUE` to create
#' a draft (unpublished) release.
#' @param prerelease default `FALSE`. Set to `TRUE` to
#' identify the release as a prerelease.
#' @inheritParams pb_upload
#' @export
#' @importFrom jsonlite toJSON
#' @importFrom httr content GET POST stop_for_status
#' @examples \dontrun{
#' gh_new_release("cboettig/piggyback", "v0.0.5")
#' }
gh_new_release <- function(repo,
                           tag,
                           commit = "master",
                           name = tag,
                           body = "Data release",
                           draft = FALSE,
                           prerelease = FALSE,
                           .token = get_token()){
  r <- strsplit(repo, "/")[[1]]

  payload <- list(
    tag_name = tag,
    target_commitish = commit,
    name = name,
    body = body,
    draft =  draft,
    prerelease = prerelease)

  ## gh fails to pass body correctly??
  #gh("/repos/:owner/:repo/releases", owner = r[[1]], repo = r[[2]],
  #   .method = "POST", body = toJSON(payload,auto_unbox = TRUE), encode="json")

  token <- .token
  resp <- httr::POST(paste0("https://api.github.com/repos/", r[[1]],"/",
                     r[[2]], "/", "releases?access_token=", token),
                     body = jsonlite::toJSON(payload,auto_unbox = TRUE))

  httr::stop_for_status(resp)
  release <- httr::content(resp)
}



#' Upload data to an existing release
#'
#' NOTE: you must first create a release if one does not already exists.
#' @param repo Repository name in format "owner/repo".
#' @param tag  tag for the GitHub release to which this data should be attached.
#' @param file path to file to be uploaded
#' @param name name for uploaded file. If not provided will use the basename of
#' `file` (i.e. filename without directory)
#' @param overwrite overwrite any existing file with the same name already
#'  attached to the on release?
#' @param .token GitHub authentication token. Typically set from an environmental
#' variable, e.g. `Sys.setenv(GITHUB_TOKEN = "xxxxx")`, which helps prevent
#' accidental disclosure of a secret token when sharing scripts.
#' @examples
#' \dontrun{
#' # Needs your real token to run
#' Sys.setenv(GITHUB_TOKEN = "xxxxx")
#'
#' readr::write_tsv(mtcars,"mtcars.tsv.xz")
#' ghdata_upload("cboettig/piggyback", "v0.0.3", "mtcars.tsv.xz")
#' }
#' @importFrom httr progress upload_file POST stop_for_status
#' @export
pb_upload <- function(repo,
                      file,
                      tag = "latest",
                      name = NULL,
                      overwrite = FALSE,
                      .token = get_token()){

  if(is.null(name)){
    name <- basename(file)
  }

  x <- release_info(repo, tag)

  if(overwrite){
    ## Get id for file
    filenames <- vapply(x$assets, `[[`, character(1), "name")
    ids <- vapply(x$assets, `[[`, integer(1), "id")

    if(name %in% filenames){
      i <- which(filenames == name)
      ## If we find matching id, Delete file from release.
      gh("DELETE /repos/:owner/:repo/releases/assets/:id",
         owner = x$owner, repo = x$repo, id = ids[i], .token = .token)
    }
  }


  r <- httr::POST(sub("\\{.+$", "", x$upload_url), query = list(name = name),
                  body = httr::upload_file(file), httr::progress("up"),
                  httr::authenticate(.token, "x-oauth-basic", "basic"))
  cat("\n")
  httr::stop_for_status(r)
  invisible(r)
}



## Helper routine:
## get the id of a file, or NA if file is not found in release assets
gh_file_id <- function(repo, file, tag = "latest", name = NULL){
  if(is.null(name)){
    name <- basename(file)
  }

  x <- release_info(repo, tag)

  filenames <- vapply(x$assets, `[[`, character(1), "name")
  ids <- vapply(x$assets, `[[`, integer(1), "id")
  if(name %in% filenames){
    i <- which(filenames == name)
    ids[i]
  } else {
    NA
  }

}


## consider plain delete function for upload
