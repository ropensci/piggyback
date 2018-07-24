
#' Download data from an existing release
#'
#' @param repo Repository name in format "owner/repo". Will guess
#' the current repo if not specified.
#' @param file name or vector of names of files to be downloaded. If `NULL`,
#' all assets attached to the release will be downloaded.
#' @param dest name of vector of names of where file should be downloaded.
#' Should be a directory or a list of filenames the same length as `file` vector.
#' Can include paths to files, but any directories in that path must already exist.
#' @param tag tag for the GitHub release to which this data is attached
#' @param overwrite Should any local files of the same name be overwritten? default `TRUE`.
#' @param ignore a list of files to ignore (if downloading "all" because `file=NULL`).
#' by default, "manifest.json" is ignored as this file is created and used only for
#' avoiding redundant file transfer by [pb_push()] and [pb_pull()]
#' @param use_timestamps If `TRUE`, then files will only be downloaded
#' if timestamp on GitHub is newer than the local timestamp (if `overwrite=TRUE`).
#' Defaults to `TRUE`.
#' @param show_progress logical, should we show progress bar for download? default TRUE.
#' @param .token GitHub authentication token. Typically set from an environmental
#' variable, e.g. in a `.Renviron` file or with `Sys.setenv(GITHUB_TOKEN = "xxxxx")`,
#' which helps prevent
#' accidental disclosure of a secret token when sharing scripts.
#' @importFrom httr GET add_headers write_disk
#' @importFrom gh gh
#' @importFrom fs dir_create
#' @export
#' @examples \donttest{
#'  ## Download a specific file:
#'  piggyback::pb_download("data/iris.tsv.gz", repo = "cboettig/piggyback")
#'
#'  ## Download all files
#'  piggyback::pb_download(repo = "cboettig/piggyback")
#' }
pb_download <- function(file = NULL,
                        dest = ".",
                        repo = guess_repo(),
                        tag = "latest",
                        overwrite = TRUE,
                        ignore = "manifest.json",
                        use_timestamps = TRUE,
                        show_progress = TRUE,
                        .token = get_token()){

  progress <- httr::progress("down")
  if(!show_progress){
    progress <- NULL
  }


  x <- release_info(repo, tag, .token)
  df <- rectangle_info(x)

  if(!is.null(file)){
    i <- which(df$file_name %in% file)
    if(length(i) < 1)
      stop(paste("file(s)",
                 paste(crayon::blue(file), collapse=" "),
                       "not found in repo",
                       crayon::blue(repo)))

    df <- df[i,]
  } else {
    i <- which(df$file_name %in% ignore)
    if(length(i) >= 1){
      df <- df[-i,]
    }
    file <- df$file_name
  }



  ## if dest not provided, we will write
  if(length(dest) == 1){
    i <- which(df$file_name %in% file)
    ## Make sure dest dir exists!
    dest <- fs::path_rel(file.path(dest, df$file_name[i]))
    fs::dir_create(fs::path_dir(dest))
  }

  if(use_timestamps){
    local_timestamp <- fs::file_info(dest)$modification_time
    update <- df$timestamp > local_timestamp
    update[is.na(update)] <- TRUE # we'll download if missing locally
    df <- df[update,]

    if(dim(df)[[1]] < 1){
      message(paste("All files up-to-date already\n"))
    }

  }

  lapply(seq_along(df$id), function(i)
    gh_download_asset(x$owner,
                      x$repo,
                      id = df$id[i],
                      destfile = dest[i],
                      overwrite = overwrite,
                      progress = progress)

  )
}


#' Get the download url of a given file
#'
#' Returns the URL download for a public file. This can be useful when writing
#' scripts that may want to download the file directly without introducing any
#' dependency on `piggyback` or authentication steps.
#' @inheritParams pb_download
#' @return the URL to download a file
#' @export
#' @examples \donttest{
#' pb_download_url("data/iris.tsv.gz", repo="cboettig/piggyback", tag = "v0.0.1")
#' }
pb_download_url <- function(file = NULL,
                            repo = guess_repo(),
                            tag = "latest",
                            .token = get_token()){


  x <- release_info(repo, tag, .token)


  gh_file_names <- vapply(x$assets, `[[`, character(1), "name")
  file_names <- local_filename(gh_file_names)

  if(!is.null(file)){
    i <- which(file_names %in% file)
  }
  urls <- vapply(x$assets, `[[`, character(1), "browser_download_url")
  urls[i]
}


## gh() fails on this, so we do with httr. See https://github.com/r-lib/gh/issues/57
## Consider option to supress progress bar?
gh_download_asset <- function(owner,
                              repo,
                              id,
                              destfile,
                              overwrite=TRUE,
                              .token = get_token(),
                              progress = httr::progress("down")
                              ){
  if(fs::file_exists(destfile) && !overwrite){
    warning(paste(destfile, "already exists, skipping download.",
                  "Set overwrite = TRUE to overwrite files."))
    return(NULL)
  }

  resp <- httr::GET(paste0("https://api.github.com/repos/", owner,"/",
                    repo, "/", "releases/assets/", id,
                    "?access_token=", .token),
                    httr::add_headers(Accept = "application/octet-stream"),
                    httr::write_disk(destfile, overwrite = overwrite),
                    progress)
  ## handle error cases? resp not found
  httr::stop_for_status(resp)
  invisible(resp)
}


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
#'  attached to the on release?
#' @param use_timestamps logical, if `TRUE`, then files will only be downloaded
#' if timestamp on GitHub is newer than the local timestamp (if `overwrite=TRUE`).
#' Defaults to `TRUE`. NOTE: GitHub only provides timestamps
#' @param show_progress logical, show a progress bar be shown for uploading? Default true.
#' @param .token GitHub authentication token. Typically set from an environmental
#' variable, e.g. in a `.Renviron` file or with `Sys.setenv(GITHUB_TOKEN = "xxxxx")`,
#' which helps prevent
#' accidental disclosure of a secret token when sharing scripts.
#' @examples
#' \donttest{
#' # Needs your real token to run
#'
#' readr::write_tsv(mtcars,"mtcars.tsv.xz")
#' pb_upload("mtcars.tsv.xz", "cboettig/piggyback",
#'           "v0.0.3", overwrite = TRUE)
#' }
#' @importFrom httr progress upload_file POST stop_for_status
#' @export
pb_upload <- function(file,
                      repo = guess_repo(),
                      tag = "latest",
                      name = NULL,
                      overwrite = FALSE,
                      use_timestamps = TRUE,
                      show_progress = TRUE,
                      .token = get_token()){
  progress <- httr::progress("up")
  if(!show_progress){
    progress <- NULL
  }

  if(is.null(name)){
    ## name is name on GitHub, technically need not be name of local file
    name <- asset_filename(file)
  }

  x <- release_info(repo, tag, .token)
  df <- rectangle_info(x)

  i <- which(df$file_name == file)

  if(length(i) > 0){ # File of same name is on GitHub

    if(use_timestamps){
      local_timestamp <- fs::file_info(file)$modification_time

      no_update <- local_timestamp <= df[i,"timestamp"]
      if(no_update){
        message(paste("matching or more recent version of",
                      file, "found on GitHub, not uploading"))
        return(NULL)
      }

    }

    if(overwrite){
      ## If we find matching id, Delete file from release.
      gh::gh("DELETE /repos/:owner/:repo/releases/assets/:id",
         owner = x$owner, repo = x$repo, id = df$id[i], .token = .token)
    } else {
      warning(paste("Skipping upload of", df$file_name[i], "as file exists on GitHub",
                    repo, "and overwrite = FALSE"))
      return(NULL)
    }
  }


  r <- httr::POST(sub("\\{.+$", "", x$upload_url),
                  query = list(name = name),
                  body = httr::upload_file(file),
                  progress,
                  httr::authenticate(.token, "x-oauth-basic", "basic"))

  cat("\n")
  httr::stop_for_status(r)
  invisible(r)
}

## Map local paths to valid names for GitHub assets
asset_filename <- function(x, start = "."){
  x <- fs::path_rel(x, start)
  x <- gsub("^\\.", "", x)
  # name relative to repo
  ## Cannot use %2f as html escape for slash
  gsub(.Platform$file.sep, ".2f", x)
}

local_filename <- function(x){
  #x <- gsub("^manifest.json$", ".manifest.json", x)

  gsub("\\.2f", .Platform$file.sep, x)
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


#' List all assets attached to a release
#' @inheritParams pb_download
#' @return a character vector of release asset names, (normalized to local paths)
#' @details To preserve path information, local path delimiters are converted to `.2f`
#' when files are uploaded as assets.  Listing will display the local filename,
#' with asset names converting the `.2f` escape code back to the system delimiter.
#' @examples
#' \dontrun{
#' pb_list("cboettig/piggyback")
#' }
#' @export
pb_list <- function(repo = guess_repo(),
                    tag="latest",
                    ignore = "manifest.json",
                    .token = get_token()){
  x <- release_info(repo, tag, .token)
  file_names <- vapply(x$assets, `[[`, character(1), "name")

  i <- which(file_names %in% ignore)
  if(length(i) > 0){
    file_names <- file_names[-i]
  }
  ## Should we report the tag name too? x$tag_name?
  local_filename(file_names)

}

#' Delete an asset attached to a release
#'
#' @inheritParams pb_upload
#' @return `TRUE` (invisibly) if a file is found and deleted.
#' Otherwise, returns `NULL` (invisibly) if no file matching the name was found.
#' @export
#' @examples
#' \donttest{
#' readr::write_tsv(mtcars, "mtcars.tsv.gz")
#' pb_upload("mtcars.tsv.gz", repo = "cboettig/piggyback",
#'           tag = "v0.0.3", overwrite = TRUE)
#' pb_delete("mtcars.tsv.gz", repo = "cboettig/piggyback", tag = "v0.0.3")
#' }
#'
pb_delete <- function(file,
                      repo = guess_repo(),
                      tag = "latest",
                      .token = get_token()){
  x <- release_info(repo, tag, .token)

  name <- asset_filename(file)

  filenames <- vapply(x$assets, `[[`, character(1), "name")
  ids <- vapply(x$assets, `[[`, integer(1), "id")

  out <- NULL
  if(name %in% filenames){
    i <- which(filenames == name)
    ## If we find matching id, Delete file from release.
    gh::gh("DELETE /repos/:owner/:repo/releases/assets/:id",
       owner = x$owner, repo = x$repo, id = ids[i], .token = .token)
    out <- TRUE
  } else {
      message(paste(name, "not found on GitHub"))
  }

  invisible(out)
}







##################### Generic helpers ##################



release_info <- function(repo = guess_repo(), tag="latest", .token = get_token()){

  ## Support error handling for: repo not found, tag not found,
  ## token issues

  r <- strsplit(repo, "/")[[1]]
  if(length(r) != 2){
    stop(paste("Could not parse", r, "as a repository",
               "Make sure you have used the format:",
               crayon::blue$bold("owner/repo")))
  }

  err <- paste0(
    "Cannot access release ", crayon::blue$bold(tag),
    " for repository ",
    crayon::blue$bold(paste0(r[[1]], "/", r[[2]])),
    ".",
    " Check that you have set a GITHUB_TOKEN and",
    " that a release named ", crayon::blue$bold(tag),
    " exists on your GitHub repository page."
  )


  ## FIXME: determine if repository exists separately from if tag
  ## exists, and handle the different errors explicitly.

  maybe({

    if(tag == "latest"){
      out <- gh("/repos/:owner/:repo/releases/latest",
                owner = r[[1]], repo = r[[2]], .token = .token)
    } else {
      out <- gh("/repos/:owner/:repo/releases/tags/:tag",
                owner = r[[1]], repo = r[[2]], tag = tag, .token = .token)
    }

  }, otherwise = stop(err))

  out$owner <- r[[1]]
  out$repo <-  r[[2]]
  out$tag <- tag
  out
}

#' @importFrom lubridate as_datetime
rectangle_info <- function(x){
data.frame(
  id = vapply(x$assets, `[[`, integer(1), "id"),
  file_name = local_filename(vapply(x$assets, `[[`,
                                    character(1), "name")),
  timestamp = lubridate::as_datetime(vapply(x$assets, `[[`,
                                character(1), "updated_at")),
  stringsAsFactors = FALSE)
}


get_token <- function(){
  Sys.getenv("GITHUB_PAT", Sys.getenv("GITHUB_TOKEN"))
}
#####################################################



#' Create a new release on GitHub repo
#'
#' @param repo Repository name in format "owner/repo". Will guess
#' the current repo if not specified.
#' @param tag tag to create for this release
#' @param commit Specifies the commit-ish value that
#'  determines where the Git tag is created from.
#'  Can be any branch or commit SHA. Unused if the
#'  git tag already exists. Default: the repository's
#'  default branch (usually `master`).
#' @param name The name of the release. Defaults to tag.
#' @param body Text describing the contents of the tag.
#'  default text is "Data release".
#' @param draft default `FALSE`. Set to `TRUE` to create
#' a draft (unpublished) release.
#' @param prerelease default `FALSE`. Set to `TRUE` to
#' identify the release as a pre-release.
#' @inheritParams pb_upload
#' @export
#' @importFrom jsonlite toJSON
#' @importFrom httr content GET POST stop_for_status
#' @examples \dontrun{
#' pb_new_release("cboettig/piggyback", "v0.0.5")
#' }
pb_new_release <- function(repo = guess_repo(),
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


maybe <- function(expr, otherwise, quiet = TRUE) {
  if (missing(otherwise)) {
    try(expr, silent = quiet)
  } else {
    tryCatch(expr,
             error = function(e) {
               if (!quiet)
                 message("Error: ", e$message)
               otherwise
             }
    )
  }
}
