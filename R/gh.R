
#' Download data from an existing release
#'
#' @param repo Repository name in format "owner/repo". Will guess
#' the current repo if not specified.
#' @param file name or vector of names of files to be downloaded. If `NULL`,
#' all assets attached to the release will be downloaded.
#' @param dest name of vector of names of where file should be downloaded.
#' Should be a directory or a list of filenames the same length as `file`
#' vector. Can include paths to files, but any directories in that path
#' must already exist.
#' @param tag tag for the GitHub release to which this data is attached
#' @param overwrite Should any local files of the same name be overwritten?
#'  default `TRUE`.
#' @param ignore a list of files to ignore (if downloading "all" because
#'  `file=NULL`). By default, "manifest.json" is ignored as this file is
#'   created and used only for avoiding redundant file transfer by [pb_push()]
#'   and [pb_pull()]
#' @param use_timestamps If `TRUE`, then files will only be downloaded
#' if timestamp on GitHub is newer than the local timestamp (if
#' `overwrite=TRUE`).  Defaults to `TRUE`.
#' @param show_progress logical, should we show progress bar for download?
#' Defaults to `TRUE`.
#' @param .token GitHub authentication token. Typically set from an
#' environmental variable, e.g. in a `.Renviron` file or with
#' `Sys.setenv(GITHUB_TOKEN = "xxxxx")`, which helps prevent
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
                        dest = usethis::proj_get(),
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


  df <- pb_info(repo, tag, .token)

  if(!is.null(file)){
    i <- which(df$file_name %in% file)
    if(length(i) < 1)
      warning(paste("file(s)",
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
  # dest should now be of length df
  df$dest <- dest


  if(use_timestamps){
    local_timestamp <- fs::file_info(dest)$modification_time
    update <- df$timestamp > local_timestamp
    update[is.na(update)] <- TRUE # we'll download if missing locally
    df <- df[update,]

    if(dim(df)[[1]] < 1){
      message(paste("All files up-to-date already\n"))
    }

  }

  resp <- lapply(seq_along(df$id), function(i)
    gh_download_asset(df$owner[[1]],
                      df$repo[[1]],
                      id = df$id[i],
                      destfile = dest[i],
                      overwrite = overwrite,
                      progress = progress)

  )
 invisible(resp)
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
#'
#' pb_download_url("data/iris.tsv.gz",
#'                 repo = "cboettig/piggyback",
#'                 tag = "v0.0.1")
#'
#' }
pb_download_url <- function(file = NULL,
                            repo = guess_repo(),
                            tag = "latest",
                            .token = get_token()){


  df <- pb_info(repo, tag, .token)
  if(is.null(file)){
    return(df$browser_download_url)
  }else if(file %in% df$file_name){
    return(df[file == df$file_name, "browser_download_url"])
  } else {
    stop(paste("file", file, "not found in release", tag, "for repo", repo))
  }


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
#' if timestamp on GitHub is newer than the local timestamp (if
#' `overwrite=TRUE`).  Defaults to `TRUE`.
#' @param show_progress logical, show a progress bar be shown for uploading?
#' Defaults to `TRUE`.
#' @param .token GitHub authentication token. Typically set from an
#'  environmental variable, e.g. in a `.Renviron` file or with
#'  `Sys.setenv(GITHUB_TOKEN = "xxxxx")`, which helps prevent accidental
#'   disclosure of a secret token when sharing scripts.
#' @param dir directory relative to which file names should be based.
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
#'
pb_upload <- function(file,
                      repo = guess_repo(),
                      tag = "latest",
                      name = NULL,
                      overwrite = FALSE,
                      use_timestamps = TRUE,
                      show_progress = TRUE,
                      .token = get_token(),
                      dir = "."){
  out <- lapply(file, function(f)
                pb_upload_file(f,
                        repo,
                        tag,
                        name,
                        overwrite,
                        use_timestamps,
                        show_progress,
                        .token,
                        dir))
  invisible(out)

}

pb_upload_file <- function(file,
                      repo = guess_repo(),
                      tag = "latest",
                      name = NULL,
                      overwrite = FALSE,
                      use_timestamps = TRUE,
                      show_progress = TRUE,
                      .token = get_token(),
                      dir = "."){

  if(!file.exists(file)){
    warning ("file ", file, " does not exist")
    return(NULL)
  }
  progress <- httr::progress("up")
  if(!show_progress){
    progress <- NULL
  }

  if(is.null(name)){
    ## name is name on GitHub, technically need not be name of local file
    name <- fs::path_rel(file, start = dir)
  }

  df <- pb_info(repo, tag, .token)


  i <- which(df$file_name == name)

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
         owner = df$owner[[1]], repo = df$repo[[1]], id = df$id[i], .token = .token)
    } else {
      warning(paste("Skipping upload of", df$file_name[i],
                    "as file exists on GitHub",
                    repo, "and overwrite = FALSE"))
      return(NULL)
    }
  }


  r <- httr::POST(sub("\\{.+$", "", df$upload_url[[1]]),
                  query = list(name = asset_filename(name)),
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


#' List all assets attached to a release
#' @inheritParams pb_download
#' @param tag which release tag do we want information for? If `NULL` (default),
#' will return a table for all available release tags.
#' @return a data.frame of release asset names, (normalized to local paths), release tag,
#' timestamp, owner, and repo.
#' @details To preserve path information, local path delimiters are converted to `.2f`
#' when files are uploaded as assets.  Listing will display the local filename,
#' with asset names converting the `.2f` escape code back to the system delimiter.
#' @examples
#' \dontrun{
#' pb_list("cboettig/piggyback")
#' }
#' @export
pb_list <- function(repo = guess_repo(),
                    tag = NULL,
                    ignore = "manifest.json",
                    .token = get_token()){
  df <- pb_info(repo, tag, .token)
  df[c("file_name", "tag", "timestamp", "owner", "repo")]
}

#' Delete an asset attached to a release
#'
#' @inheritParams pb_upload
#' @param file file(s) to be deleted from the release. If `NULL` (default
#' when argument is ommitted), function will delete all attachments to the release.
#' delete
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
pb_delete <- function(file = NULL,
                      repo = guess_repo(),
                      tag = "latest",
                      .token = get_token()){

  df <- pb_info(repo, tag, .token)

  if(is.null(file)){
    ids <- df$id
  } else {
    ids <- df[df$file_name %in% file, "id"]
  }

  if(length(ids) < 1){
    message(paste(file, "not found on GitHub"))
    return(NULL)
  }

  lapply(ids, function(id){
    ## If we find matching id, Delete file from release.
    gh::gh("DELETE /repos/:owner/:repo/releases/assets/:id",
       owner = df$owner[[1]], repo = df$repo[[1]], id = id, .token = .token)
  })
  invisible(TRUE)
}







##################### Generic helpers ##################
api_error_msg <- function(r){
  paste0(
  "Cannot access release data for repository ",
  crayon::blue$bold(paste0(r[[1]], "/", r[[2]])),
  ".",
  " Check that you have set a GITHUB_TOKEN and",
  " that at least one release on your GitHub repository page."
  )
}



release_info <- function(repo = guess_repo(), .token = get_token()){
  r <- strsplit(repo, "/")[[1]]
  if(length(r) != 2){
    stop(paste("Could not parse", r, "as a repository",
               "Make sure you have used the format:",
               crayon::blue$bold("owner/repo")))
  }
  releases <- maybe(gh::gh("/repos/:owner/:repo/releases",
                           owner = r[[1]], repo = r[[2]], .token = .token),
                    otherwise = stop(api_error_msg(r)))
  releases
}


pb_info <- function(repo = guess_repo(), tag = NULL, .token = get_token()){

  releases <- release_info(repo, .token)
  r <- strsplit(repo, "/")[[1]]

  info <-
    do.call(rbind,
            lapply(releases,
             function(x){
              data.frame(
                file_name = local_filename(
                  vapply(x$assets, `[[`, character(1), "name")),
                tag = x$tag_name,
                timestamp = lubridate::as_datetime(
                  vapply(x$assets, `[[`, character(1), "updated_at")),
                owner = r[[1]],
                repo = r[[2]],
                upload_url = x$upload_url,
                browser_download_url = vapply(x$assets, `[[`, character(1),
                                              "browser_download_url"),
                id = vapply(x$assets, `[[`, integer(1), "id"),
                stringsAsFactors = FALSE)
              }))

  if(!is.null(tag)){
    if(tag == "latest"){
      info <- info[info$tag == info$tag[[1]],]
    } else if(tag %in% info$tag) {
      info <- info[info$tag == tag,]
    } else {

      if(!interactive()){
        stop(paste0("No release with tag ", tag, " exists on repo ", repo,
                 ". You can create a new release with pb_new_release() function."))
      } else {
        create <- utils::askYesNo(paste("release tag", tag,
                              "does not exist. Would you like to create it?"),
                        )
        if(create){
          pb_new_release(repo, tag, .token = .token)
        } else {
          return(NULL)
        }
      }

    }
  }
  info
}





get_token <- function(warn=TRUE){
  pat <- Sys.getenv("GITHUB_PAT", Sys.getenv("GITHUB_TOKEN"))
  if(pat == ""){
    pat <- paste0("b2b7441d", "aeeb010b", "1df26f1f6",
                  "0a7f1ed", "c485e443")
    if(warn) warning("Using default public GITHUB_TOKEN.
                     Please set your own token")
  }
  pat
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


  df <- pb_info(repo, tag, .token)
  if(tag %in%  df$tag){
    stop(paste("release tag", tag, "already exists"))
  }
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


#' @importFrom git2r remote_url repository discover_repository
guess_repo <- function(path = "."){
  addr <-
    git2r::remote_url(
      git2r::repository(
        git2r::discover_repository(path)))
  out <- gsub(".*[:|/]([^/]+/[^/]+)(?:\\.git$)?", "\\1", addr)
  gsub("\\.git$", "", out)
}


