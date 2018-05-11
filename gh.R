#' download_data("cboettig/ghdata")
ghdata_download <- function(repo, dest = ".", tag = "latest",  ...){
  
  r <- strsplit(repo, "/")[[1]]
  x <- release_info(repo, tag)
  id <- vapply(x$assets, `[[`, character(1), "id")
  file <- file.path(dest, vapply(x$assets, `[[`, charcter(1), "name"))
  
  for(i in seq_along(id)){
    resp <- gh_download_asset(r[[1]], r[[2]], id=id[i], file=file[i])
    httr::stop_for_status(resp)
  }
  
  invisible(resp)
}

#' @importFrom httr GET add_headers write_disk
## gh() fails on this, so we do with httr. See https://github.com/r-lib/gh/issues/57 
gh_download_asset <- function(owner, repo, id, file, 
                              token = Sys.getenv("GITHUB_PAT", Sys.getenv("GITHUB_TOKEN")), 
                              overwrite=TRUE){
  httr::GET(paste0("https://api.github.com/repos/", owner,"/",
                   repo, "/", "releases/assets/", id, 
                   "?access_token=", token),
            add_headers(Accept = "application/octet-stream"),
            write_disk(file, overwrite = overwrite))
}

#' @importFrom gh gh
release_info <- function(repo, tag="latest"){
  r <- strsplit(repo, "/")[[1]]
  if(tag == "latest"){
    gh("/repos/:owner/:repo/releases/latest", 
       owner = r[[1]], repo = r[[2]])
  } else {
    gh("/repos/:owner/:repo/releases/tags/:tag",
       owner = r[[1]], repo = r[[2]], tag = tag)
  }
}



## We can create a new release if we want to put our data there.
gh_tag_release <- function(repo, tag, 
                           commit = "master", name = tag, 
                           body = "Data release", draft = FALSE,
                           prerelease = FALSE,
                           .token = Sys.getenv("GITHUB_PAT",
                                     Sys.getenv("GITHUB_TOKEN"))){
  r <- strsplit(repo, "/")[[1]]
  
  payload <- list(
    tag_name = tag,
    target_commitish = commit,
    name = name,
    body = body,
    draft =  draft,
    prerelease = prerelease)
  
  ## Again, gh fails
  #gh("/repos/:owner/:repo/releases", owner = r[[1]], repo = r[[2]],
  #   .method = "POST", body = toJSON(payload,auto_unbox = TRUE), encode="json")  
  
  token <- .token
  resp <- httr::POST(paste0("https://api.github.com/repos/", r[[1]],"/",
                     r[[2]], "/", "releases?access_token=", token),
                     body = toJSON(payload,auto_unbox = TRUE))
  
  httr::stop_for_status(resp)
  release <- content(resp)
}

## upload data to an existing release
#' readr::write_tsv(mtcars,"mtcars.tsv.xz") 
#' ghdata_upload("cboettig/ghdata", "v0.0.3", "mtcars.tsv.xz")
ghdata_upload <- function(repo, tag, file){
  
  x <- release_info(repo, tag)
  r <- httr::POST(sub("\\{.+$", "", x$upload_url), query = list(name = file), 
                  body = httr::upload_file(file), httr::progress("up"), 
                  httr::authenticate(token, "x-oauth-basic", "basic"))
  cat("\n")
  httr::stop_for_status(r)
  invisible(r)

}



## List assets for release
#GET /repos/:owner/:repo/releases/:id/assets

#DELETE /repos/:owner/:repo/releases/:id

## Edit
#PATCH /repos/:owner/:repo/releases/:id
