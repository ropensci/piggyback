#' Get the download url of a given file
#'
#' Returns the URL download for a given file. This can be useful when using
#' functions that are able to accept URLs.
#'
#' @param url_type choice: one of "browser" or "api" - default "browser" is a
#' web-facing URL that is not subject to API ratelimits but does not work for
#' private repositories. "api" URLs work for private repos, but require a GitHub
#' token passed in an Authorization header (see examples)
#' @inheritParams pb_download
#' @return the URL to download a file
#' @export
#' @examples \donttest{
#' \dontshow{try(\{}
#'
#' # returns browser url by default (and all files if none are specified)
#' browser_url <- pb_download_url(
#'   repo = "tanho63/piggyback-tests",
#'   tag = "v0.0.2"
#'   )
#' print(browser_url)
#' utils::read.csv(browser_url[[1]])
#'
#' # can return api url if desired
#' api_url <- pb_download_url(
#'   "mtcars.csv",
#'   repo = "tanho63/piggyback-tests",
#'   tag = "v0.0.2"
#'   )
#' print(api_url)
#'
#' # for public repositories, this will still work
#' utils::read.csv(api_url)
#'
#' # for private repos, can use httr or curl to fetch and then pass into read function
#' gh_pat <- Sys.getenv("GITHUB_PAT")
#'
#' if(!identical(gh_pat, "")){
#'   resp <- httr::GET(api_url, httr::add_headers(Authorization = paste("Bearer", gh_pat)))
#'   utils::read.csv(text = httr::content(resp, as = "text"))
#' }
#'
#' # or use pb_read which bundles some of this for you
#'
#' \dontshow{\})}
#' }
pb_download_url <- function(file = NULL,
                            repo = guess_repo(),
                            tag = "latest",
                            url_type = c("browser","api"),
                            .token = gh::gh_token()) {
  url_type <- rlang::arg_match(url_type, values = c("browser","api"))

  df <- pb_info(repo, tag, .token)

  if(is.null(file)) {
    switch(
      url_type,
      "browser" = return(df$browser_download_url),
      "api" = return(df$api_download_url)
    )
  }

  if(any(!file %in% df$file_name)) {

    missing <- file[!file %in% df$file_name]

    cli::cli_warn("file {.val {missing}} not found in release {.val {tag}} for repo {.val {repo}}")

    file <- file[file %in% df$file_name]
  }

  if(length(file) == 0) return(cli::cli_abort("No download URLs to return."))

  switch(
    url_type,
    "browser" = return(df$browser_download_url[df$file_name %in% file]),
    "api" = return(df$api_download_url[df$file_name %in% file])
  )
}
