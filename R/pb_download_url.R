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
#' # returns browser url by default
#' pb_download_url("iris.tsv.xz", repo = "cboettig/piggyback-tests", tag = "v0.0.1")
#'
#' # can return api url if desired
#' pb_download_url("iris.tsv.xz", repo = "cboettig/piggyback-tests", tag = "v0.0.1", url_type = "api")
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
