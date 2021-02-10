
#' Get the download url of a given file
#'
#' Returns the URL download for a public file. This can be useful when writing
#' scripts that may want to download the file directly without introducing any
#' dependency on `piggyback` or authentication steps.
#' @inheritParams pb_download
#' @return the URL to download a file
#' @export
#' @examples \dontrun{
#'
#' pb_download_url("data/iris.tsv.xz",
#'                 repo = "cboettig/piggyback-tests",
#'                 tag = "v0.0.1")
#'
#' }
pb_download_url <- function(file = NULL,
                            repo = guess_repo(),
                            tag = "latest",
                            .token = get_token()) {
  df <- pb_info(repo, tag, .token)
  if (is.null(file)) {
    return(df$browser_download_url)
  } else if (file %in% df$file_name) {
    return(df[file == df$file_name, "browser_download_url"])
  } else {
    stop(paste("file", file, "not found in release", tag, "for repo", repo))
  }
}
