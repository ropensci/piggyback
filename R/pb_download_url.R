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
#' pb_download_url("iris.tsv.xz",
#'                 repo = "cboettig/piggyback-tests",
#'                 tag = "v0.0.1")
#'
#' }
pb_download_url <- function(file = NULL,
                            repo = guess_repo(),
                            tag = "latest",
                            .token = gh::gh_token()) {
  df <- pb_info(repo, tag, .token)

  if(is.null(file)) return(df$browser_download_url)

  if(any(!file %in% df$file_name)) {

    missing <- file[!file %in% df$file_name]

    cli::cli_warn("file {.val {missing}} not found in release {.val {tag}} for repo {.val {repo}}")

    file <- file[file %in% df$file_name]
  }

  if(length(file) == 0) return(cli::cli_abort("No download URLs to return."))

  return(df[df$file_name %in% file,"browser_download_url"])
}
