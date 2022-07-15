#' Create a new release on GitHub repo
#'
#' @param repo Repository name in format "owner/repo". Defaults to `guess_repo()`.
#' @param tag tag name to delete. Must be one of those found in `pb_releases()$tag_name`.
#' @param .token GitHub authentication token, see `[gh::gh_token()]`
#' @examples \dontrun{
#' pb_release_delete("cboettig/piggyback-tests", "v0.0.5")
#' }
#' @family release_management
#' @export
#'
pb_release_delete <- function(repo = guess_repo(), tag, .token = gh::gh_token()) {

  releases <- pb_releases(repo = repo, .token = .token)

  stopifnot(
    length(repo)   == 1 && is.character(repo),
    length(tag)    == 1 && is.character(tag),
    tag %in% releases$tag_name,
    length(.token) == 1 && is.character(.token)
  )

  release_id <- releases$release_id[releases$tag_name == tag]
  r <- parse_repo(repo)

  resp <- httr::DELETE(
    glue::glue(
      "https://api.github.com/repos/{owner}/{repo}/releases/{release_id}",
      owner = r[1],
      repo = r[2],
      release_id = release_id
    ),
    httr::add_headers(
      Authorization = paste("token",.token)
    )
  )

  if(httr::http_error(resp)){
    cli::cli_abort(
      c(
        "!" = "HTTP error {.val {httr::status_code(resp)}}:
        Could not delete release named {.val {tag}} in {.val {repo}}",
        "See returned error body for more details."
      )
    )
    return(resp)
  }

  try({
    memoise::forget(pb_releases)
    memoise::forget(pb_info)
  })

  cli::cli_alert_success("Deleted release {.val {tag}} from {.val {repo}}.")

  invisible(resp)
}
