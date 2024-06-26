#' Delete release from GitHub repo
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
    length(.token) == 1 && is.character(.token) && nchar(.token) > 0
  )

  release_id <- releases$release_id[releases$tag_name == tag]
  r <- parse_repo(repo)

  resp <- httr::RETRY(
    verb = "DELETE",
    glue::glue(
      "{.gh_api_url()}/repos/{owner}/{repo}/releases/{release_id}",
      owner = r[1],
      repo = r[2],
      release_id = release_id
    ),
    httr::add_headers(Authorization = paste("token",.token)),
    terminate_on = c(400, 401, 403, 404, 422)
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
  resp2 <- httr::RETRY(
    verb = "DELETE",
    glue::glue(
      "{.gh_api_url()}/repos/{owner}/{repo}/git/refs/tags/{tag}",
      owner = r[1],
      repo = r[2],
      tag = tag
    ),
    httr::add_headers(Authorization = paste("token",.token)),
    terminate_on = c(400, 401, 403, 404, 422)
  )

  if(httr::http_error(resp2)){
    cli::cli_abort(
      c(
        "!" = "HTTP error {.val {httr::status_code(resp2)}}:
        Could not delete git reference named {.val {tag}} in {.val {repo}}",
        "See returned error body for more details."
      )
    )
    return(resp2)
  }

  .pb_cache_clear()

  cli::cli_alert_success("Deleted release {.val {tag}} from {.val {repo}}.")

  invisible(resp)
}
