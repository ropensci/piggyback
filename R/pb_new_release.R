#' Create a new release on GitHub repo
#'
#' @param repo Repository name in format "owner/repo". Will guess
#' the current repo if not specified.
#' @param tag tag to create for this release
#' @param commit Specifies the commit-ish value that
#'  determines where the Git tag is created from.
#'  Can be any branch or full commit SHA (not the short hash). Unused if the
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
#' pb_new_release("cboettig/piggyback-tests", "v0.0.5")
#' }
pb_new_release <- function(repo = guess_repo(),
                           tag,
                           commit = NULL,
                           name = tag,
                           body = "Data release",
                           draft = FALSE,
                           prerelease = FALSE,
                           .token = gh::gh_token()) {

  releases <- pb_releases(repo = repo, .token = .token, verbose = FALSE)

  # if no releases exist, pb_releases returns a dataframe of releases
  if(nrow(releases) > 0 && tag %in% releases$tag_name){
      cli::cli_abort("Release tag {.val {tag}} already exists!")
  }

  r <- parse_repo(repo)

  payload <- compact(list(
    tag_name = tag,
    target_commitish = commit,
    name = name,
    body = body,
    draft = draft,
    prerelease = prerelease
  ))

  # gh fails to pass body correctly??
  # gh("/repos/:owner/:repo/releases", owner = r[[1]], repo = r[[2]],
  #  .method = "POST", body = toJSON(payload,auto_unbox = TRUE), encode="json")

  resp <- httr::POST(
    glue::glue("https://api.github.com/repos/{r[[1]]}/{r[[2]]}/releases"),
    httr::add_headers(Authorization = paste("token",.token)),
    body = jsonlite::toJSON(payload, auto_unbox = TRUE)
  )

  if(httr::http_error(resp)) {
    cli::cli_warn(
      c("!"="Failed to create release: HTTP error {.val {httr::status_code(resp)}}.",
        "See returned error messages for more details"))

    return(httr::content(resp))
  }

  ## Release info changed, so break caches
  memoise::forget(pb_info)
  memoise::forget(pb_releases)

  release <- httr::content(resp)
  cli::cli_alert_success()
  invisible(release)
}
