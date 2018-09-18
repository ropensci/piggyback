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
                           .token = get_token()) {
  df <- pb_info(repo, tag, .token)
  if (tag %in% df$tag) {
    stop(paste("release tag", tag, "already exists"))
  }
  r <- strsplit(repo, "/")[[1]]

  payload <- list(
    tag_name = tag,
    target_commitish = commit,
    name = name,
    body = body,
    draft = draft,
    prerelease = prerelease
  )

  ## gh fails to pass body correctly??
  # gh("/repos/:owner/:repo/releases", owner = r[[1]], repo = r[[2]],
  #   .method = "POST", body = toJSON(payload,auto_unbox = TRUE), encode="json")

  token <- .token
  resp <- httr::POST(paste0(
    "https://api.github.com/repos/", r[[1]], "/",
    r[[2]], "/", "releases?access_token=", token
  ),
  body = jsonlite::toJSON(payload, auto_unbox = TRUE)
  )

  httr::stop_for_status(resp)
  release <- httr::content(resp)
}
