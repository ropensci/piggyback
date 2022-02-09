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
#' pb_new_release("cboettig/piggyback-tests", "v0.0.5")
#' }
pb_new_release <- function(repo = guess_repo(),
                           tag,
                           commit = NULL,
                           name = tag,
                           body = "Data release",
                           draft = FALSE,
                           prerelease = FALSE,
                           .token = get_token()) {

  releases <- release_info(repo, .token)

  # if no releases exist, release_info returns a gh_response length-0 list
  if(length(releases) > 0){
    # Otherwise, list is at least length 1, with names.
    if("tag_name" %in% names(releases[[1]])){
      current_tags <- lapply(releases, `[[`, "tag_name")
      if (tag %in% current_tags) {
        stop(paste("release tag", tag, "already exists"))
      }
    }
  }


  r <- strsplit(repo, "/")[[1]]

  payload <- compact(list(
    tag_name = tag,
    target_commitish = commit,
    name = name,
    body = body,
    draft = draft,
    prerelease = prerelease
  ))

  ## gh fails to pass body correctly??
  #gh("/repos/:owner/:repo/releases", owner = r[[1]], repo = r[[2]],
  #   .method = "POST", body = toJSON(payload,auto_unbox = TRUE), encode="json")

  resp <- httr::POST(paste0(
      "https://api.github.com/repos/", r[[1]], "/",
      r[[2]], "/", "releases"),
    httr::add_headers(Authorization = paste("token",.token)),
    body = jsonlite::toJSON(payload, auto_unbox = TRUE)
  )

  if(getOption("verbose")) httr::warn_for_status(resp)

  ## Release info changed, so break cache
  memoise::forget(memoised_pb_info)

  ## refresh
  pb_info(repo = repo, tag = tag, .token = .token)

  release <- httr::content(resp)
  invisible(release)
}
