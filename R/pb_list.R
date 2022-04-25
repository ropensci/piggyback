#' List all assets attached to a release
#' @inheritParams pb_download
#' @param tag which release tag(s) do we want information for? If `NULL` (default),
#' will return a table for all available release tags.
#' @return a data.frame of release asset names, release tag, timestamp, owner, and repo.
#' @examples
#' \dontrun{
#' pb_list("cboettig/piggyback-tests")
#' }
#' @seealso `pb_releases` for a list of all releases in repository
#' @export
pb_list <- function(repo = guess_repo(),
                    tag = NULL,
                    .token = get_token()) {
  df <- pb_info(repo, tag, .token)
  df[c("file_name", "size", "timestamp", "tag", "owner", "repo")]
}
