
#' List all assets attached to a release
#' @inheritParams pb_download
#' @param tag which release tag do we want information for? If `NULL` (default),
#' will return a table for all available release tags.
#' @return a data.frame of release asset names, (normalized to local paths), release tag,
#' timestamp, owner, and repo.
#' @details To preserve path information, local path delimiters are converted to `.2f`
#' when files are uploaded as assets.  Listing will display the local filename,
#' with asset names converting the `.2f` escape code back to the system delimiter.
#' @examples
#' \dontrun{
#' pb_list("cboettig/piggyback-tests")
#' }
#' @export
pb_list <- function(repo = guess_repo(),
                    tag = NULL,
                    ignore = "manifest.json",
                    .token = get_token()) {
  df <- pb_info(repo, tag, .token)
  df[c("file_name", "tag", "timestamp", "owner", "repo")]
}
