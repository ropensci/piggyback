#' Delete an asset attached to a release
#'
#' @inheritParams pb_upload
#' @param file file(s) to be deleted from the release. If `NULL` (default
#' when argument is omitted), function will delete all attachments to the release.
#' delete
#' @return `TRUE` (invisibly) if a file is found and deleted.
#' Otherwise, returns `NULL` (invisibly) if no file matching the name was found.
#' @export
#' @examples
#' \donttest{
#' readr::write_tsv(mtcars, "mtcars.tsv.gz")
#' ## Upload
#' pb_upload("mtcars.tsv.gz",
#'           repo = "cboettig/piggyback-tests",
#'            overwrite = TRUE)
#' pb_delete("mtcars.tsv.gz",
#'           repo = "cboettig/piggyback-tests",
#'           tag = "v0.0.1")
#' }
#'
pb_delete <- function(file = NULL,
                      repo = guess_repo(),
                      tag = "latest",
                      .token = get_token()) {
  df <- pb_info(repo, tag, .token)

  if (is.null(file)) {
    ids <- df$id
  } else {
    ids <- df[df$file_name %in% file, "id"]
  }

  if (length(ids) < 1) {
    message(paste(file, "not found on GitHub"))
    return(NULL)
  }

  lapply(ids, function(id) {
    ## If we find matching id, Delete file from release.
    gh::gh("DELETE /repos/:owner/:repo/releases/assets/:id",
           owner = df$owner[[1]], repo = df$repo[[1]], id = id, .token = .token
    )
  })
  invisible(TRUE)
}
