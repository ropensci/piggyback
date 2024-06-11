#' List releases in repository
#'
#' This function retrieves information about all releases attached to a given repository.
#'
#' @param repo GitHub repository specification in the form of `"owner/repo"`, if not specified will try to guess repo based on current working directory.
#' @param .token a GitHub API token, defaults to `gh::gh_token()`
#' @param verbose defaults to TRUE, use FALSE to silence messages
#'
#' @examples
#' \donttest{
#' try({ # wrapped in try block to prevent CRAN errors
#'  pb_releases("nflverse/nflverse-data")
#' })
#' }
#' @return a dataframe of all releases available within a repository.
#'
#' @export
pb_releases <- function(repo = guess_repo(),
                        .token = gh::gh_token(),
                        verbose = getOption("piggyback.verbose", default = TRUE)){

  r <- parse_repo(repo)
  # get release ids
  releases <- tryCatch(
    gh::gh("/repos/:owner/:repo/releases",
           owner = r[[1]],
           repo = r[[2]],
           .limit = Inf,
           .token = .token),
    error = function(cnd){

      cli::cli_abort(
        c("!"="Cannot access release data for repo {.val {repo}}.",
          "Check that you have provided a {.code .token} and that the repo is correctly specified.",
          unlist(strsplit(cnd$message,"\\n"))
          )

      )
    }
  )

  if(length(releases) == 0) {
    if(verbose){
      cli::cli_warn(
        c("!" = "No GitHub releases found for {.val {repo}}!",
          "You can make a new one with {.fun piggyback::pb_new_release}")
      )}
    return(invisible(data.frame()))
  }

  latest_release <- gh::gh(
    "/repos/:owner/:repo/releases/latest",
    owner = r[[1]],
    repo = r[[2]],
    .token = .token
  ) |>
    getElement("tag_name")

  out <- data.frame(
    release_name = .extract_chr(releases, "name"),
    release_id = .extract_int(releases, "id"),
    release_body = .extract_chr(releases, "body"),
    tag_name = .extract_chr(releases, "tag_name"),
    draft = .extract_lgl(releases, "draft"),
    latest = .extract_chr(releases, "tag_name") %in% latest_release,
    created_at = .extract_chr(releases, "created_at"),
    published_at = .extract_chr(releases, "published_at"),
    html_url = .extract_chr(releases, "html_url"),
    upload_url = .extract_chr(releases, "upload_url"),
    n_assets = .map_int(releases, function(x) length(x[["assets"]]))
  )

  return(out)
}

#' @keywords internal
#' @param releases as created by `pb_releases()`
#' @param r a list of owner/repo as created by `parse_repo()`
#' @noRd
get_release_assets <- function(releases, r, .token) {

  if(nrow(releases)==0) return(data.frame())

  asset_list <- vector("list", length = nrow(releases))

  # fetch asset meta-data individually for each release, see #19
  for (i in seq_along(releases$tag_name)) {
    a <- gh::gh(
      endpoint = "/repos/:owner/:repo/releases/:release_id/assets",
      owner = r[[1]],
      repo = r[[2]],
      release_id = releases$release_id[[i]],
      .limit = Inf,
      .token = .token,
      .progress = getOption("piggyback.verbose", default = interactive())
    )
    if(length(a) == 0) next
    if (!identical(a[[1]], "")) {
      # convert list to dataframe and store in asset list
      a_df <- data.frame(
        file_name = .extract_chr(a, "name"),
        size = .extract_int(a, "size"),
        timestamp = .as_datetime(.extract_chr(a, "updated_at")),
        tag = releases$tag_name[i],
        owner = r[[1]],
        repo = r[[2]],
        upload_url = releases$upload_url[i],
        browser_download_url = .extract_chr(a, "browser_download_url"),
        api_download_url = glue::glue(
          "{.gh_api_url()}/repos/{r[[1]]}/{r[[2]]}/releases/assets/{.extract_int(a, 'id')}"
        ),
        id = .extract_int(a, "id"),
        state = .extract_chr(a, "state"),
        stringsAsFactors = FALSE
      )

      asset_list[[i]] <- a_df
    }
  }

  # convert list of asset dataframes to single dataframe
  release_assets <- do.call(rbind, asset_list)

  # return result
  return(release_assets)
}

pb_info <- function(repo = guess_repo(),
                    tag = NULL,
                    .token = gh::gh_token()) {

  r <- parse_repo(repo)

  # get all releases
  releases <- pb_releases(repo = repo, .token = .token, verbose = FALSE)

  # if no releases return empty df
  if(nrow(releases) == 0) {
    return(
      data.frame(
        file_name = "",
        size = 0L,
        timestamp = .as_datetime(0),
        tag = "",
        owner = r[[1]],
        repo = r[[2]],
        upload_url = "",
        browser_download_url = "",
        api_download_url = "",
        id = "",
        state = "",
        stringsAsFactors = FALSE
      ))
  }

  # if tag is "latest" (and no tag is literally named "latest"), set tag to
  # GitHub's idea of latest release tag
  if(identical(tag, "latest") && !"latest" %in% releases$tag_name) {
    tag <- releases$tag_name[releases$latest]
  }

  # if tag is present, filter the releases to search to just the tags requested
  if(!is.null(tag)) releases <- releases[releases$tag_name %in% tag,]

  # get release assets and metadata for each release
  info <- get_release_assets(releases = releases, r = r, .token = .token)

  return(info)
}

