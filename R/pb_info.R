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

  out <- data.frame(
    release_name = vapply(releases, `[[`, character(1),"name"),
    release_id = vapply(releases, `[[`, integer(1),"id"),
    release_body = vapply(releases, `[[`, character(1),"body"),
    tag_name = vapply(releases, `[[`, character(1),"tag_name"),
    draft = vapply(releases, `[[`, logical(1),"draft"),
    created_at = vapply(releases, `[[`, character(1),"created_at"),
    published_at = vapply(releases, `[[`, character(1),"published_at"),
    html_url = vapply(releases, `[[`, character(1),"html_url"),
    upload_url = vapply(releases, `[[`,character(1),"upload_url"),
    n_assets = vapply(releases, function(x) length(x[["assets"]]), integer(1))
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
    a <- gh::gh(endpoint = "/repos/:owner/:repo/releases/:release_id/assets",
                owner = r[[1]],
                repo = r[[2]],
                release_id = releases$release_id[[i]],
                .limit = Inf,
                .token = .token)
    if(length(a) == 0) next
    if (!identical(a[[1]], "")) {
      # convert list to dataframe and store in asset list
      a_df <- data.frame(
        file_name = vapply(a, `[[`, character(1), "name"),
        size = vapply(a, `[[`, integer(1), "size"),
        timestamp = lubridate::as_datetime(vapply(a, `[[`, character(1), "updated_at")),
        tag = releases$tag_name[i],
        owner = r[[1]],
        repo = r[[2]],
        upload_url = releases$upload_url[i],
        browser_download_url = vapply(a, `[[`, character(1L), "browser_download_url"),
        id = vapply(a, `[[`, integer(1L), "id"),
        state = vapply(a, `[[`, character(1L), "state"),
        stringsAsFactors = FALSE
      )

      asset_list[[i]] <- a_df
    }
  }

  # convert list of asset dataframes to single dataframe
  release_assets <- do.call(rbind,asset_list)

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
        timestamp = lubridate::as_datetime(0),
        tag = x$tag_name,
        owner = r[[1]],
        repo = r[[2]],
        upload_url = x$upload_url,
        browser_download_url = "",
        id = "",
        state = "",
        stringsAsFactors = FALSE
      ))
  }

  # if tag is latest, set tag to first tag present in releases
  if(!is.null(tag) && length(tag) == 1 && tag == "latest") tag <- releases$tag_name[[1]]

  # if tag is present, filter the releases to search to just the tags requested
  if(!is.null(tag)) releases <- releases[releases$tag_name %in% tag,]

  # get release assets and metadata for each release
  info <- get_release_assets(releases = releases, r = r, .token = .token)

  return(info)
}

