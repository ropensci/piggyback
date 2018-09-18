release_info <- function(repo = guess_repo(), .token = get_token()) {
  r <- strsplit(repo, "/")[[1]]
  if (length(r) != 2) {
    stop(paste(
      "Could not parse", r, "as a repository",
      "Make sure you have used the format:",
      crayon::blue$bold("owner/repo")
    ))
  }
  releases <- maybe(gh::gh("/repos/:owner/:repo/releases",
                           owner = r[[1]], repo = r[[2]], .token = .token
  ),
  otherwise = stop(api_error_msg(r))
  )
  releases
}




pb_info_fn <- function(repo = guess_repo(), tag = NULL, .token = get_token()) {
  releases <- release_info(repo, .token)
  r <- strsplit(repo, "/")[[1]]

  info <-
    do.call(
      rbind,
      lapply(
        releases,
        function(x) {
          data.frame(
            file_name = local_filename(
              vapply(x$assets, `[[`, character(1), "name")
            ),
            tag = x$tag_name,
            timestamp = lubridate::as_datetime(
              vapply(x$assets, `[[`, character(1), "updated_at")
            ),
            owner = r[[1]],
            repo = r[[2]],
            upload_url = x$upload_url,
            browser_download_url = vapply(
              x$assets, `[[`, character(1),
              "browser_download_url"
            ),
            id = vapply(x$assets, `[[`, integer(1), "id"),
            stringsAsFactors = FALSE
          )
        }
      )
    )

  if (!is.null(tag)) {
    if (tag == "latest") {
      info <- info[info$tag == info$tag[[1]], ]
    } else if (tag %in% info$tag) {
      info <- info[info$tag == tag, ]
    } else {
      if (!interactive()) {
        stop(paste0(
          "No release with tag ", tag, " exists on repo ", repo,
          ". You can create a new release with pb_new_release() function."
        ))
      } else {
        create <- utils::askYesNo(paste(
          "release tag", tag,
          "does not exist. Would you like to create it?"
        ), )
        if (create) {
          pb_new_release(repo, tag, .token = .token)
        } else {
          return(NULL)
        }
      }
    }
  }
  info
}



#' @importFrom memoise memoise timeout
pb_info <- memoise::memoise(pb_info_fn,
                            ~memoise::timeout(as.numeric(
                              Sys.getenv("piggyback_cache_duration", "1"))))
