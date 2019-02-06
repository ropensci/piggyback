release_info <- function(repo = guess_repo(), .token = get_token()) {
  r <- strsplit(repo, "/")[[1]]
  if (length(r) != 2) {
    stop(paste(
      "Could not parse", r, "as a repository",
      "Make sure you have used the format:",
      crayon::blue$bold("owner/repo")
    ))
  }

  # get release ids
  releases <- maybe(gh::gh("/repos/:owner/:repo/releases",
                           owner = r[[1]], repo = r[[2]],
                           .limit = Inf,
                           .token = .token
  ),
  otherwise = stop(api_error_msg(r))
  )

  # fetch asset meta-data individually, see #19
  for (i in seq_along(releases)) {
     a <- gh::gh(endpoint = "/repos/:owner/:repo/releases/:release_id/assets",
                 owner = r[[1]],
                 repo = r[[2]],
                 release_id = releases[[i]]$id,
                 .limit = Inf,
                 .token = .token)

     if (!identical(a[[1]], "")) {
      # if the i'th release does not have any assets then we skip updating
      # the assets in the releases object

      ## Now use assets given by the release id as the returned assets
      class(a) <- "list"
      attributes(a) <- NULL
      releases[[i]]$assets <- a
    }
  }

  # return result
  releases
}
#' @importFrom lubridate as_datetime
release_data <- function(x, r) {

  if(!"assets" %in% names(x))
    return(data.frame())

  if(length(x$assets) == 0){
    ## Release exists but has no assets
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

  data.frame(
    file_name = local_filename(
      vapply(x$assets, `[[`, character(1), "name")
    ),
    size =
      vapply(x$assets, `[[`, integer(1), "size"),
    timestamp = lubridate::as_datetime(
      vapply(x$assets, `[[`, character(1), "updated_at")
    ),
    tag = x$tag_name,
    owner = r[[1]],
    repo = r[[2]],
    upload_url = x$upload_url,
    browser_download_url = vapply(
      x$assets, `[[`, character(1L),
      "browser_download_url"
    ),
    id = vapply(x$assets, `[[`, integer(1L), "id"),
    state = vapply(x$assets, `[[`, character(1L), "state"),
    stringsAsFactors = FALSE
  )
}

null_chr <- function(x){
  if(is.null(x)) return("")
  as.character(NA)
}

pb_info_fn <- function(repo = guess_repo(),
                       tag = NULL,
                       .token = get_token()) {
  releases <- release_info(repo, .token)
  r <- strsplit(repo, "/")[[1]]

  info <- do.call(rbind, lapply(releases, release_data, r))

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
        create <- askYesNo(paste(
          "release tag", tag,
          "does not exist. Would you like to create it?"
        ))
        if (create) {
          pb_new_release(repo, tag, .token = .token)
        } else {
          return(NULL)
        }
      }
    }
  }

  # we deleted a file, so we better break cache.
  memoise::forget(memoised_pb_info)

  info
}

memoised_pb_info <-
  memoise::memoise(pb_info_fn,
                   ~memoise::timeout(as.numeric(
                     Sys.getenv("piggyback_cache_duration", "1")
                     ))
                   )


#' @importFrom memoise memoise timeout forget
pb_info <- function(repo = guess_repo(),
                    tag = NULL,
                    .token = get_token(),
                    cache = TRUE){

  seconds <- as.numeric(Sys.getenv("piggyback_cache_duration", "1"))
  if(seconds == 0) cache <- FALSE
  if(cache){

    memoised_pb_info(repo, tag, .token)

  } else {
    pb_info_fn(repo, tag, .token)
  }

}


