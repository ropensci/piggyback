#' Clear cached functions
#'
#' This function clears the cache for memoised piggyback functions.
#'
#' @examples .pb_cache_clear()
#'
#' @return invisible: TRUE on success
#' @export
#' @keywords internal
.pb_cache_clear <- function(){
  # the ones memoised in zzz.R
  memoised_functions <- c("pb_info", "pb_releases")
  try(lapply(memoised_functions, memoise::forget), silent = TRUE)
  invisible(TRUE)
}
