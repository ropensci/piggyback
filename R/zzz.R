#nocov start

.onLoad <- function(libname,pkgname){

  # piggyback caches repository info only and does not cache download/upload commands
  use_cache <- getOption("piggyback.cache", default = TRUE)

  if(!use_cache) cli::cli_alert_info("piggyback.cache set to FALSE - caching disabled")

  if(use_cache){
    # default 600 seconds = 10 minutes
    cache_duration <- as.numeric(Sys.getenv("piggyback_cache_duration", unset = "600"))
    assign(x = "pb_info",
           value = memoise::memoise(pb_info, ~ memoise::timeout(cache_duration)),
           envir = rlang::ns_env("piggyback"))
    assign(x = "pb_releases",
           value = memoise::memoise(pb_releases, ~ memoise::timeout(cache_duration)),
           envir = rlang::ns_env("piggyback"))
    assign(x = ".check_test_token",
           value = memoise::memoise(.check_test_token, ~ memoise::timeout(cache_duration)),
           envir = rlang::ns_env("piggyback"))
  }
}

#nocov end
