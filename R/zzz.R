.onLoad <- function(libname,pkgname){
  cache_duration <- as.numeric(Sys.getenv("piggyback_cache_duration",unset = "600"))
  assign(x = "pb_info",
         value = memoise::memoise(pb_info, ~ memoise::timeout(cache_duration)),
         envir = parent.env(environment()))
  assign(x = "pb_releases",
         value = memoise::memoise(pb_info, ~ memoise::timeout(cache_duration)),
         envir = parent.env(environment()))
}
