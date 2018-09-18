library(testthat)
library(piggyback)
Sys.setenv(piggyback_cache_duration=0)
test_check("piggyback")
