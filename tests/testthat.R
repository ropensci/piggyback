library(testthat)
library(piggyback)
Sys.setenv(piggyback_cache_duration=1e-6)
test_check("piggyback")
