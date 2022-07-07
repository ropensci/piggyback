
library(piggyback)
library(datasets)
library(readr)

repo <- "cboettig/piggyback-tests"
pb_new_release(repo, "v0.0.1")
pb_list(repo)

write_tsv(iris, "iris2.tsv.gz")
pb_upload( "iris2.tsv.gz", repo = repo, tag = "v0.0.1")

dir.create("data", showWarnings = FALSE)
write_tsv(mtcars, "data/mtcars.tsv.gz")

pb_upload( "data/mtcars.tsv.gz", repo = repo, tag = "v0.0.1")
