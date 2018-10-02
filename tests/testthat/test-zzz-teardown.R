testthat::teardown({
  unlink("iris.tsv.gz")
  unlink("iris2.tsv.gz")
  unlink("iris3.tsv.gz")
}
)
