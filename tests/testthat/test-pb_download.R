context("Download assets without authentication")
tmp <- tempdir(check = TRUE)

test_that(
  "we can download a requested file from the requested release", {
    skip_if_offline("api.github.com")

    pb_download(
      file = "iris.tsv.gz",
      repo = "cboettig/piggyback-tests",
      tag = "v0.0.1",
      dest = tmp,
      show_progress = FALSE,
      overwrite = TRUE,
      .token = ""
    )

    expect_true(file.exists(file.path(tmp, "iris.tsv.gz")))
    pb_iris_2 <- read.delim(file.path(tmp, "iris.tsv.gz"))
    expect_equivalent(datasets::iris[[2]], pb_iris_2[[2]])

    unlink(file.path(tmp, "iris.tsv.gz"))
  }
)

context("Download assets with default auth")

test_that(
  "we can download all files from the a specific release, and ignore specific ones", {
    skip_if_offline("api.github.com")
    pb_download(
      repo = "cboettig/piggyback-tests",
      dest = tempdir(),
      tag = "v0.0.1",
      ignore = c("manifest.json", "big_data_file.csv"),
      show_progress = FALSE,
      .token = gh::gh_token()
    )

    expect_true(file.exists(file.path(tmp, "iris.tsv.gz")))
    pb_iris_1 <- read.delim(file.path(tmp, "iris.tsv.gz"))
    expect_equivalent(datasets::iris[[2]], pb_iris_1[[2]])
    expect_true(file.exists(file.path(tmp, "iris2.tsv.gz")))

  }
)

test_that(
  "we can download a requested file from the requested release", {
    skip_if_offline("api.github.com")

    pb_download(
      file = "iris.tsv.gz",
      repo = "cboettig/piggyback-tests",
      tag = "v0.0.1",
      dest = tmp,
      show_progress = FALSE,
      overwrite = TRUE,
      .token = gh::gh_token()
    )

    expect_true(file.exists(file.path(tmp, "iris.tsv.gz")))
    pb_iris_2 <- read.delim(file.path(tmp, "iris.tsv.gz"))
    expect_equivalent(datasets::iris[[2]], pb_iris_2[[2]])

    unlink(file.path(tmp, "iris.tsv.gz"))
  }
)

test_that("we can get all download urls", {
  skip_if_offline("api.github.com")

  x <- pb_download_url(
    repo = "cboettig/piggyback-tests",
    tag = "v0.0.1",
    .token = gh::gh_token()
  )
  expect_is(x, "character")
  expect_gt(length(x), 1)
  expect_true(grepl(pattern = "^http", x = x[1])) # is URL
})

test_that("we can get one download url", {
  skip_if_offline("api.github.com")

  x <- pb_download_url(
    file = "iris.tsv.gz",
    repo = "cboettig/piggyback-tests",
    tag = "v0.0.1",
    .token = gh::gh_token()
  )
  expect_is(x, "character")
  expect_true(length(x) == 1)
  expect_true(grepl(pattern = "^http", x = x[1]))
})

test_that("Missing files are reported in download and download_url", {
  skip_if_offline("api.github.com")

  expect_warning({

    expect_error({
      x <- pb_download_url(
        file = "iris.csv.gz",
        repo = "cboettig/piggyback-tests",
        tag = "v0.0.1",
        .token = gh::gh_token()
      )
    },
    "No download URLs"
    )
  },
  "not found"
  )

  expect_warning({
      x <- pb_download(
        file = "iris.csv.gz",
        repo = "cboettig/piggyback-tests",
        dest = tmp,
        tag = "v0.0.1",
        .token = gh::gh_token()
      )
    },
    "not found"
    )

})

context("pb_read")
test_that("pb_read can read a file from release directly into memory", {
  skip_if_offline("api.github.com")

  test_tsv <- pb_read(
    file = "iris.tsv.gz",
    repo = "cboettig/piggyback-tests",
    tag = "v0.0.1",
    .token = gh::gh_token()
  )

  expect_equivalent(datasets::iris[[2]], test_tsv[[2]])
})

test_that("pb_read can autodetect different file formats",{
  test_rds <- pb_read(
    file = "mtcars.rds",
    repo = "tanho63/piggyback-tests",
    tag = "v0.0.2"
  )

  expect_equal(nrow(mtcars), nrow(test_rds))

  skip_if_not_installed("arrow")
  test_parquet <- pb_read(
    file = "mtcars.parquet",
    repo = "tanho63/piggyback-tests",
    tag = "v0.0.2"
  )
  expect_equal(nrow(mtcars), nrow(test_parquet))
})

test_that("pb_read can accept a custom read_function",{
  skip_if_not_installed("readr")
  test_parquet <- pb_read(
    file = "mtcars.csv",
    repo = "tanho63/piggyback-tests",
    tag = "v0.0.2",
    read_function = readr::read_csv
  )
  expect_equal(nrow(mtcars), nrow(test_parquet))
})
