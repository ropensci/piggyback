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
      .token = Sys.getenv("GITHUB_TOKEN")
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
      .token = Sys.getenv("GITHUB_TOKEN")
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
    .token = Sys.getenv("GITHUB_TOKEN")
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
    .token = Sys.getenv("GITHUB_TOKEN")
  )
  expect_is(x, "character")
  expect_true(length(x) == 1)
  expect_true(grepl(pattern = "^http", x = x[1]))
})
