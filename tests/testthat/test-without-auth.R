context("Without Authentication")

tmp <- tempdir()

## Even though authentication is not required for these tests,
## they do call the GH API and are subject to tight rate-limiting
## when no Token is available.  It is preferable / advisable to have
## set GITHUB_TOKEN env var for any testing, as we do on Appveyor
## and Travis
test_that(
  "we can download all files from the a specific release", {
    skip_on_cran()
    pb_download(
      repo = "cboettig/piggyback-tests",
      dest = tmp,
      tag = "v0.0.1",
      show_progress = FALSE
    )

    f <- fs::path(tmp, "data", "mtcars.tsv.gz")
    expect_true(file.exists(f))
    cars <- readr::read_tsv(f)
    expect_equivalent(cars, mtcars)

    expect_true(file.exists(file.path(tmp, "iris.tsv.gz")))
    expect_true(file.exists(file.path(tmp, "iris2.tsv.gz")))
    expect_true(file.exists(file.path(tmp, "data", "iris.tsv.xz")))

    unlink(f)
  }
)





test_that(
  "we can list files", {
    skip_on_cran()

    x <- pb_list(
      repo = "cboettig/piggyback-tests",
      tag = "v0.0.1"
    )
    expect_true("iris.tsv.gz" %in% x$file_name)
  }
)



test_that(
  "we can list multiple ignore files, including non-existent ones", {
    skip_on_cran()
    pb_download(
      repo = "cboettig/piggyback-tests",
      tag = "v0.0.1",
      ignore = c("manifest.json", "big_data_file.csv"),
      dest = tempdir(),
      show_progress = FALSE
    )
    expect_true(TRUE)
  }
)



test_that(
  "we can download a requested file from the requested release", {
    skip_on_cran()


    pb_download(
      file = "iris.tsv.gz",
      repo = "cboettig/piggyback-tests",
      tag = "v0.0.1",
      dest = tmp,
      show_progress = FALSE,
      overwrite = TRUE
    )

    expect_true(file.exists(file.path(tmp, "iris.tsv.gz")))
    expect_equivalent(datasets::iris[[2]], iris[[2]])

    unlink(file.path(tmp, "iris.tsv.gz"))
  }
)

test_that(
  "we can download a requested file to requested subdirectory", {
    skip_on_cran()


    dir.create(file.path(tmp, "test_data/"))
    pb_download(
      file = "data/mtcars.tsv.gz",
      dest = file.path(tmp, "test_data/"),
      repo = "cboettig/piggyback-tests",
      tag = "v0.0.1",
      show_progress = FALSE
    )

    path <- file.path(tmp, "test_data", "data", "mtcars.tsv.gz")
    expect_true(file.exists(path))
    cars <- readr::read_tsv(path)
    expect_equivalent(cars, mtcars)
    unlink(path)
  }
)


#######  We need to be in an active project to track something

test_that("we can track data", {
  skip_on_cran()

  cur <- getwd()
  proj_dir <- file.path(tmp, "piggyback-test")
  suppressMessages(usethis::create_project(proj_dir,
    open = FALSE
  ))
  setwd(proj_dir)

  pb_track("*.tsv")
  out <- pb_download(repo = "cboettig/piggyback-tests",
                     show_progress = FALSE)
  expect_true(TRUE)

  setwd(cur)
})

test_that("we can get all download urls", {
  skip_on_cran()

  x <- pb_download_url(
    repo = "cboettig/piggyback-tests",
    tag = "v0.0.1",
    .token = piggyback:::get_token()
  )
  expect_is(x, "character")
  expect_gt(length(x), 1)
})
