# context("Error Handling")
#
# Sys.setenv(piggyback_cache_duration="1e-6")
# tmp <- tempdir()

# test_that(
#   "Attempt upload without authentication", {
#
#     readr::write_tsv(datasets::iris, "iris3.tsv.gz")
#     expect_error(
#       out <- pb_upload(
#         repo = "cboettig/piggyback-tests",
#         file = "iris3.tsv.gz",
#         tag = "v0.0.1",
#         overwrite = TRUE,
#         .token = "not_valid_token",
#         show_progress = FALSE
#       ),
#       ".token"
#     )
#     unlink("iris.tsv.gz")
#   }
# )

# test_that(
#   "Attempt overwrite on upload when overwrite is FALSE", {
#     skip_on_cran()
#
#     data <- readr::write_tsv(datasets::iris, "iris.tsv.gz")
#     expect_warning(
#       out <- pb_upload(
#         repo = "cboettig/piggyback-tests",
#         file = "iris.tsv.gz",
#         tag = "v0.0.1",
#         overwrite = FALSE,
#         show_progress = FALSE
#       ),
#       "Skipping upload .+ as file exists"
#     )
#     unlink("iris.tsv.gz")
#   }
# )
#
# test_that(
#   "Attempt upload non-existent file", {
#     skip_on_cran()
#
#     expect_warning(
#       out <- pb_upload(
#         repo = "cboettig/piggyback-tests",
#         file = "not-a-file",
#         tag = "v0.0.1",
#         use_timestamps = FALSE,
#         show_progress = FALSE
#       ),
#       "file .+ does not exist"
#     )
#
#   }
# )
#

test_that(
  "Attempt download non-existent file", {
    skip_on_cran()

    expect_warning(
      out <- pb_download(
        repo = "cboettig/piggyback-tests",
        file = "not-a-file",
        tag = "v0.0.1",
        dest = tmp,
        show_progress = FALSE
      ),
      "not found"
    )

  }
)

test_that(
  "Attempt upload to non-existent tag", {
    skip_on_cran()

    ## Note: in interactive use this will prompt instead
    skip_if(interactive())

    path <- file.path(tmp, "iris.tsv.gz")
    readr::write_tsv(datasets::iris,
                     path)
    expect_error(
      out <- pb_upload(
        repo = "cboettig/piggyback-tests",
        file = path,
        tag = "not-a-tag",
        overwrite = TRUE,
        show_progress = FALSE
      ),
      "Release .* not found"
    )

    unlink(path)
  }
)




test_that(
  "Attempt overwrite on download when overwrite is FALSE", {
    skip_on_cran()

    tmp <- tempdir()
    path <- "iris.tsv.gz"

    ## Note: pb_download file argument needs relative path.
    readr::write_tsv(datasets::iris,
                     path)
    readr::write_tsv(datasets::iris,
                     file.path(tmp, path))

    expect_warning(
      out <- pb_download(
        repo = "cboettig/piggyback-tests",
        file = path,
        dest = tmp,
        tag = "v0.0.1",
        overwrite = FALSE,
        use_timestamps = FALSE,
        show_progress = FALSE
      ),
      "exists"
    )
    unlink(path)
    unlink(file.path(tmp, path))
  }
)

test_that(
  "Attempt to delete non-existent file", {

    skip_on_cran()

    expect_warning(
      pb_delete(
        repo = "cboettig/piggyback-tests",
        file = "mtcars8.tsv.gz",
        tag = "v0.0.1"
      ),
      "No file deletions performed"
    )
  }
)


test_that(
  "message when tag already exists", {

    skip_on_cran()


    expect_error(
      pb_new_release(
        repo = "cboettig/piggyback-tests",
        tag = "v0.0.1"
      ),
      "already exists"
    )

  }
)


test_that("download url error", {
  skip_on_cran()
  expect_warning(
    expect_error(
      pb_download_url("not-a-file", repo = "cboettig/piggyback-tests", tag = "v0.0.1", .token = gh::gh_token()),
      "No download URLs to return"),
    "file .+ not found in release"
  )
})

