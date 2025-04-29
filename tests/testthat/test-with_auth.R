#' The following tests require push permissions for a given repository and
#' ensure that you can
#' - create a new release
#' - upload files to the release
#' - delete files from the release
#' - delete the release
skippy <- function(use_auth = Sys.getenv("PIGGYBACK_USE_AUTH_TESTS", unset = FALSE)) {
  if (!identical(toupper(use_auth), "TRUE")) skip("envvar PIGGYBACK_USE_AUTH_TESTS is not TRUE")
  if (!identical(Sys.getenv("MY_UNIVERSE",""), "")) skip("skip tests when run on r-universe testing")
  skip_if_offline("api.github.com")
  token_check <- .check_test_token()
  skip_if(!all(token_check), names(token_check)[!token_check])
}

# test_repo <- "ropensci/piggyback"
test_repo <- "tanho63/piggyback"

test_release_tag <- paste("test",
                          format(Sys.Date(),"%Y%m%d"),
                          paste(sample(c(letters,0:9),5),collapse = ""),
                          sep = "_")

upload_files <- system.file(c("iris_upload.tsv.gz", "mtcars_upload.tsv.gz"),
                            package = "piggyback",
                            mustWork = TRUE)

token <- Sys.getenv("TAN_GH_TOKEN")

context("Release creation")

test_that("We can create a new release",{
  skippy(TRUE)

  expect_message({
    x <- pb_release_create(repo = test_repo, tag = test_release_tag, .token = token)
  },
  "Created new release"
  )

  Sys.sleep(3)

  expect_equal(x$tag_name, test_release_tag)
})

test_that("Error if we try to create an existing release",{
  skippy(TRUE)

  expect_warning(
    pb_release_create(repo = test_repo, tag = test_release_tag, .token = token),
    "Failed to create"
  )

})

context("pb_write")
test_that("pb_write can write file from memory to release", {
  skippy(TRUE)
  skip_if_offline("api.github.com")

  out <- pb_write(
    x = mtcars,
    file = "mtcars.rds",
    repo = test_repo,
    tag = test_release_tag,
    .token = token
  )
  expect_type(out,"list")
  expect_equal(out[[1]][["status_code"]], 201)
})

test_that("pb_write can autodetect different file formats",{
  out <- pb_write(
    x = mtcars,
    file = "mtcars.csv",
    repo = test_repo,
    tag = test_release_tag,
    .token = token
  )

  expect_type(out,"list")
  expect_equal(out[[1]][["status_code"]], 201)

  skip_if_not_installed("arrow")
  out <- pb_write(
    x = mtcars,
    file = "mtcars.parquet",
    repo = test_repo,
    tag = test_release_tag,
    .token = token
  )
  expect_type(out,"list")
  expect_equal(out[[1]][["status_code"]], 201)
})

test_that("pb_write can accept a custom write_function",{
  skip_if_not_installed("readr")
  out <- pb_write(
    x = mtcars,
    file = "mtcars.csv.gz",
    repo = test_repo,
    tag = test_release_tag,
    .token = token,
    write_function = readr::write_csv
  )
  expect_type(out,"list")
  expect_equal(out[[1]][["status_code"]], 201)
})

context("File upload")

test_that("We can upload data", {

  skippy(TRUE)

  out <- pb_upload(
    repo = test_repo,
    file = upload_files,
    tag = test_release_tag,
    overwrite = TRUE,
    show_progress = FALSE,
    .token = token
  )

  expect_type(out,"list")
  expect_equal(out[[1]][["status_code"]], 201)
})

test_that("use_timestamps behaviour of upload overwrite works",{
  skippy(TRUE)
  Sys.sleep(3)
  expect_warning(
    withr::with_options(list(piggyback.verbose = TRUE),{

      out <- pb_upload(
        file = upload_files,
        repo = test_repo,
        tag = test_release_tag,
        overwrite = "use_timestamps",
        .token = token
      )
    }),
    "more recent version of"
  )

})

test_that("not overwriting existing files if overwrite = FALSE",{
  skippy(TRUE)

  expect_warning(
    withr::with_options(list(piggyback.verbose = TRUE),{
      out <- pb_upload(
        file = upload_files,
        repo = test_repo,
        tag = test_release_tag,
        overwrite = FALSE,
        .token = token)
    }),
    "Skipping upload"
  )
})

test_that("cannot upload with invalid token", {

  skippy(TRUE)

  expect_error(
    pb_upload(
      repo = test_repo,
      file = upload_files,
      tag = test_release_tag,
      overwrite = TRUE,
      show_progress = FALSE,
      .token = "not_a_valid_token"
    ),
    "401"
  )
})

test_that("cannot upload nonexistent file", {

  skippy(TRUE)

  expect_warning(
    pb_upload(
      repo = test_repo,
      file = "mtcars.csv.asdf",
      tag = test_release_tag,
      overwrite = TRUE,
      show_progress = FALSE,
      .token = token
    ),
    "File .+ does not exist"
  )
})

test_that("cannot upload to nonexistent release", {

  skippy(TRUE)

  expect_error(
    pb_upload(
      repo = test_repo,
      file = upload_files,
      tag = "mytag",
      overwrite = TRUE,
      show_progress = FALSE,
      .token = token
    ),
    "Could not find .* in.*pb_releases"
  )
})

test_that("pb_upload finds latest release",{

  skippy(TRUE)

  withr::with_options(
    list(piggyback.verbose = TRUE),
    {

      suppressWarnings(
        expect_message(
          pb_upload(
            repo = test_repo,
            file = upload_files,
            tag = "latest",
            .token = token
          ),
          "latest release:"
        )
      )

    }
  )

})

context("File delete")

test_that("can delete files from release",{
  skippy(TRUE)

  count_start <- nrow(pb_info(test_repo, test_release_tag))

  withr::with_options(list(piggyback.verbose = TRUE),{
    expect_message(
      pb_delete(file = basename(upload_files)[[1]],
                repo =  test_repo,
                tag =  test_release_tag,
                .token = token),
      "Deleted"
    )
  })

  count_end <- nrow(pb_info(test_repo, test_release_tag))
  expect_equal(count_start - 1, count_end)
})

test_that("warn if file to delete is not found",{
  skippy(TRUE)

  withr::with_options(list(piggyback.verbose = TRUE),{
    expect_warning(
      pb_delete(file = basename(upload_files),
                repo = test_repo,
                tag = test_release_tag,
                .token = token),
      "not found")

    expect_warning(
      pb_delete(file = basename(upload_files),
                repo = test_repo,
                tag = test_release_tag,
                .token = token),
      "No file deletions")
  })
})

context("Release delete")

test_that("can delete release",{
  skippy(TRUE)

  expect_message({
    x <- pb_release_delete(repo = test_repo,
                           tag =  test_release_tag,
                           .token = token)
  },
  "Deleted release"
  )

  expect_equivalent(httr::status_code(x), 204)
})

context("Private repo download")
test_that("can download private repo file",{
  skippy(TRUE)

  pb_download(
    file = "iris_example.csv",
    repo = "tanho63/piggyback-private",
    tag = "iris",
    dest = tempdir(),
    .token = Sys.getenv("TAN_GH_TOKEN")
  )

  x <- read.csv(file.path(tempdir(),"iris_example.csv"))

  expect_equal(
    nrow(x),
    150
  )
})

test_that("can read private repo files",{
  skippy(TRUE)

  x <- pb_read(
    file = "iris_example.csv",
    repo = "tanho63/piggyback-private",
    tag = "iris",
    .token = Sys.getenv("TAN_GH_TOKEN")
  )

  expect_equal(nrow(x), 150)
})
