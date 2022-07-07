#' The following tests require push permissions for a given repository and
#' ensure that you can
#' - create a new release
#' - upload files to the release
#' - delete files from the release
#' - delete the release


skippy <- function(auth = FALSE){
  skip_if_offline("api.github.com")
  if(auth) skip_if(Sys.getenv("GITHUB_TOKEN") == "")
  if(auth) skip_if_not(Sys.getenv("IS_GHA") == "true")
  # ideally skip if not able to write to the repo. unsure how to check.

}

test_repo <- "ropensci/piggyback"

# test_repo <- "tanho63/tanho63"

test_release_tag <- paste("test",
                          format(Sys.Date(),"%Y%m%d"),
                          paste(sample(c(letters,0:9),5),collapse = ""),
                          sep = "_")

upload_files <- system.file(c("iris_upload.tsv.gz","mtcars_upload.tsv.gz"), package = "piggyback")

context("Release creation")

test_that("We can create a new release",{
  skippy(TRUE)

  expect_message({
    x <- pb_release_create(repo = test_repo, tag = test_release_tag)
  },
  "Created new release"
  )

  Sys.sleep(3)

  expect_equal(x$tag_name, test_release_tag)
})

test_that("Error if we try to create an existing release",{
  skippy(TRUE)


  expect_condition(
    pb_release_create(repo = test_repo, tag = test_release_tag),
    "Failed to create"
  )
})

context("File upload")

test_that("We can upload data", {

  skippy(TRUE)

  out <- pb_upload(
    repo = test_repo,
    file = upload_files,
    tag = test_release_tag,
    overwrite = TRUE,
    show_progress = FALSE
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
        overwrite = "use_timestamps"
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
        overwrite = FALSE)
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
      show_progress = FALSE
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
      show_progress = FALSE
    ),
    "Release .* not found"
  )
})

test_that("pb_upload offering to create release if missing",{
  # would ideally test with rlang::with_interactive() etc
  # but not sure how to trigger menu in test. ¯\_(ツ)_/¯
})

context("File delete")

test_that("can delete files from release",{
  skippy(TRUE)

  withr::with_options(list(piggyback.verbose = TRUE),{
    expect_message(
      pb_delete(basename(upload_files)[[1]], test_repo, test_release_tag),
      "Deleted"
    )
  })

  expect_equal(nrow(pb_info(test_repo, test_release_tag)), 1)
})

test_that("warn if file to delete is not found",{
  skippy(TRUE)

  withr::with_options(list(piggyback.verbose = TRUE),{
    expect_warning(
      pb_delete(basename(upload_files), test_repo, test_release_tag),
      "not found")

    expect_warning(
      pb_delete(basename(upload_files), test_repo, test_release_tag),
      "No file deletions")
  })
})

context("Release delete")

test_that("can delete release",{
  skippy(TRUE)

  expect_message({
    x <- pb_release_delete(test_repo, test_release_tag)
  },
  "Deleted release"
  )

  rels <- pb_releases(test_repo)
  expect_true(!test_release_tag %in% rels$tag_name)
})
