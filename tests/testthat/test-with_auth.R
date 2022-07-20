#' The following tests require push permissions for a given repository and
#' ensure that you can
#' - create a new release
#' - upload files to the release
#' - delete files from the release
#' - delete the release

skippy <- function(auth = FALSE){
  skip_if_offline("api.github.com")
  if(auth) skip_if(Sys.getenv("TAN_GH_TOKEN") == "", message = "env variable TAN_GH_TOKEN not found")
  # ideally skip if not able to write to the repo. unsure how to check at the moment.
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

  expect_condition(
    pb_release_create(repo = test_repo, tag = test_release_tag, .token = token),
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
    "Release .* not found"
  )
})

test_that("pb_upload offering to create release if missing",{
  # would ideally test with rlang::with_interactive() etc
  # but not sure how to trigger menu in test. ¯\_(ツ)_/¯
})

test_that("pb_upload finds latest release",{

  skippy(TRUE)

  withr::with_options(
    list(piggyback.verbose = TRUE),
    {

      expect_warning(
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

  withr::with_options(list(piggyback.verbose = TRUE),{
    expect_message(
      pb_delete(file = basename(upload_files)[[1]],
                repo =  test_repo,
                tag =  test_release_tag,
                .token = token),
      "Deleted"
    )
  })

  expect_equal(nrow(pb_info(test_repo, test_release_tag)), 1)
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
  #   skippy(TRUE)
  #
  #   pb_download(
  #     file = "iris_example.csv",
  #     repo = "tanho63/piggyback-private",
  #     tag = "iris",
  #     dest = tempdir(),
  #     .token = Sys.getenv("TAN_GH_TOKEN")
  #     )
  #
  #   x <- read.csv(file.path(tempdir(),"iris_example.csv"))
  #
  #   warning(paste(readLines(file.path(tempdir(),"iris_example.csv")), collapse = "\n"))
  #
  #   expect_equal(
  #     nrow(x),
  #     150
  #   )

})
