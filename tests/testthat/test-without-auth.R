context("Without Authentication")

tmp <- tempdir()

## Even though authentication is not required for these tests,
## they do call the GH API and are subject to tight rate-limiting
## when no Token is available.  It is preferable / advisable to have
## set GITHUB_TOKEN env var for any testing, as we do on Appveyor
## and Travis

testthat::with_mock(
  `gh::gh_token` = function(...) return(""),
  {

    test_that(
      "we can download all files from the a specific release", {
        skip_on_cran()
        pb_download(
          repo = "cboettig/piggyback-tests",
          dest = tempdir(),
          tag = "v0.0.1",
          show_progress = TRUE
        )

        expect_true(file.exists(file.path(tmp, "iris.tsv.gz")))
        pb_iris_1 <- read.delim(file.path(tmp, "iris.tsv.gz"))
        expect_equivalent(datasets::iris[[2]], pb_iris_1[[2]])
        expect_true(file.exists(file.path(tmp, "iris2.tsv.gz")))

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
        pb_iris_2 <- read.delim(file.path(tmp, "iris.tsv.gz"))
        expect_equivalent(datasets::iris[[2]], pb_iris_2[[2]])

        unlink(file.path(tmp, "iris.tsv.gz"))
      }
    )

    # #######  We need to be in an active project to track something
    #
    # test_that("we can track data", {
    #   skip_on_cran()
    #   skip_on_travis() # No idea
    #
    #   cur <- getwd()
    #   proj_dir <- fs::path_abs(fs::path(fs::path_temp(), "piggyback-test"))
    #   fs::dir_create(proj_dir)
    #   suppressMessages(usethis::create_project(proj_dir,
    #                                            open = FALSE
    #   ))
    #   setwd(proj_dir)
    #
    #   out <- pb_download(repo = "cboettig/piggyback-tests",
    #                      show_progress = FALSE)
    #   expect_true(TRUE)
    #
    #   setwd(cur)
    # })

    test_that("we can get all download urls", {
      skip_on_cran()

      x <- pb_download_url(
        repo = "cboettig/piggyback-tests",
        tag = "v0.0.1",
        .token = gh::gh_token()
      )
      expect_is(x, "character")
      expect_gt(length(x), 1)
    })

  })
