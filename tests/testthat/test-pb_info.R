context("Get release/asset info without authentication")
testthat::with_mock(
  `gh::gh_token` = function(...) return(""),
  {
    test_that(
      "we can list files without auth", {
        skip_if_offline("api.github.com")

        x <- pb_list(
          repo = "cboettig/piggyback-tests",
          tag = "v0.0.1"
        )

        expect_true(nrow(x) > 1)
        expect_true("iris.tsv.gz" %in% x$file_name)
      }
    )

    test_that(
      "we can list releases without auth", {
        skip_if_offline("api.github.com")

        x <- pb_releases(
          repo = "cboettig/piggyback-tests"
        )

        expect_true(nrow(x) > 1)
        expect_true("v0.0.1" %in% x$tag_name)
      }
    )
  })

context("Get release/asset info with default token")
## We also expect users to be able to access public repos with their own token
test_that(
  "we can list files with default auth", {
    skip_if_offline("api.github.com")

    x <- pb_list(
      repo = "cboettig/piggyback-tests",
      tag = "v0.0.1"
    )

    expect_true(nrow(x) > 1)
    expect_true("iris.tsv.gz" %in% x$file_name)
  }
)

test_that(
  "we can list releases with default auth", {
    skip_if_offline("api.github.com")

    x <- pb_releases(
      repo = "cboettig/piggyback-tests"
    )

    expect_true(nrow(x) > 1)
    expect_true("v0.0.1" %in% x$tag_name)
  }
)
