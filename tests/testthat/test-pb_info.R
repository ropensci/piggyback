context("Get release/asset info without authentication")
test_that(
  "we can list files without auth", {
    skip_if_offline("api.github.com")

    x <- pb_list(
      repo = "cboettig/piggyback-tests",
      tag = "v0.0.1",
      .token = ""
    )

    expect_true(nrow(x) > 1)
    expect_true("iris.tsv.gz" %in% x$file_name)
  }
)

context("Get release/asset info with default/CI token")

test_that(
  "we can list files with default auth", {
    skip_if_offline("api.github.com")

    x <- pb_list(
      repo = "cboettig/piggyback-tests",
      tag = "v0.0.1",
      .token = Sys.getenv("GITHUB_TOKEN")
    )

    expect_true(nrow(x) > 1)
    expect_true("iris.tsv.gz" %in% x$file_name)
  }
)

test_that(
  "using 'latest' will find files of the most recent release",{

    skip_if_offline("api.github.com")

    x <- pb_list(
      repo = "cboettig/piggyback-tests",
      tag = "latest",
      .token = Sys.getenv("GITHUB_TOKEN")
    )

    expect_equivalent(unique(x$tag), "v3")
    expect_equivalent(nrow(x), 2)
  }
)

test_that(
  "we can list releases with default auth", {
    skip_if_offline("api.github.com")

    x <- pb_releases(
      repo = "cboettig/piggyback-tests",
      .token = Sys.getenv("GITHUB_TOKEN")
    )

    expect_true(nrow(x) > 1)
    expect_true("v0.0.1" %in% x$tag_name)
  }
)

test_that(
  "repos with no releases are handled correctly", {
    skip_if_offline("api.github.com")

    expect_warning(
    x <- pb_releases(
      repo = "tanho63/tanho63",
      .token = Sys.getenv("GITHUB_TOKEN")
    ),
    "No GitHub releases"
    )

    expect_equivalent(nrow(x), 0)

    x <- pb_list(
      repo = "tanho63/tanho63",
      .token = Sys.getenv("GITHUB_TOKEN")
    )
  }
)

