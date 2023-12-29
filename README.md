
<!-- README.md is generated from README.Rmd. Please edit that file -->

# piggyback <img src="man/figures/logo.svg" align="right" alt="" width="120" />

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/ropensci/piggyback/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci/piggyback/actions)
[![Coverage
status](https://codecov.io/gh/ropensci/piggyback/branch/master/graph/badge.svg)](https://app.codecov.io/github/ropensci/piggyback?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/piggyback)](https://cran.r-project.org/package=piggyback)
[![Peer Review
Status](https://badges.ropensci.org/220_status.svg)](https://github.com/ropensci/software-review/issues/220)
[![DOI](https://zenodo.org/badge/132979724.svg)](https://zenodo.org/badge/latestdoi/132979724)
[![DOI](http://joss.theoj.org/papers/10.21105/joss.00971/status.svg)](https://doi.org/10.21105/joss.00971)
<!-- badges: end -->

`{piggyback}` provides an R interface for storing files as GitHub
release assets, which is a convenient way for large/binary data files to
*piggyback* onto public and private GitHub repositories. This package
includes functions for file downloads, uploads, and managing releases,
which then are passed to the GitHub API.

No authentication is required to download data from public repositories.

## Installation

Install from CRAN via:

``` r
install.packages("piggyback")
```

You can install the development version from
[GitHub](https://github.com/ropensci/piggyback) with either r-universe
or with remotes:

``` r
install.packages("piggyback", repos = c('https://ropensci.r-universe.dev', getOption("repos")))
# install.packages("remotes")
remotes::install_github("ropensci/piggyback")
```

## Usage

See [getting started
vignette](https://docs.ropensci.org/piggyback/articles/intro.html) for a
more comprehensive introduction.

Download data attached to a GitHub release:

``` r
library(piggyback)
pb_download("iris2.tsv.gz", 
            repo = "cboettig/piggyback-tests",
            tag = "v0.0.1",
            dest = tempdir())
#> ℹ Downloading "iris2.tsv.gz"...
#> |======================================================| 100%
fs::dir_tree(tempdir())
#> /tmp/RtmpWxJSZj
#> └── iris2.tsv.gz
```

Downloading from private repos or uploading to any repo requires
authentication, specifically a GitHub Personal Access Token (PAT). This
can be stored as a
[gh::gh_token()](https://usethis.r-lib.org/articles/git-credentials.html#get-a-personal-access-token-pat)
or a GITHUB_PAT environment variable - for more information, see the
vignette notes on
[authentication](https://docs.ropensci.org/piggyback/articles/piggyback.html#authentication).

We can also upload data to a release. Start by creating a release:

``` r
pb_release_create(repo = "cboettig/piggyback-tests", tag = "v0.0.2")
#> ✔ Created new release "v0.0.2".
```

then upload to it:

``` r
readr::write_tsv(mtcars, "mtcars.tsv.gz")
pb_upload("mtcars.tsv.gz", repo = "cboettig/piggyback-tests")
#> ℹ Uploading to latest release: "v0.0.2".
#> ℹ Uploading mtcars.tsv.gz ...
#> |===================================================| 100%
```

For improved performance, we can also use piggyback files with [cloud
native](https://docs.ropensci.org/piggyback/articles/cloud_native.html)
workflows to query data without downloading it first.

## Motivations

A brief video overview presented as part of Tan Ho’s [RStudioConf2022
talk](https://www.youtube.com/watch?v=wzcz4xNGeTI&t=655s):

<https://github.com/ropensci/piggyback/assets/38083823/a1dff640-1bba-4c06-bad2-feda34f47387>

`piggyback` allows you to store data alongside your repository as
release assets, which helps you:

- store files larger than 50MB
- bypass the 2GB GitHub repo size limit <!-- 
  original URL:
  https://angryfrenchman.org/github-s-large-file-storage-is-no-panacea-for-open-source-quite-the-opposite-12c0e16a9a91 
  -->
- avoid the [downsides](https://archive.is/3D16r) of Git LFS
- version data flexibly (by creating/uploading to a new release)
- work with public and private repositories, **for free**

For more about motivations, see this discussion of
[alternatives](https://docs.ropensci.org/piggyback/articles/alternatives.html).

## Contributing

Please note that this project is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By participating in
this project you agree to abide by its terms.

[![ropensci_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
