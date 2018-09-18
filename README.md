
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis-CI Build
Status](https://travis-ci.org/cboettig/piggyback.svg?branch=master)](https://travis-ci.org/cboettig/piggyback)
[![Coverage
status](https://codecov.io/gh/cboettig/piggyback/branch/master/graph/badge.svg)](https://codecov.io/github/cboettig/piggyback?branch=master)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/cboettig/piggyback?branch=master&svg=true)](https://ci.appveyor.com/project/cboettig/piggyback)
[![CRAN
status](https://www.r-pkg.org/badges/version/piggyback)](https://cran.r-project.org/package=piggyback)
[![](https://badges.ropensci.org/220_status.svg)](https://github.com/ropensci/onboarding/issues/220)

# piggyback

`piggyback` is basically a poor soul’s [Git
LFS](https://git-lfs.github.com/). GitHub rejects commits containing
files larger than 50 Mb. Git LFS is not only expensive, it also [breaks
GitHub’s collaborative
model](https://medium.com/@megastep/github-s-large-file-storage-is-no-panacea-for-open-source-quite-the-opposite-12c0e16a9a91).
(Someone wants to submit a PR with a simple edit to your docs, they
cannot fork) Unlike Git LFS, `piggyback` doesn’t take over your standard
`git` client, it just perches comfortably on the shoulders of your
existing GitHub API. Data can be versioned by `piggyback`, but relative
to `git LFS` versioning is less strict: uploads can be set as a new
version or allowed to overwrite previously uploaded data. `piggyback`
works with both public and private repositories.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cboettig/piggyback")
```

## Quickstart

See the [piggyback
vignette](https://cboettig.github.io/piggyback/articles/intro.html) for
details on authentication and additional package functionality.

Piggyback can download data attached to a release on any repository:

``` r
library(piggyback)
pb_download( "data/mtcars.tsv.gz", repo = "cboettig/piggyback")
```

Downloading from private repos or uploading to any repo requires
authentication, so be sure to set a `GITHUB_TOKEN` (or `GITHUB_PAT`)
environmental variable, or include the `.token` argument. See
[vignette](https://cboettig.github.io/piggyback/articles/intro.html) or
function documentation for details.

We can then also upload data to an existing release:

``` r
## We'll need some example data first.
## Pro tip: compress your tabular data to save space & speed upload/downloads
readr::write_tsv(mtcars, "mtcars.tsv.gz")

pb_upload("mtcars.tsv.gz", repo = "cboettig/piggyback")
```

### Tracking data files

For a [Git LFS](https://git-lfs.github.com/) style workflow, just
specify the type of files you wish to track using `pb_track()`.
Piggyback will retain a record of these files in a hidden
`.pbattributes` file in your repository, and add these to `.gitignore`
so you don’t accidentally commit them to GitHub. `pb_trak` will aslo
return a list of such files that you can easily pass to `pb_upload()`:

``` r
library(piggyback)
# track csv files, compressed data, and geotiff files:
pb_track(c("*.csv", "*.gz", "*.tif")) %>%
pb_upload()
```

You can easily download the latest version of all data attached to a
given release with `pb_download()` with no file argument (analgous to a
`git pull` for data):

``` r
pb_download()
```

-----

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.
