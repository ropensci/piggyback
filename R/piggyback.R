#' piggyback: Managing Larger Data on a GitHub Repository
#'
#' Because larger (> 50 MB) data files cannot easily be committed to git,
#' a different approach is required to manage data associated with an analysis in a
#' GitHub repository.  This package provides a simple work-around by allowing larger
#' (up to 2 GB) data files to piggyback on a repository as assets attached to individual
#' GitHub releases.  These files are not handled by git in any way, but instead are
#' uploaded, downloaded, or edited directly by calls through the GitHub API. These
#' data files can be versioned manually by creating different releases.  This approach
#' works equally well with public or private repositories.  Data can be uploaded
#' and downloaded programmatically from scripts. No authentication is required to
#' download data from public repositories.
#'
#' It has two main modes or workflows:
#'
#' - `pb_upload()` / `pb_download()`:  Upload and download individual files to/from
#'   the desired release of the specified repository
#' - `pb_track()` + `pb_pull()` / `pb_push()`: Use a `git-lfs` style tracking of specific
#'    file types and sync all tracked files with `pull` and `push` commands.  Convenient
#'    for projects working with many piggybacked data files.
#'
"_PACKAGE"
