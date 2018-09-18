
#' Track data files of a given pattern or location
#'
#' @param glob vector of file names and/or glob pattern (e.g. `*.csv`, `data/*.csv`)
#' which will be tracked by piggyback.  Omit (default `NULL`) to just return
#' a list of files currently tracked.
#' @param repo_root repository root, will be guessed by `usethis` otherwise.
#' @details Note: tracked patterns are simply written to `.pbattributes`
#' (analogous to `.gitattributes` in `git-lfs`.)  You can also edit this
#' file manually.  You will probably want to check in `.psattributes` to
#' as to version control., with `git add .psattributes`.  Note that
#' tracked file patterns will also be added to `.gitignore`.
#' @importFrom usethis use_git_ignore proj_get
#' @importFrom fs path_join
#' @return list of tracked files (invisibly)
#' @export
#' @examples
#' \dontrun{
#' ## Track all .csv and .tsv files
#' pb_track(c("*.tsv", "*.tsv.gz"))
#'
#' }
pb_track <- function(glob = NULL, repo_root = usethis::proj_get()) {
  if (!is.null(glob)) {
    write_union(
      usethis::proj_get(),
      ".pbattributes",
      glob
    )
    usethis::use_build_ignore(c(".pbattributes", "manifest.json"))
    if (!is.null(git2r::discover_repository("."))) {
      usethis::use_git_ignore("manifest.json")
      usethis::use_git_ignore(glob)
    }
  }


  pbattrs <- fs::path_rel(".pbattributes", repo_root)
  if (file.exists(pbattrs)) {
    glob <- readLines(pbattrs, warn = FALSE)
  } else {
    glob <- character(0L)
  }
  invisible(match_globs(glob, repo_root))
}

## Helper function
#' @importFrom fs path_rel path_filter
match_globs <- function(globs, proj_dir = usethis::proj_get()) {
  unique(
    unname(unlist(lapply(globs, function(g) {
      ## only match files (i.e. things we can hash)
      fs::dir_ls(path = proj_dir, recursive = TRUE, type = "file") %>%
        fs::path_rel(proj_dir) %>%
        fs::path_filter(glob = g)
    })))
  )
}

