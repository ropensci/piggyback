#' Write one object to repo/release
#'
#' A convenience wrapper around writing an object to a temporary file and then
#' uploading to a specified repo/release.
#'
#' @param x object: memory object to save to piggyback
#' @param file string: file name
#' @param repo string: GH repository name in format "owner/repo". Default
#' `guess_repo()` tries to guess based on current working directory's git repo
#' @param tag  string: tag for the GH release, defaults to "latest"
#' @param write_function function: specifies how to read in the data. Default
#' tries to guess a function based on file extension (csv, rds, txt, parquet, json)
#' @param ... additional arguments passed to `write_function`
#' @param .token GitHub authentication token, see [gh::gh_token()]
#'
#' @export
#'
#' @return Writes file to release and returns github API response
#' @examples \donttest{
#' if (interactive()) {
#'   pb_write(mtcars, "mtcars.rds", repo = "tanho63/piggyback-tests")
#'   #> ℹ Uploading to latest release: "v0.0.2".
#'   #> ℹ Uploading mtcars.rds ...
#'   #> |===============================================================| 100%
#' })
#' }
pb_write <- function(x,
                     file,
                     repo = guess_repo(),
                     tag = "latest",
                     write_function = guess_write_function(file),
                     ...,
                     .token = gh::gh_token()) {
  stopifnot(
    is.character(file) && length(file) == 1,
    is.character(repo) && length(repo) == 1,
    is.character(tag) && length(tag) == 1,
    rlang::is_function(write_function)
  )
  destfile <- file.path(tempdir(check = TRUE), file)
  on.exit(try(unlink(destfile)))
  write_function(x, destfile, ...)
  pb_upload(destfile, repo = repo, tag = tag, .token = .token)
}

guess_write_function <- function(file){
  file_ext <- tools::file_ext(gsub(x = file, pattern = ".gz$|.xz$", replacement = ""))
  if (file_ext == "parquet") rlang::check_installed("arrow")

  write_fn <- switch(
    file_ext,
    "rds" = saveRDS,
    "csv" = write.csv,
    "txt" = writeLines,
    "parquet" = arrow::write_parquet,
    "json" = jsonlite::toJSON,
    cli::cli_abort("File type {.val {file_ext}} is not recognized, please provide a {.arg write_function}")
  )

  return(write_fn)
}
