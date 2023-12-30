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
#' @param write_function function: used to write an R object to file, where the
#' object is passed as the first argument, the filename as the second argument,
#' and any additional arguments are subsequently passed in via `...`. Default
#' `guess_write_function(file)` will check the file extension and try to find an
#' appropriate write function if the extension is one of rds, csv, tsv, parquet,
#' txt, or json, and will abort if not found.
#' @param ... additional arguments passed to `write_function`
#' @param .token GitHub authentication token, see [gh::gh_token()]
#'
#' @export
#' @family pb_rw
#'
#' @return Writes file to release and returns github API response
#' @examples \donttest{
#' \dontshow{if (interactive()) \{}
#'   pb_write(mtcars, "mtcars.rds", repo = "tanho63/piggyback-tests")
#'   #> ℹ Uploading to latest release: "v0.0.2".
#'   #> ℹ Uploading mtcars.rds ...
#'   #> |===============================================================| 100%
#' \dontshow{\}}
#'}
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

#' Guess write function from file extension
#'
#' This function accepts a filename and tries to return a valid function for
#' writing to it.
#'
#' `guess_write_function` understands the following file extensions:
#' - rds with `saveRDS`
#' - csv, csv.gz, csv.xz with `utils::write.csv`
#' - tsv, tsv.gz, tsv.xz with a modified `utils::write.csv` where sep is set to `"\t"`
#' - parquet with `arrow::write_parquet`
#' - txt, txt.gz, txt.xz with `writeLines`
#' - json, json.gz, json.xz with `jsonlite::write_json`
#'
#' @family pb_rw
#' @param file filename to parse
#' @return function for reading the file, if found
#' @keywords internal
guess_write_function <- function(file){
  file_ext <- tools::file_ext(gsub(x = file, pattern = ".gz$|.xz$", replacement = ""))
  if (file_ext == "parquet") rlang::check_installed("arrow")

  write_fn <- switch(
    file_ext,
    "rds" = saveRDS,
    "csv" = utils::write.csv,
    "tsv" = function(x, file, ..., sep = "\t") utils::write.csv(x = x, file = file, sep = sep, ...),
    "txt" = writeLines,
    "parquet" = arrow::write_parquet,
    "json" = jsonlite::write_json,
    cli::cli_abort("File type {.val {file_ext}} is not recognized, please provide a {.arg write_function}")
  )

  return(write_fn)
}
