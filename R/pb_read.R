#' Read one file into memory
#'
#' A convenience wrapper around writing an object to a temporary file and then
#' uploading to a specified repo/release. This convenience comes at a cost to
#' performance efficiency, since it first downloads the data to disk and then
#' reads the data from disk into memory. See `vignette("duckdb_arrow")` for
#' alternative ways to bypass this flow and work with the data directly.
#'
#' @param file string: file name
#' @param repo string: GH repository name in format "owner/repo". Default
#' `guess_repo()` tries to guess based on current working directory's git repo
#' @param tag  string: tag for the GH release, defaults to "latest"
#' @param read_function function: specifies how to read in the data. Default
#' tries to guess a function based on file extension (csv, rds, parquet, txt, json)
#' @param ... additional arguments passed to `read_function`
#' @param .token GitHub authentication token, see [gh::gh_token()]
#'
#' @export
#'
#' @return Result of reading in the file in question.
#' @examples \donttest{
#' try({ # try block is to avoid CRAN issues and is not required in ordinary usage
#'  piggyback::pb_read("mtcars.tsv.gz", repo = "cboettig/piggyback-tests")
#' })
#' }
pb_read <- function(file,
                    repo = guess_repo(),
                    tag = "latest",
                    read_function = guess_read_function(file),
                    ...,
                    .token = gh::gh_token()) {
  stopifnot(
    is.character(file) && length(file) == 1,
    is.character(repo) && length(repo) == 1,
    is.character(tag) && length(tag) == 1,
    rlang::is_function(read_function)
  )

  on.exit(unlink(file.path(tempdir(), file)))

  pb_download(
    file = file,
    dest = tempdir(check = TRUE),
    repo = repo,
    tag = tag,
    overwrite = TRUE,
    .token = .token
  )

  read_function(file.path(tempdir(), file), ...)
}

guess_read_function <- function(file){
  file_ext <- tools::file_ext(gsub(x = file, pattern = ".gz$|.xz$", replacement = ""))
  if (file_ext == "parquet") rlang::check_installed("arrow")

  read_fn <- switch(
    file_ext,
    "rds" = readRDS,
    "csv" = read.csv,
    "tsv" = read.delim,
    "parquet" = arrow::read_parquet,
    "txt" = readLines,
    "json" = jsonlite::fromJSON,
    cli::cli_abort("File type {.val {file_ext}} is not recognized, please provide a {.arg read_function}")
  )

  return(read_fn)
}
