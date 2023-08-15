#' Remove null elements of a list
#' @keywords internal
#' @noRd
compact <- function(l) Filter(Negate(is.null), l)

#' Parses repository spec and errors if it fails
#' @keywords internal
#' @noRd
parse_repo <- function(repo) {
  r <- strsplit(repo, "/")[[1]]

  if (length(r) != 2) {
    cli::cli_abort(
      c(
        "Could not parse {.val {repo}} as a GitHub repository.",
        "Make sure you have used the format: {.val owner/repo}"
      )
    )
  }

  return(r)
}

#' Guesses GH repo based on git remote info for current git directory
#' @keywords internal
#' @noRd
guess_repo <- function(path = ".") {
  paste(gh::gh_tree_remote(path), collapse = "/")
}

#' Extraction utils
#' @keywords internal
#' @noRd
.extract <- function(x, element, .default = NA) {
  out <- getElement(x, element)
  if (length(out) == 0) {
    return(.default)
  }
  return(out)
}

.extract_chr <- function(.x, .e) .map_chr(.x, .extract, .e, NA_character_)
.extract_int <- function(.x, .e) .map_int(.x, .extract, .e, NA_integer_)
.extract_lgl <- function(.x, .e) .map_lgl(.x, .extract, .e, NA)

.map_int <- function(.x, .f, ...) vapply(.x, .f, integer(1), ...)
.map_chr <- function(.x, .f, ...) vapply(.x, .f, character(1), ...)
.map_lgl <- function(.x, .f, ...) vapply(.x, .f, logical(1), ...)

#' date utils
#' @keywords internal
#' @noRd
.as_datetime <- function(x) {
  origin <- structure(0.0, class = c("POSIXct", "POSIXt"), tzone = "UTC")
  as.POSIXct(x, origin = origin, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
}

#' date utils
#' @keywords internal
#' @noRd
.as_date <- function(x) {
  as.Date(x, tz = "UTC")
}
