
# asset_filename <- function(x, start = ".") {
#   ## piggyback will no longer embed file structure in filename
#   ## Asset uploading is simply flat storage
#   x
# }

# local_filename <- function(x) {
#   # x <- gsub("^manifest.json$", ".manifest.json", x)
#   gsub("\\.2f", .Platform$file.sep, x)
# }


##################### Generic helpers ##################
# api_error_msg <- function(r) {
#   cli::cli_warn(
#     c("!"="Cannot access release data for repo {.val {paste0(r[[1]], '/', r[[2]])}}.",
#       "Check that you have provided a {code .token} and that there is at least one release on your repo"
#   ))
# }


# Adapted from `usethis` and under
# GPL-3 (Copyright RStudio Inc)
# https://github.com/r-lib/usethis/blob/aacf687445c94d4f726e31f682308a9a3ef3f820/R/write.R
# See https://github.com/r-lib/usethis/issues/366
# write_union <- function(base_path, path, new_lines, quiet = FALSE) {
#   stopifnot(is.character(new_lines))
#
#   full_path <- file.path(base_path, path)
#   if (file.exists(full_path)) {
#     lines <- readLines(full_path, warn = FALSE)
#   } else {
#     lines <- character()
#   }
#
#   new <- setdiff(new_lines, lines)
#   if (length(new) == 0) {
#     return(invisible(FALSE))
#   }
#
#   if (!quiet) {
#     quoted <- paste0(value(new), collapse = ", ")
#     done("Adding ", quoted, " to ", value(path))
#   }
#
#   all <- union(lines, new_lines)
#   write_utf8(full_path, all)
# }
#
# write_utf8 <- function(path, lines) {
#   stopifnot(is.character(path))
#   stopifnot(is.character(lines))
#
#   con <- file(path, encoding = "utf-8")
#   on.exit(close(con), add = TRUE)
#
#   if (length(lines) > 1) {
#     lines <- paste0(lines, "\n", collapse = "")
#   }
#   cat(lines, file = con, sep = "")
#
#   invisible(TRUE)
# }
#
# done <- function(...) {
#   bullet(paste0(...), bullet = crayon::green(clisymbols::symbol$tick))
# }
#
# value <- function(...) {
#   x <- paste0(...)
#   crayon::blue(encodeString(x, quote = "'"))
# }
#
# bullet <- function(lines, bullet) {
#   lines <- paste0(bullet, " ", lines)
#   cat_line(lines)
# }
#
# cat_line <- function(...) {
#   cat(..., "\n", sep = "")
# }

# utils::askYesKnow is new to R 3.5.0; avoid using it for backwards compatibility
# askYesNo <- function(msg){
#
#   prompts <- c("Yes", "No", "Cancel")
#   choices <- tolower(prompts)
#   msg1 <- paste0("(", paste(choices, collapse = "/"), ") ")
#
#   if (nchar(paste0(msg, msg1)) > 250) {
#     cat(msg, "\n")
#     msg <- msg1
#   }
#   else msg <- paste0(msg, " ", msg1)
#
#   ans <- readline(msg)
#   match <- pmatch(tolower(ans), tolower(choices))
#
#   if (!nchar(ans))
#     TRUE
#   else if (is.na(match))
#     stop("Unrecognized response ", dQuote(ans))
#   else c(TRUE, FALSE, NA)[match]
# }
