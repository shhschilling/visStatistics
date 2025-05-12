# tools/check_long_lines.R

#' Check all .R files for lines exceeding a given length
#'
#' Use from the package root (not from inside tools/).
#'
#' @param path Path to directory with .R files (default: "../R")
#' @param limit Max allowed characters per line (default: 80)
#' @export
check_long_lines <- function(path = "R", limit = 80) {
  r_files <- list.files(path, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)

  found <- FALSE

  for (file in r_files) {
    lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
    long_lines <- which(nchar(lines) > limit)

    if (length(long_lines)) {
      found <- TRUE
      for (i in long_lines) {
        cat(sprintf(
          "File: %s\nLine: %d (%d chars)\n→ %s\n\n",
          normalizePath(file), i, nchar(lines[i]), substr(lines[i], 1, 120)
        ))
      }
    }
  }

  if (!found) {
    message("✔ No lines longer than ", limit, " characters found in ", normalizePath(path), "/")
  }
}
