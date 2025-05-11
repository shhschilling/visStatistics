# tools/break_long_lines.R

#' Break long lines (>80 chars) in .R files in R/ folder
#'
#' Breaks long lines at word boundaries (spaces) and indents continuation lines.
#' @param path Directory with .R files. Default: "R"
#' @param limit Maximum line length before breaking. Default: 80
#' @export
break_long_lines <- function(path = "R", limit = 80) {
  r_files <- list.files(path, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  
  for (file in r_files) {
    lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
    out <- character()
    
    for (line in lines) {
      if (nchar(line) <= limit) {
        out <- c(out, line)
      } else {
        while (nchar(line) > limit) {
          split_at <- max(gregexpr(" ", substr(line, 1, limit))[[1]])
          if (split_at == -1) split_at <- limit
          out <- c(out, substr(line, 1, split_at))
          line <- paste0("  ", trimws(substr(line, split_at + 1, nchar(line))))
        }
        out <- c(out, line)
      }
    }
    
    writeLines(out, file, useBytes = TRUE)
    cat("✔ Rewrote:", file, "\n")
  }
}