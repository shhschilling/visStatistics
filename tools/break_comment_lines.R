break_comment_lines <- function(path = "R", limit = 80) {
  r_files <- list.files(path, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  
  for (file in r_files) {
    lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
    changed <- FALSE
    out <- list()
    
    for (line in lines) {
      # Only process real comments (skip Roxygen and code)
      if (!grepl("^\\s*#(?!')", line, perl = TRUE) || nchar(line) <= limit) {
        out[[length(out) + 1]] <- line
        next
      }
      
      changed <- TRUE
      indent <- sub("(#\\s*).*", "\\1", line)  # preserve comment prefix
      text <- sub("^\\s*#\\s*", "", line)
      
      # Split words greedily into blocks
      words <- strsplit(text, "\\s+")[[1]]
      buf <- indent
      for (word in words) {
        if (nchar(buf) + nchar(word) + 1 > limit) {
          out[[length(out) + 1]] <- buf
          buf <- paste0(indent, word)
        } else {
          buf <- paste(buf, word)
        }
      }
      out[[length(out) + 1]] <- buf
    }
    
    if (changed) {
      writeLines(unlist(out), file, useBytes = TRUE)
      cat("✔ Wrapped:", file, "\n")
    }
  }
}