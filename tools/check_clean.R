toggle_vignette_mode <- function(file = "vignettes/visStatistics.Rmd", mode = c("cran", "dev")) {
  mode <- match.arg(mode)
  
  cran_yaml <- c(
    "---",
    'title: "visStatistics"',
    'author: "Sabine Schilling"',
    'date: "`r Sys.Date()`"',
    "output:",
    "  html_vignette:",
    "    number_sections: true",
    "    toc: true",
    "    toc_depth: 3",
    "bibliography: visstat.bib",
    "vignette: >",
    "  %\\VignetteIndexEntry{visStatistics}",
    "  %\\VignetteEncoding{UTF-8}",
    "  %\\VignetteEngine{knitr::rmarkdown}",
    "editor_options:",
    "  markdown:",
    "    wrap: sentence",
    "---"
  )
  
  dev_yaml <- c(
    "---",
    'title: "visStatistics"',
    'author: "Sabine Schilling"',
    'date: "`r Sys.Date()`"',
    "output:",
    "  bookdown::html_document2:",
    "    number_sections: true",
    "    toc: true",
    "    toc_depth: 3",
    "bibliography: visstat.bib",
    "editor_options:",
    "  markdown:",
    "    wrap: sentence",
    "---"
  )
  
  lines <- readLines(file)
  
  # Find YAML header block
  delim_indices <- which(trimws(lines) == "---")
  if (length(delim_indices) < 2) stop("YAML header must start and end with '---'")
  
  # Keep rest of the file after header
  content <- lines[(delim_indices[2] + 1):length(lines)]
  
  # Replace header
  new_yaml <- if (mode == "cran") cran_yaml else dev_yaml
  new_lines <- c(new_yaml, content)
  
  writeLines(new_lines, file)
  message("📄 YAML header updated to ", mode, " mode.")
}

check_clean <- function(pkg = ".", ...) {
  toggle_vignette_mode(mode = "dev")
  
  message("🧪 Running devtools::check() with --as-cran and compact vignettes...")
  devtools::check(pkg = pkg, args = "--as-cran", build_args = "--compact-vignettes=qpdf", ...)
  
  message("🧹 Removing .DS_Store files...")
  system("find . -name '.DS_Store' -delete")
  
  message("🧼 Removing *.Rcheck directories...")
  rcheck_dirs <- list.dirs(pkg, full.names = TRUE, recursive = FALSE)
  rcheck_dirs <- rcheck_dirs[grepl("\\.Rcheck$", rcheck_dirs)]
  unlink(rcheck_dirs, recursive = TRUE)
  
  message("✅ All clean. Check complete.")
}