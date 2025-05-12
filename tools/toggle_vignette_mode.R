toggle_vignette_mode <- function(file = "vignettes/visStatistics.Rmd", mode = c("cran", "dev")) {
  mode <- match.arg(mode)

  common_fields <- c(
    'title: "visStatistics"',
    'author: "Sabine Schilling"',
    'date: "`r Sys.Date()`"',
    "bibliography: visstat.bib",
    "vignette: >",
    "  %\\VignetteIndexEntry{visStatistics}",
    "  %\\VignetteEngine{knitr::rmarkdown}",
    "  %\\VignetteEncoding{UTF-8}",
    "editor_options:",
    "  markdown:",
    "    wrap: sentence"
  )

  output_block <- switch(mode,
    cran = c(
      "output:",
      "  html_vignette:",
      "    toc: true",
      "    number_sections: true",
      "    toc_depth: 3"
    ),
    dev = c(
      "output:",
      "  bookdown::html_document2:",
      "    toc: true",
      "    number_sections: true",
      "    toc_depth: 3"
    )
  )

  new_yaml <- c("---", output_block, common_fields, "---")

  lines <- readLines(file)
  delim_indices <- which(trimws(lines) == "---")
  if (length(delim_indices) < 2) stop("Could not find complete YAML block.")

  content <- lines[(delim_indices[2] + 1):length(lines)]
  writeLines(c(new_yaml, content), con = file)

  message("📄 YAML header updated to ", mode, " mode.")
}
