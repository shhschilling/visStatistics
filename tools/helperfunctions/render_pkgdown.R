render_pkgdown <- function(docs_dir = "docs", install = FALSE) {
  if (!requireNamespace("pkgdown", quietly = TRUE)) {
    stop("Package 'pkgdown' is required.", call. = FALSE)
  }

  if (isTRUE(requireNamespace("sass", quietly = TRUE))) {
    options(sass.cache = file.path(tempdir(), "sass-cache"))
  }

  config <- "_pkgdown.yml"
  if (file.exists(config) && requireNamespace("yaml", quietly = TRUE)) {
    original_config <- readLines(config, warn = FALSE)
    on.exit(writeLines(original_config, config, useBytes = TRUE), add = TRUE)

    pkgdown_config <- yaml::read_yaml(config)
    pkgdown_config$articles <- Filter(
      function(section) !identical(section$title, "internal"),
      pkgdown_config$articles
    )
    yaml::write_yaml(pkgdown_config, config)
  }

  pkgdown::build_site(install = install)

  cleanup <- file.path("tools", "helperfunctions", "clean_pkgdown_private_pages.R")
  if (file.exists(cleanup)) {
    source(cleanup, local = TRUE)
    clean_pkgdown_private_pages(docs_dir)
  }

  invisible(normalizePath(docs_dir, mustWork = FALSE))
}

args <- commandArgs(trailingOnly = TRUE)
docs_dir <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else "docs"
render_pkgdown(docs_dir = docs_dir)
