clean_pkgdown_private_pages <- function(docs_dir = "docs") {
  private_pages <- c("AGENTS", "CLAUDE")
  private_articles <- c(
    "discussion_addition_draft",
    "internally_studentised_residuals_old_versions",
    "residual_covariance_draft",
    "residualtest",
    "standardised_residuals_old_versions",
    "sw_on_residuals_research",
    "visStatistics_redlined",
    "visStatistics202512"
  )
  private_paths <- file.path(
    docs_dir,
    as.vector(outer(private_pages, c(".html", ".md"), paste0))
  )
  private_article_paths <- file.path(
    docs_dir,
    "articles",
    as.vector(outer(private_articles, c(".html", ".md"), paste0))
  )
  private_article_dirs <- file.path(
    docs_dir,
    "articles",
    paste0(private_articles, "_files")
  )
  unlink(private_paths, force = TRUE)
  unlink(private_article_paths, force = TRUE)
  unlink(private_article_dirs, recursive = TRUE, force = TRUE)

  sitemap <- file.path(docs_dir, "sitemap.xml")
  if (file.exists(sitemap)) {
    lines <- readLines(sitemap, warn = FALSE)
    private_article_pattern <- paste(private_articles, collapse = "|")
    keep <- !grepl("/(AGENTS|CLAUDE)\\.html", lines) &
      !grepl(paste0("/articles/(", private_article_pattern, ")\\.html"), lines)
    writeLines(lines[keep], sitemap, useBytes = TRUE)
  }

  search <- file.path(docs_dir, "search.json")
  if (file.exists(search) && requireNamespace("jsonlite", quietly = TRUE)) {
    entries <- jsonlite::fromJSON(search, simplifyDataFrame = TRUE)
    private_article_pattern <- paste(private_articles, collapse = "|")
    keep <- !grepl("/(AGENTS|CLAUDE)\\.html", entries$path) &
      !grepl(paste0("articles/(", private_article_pattern, ")\\.html"), entries$path)
    jsonlite::write_json(
      entries[keep, , drop = FALSE],
      search,
      auto_unbox = TRUE,
      null = "null"
    )
  }

  metadata <- file.path(docs_dir, "pkgdown.yml")
  if (file.exists(metadata)) {
    lines <- readLines(metadata, warn = FALSE)
    private_article_pattern <- paste(private_articles, collapse = "|")
    keep <- !grepl(paste0("^  (", private_article_pattern, "): "), lines)
    writeLines(lines[keep], metadata, useBytes = TRUE)
  }
}

args <- commandArgs(trailingOnly = TRUE)
clean_pkgdown_private_pages(if (length(args)) args[[1]] else "docs")
