build_pkgdown_site <- function(open = TRUE) {
  # STEP 0: Remove stale index.md if it exists.
  # pkgdown's build_home() prefers index.md > README.md, so a stray
  # index.md would shadow the freshly rendered README.md.
  if (file.exists("index.md")) {
    message(" Removing stale index.md (would shadow README.md in pkgdown) ...")
    file.remove("index.md")
  }

  # STEP 1: Render README.Rmd -> README.md using the github_document
  # format declared in the YAML header of README.Rmd.
  # devtools::build_readme() is the canonical wrapper and avoids the
  # bug of overriding the YAML format with "md_document".
  message(" Building README.md from README.Rmd via devtools::build_readme() ...")
  devtools::build_readme()

  # STEP 2: Update documentation (roxygen2)
  message(" Running devtools::document() ...")
  devtools::document()

  # STEP 3: Clean old site
  if (dir.exists("docs")) {
    message(" Removing old docs/ directory ...")
    unlink("docs", recursive = TRUE)
  }

  # STEP 4: Build the site.
  # Use build_site_github_pages() to mirror the CI workflow exactly:
  # it adds .nojekyll and a 404 handler that build_site() does not.
  message(" Building pkgdown site (GitHub Pages flavour) ...")
  pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)

  # STEP 5: Open in browser if desired
  if (open) {
    message(" Opening site in browser ...")
    browseURL("docs/index.html")
  }

  message(" Website build complete.")
}