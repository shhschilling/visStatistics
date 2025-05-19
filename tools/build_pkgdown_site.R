build_pkgdown_site <- function(open = TRUE) {
  # STEP 1: Render README.Rmd -> README.md (your working line)
  message(" Rendering README.Rmd to README.md ...")
  rmarkdown::render(
    "README.Rmd",
    output_format = "md_document",
    output_file = "README.md"
  )
  
  # STEP 2: Update documentation (roxygen2)
  message(" Running devtools::document() ...")
  devtools::document()
  
  # STEP 3: Clean old site
  if (dir.exists("docs")) {
    message(" Removing old docs/ directory ...")
    unlink("docs", recursive = TRUE)
  }
  
  # STEP 4: Build the site
  message(" Building pkgdown site ...")
  pkgdown::build_site()
  
  # STEP 5: Open in browser if desired
  if (open) {
    message(" Opening site in browser ...")
    browseURL("docs/index.html")
  }
  
  message(" Website build complete.")
}