build_pkgdown_site <- function(open = TRUE) {
  
  if (dir.exists("docs")) {
    unlink("docs", recursive = TRUE)
  }
  
  message("Running roxygen2 documentation ...")
  devtools::document()
  
  message("Building pkgdown site ...")
  pkgdown::build_site_github_pages(
    new_process = TRUE,
    install = FALSE
  )
  
  if (open) {
    browseURL("docs/index.html")
  }
  
  message("Done.")
}