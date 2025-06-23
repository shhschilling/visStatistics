# tools/pkgdown-build.R


# Build the pkgdown site
pkgdown::build_site()
# Re-render README.Rmd to index.md for pkgdown homepage
rmarkdown::render(
  "README.Rmd",
  output_format = "md_document",
  output_file = "index.md"
)