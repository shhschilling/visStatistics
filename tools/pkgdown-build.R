# tools/pkgdown-build.R

# Rebuild README.md for GitHub
devtools::build_readme()

# Re-render README.Rmd to index.md for pkgdown homepage
rmarkdown::render(
  "README.Rmd",
  output_format = "md_document",
  output_file = "index.md"
)

# Rebuild vignettes
devtools::build_vignettes()

# Build the pkgdown site
pkgdown::build_site()
