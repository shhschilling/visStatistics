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