## Clean script for visStatistics development
## Safely removes installed package and clears build artifacts

pkg <- "visStatistics"

## 1. Unload package if loaded
if (pkg %in% loadedNamespaces()) {
  try(detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE), silent = TRUE)
}

## 2. Remove installed version completely
lib_paths <- .libPaths()
pkg_dirs <- file.path(lib_paths, pkg)
existing <- pkg_dirs[file.exists(pkg_dirs)]

if (length(existing)) {
  message("Removing installed package at: ", existing)
  unlink(existing, recursive = TRUE, force = TRUE)
} else {
  message("No installed version of ", pkg, " found.")
}

## 3. Clean local source directory (assumes working dir is package root)
pkg_root <- getwd()

# Remove compiled objects and database artifacts
to_delete <- c("NAMESPACE", "man", "build", "Meta", 
               "R/visStatistics.rdb", "R/visStatistics.rdx", "R/visStatistics.rds",
               "R/*.rdb", "R/*.rdx", "R/*.rds", 
               "R/*.so", "R/*.dll", "R/*.o", "R/*.a",
               ".Rbuildignore", ".Rproj.user", ".Rhistory", ".RData")

for (pattern in to_delete) {
  files <- Sys.glob(file.path(pkg_root, pattern))
  unlink(files, recursive = TRUE, force = TRUE)
}

# Optionally also remove vignettes or documentation builds
unlink(file.path(pkg_root, "vignettes/*.html"), force = TRUE)
unlink(file.path(pkg_root, "vignettes/*.R"), force = TRUE)
unlink(file.path(pkg_root, "vignettes/*.tex"), force = TRUE)

# Optional: remove roxygen2 generated folders
# unlink(file.path(pkg_root, "man"), recursive = TRUE, force = TRUE)

message("Cleanup complete. Ready for fresh build.")