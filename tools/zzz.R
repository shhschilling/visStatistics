.onAttach <- function(libname, pkgname) {
  # Get all exported functions of the package
  pkg_exports <- getNamespaceExports(pkgname)
  
  # Find which are masked in .GlobalEnv (manually sourced or defined)
  masked <- pkg_exports[sapply(pkg_exports, function(f)
    exists(f, envir = .GlobalEnv, inherits = FALSE))]
  
  if (length(masked) > 0) {
    # Remove the masking symbols from global environment
    rm(list = masked, envir = .GlobalEnv)
    
    # Inform the user
    packageStartupMessage(
      "Unlinked the following functions from .GlobalEnv to avoid masking visStatistics exports:\n",
      paste0("  - ", masked, collapse = "\n"),
      "\nNow using package versions only."
    )
  }
}