detach_all <- function() {
  basic_packages <- c("package:stats", "package:graphics", "package:grDevices", 
                      "package:utils", "package:datasets", "package:methods", 
                      "package:base")
  
  package_list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1, TRUE, FALSE)]
  package_list <- setdiff(package_list, basic_packages)
  
  if (length(package_list) > 0) {
    for (package in package_list) {
      detach(package, character.only = TRUE, unload = TRUE)
    }
  }
}

# Use it
detach_all()