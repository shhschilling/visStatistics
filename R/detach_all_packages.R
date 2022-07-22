detach_all_packages <- function() {
  
  basic.packages <- c("package:grid","package:tools","package:compiler","package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search())) == 1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  lapply(package.list, detach, character.only = TRUE, unload = TRUE)
  # if (length(package.list) > 0)  {
  #   for (package in package.list) detach(package, character.only = TRUE, unload - TRUE)}
}