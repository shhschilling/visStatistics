#' Saves Graphical Output 
#'
#' Saves Graphical Output of call to function \code{visstat}
#'
#'to be filled 
#'
#
#' @param file name of file to be created or connection to write to without file extension ".\code{type}". Default file name "visstat". 
#' @param type see \code{Cairo()}
#' @param oldplotName old file name including .type extension to be overwritten 
#' @return NULL, if no \code{type} or \code{file} is provided
#' @examples
#' # return NULL
#' saveGraphVisstat()
#' 
#' # very simple KDE (adapted from example in \code{Cairo()})
#'openGraphCairo(type="png",mag =2.54) #mag is conversion factor cm to inch
#'plot(rnorm(4000),rnorm(4000),col ="#ff000018",pch=19,cex=2) # semi-transparent red
#'saveGraphVisstat(file="random",type ="png")
#'if (!interactive()) file.remove("random.png"

#' 
#' @export saveGraphVisstat

saveGraphVisstat = function(file=NULL, type=NULL, oldPlotName = NULL) {
  #return if no file file is provided or no file is provided 
  
   if (is.null(file))
    {
      #message("saveGraphVisstat() returns NULL if file=NULL")
      return()
   }else if (is.null(type)) {
    # message("saveGraphVisstat() returns NULL if type=NULL")
     return() 
}else{  
  if (is.null(oldPlotName)) {
    dummy_name = "visstat_plot"
    oldPlotName = paste(dummy_name,".",type,sep = "")
  }
  
  #dev.off()
  while (!is.null(dev.list()))  dev.off()
  file2 = gsub("[^[:alnum:]]", "_", file)
  file3 = gsub("_{2,}", "_", file2)
  newFileName = paste0(file3,".", type)
  file.copy(oldPlotName,newFileName,overwrite = T)
  
  if (file.exists(oldPlotName)) {
  file.remove(oldPlotName)}
  
}

}
