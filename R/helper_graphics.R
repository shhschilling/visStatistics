
##for the first installation
library(Cairo)
dummy_name="dummy_plot"
#dummy_name_with_filetype=paste(dummy_name,".",graphicsoutput,sep="")
## Open Cairo output-------
openGraphCairo = function(file =dummy_name ,type=NULL ,width = 8,
                          height = 8 / sqrt(2) ,
                          mag = 1.0 ,
                          ...) {

  if (is.null(type))
  {return()}
  else{
  if(is.null(file)) {
    file="dummyfile"}



  picture_type=type
  Cairo(paste(file, ".", type, sep = ""),
        type = picture_type,
        width = width * mag,
        height = height * mag,
        units = "in",
        dpi =150,
        pointsize = 12)
  }
}


## Save Cairo output-------
saveGraphCairo = function(file, type=NULL,oldPlotName =NULL) {
  #close file first
  if(is.null(type))
  {return()}
    else{



  if(is.null(oldPlotName)) {
    dummy_name="dummy_plot"
    oldPlotName=paste(dummy_name,".",type,sep="")
  }

  dev.off()
  file2 = gsub("[^[:alnum:]]", "_", file)
  file3 = gsub("_{2,}", "_", file2)
  newFileName = paste0(file3,".", type)
  file.copy(oldPlotName,newFileName,overwrite=T)

  file.remove(oldPlotName)

}
}

