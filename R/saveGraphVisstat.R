#' Saves Graphical Output
#'
#' Closes all graphical devices with \code{dev.off()} and saves the output only
#' if both \code{fileName} and \code{type} are provided.
#'
#' @param fileName name of file to be created in directory \code{fileDirectory}
#'   without file extension '.\code{type}'.
#' @param type see \code{Cairo()}.
#' @param fileDirectory path of directory, where graphic is stored. Default
#'   setting current working directory.
#' @param oldfile old file of same name to be overwritten
#'
#' @return NULL, if no \code{type} or \code{fileName} is provided, TRUE if graph
#'   is created
#' @examples
#'
#' # very simple KDE (adapted from example in Cairo())
#' openGraphCairo(type = "png", fileDirectory = tempdir())
#' plot(rnorm(4000), rnorm(4000), col = "#ff000018", pch = 19, cex = 2)
#' # save file 'norm.png' in directory specified in fileDirectory
#' saveGraphVisstat("norm", type = "png", fileDirectory = tempdir())
#' file.remove(file.path(tempdir(), "norm.png")) # remove file 'norm.png'
#'
#' @export
saveGraphVisstat <- function(fileName = NULL,
                             type = NULL,
                             fileDirectory = getwd(),
                             oldfile = NULL) {
  # return if no fileName is provided
  
  if (is.null(fileName)) {
   # message('saveGraphVisstat() returns NULL if fileName=NULL')
    return()
  } else if (is.null(type)) {
    #message('saveGraphVisstat() returns NULL if type=NULL')
    return()
  } else if (is.null(oldfile)) {
    dummy_name <- "visstat_plot"
    oldPlotName <- paste(dummy_name, ".", type, sep = "")
    oldfile <- file.path(fileDirectory, oldPlotName)
  }
  
  
  while (!is.null(dev.list())) {
    dev.off()
  } # closes all devices
  
  # overwrite existing files
  file2 <- gsub("[^[:alnum:]]", "_", fileName) # replaces numbers and '^' with
  # underscore
  file3 <- gsub("_{2,}", "_", file2)
  
  newFileName <- paste0(file3, ".", type)
  Cairofile <- file.path(fileDirectory, newFileName)
  file.copy(oldfile, Cairofile, overwrite = T)
  
  if (file.exists(oldfile)) {
    file.remove(oldfile)
  }
  
  return(invisible(Cairofile))
}
