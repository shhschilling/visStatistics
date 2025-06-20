% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opening_saving_Cairo.R
\name{saveGraphVisstat}
\alias{saveGraphVisstat}
\title{Saves Graphical Output with plot capture capability}
\usage{
saveGraphVisstat(
  fileName = NULL,
  type = NULL,
  fileDirectory = getwd(),
  oldfile = NULL,
  capture_env = NULL
)
}
\arguments{
\item{fileName}{name of file to be created in directory \code{fileDirectory}
without file extension '.\code{type}'.}

\item{type}{see \code{Cairo()}.}

\item{fileDirectory}{path of directory, where graphic is stored. Default
setting current working directory.}

\item{oldfile}{old file of same name to be overwritten}

\item{capture_env}{Environment to store captured plots. If NULL, no capture occurs.}
}
\value{
NULL, if no \code{type} or \code{fileName} is provided, file path if graph
  is created
}
\description{
Closes all graphical devices with \code{dev.off()} and saves the output only
if both \code{fileName} and \code{type} are provided. Enhanced version that
can capture plots before closing devices.
}
\examples{

# very simple KDE (adapted from example in Cairo())
openGraphCairo(type = "png", fileDirectory = tempdir())
plot(rnorm(4000), rnorm(4000), col = "#ff000018", pch = 19, cex = 2)
# save file 'norm.png' in directory specified in fileDirectory
saveGraphVisstat("norm", type = "png", fileDirectory = tempdir())
file.remove(file.path(tempdir(), "norm.png")) # remove file 'norm.png'

}
