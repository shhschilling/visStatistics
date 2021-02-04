#choose output directory 
filedir=tempdir()


graphicsoutput="png"
openGraphCairo(type = graphicsoutput, fileDirectory=filedir)
plot(rnorm(4000),rnorm(4000),col="#ff000018",pch=19,cex=2) 
saveGraphVisstat(
  "norm1",type = graphicsoutput,fileDirectory=filedir)



graphicsoutput="pdf"
openGraphCairo(type = graphicsoutput, fileDirectory=filedir)
plot(rnorm(4000),rnorm(4000),col="#ff000018",pch=19,cex=2) 
saveGraphVisstat(
  "norm2",type = graphicsoutput,fileDirectory=filedir)

graphicsoutput="ps"
openGraphCairo(type = graphicsoutput,fileDirectory = filedir)
plot(rnorm(4000),rnorm(4000),col="#ff000018",pch=19,cex=2) 
saveGraphVisstat(
  "norm3",type = graphicsoutput, fileDirectory=filedir)

graphicsoutput="svg"
openGraphCairo(type = graphicsoutput, fileDirectory = filedir)
plot(rnorm(4000),rnorm(4000),col="#ff000018",pch=19,cex=2) 
saveGraphVisstat(
  "norm4",type = graphicsoutput, fileDirectory=filedir)




#remove output  plots
graphicaltypes=c(".png", ".pdf", ".svg", ".ps")
for (i in graphicaltypes) {
  plotname=dir(filedir,pattern=i)
  print(file.path(filedir,plotname))
  file.remove(file.path(filedir,plotname))
  }



