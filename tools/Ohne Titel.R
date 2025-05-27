openGraphCairo(type = "png", fileDirectory = tempdir())
plot(rnorm(4000), rnorm(4000), col = "#ff000018", pch = 19, cex = 2)
hans=saveGraphVisstat("norm", type = "png", fileDirectory = tempdir())
