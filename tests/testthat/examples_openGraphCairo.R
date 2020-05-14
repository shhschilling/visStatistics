openGraphCairo(type="png",mag=2.54) #mag is conversion factor cm to inch
plot(rnorm(4000),rnorm(4000),col="#ff000018",pch=19,cex=2) # semi-transparent red
dev.off() # creates a file "visstat_plot.png" with the above plot