openGraphCairo(type="png")
plot(rnorm(1000),rnorm(1000),col="#ff000018",pch=19,cex=2) 
saveGraphVisstat(file="hansi2",type="png")
file.remove("hansi2.png")

visstat(ToothGrowth,"len", "supp", graphicsoutput = "png")


HairEyeColorMaleFisher=HairEyeColor[,,1]
HairEyeColorMaleFisher[HairEyeColorMaleFisher<10]=4 #create example enforcing Cochran's rule
# #transform contingency table to data.frame
HairEyeColorMaleFisher = counts_to_cases(as.data.frame(HairEyeColorMaleFisher)) 
visstat(HairEyeColorMaleFisher,"Hair","Eye", graphicsoutput ="png") #example creating two output files 
