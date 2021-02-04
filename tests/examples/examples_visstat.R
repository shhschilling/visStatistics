##Examples------
library(visStatistics)

#specify directory where plots will be stored. Without definition of plotDirectory: current working directory
filedir='/Users/sschilli/Desktop'

# linear regression: trees data set:----
visstat(trees,"Girth","Height") 

# If graphicsoutput parameter is set, all plots are stored in the directory specified in the parameter plotDirectory. 
# Default directory of plotDirectory is the working directory.
# Graphical output is named following the naming convention
#"statisticalTestName_varsample_varfactor.graphicsoutput" 
linear_regression_trees=visstat(trees,"Girth","Height",graphicsoutput = "png",plotDirectory=filedir) ; 
#nach zweitem Aufruf Fehler par(oldpa)
linear_regression_trees=visstat(trees,"Girth","Height",graphicsoutput = "pdf",plotDirectory=filedir) ; 
linear_regression_trees=visstat(trees,"Girth","Height",graphicsoutput = "svg",plotDirectory=filedir) ; 


#display stats of linear regression
linear_regression_trees

#Welch two sample t.test: mtcars data set ----
mtcars$am = as.factor(mtcars$am)
welch_cars=visstat(mtcars,"mpg","am")
#store graphical output in different formats in directory defined in argument plotDirectory
welch_cars=visstat(mtcars,"mpg","am",graphicsoutput="png",plotDirectory=filedir) 
welch_cars=visstat(mtcars,"mpg","am",graphicsoutput="pdf",plotDirectory=filedir) 
welch_cars=visstat(mtcars,"mpg","am",graphicsoutput="svg",plotDirectory=filedir) 
welch_cars=visstat(mtcars,"mpg","am",graphicsoutput="ps",plotDirectory=filedir) 
welch_cars


#Kruskal-Wallis Insect spray----

kruskal_insects=visstat(InsectSprays,"count","spray")
warnkruskal_insects=visstat(InsectSprays,"count","spray",graphicsoutput="png",plotDirectory=filedir)

#Kruskal-Wallis test: iris----
visstat(iris,"Petal.Width", "Species")



#Welch two sample t.test: InsectSprays ----
# select sprays A and B
InsectSpraysAB <- InsectSprays[ which(InsectSprays$spray == 'A'
| InsectSprays$spray == 'B'), ]
InsectSpraysAB$spray = factor(InsectSpraysAB$spray)
#Welcht-t-Test
visstat(InsectSpraysAB,"count","spray") #plots not saved
visstat(InsectSpraysAB,"count","spray",graphicsoutput = "png",plotDirectory=filedir) 

#Wilcoxon rank sum test: ToothGrowth ----
visstat(ToothGrowth,"len", "supp")
visstat(ToothGrowth,"len", "supp",graphicsoutput = "png",plotDirectory=filedir)
visstat(ToothGrowth,"len", "supp",graphicsoutput = "pdf",plotDirectory=filedir)

#Chi squqred, mosaic plots with Titanic data set----
#install.packages("titanic")
#example categorical data,
library(titanic)
titanic_train$Survived = as.factor(titanic_train$Survived)
titanic_train$Pclass = as.factor(titanic_train$Pclass)
#Pearsons Chi squared, mosaic plot with Pearson's residuals
visstat(titanic_train,"Survived","Pclass")


#Chi squared, mosaic plots with HairEyeColor----
#HairEyeColor data set: Pearsons Chi squared, mosaic plot with Pearson's residuals
HairEyeColorMale = counts_to_cases(as.data.frame(HairEyeColor[,,1]));
visstat(HairEyeColorMale,"Hair","Eye")
HairEyeColorMaleFisher =  HairEyeColor[,,1]
#replace cells to smaller values to enforce Cochran's rule
HairEyeColorMaleFisher[HairEyeColorMaleFisher<10] = 4
HairEyeColorMaleFisher = counts_to_cases(as.data.frame(HairEyeColorMaleFisher));
res_chi=visstat(HairEyeColorMaleFisher,"Hair","Eye") #test statistics stored in res_chi
res_chi=visstat(HairEyeColorMaleFisher,"Hair","Eye",graphicsoutput = "png",plotDirectory=filedir) #stores two graphics outputs

#2x2 contingency tables---
HairEyeColorMaleFisher = HairEyeColor[,,1]
#slicing out a 2 x2 contingency table
blackBrownHazelGreen = HairEyeColorMaleFisher[1:2,3:4]
fishertest = blackBrownHazelGreen
blackBrownHazelGreen = counts_to_cases(as.data.frame(blackBrownHazelGreen));
visstat(blackBrownHazelGreen,"Hair","Eye")



#remove output plots
graphicaltypes=c(".png", ".pdf", ".svg", ".ps")
for (i in graphicaltypes) {
  plotname=dir(filedir,pattern=i)
  print(file.path(filedir,plotname))
  file.remove(file.path(filedir,plotname))
}


