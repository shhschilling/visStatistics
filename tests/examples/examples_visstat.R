
##Examples------
#Trees data set----
#linear Regression
visstat(trees,"Girth","Height") 
#linear regression saving both statistics and generated plot
#
#save statistics to "linear_regression_trees" and graph to regression_Girth_Height.png
# in working directory
# If graphicsoutput variable is set, plots are stored in the working directory with a name following the pattern
#"statisticalTestName_varsample_varfactor.graphicsoutput"
linear_regression_trees = visstat(trees,"Girth","Height",graphicsoutput = "png") 
#remove file
file.remove("regression_Girth_Height.png")

#example Welch two sample t.test
mtcars$am = as.factor(mtcars$am)
welch_cars=visstat(mtcars,"mpg","am")
welch_cars




#InsectSprays  data set----
#ANOVA
visstat(InsectSprays,"count","spray")

#example Welch two sample t.test
# select sprays A and B
InsectSpraysAB <- InsectSprays[ which(InsectSprays$spray == 'A'
| InsectSprays$spray == 'B'), ]
InsectSpraysAB$spray = factor(InsectSpraysAB$spray)

#welcht-t-Test
visstat(InsectSpraysAB,"count","spray")



#ANOVA
visstat(InsectSprays,"count","spray")


#Iris data set----
#Kruskal-Wallis test
visstat(iris,"Petal.Width", "Species")


#Chick weight data set----
#Kruskal-Wallis test
visstat(ChickWeight,"weight", "Diet")

#ToothGrowth data set----
#Wilcoxon rank sum test
visstat(ToothGrowth,"len", "supp")



#Titanic data set----
#install.packages("titanic")
#example categorical data,
library(titanic)
titanic_train$Survived = as.factor(titanic_train$Survived)
titanic_train$Pclass = as.factor(titanic_train$Pclass)
#Pearsons Chi squared, mosaic plot with Pearson's residuals
visstat(titanic_train,"Survived","Pclass")



#HairEyeColor data set: Pearsons Chi squared, mosaic plot with Pearson's residuals
HairEyeColorMale = counts_to_cases(as.data.frame(HairEyeColor[,,1]));
visstat(HairEyeColorMale,"Hair","Eye")
HairEyeColorMaleFisher =  HairEyeColor[,,1]
#replace cells to smaller values to enforce Cochran's rule
HairEyeColorMaleFisher[HairEyeColorMaleFisher<10] = 4
HairEyeColorMaleFisher = counts_to_cases(as.data.frame(HairEyeColorMaleFisher));
visstat(HairEyeColorMaleFisher,"Hair","Eye")


#2x2 contingency tables....
HairEyeColorMaleFisher = HairEyeColor[,,1]
#slicing out a 2 x2 contingency table
blackBrownHazelGreen = HairEyeColorMaleFisher[1:2,3:4]
fishertest = blackBrownHazelGreen
blackBrownHazelGreen = counts_to_cases(as.data.frame(blackBrownHazelGreen));
visstat(blackBrownHazelGreen,"Hair","Eye")







