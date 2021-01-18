
##Examples------
library(visStatistics)

# linear regression: trees data set:----
visstat(trees,"Girth","Height") 


# If graphicsoutput parameter is set, all plots are stored in the working directory 
# following the naming convention
#"statisticalTestName_varsample_varfactor.graphicsoutput" 
linear_regression_trees=visstat(trees,"Girth","Height",graphicsoutput = "png") ; 
file.remove("regression_Girth_Height.png")


#display stats of linear regression
linear_regression_trees

#Welch two sample t.test: mtcars data set ----
mtcars$am = as.factor(mtcars$am)
welch_cars=visstat(mtcars,"mpg","am")
welch_cars




#ANOVA: InsectSprays  data set----
#ANOVA
anova_insects=visstat(InsectSprays,"count","spray")

#Welch two sample t.test: InsectSprays 
# select sprays A and B
InsectSpraysAB <- InsectSprays[ which(InsectSprays$spray == 'A'
| InsectSprays$spray == 'B'), ]
InsectSpraysAB$spray = factor(InsectSpraysAB$spray)
#Welcht-t-Test
visstat(InsectSpraysAB,"count","spray")



#Kruskal-Wallis test: iris----
visstat(iris,"Petal.Width", "Species")


#Kruskal-Wallis test: ChickWeight ----
visstat(ChickWeight,"weight", "Diet")


#Wilcoxon rank sum test: TootGrowth ----
visstat(ToothGrowth,"len", "supp")



#Chi squqred, mosaic plots with Titanic data set----
#install.packages("titanic")
#example categorical data,
library(titanic)
titanic_train$Survived = as.factor(titanic_train$Survived)
titanic_train$Pclass = as.factor(titanic_train$Pclass)
#Pearsons Chi squared, mosaic plot with Pearson's residuals
visstat(titanic_train,"Survived","Pclass")


#Chi squqred, mosaic plots with HairEyeColor----
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







