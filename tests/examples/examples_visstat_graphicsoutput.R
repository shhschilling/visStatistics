


##Examples------ 
#Trees data set----
#linear Regression- running
#visstat(trees,"Girth","Height") #without saving of plot
visstat(trees,"Girth","Height",graphicsoutput = "png") #checked 

#example welch two sample t.test
mtcars$am = as.factor(mtcars$am)
visstat(mtcars,"mpg","am",graphicsoutput = "png") 

#InsectSprays  data set----
#ANOVA

visstat(InsectSprays,"count","spray",graphicsoutput = "png") 
#example Welch two sample t.test
# select sprays A and B
InsectSpraysAB <- InsectSprays[ which(InsectSprays$spray == 'A'
| InsectSprays$spray == 'B'), ]
InsectSpraysAB$spray = factor(InsectSpraysAB$spray)

#welcht-t-Test

visstat(InsectSpraysAB,"count","spray",graphicsoutput = "png") 


#ANOVA
visstat(InsectSprays,"count","spray",graphicsoutput = "png")


#Iris data set----
#Kruskal-Wallis test

visstat(iris,"Petal.Width", "Species",graphicsoutput = "png")

#Chick weight data set----
#Kruskal-Wallis test

visstat(ChickWeight,"weight", "Diet",graphicsoutput = "png")
#ToothGrowth data set----
#Wilcoxon rank sum test

visstat(ToothGrowth,"len", "supp",graphicsoutput = "png")


#Titanic data set---- 
#install.packages("titanic")
#example categorical data,
library(titanic) 
titanic_train$Survived = as.factor(titanic_train$Survived) 
titanic_train$Pclass = as.factor(titanic_train$Pclass) 
#Pearsons Chi squared, mosaic plot with Pearson's residuals
visstat(titanic_train,"Survived","Pclass") 



#HairEyeColor data set ----
HairEyeColorMale = counts_to_cases(as.data.frame(HairEyeColor[,,1]));
visstat(HairEyeColorMale,"Hair","Eye")
visstat(HairEyeColorMale,"Hair","Eye",,graphicsoutput = "png")
rm(HairEyeColorMale)


#pngplots=dir(getwd(),pattern=".png")
#file.remove(pngplots)




