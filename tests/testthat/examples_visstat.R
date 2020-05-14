rm(list = setdiff(ls(), lsf.str()))
rm(list = ls(all.names = TRUE))
rm(list = ls())
##Examples------ 
#Trees data set----
#linear Regression
visstat(trees,"Girth","Height") #without saving of plot

#mtcars data set ---- 
#example welch two sample t.test
mtcars$am = as.factor(mtcars$am)
visstat(mtcars,"mpg","am")

#InsectSprays  data set----
#ANOVA
visstat(InsectSprays,"count","spray")

#example Welch two sample t.test
# select sprays A and B
InsectSpraysAB <- InsectSprays[ which(InsectSprays$spray == 'A'
| InsectSprays$spray == 'B'), ]
InsectSpraysAB$spray = factor(InsectSpraysAB$spray)
visstat(InsectSpraysAB,"count","spray")
rm(InsectSpraysAB)




#ANOVA
visstat(InsectSprays,"count","spray")


#Iris data set----
#Kruskal-Wallis test
visstat(iris,"Petal.Width", "Species")
visstat(iris,"Petal.Width", "Species",graphicsoutput = "png")

#Chick weight data set----
#Kruskal-Wallis test

visstat(ChickWeight,"weight", "Diet")



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
