rm(list = setdiff(ls(), lsf.str()))
rm(list = ls(all.names = TRUE))
rm(list = ls())


source(detach_package)
detach_package(visStatistics)

unloadNamespace("visStatistics")


library(visStatistics)
##Examples------ 
#Trees data set----
#linear Regression
visstat(trees,"Girth","Height") #without saving of plot
visstat(trees,"Girth","Height",graphicsoutput = "png")
#mtcars data set ---- 
#example welch two sample t.test
mtcars$am = as.factor(mtcars$am)
visstat(mtcars,"mpg","am")
visstat(mtcars,"mpg","am",graphicsoutput = "png") #not producing any output

#InsectSprays  data set----
#ANOVA
visstat(InsectSprays,"count","spray",graphicsoutput = "png")

#example Welch two sample t.test
# select sprays A and B
InsectSpraysAB <- InsectSprays[ which(InsectSprays$spray == 'A'
| InsectSprays$spray == 'B'), ]
InsectSpraysAB$spray = factor(InsectSpraysAB$spray)

visstat(InsectSpraysAB,"count","spray")
visstat(InsectSpraysAB,"count","spray",graphicsoutput = "png") #not functioning

small=rnorm(100);
big=rnorm(100)*2
test_t_test=data.frame(small,big)

rm(InsectSpraysAB)
visstat(test_t_test,"small","big")



#ANOVA
visstat(InsectSprays,"count","spray")


#Iris data set----
#Kruskal-Wallis test
visstat(iris,"Petal.Width", "Species")
visstat(iris,"Petal.Width", "Species",graphicsoutput = "png")

#Chick weight data set----
#Kruskal-Wallis test
visstat(ChickWeight,"weight", "Diet")
visstat(ChickWeight,"weight", "Diet",graphicsoutput = "png")
#ToothGrowth data set----
#Wilcoxon rank sum test
visstat(ToothGrowth,"len", "supp")
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
rm(HairEyeColorMale)

vector1=rnorm(100)

vector2=rnorm(100)*2
longvector=c(vector1,vector2)
matrix(longvector,nrow=100,ncol=2)
