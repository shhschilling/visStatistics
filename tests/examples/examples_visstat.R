


##Examples------ 
#Trees data set----
#linear Regression- running
visstat(trees,"Girth","Height") #linear regression without saving of plot
visstat(trees,"Girth","Height",graphicsoutput = "png") 

#example welch two sample t.test
mtcars$am = as.factor(mtcars$am)
visstat(mtcars,"mpg","am")

test_norm(trees$Girth)



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
visstat(HairEyeColorMale,"Hair","Eye") #can not find mosaic function

HairEyeColorMaleFisher=HairEyeColor[,,1]
#replace cells to smaller values to enforce Cochran's rule
HairEyeColorMaleFisher[HairEyeColorMaleFisher<10]=4
HairEyeColorMaleFisher = counts_to_cases(as.data.frame(HairEyeColorMaleFisher));
visstat(HairEyeColorMaleFisher,"Hair","Eye")






pngplots=dir(getwd(),pattern=".png")
file.remove(pngplots)




