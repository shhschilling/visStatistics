##Examples------ 
#Trees data set----
#linear Regression
visstat(trees,"Girth","Height") #without saving of plot

#Iris data set----
#linear Regression
visstat(iris,"Petal.Width", "Species")

#linear Regression
visstat(iris,"Petal.Width", "Petal.Length")

#InsectSprays  data set----
#ANOVA
visstat(InsectSprays,"count","spray")

#Titanic data set---- 
#install.packages("titanic")
#example categorical data,
library(titanic)
titanic_train$Survived=as.factor(titanic_train$Survived)
titanic_train$Pclass=as.factor(titanic_train$Pclass)
#Pearsons Chi squared, mosaic plot with Pearson's residuals
visstat(titanic_train,"Survived","Pclass")

#mtcars data set ---- 
#example welch two sample t.test
mtcars$am=as.factor(mtcars$am)
visstat(mtcars,"mpg","am")




