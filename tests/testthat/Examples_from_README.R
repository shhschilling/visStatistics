##Examples------ 
#Trees data set----
visstat(trees,"Girth","Height") #without saving of plot

#Iris data set----
visstat(iris,"Petal.Width", "Species")
visstat(iris,"Petal.Width", "Petal.Length")

#InsectSprays  data set----
visstat(InsectSprays,"count","spray")

#Titanic data set---- 
#install.packages("titanic")
library(titanic)
titanic_train$Survived=as.factor(titanic_train$Survived)
titanic_train$Pclass=as.factor(titanic_train$Pclass)
visstat(titanic_train,"Survived","Pclass")


#mtcars data set t---- 
#example welch two sample t.test

mtcars$am=as.factor(mtcars$am)
visstat(mtcars,"mpg","am")




