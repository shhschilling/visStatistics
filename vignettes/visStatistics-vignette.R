## ---- include = FALSE------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----setup-----------------------------------------------------------------------------------------------
library(visStatistics)

## --------------------------------------------------------------------------------------------------------
mtcars$am = as.factor(mtcars$am)
ttestStatistics = visstat(mtcars,"mpg","am") 
#  uncomenting the next line prints out test statistics 
# ttestStatistics

## --------------------------------------------------------------------------------------------------------
#Increasing the confidence level
ttestStatistics = visstat(mtcars,"mpg","am",conf.level = 0.99) 

## --------------------------------------------------------------------------------------------------------
grades_gender <- data.frame(
Sex = as.factor(c(rep("Girl", 20), rep("Boy", 20))),
Grade = c(19.3, 18.1, 15.2, 18.3, 7.9, 6.2, 19.4, 
            20.3, 9.3, 11.3, 18.2,17.5,10.2,20.1,13.3,17.2,15.1,16.2,17.3,
            16.5, 5.1, 15.3, 17.1, 14.8, 15.4, 14.4, 7.5, 15.5, 6.0,17.4,
            7.3, 14.3,13.5,8.0,19.5,13.4,17.9,17.7,16.4,15.6))

wilcoxonStatistics = visstat(grades_gender,"Grade", "Sex")

## --------------------------------------------------------------------------------------------------------
 anova_npk = visstat(npk,"yield","block")
# uncomment the following line to display the full list which is generated
#anova_npk

## --------------------------------------------------------------------------------------------------------
visstat(iris,"Petal.Width", "Species")

