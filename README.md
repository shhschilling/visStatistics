# visStatistic

 Visualization of the statistical hypothesis test between two groups of categorical or numerical data.

 The function `visstat` **vis**ualizes the **stat**istical hypothesis testing between two groups of data, where `varsample` is the dependent variable (or response) and `varfactor` is the independent variable (feature).
 The statistical hypothesis test with the highest statistical power and fulfilling the assumptions of the corresponding  is performed and visualized.
 A graph displaying the raw data accordingly to the chosen test as well as the test statistics is generated. Furthermore
 `visstat` returns the corresponding test statistics as text.
  Implemented tests: `lm, t.test, wilcox.test, aov, kruskal.test, fisher.test,chisqu.test`.

  To test assumptions of normality of distributions: `shapiro.test, ks.test`


## Installation
1. First, you need to install the devtools package. You can do this from CRAN. Invoke R and then type
`install.packages("devtools")`
2.  Load the devtools package.
`library(devtools)`
3. Install the package from the github- repository
`install_github("shhschilling/visStatistic")`
4. Load the package 
`library(visStatistic)`
5. Help on the function usage
`?visstat`

## Examples 
### Trees data set
`visstat(trees,"Girth","Height")` #without saving of plot
`visstat(trees,"Girth","Height",graphicsoutput="png)`# saving of the plot as"png"-file

###  Iris data set
`visstat(iris,"Petal.Width", "Species")`
`visstat(iris,"Petal.Width", "Petal.Length")`

###  InsectSprays  data set 
`visstat(InsectSprays,"count","spray")`

###  Titanic data set 

`install.packages("titanic")`
`library(titanic)`
`titanic_train$Survived=as.factor(titanic_train$Survived)`
`titanic_train$Pclass=as.factor(titanic_train$Pclass)`
`visstat(titanic_train,"Survived","Pclass")`
