# visStatistics

 Visualization of the statistical hypothesis test between two groups of categorical or numerical data.

 The function `visstat()` **vis**ualizes the **stat**istical hypothesis testing between two groups of data, where `varsample` is the dependent variable (or response) and `varfactor` is the independent variable (feature).
 The statistical hypothesis test with the highest statistical power and fulfilling the assumptions of the corresponding test is performed and visualized.
 A graph displaying the raw data accordingly to the chosen test as well as the test statistics is generated. Furthermore
 `visstat` returns the corresponding test statistics as text.
  Implemented tests: `lm(), t.test(), wilcox.test(), aov(), oneway.test(),kruskal.test(), fisher.test(),chisqu.test()`.
`vissta()t` tests the fulfillment of the underlying assumptiosn of `aov` and `oneway.test` by calling the internal function `vis_anova_assumptions`.




## Installation from GitHub
1. Firstly, you need to install the devtools package. You can do this from CRAN. Invoke R and then type
`install.packages("devtools")`
2.  Load the devtools package.
`library(devtools)`
3. Install the package from the github- repository
`install_github("shhschilling/visStatistics")`
4. Load the package 
`library(visStatistics)`
5. Help on the function usage
`?visstat`

## Examples 
### Trees data set: linear Regression
`visstat(trees,"Girth","Height")` #without saving of plot

`visstat(trees,"Girth","Height",graphicsoutput="png")`# saving the plot as"png"-file

###  Iris data set: Kruskal-Wallis test
`visstat(iris,"Petal.Width", "Species")`


###  InsectSprays  data set: ANOVA
`visstat(InsectSprays,"count","spray")`

### InsectSprays data set: Welch two sample t.test
`InsectSpraysAB <- InsectSprays[ which(InsectSprays$spray == 'A'| InsectSprays$spray == 'B'), ] #select only sprays 'A und 'B'`

`InsectSpraysAB$spray = factor(InsectSpraysAB$spray)`

`visstat(InsectSpraysAB,"count","spray")`

### ToothGrowth data set: Wilcoxon rank sum test with coninuity correction
`visstat(ToothGrowth,"len", "supp")`

#### HairEyeColor data set: Pearson's Chi-squared test
`HairEyeColorMale = counts_to_cases(as.data.frame(HairEyeColor[,,1]))`

`visstat(HairEyeColorMale,"Hair","Eye")`


