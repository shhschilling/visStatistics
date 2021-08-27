# visStatistics

Visualization of the statistical hypothesis test between two groups of categorical or numerical data.

Statistical consulting  requires often both a quick first visualization and a reproducible statistical analysis 
of the presented raw data. The package `visStatistics` with its core function `visstat()` fulfils this need. 
Based on a decision tree it picks the statistical hypothesis test  with the highest statistical 
power between the dependent variable (response) `varsample` and the independent variable (feature) `varfactor`. 
The corresponding test statistics including eventual post-hoc-analysis are returned and 
a graph showing key statistics of the underlying test is generated. 

This fully automated workflow is especially suited for browser based interfaces to server-based
deployments of R and has been successfully implemented to analyse medical raw data in an unbiased fashion.
 

## Implemented tests
`lm()`, `t.test()`, `wilcox.test()`, `aov()`, `kruskal.test()`, `fisher.test()`, `chisqu.test()`

### Implemented tests to check the normal distribution of standardized residuals
`shapiro.test()` and `ad.test()`

### Implemented post-hoc tests
`TukeyHSD()` for `aov() `and `pairwise.wilcox.test()` for `kruskal.test()`


## Installation from CRAN
1. Install the package
`install.packages("visStatistics")`
2. Load the package
`library(visStatistics)`

## Installation from GitHub (always latest, developing version)
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

## Getting Started
The package vignette allows you to get familiar with all features of `visStatistics`. It documents in detail the algorithm of the decision tree and illustrates it with plenty of examples. 

## Examples 
### Trees data set: Linear regression
`visstat(trees,"Girth","Height")` 

`visstat(iris,"Petal.Width", "Species")`

###  NPK factorial experiment: ANOVA
`visstat(npk,"yield","block")`

### InsectSprays data set: Welch two sample t.test
`InsectSpraysAB <- InsectSprays[ which(InsectSprays$spray == 'A'| InsectSprays$spray == 'B'), ] #select only sprays 'A und 'B'`

`InsectSpraysAB$spray = factor(InsectSpraysAB$spray)`

`visstat(InsectSpraysAB,"count","spray")`

### ToothGrowth data set: Wilcoxon rank sum test with continuity correction
`visstat(ToothGrowth,"len", "supp")`

### Welch t.test
`visstat(mtcars,"mpg","am")`

### HairEyeColor data set: Pearson's Chi-squared test
`HairEyeColorMale = counts_to_cases(as.data.frame(HairEyeColor[,,1]))`

`visstat(HairEyeColorMale,"Hair","Eye")`

### Iris data set: Kruskal-Wallis test
### Saving the graphical output of type pdf in plotDirectory tempdir()

`visstat(iris,"Petal.Width","Species",graphicsoutput="pdf",plotDirectory=tempdir())`


