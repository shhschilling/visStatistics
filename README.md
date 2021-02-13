# visStatistics

 Visualization of the statistical hypothesis test between two groups of categorical or numerical data.

 The function `visstat()` **vis**ualizes the **stat**istical hypothesis testing between two groups of data, where `varsample` is the dependent variable (or response) and `varfactor` is the independent variable (feature).
 The statistical hypothesis test with the highest statistical power and fulfilling the assumptions of the corresponding test is performed and visualized.
 A graph displaying the raw data accordingly to the chosen test as well as the test statistics is generated. Furthermore
 `visstat()` returns the corresponding test statistics as text. The automated workflow is especially suited for browser based interfaces to server-based deployments of R. 
Implemented tests to check the normal distribution of standardized residuals: `shapiro.test()` and `ad.test()`.
Implemented post-hoc tests: `TukeyHSD()` for `aov()` and `pairwise.wilcox.test()` for `kruskal.test()`.
For the comparison of averages, the implemented algorithm  depends on the value of the parameter of `conf.level`, which defaults to 0.95: 
If the p-values of the standardized residuals of both `shapiro.test()` or `ad.test()` are smaller 
than  the error probability 1-`conf.level`,`kruskal.test()` resp. `wilcox.test()` are performed, otherwise the `oneway.test()`
and `aov()` resp. t.test() are performed and displayed. Exception: 
If the sample size is bigger than 100, t.test() is always performed and wilcox.test() is never executed 
(Lumley et al. (2002) <doi:10.1146/annurev.publhealth.23.100901.140546>).
For the test of independence of count data, Cochran's rule (Cochran (1954) <doi:10.2307/3001666>) is implemented: 
If more than 20 percent of all cells have a count smaller than 5, fisher.test() is performed and displayed,
otherwise chisqu.test(). In both cases case an additional mosaic plot is generated.  
  
  

##Installation from CRAN
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

## Examples 
### Trees data set: Linear regression
`visstat(trees,"Girth","Height")` 


###  Iris data set: Kruskal-Wallis test
`visstat(iris,"Petal.Width", "Species")`


###  InsectSprays data set: ANOVA
`visstat(InsectSprays,"count","spray")`

### InsectSprays data set: Welch two sample t.test
`InsectSpraysAB <- InsectSprays[ which(InsectSprays$spray == 'A'| InsectSprays$spray == 'B'), ] #select only sprays 'A und 'B'`

`InsectSpraysAB$spray = factor(InsectSpraysAB$spray)`

`visstat(InsectSpraysAB,"count","spray")`

### ToothGrowth data set: Wilcoxon rank sum test with continuity correction
`visstat(ToothGrowth,"len", "supp")`

#### HairEyeColor data set: Pearson's Chi-squared test
`HairEyeColorMale = counts_to_cases(as.data.frame(HairEyeColor[,,1]))`

`visstat(HairEyeColorMale,"Hair","Eye")`


