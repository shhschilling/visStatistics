
<!-- README.md is automatically generated from README.Rmd. Please only edit this Rmd file! -->
<!-- knitr before every resubmission -->

# visStatistics

Visualization of the statistical hypothesis test between two groups of
categorical or numerical data.

Statistical consulting requires often both a quick first visualization
and a reproducible statistical analysis of the presented raw data. The
package `visStatistics` with its core function `visstat()` fulfils this
need. Based on a decision tree it picks the statistical hypothesis test
with the highest statistical power between the dependent variable
(response) `varsample` and the independent variable (feature)
`varfactor`. The corresponding test statistics including eventual
post-hoc-analysis are returned and a graph showing key statistics of the
underlying test is generated.

This fully automated workflow is especially suited for browser based
interfaces to server-based deployments of R and has been successfully
implemented to analyse medical raw data in an unbiased fashion.

A detailed description of the package’s functionality and its underlying
decision tree, can be found in the `vignette` accompanying this package.
## Implemented tests `lm()`, `t.test()`, `wilcox.test()`, `aov()`,
`kruskal.test()`, `fisher.test()`, `chisqu.test()`

### Implemented tests to check the normal distribution of standardized residuals

`shapiro.test()` and `ad.test()`

### Implemented post-hoc tests

`TukeyHSD()` for `aov()`and `pairwise.wilcox.test()` for
`kruskal.test()`

## Installation from CRAN

1.  Install the package `install.packages("visStatistics")`
2.  Load the package `library(visStatistics)`

## Installation from GitHub (always latest, developing version)

1.  Install the devtools package from CRAN. Invoke R and type
    `install.packages("devtools")`
2.  Load the devtools package. `library(devtools)`
3.  Install the package from the github-repository
    `install_github("shhschilling/visStatistics")`
4.  Load the package `library(visStatistics)`
5.  Help on the function usage `?visstat`

## Getting Started

The package vignette allows you to get familiar with all features of
`visStatistics`. It documents in detail the algorithm of the decision
tree illustrated by examples.

## Examples

``` r
library(visStatistics)
```

### Welch Two Sample t.test

#### InsectSprays data set

``` r
InsectSpraysAB <- InsectSprays[ which(InsectSprays$spray == 'A' | InsectSprays$spray == 'B'), ] 
InsectSpraysAB$spray = factor(InsectSpraysAB$spray)
visstat(InsectSpraysAB,"count","spray")
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

#### mtcars data set

``` r
mtcars$am = as.factor(mtcars$am)
ttestStatistics = visstat(mtcars,"mpg","am") 
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />
Uncomment below line to print out summary statistics:

``` r
#ttestStatistics
```

### Wilcoxon rank sum test with continuity correction

``` r
visstat(ToothGrowth,"len", "supp")
#> Warning in wilcox.test.default(x = c(15.2, 21.5, 17.6, 9.7, 14.5, 10, 8.2, :
#> cannot compute exact p-value with ties
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

### One-way test

``` r
 anova_npk = visstat(npk,"yield","block")
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-7-2.png" width="100%" />

### Kruskal-Wallis test

The generated graphs can be saved in all available formats of the
`Cairo` package. Here we save the graphical output of type “pdf” in the
`plotDirectory` `tempdir()`:

``` r
visstat(iris,"Petal.Width","Species",graphicsoutput = "pdf",plotDirectory = tempdir())
```

### Linear Regression

``` r
linreg_cars = visstat(cars,"dist","speed")
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-9-2.png" width="100%" />
Increasing the confidence level `conf.level` from the default 0.95 to
0.99 leads two wider confidence and prediction bands:
<img src="man/figures/README-pressure-1.png" width="100%" /><img src="man/figures/README-pressure-2.png" width="100%" />
### Pearson’s Chi-squared test

Count data sets are often presented as multidimensional arrays, so
called contingency tables, whereas `visstat()` requires a `data.frame`
with a column structure. Arrays can be transformed to this column wise
structure with the helper function `counts_to_cases()`:

``` r
HairEyeColorDataFrame = counts_to_cases(as.data.frame(HairEyeColor))
visstat(HairEyeColorDataFrame,"Hair","Eye")
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-10-2.png" width="100%" />

### Fisher’s exact test

``` r
HairEyeColorMaleFisher = HairEyeColor[,,1]
#slicing out a 2 x2 contingency table
blackBrownHazelGreen = HairEyeColorMaleFisher[1:2,3:4]
blackBrownHazelGreen = counts_to_cases(as.data.frame(blackBrownHazelGreen));
fisher_stats = visstat(blackBrownHazelGreen,"Hair","Eye")
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-11-2.png" width="100%" />

For details regarding the generated mosaic plots, please refer to the
`mosaic()` in the `vcd` package.
