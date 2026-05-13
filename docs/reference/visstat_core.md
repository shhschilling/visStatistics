# Automated Visualization of Statistical Hypothesis Testing

`visstat_core()` provides automated selection and visualization of a
statistical hypothesis test between a two vectors in a given
`data.frame` named `dataframe` based on the data's type, distribution,
sample size, and the specified `conf.level`. `visstat_core()` is called
by the main wrapper function
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md).
`varsample` and `varfactor` are `character` strings corresponding to the
column names of the chosen vectors in `dataframe`. These vectors must be
of type `integer`, `numeric` or `factor`. The automatically generated
output figures illustrate the selected statistical hypothesis test,
display the main test statistics, and include assumption checks and post
hoc comparisons when applicable. The primary test results are returned
as a list object.

## Usage

``` r
visstat_core(
  dataframe,
  varsample,
  varfactor,
  conf.level = 0.95,
  correlation = FALSE,
  numbers = TRUE,
  minpercent = 0.05,
  graphicsoutput = NULL,
  plotName = NULL,
  plotDirectory = getwd()
)
```

## Arguments

- dataframe:

  `data.frame` with at least two columns.

- varsample:

  `character` string matching a column name in `dataframe`. Interpreted
  as the response if the referenced column is of class `numeric` or
  `integer` and the column named by `varfactor` is of class `factor`.

- varfactor:

  `character` string matching a column name in `dataframe`. Interpreted
  as the grouping variable if the referenced column is of class `factor`
  and the column named by `varsample` is of class `numeric` or
  `integer`.

- conf.level:

  Confidence level

- correlation:

  Logical. If FALSE (default), performs simple linear regression
  analysis with confidence and prediction bands. If TRUE, performs
  Spearman correlation analysis with trend line only (no regression
  interpretation).

- numbers:

  a logical indicating whether to show numbers in mosaic count plots.

- minpercent:

  number between 0 and 1 indicating minimal fraction of total count data
  of a category to be displayed in mosaic count plots.

- graphicsoutput:

  saves plot(s) of type "png", "jpg", "tiff" or "bmp" in directory
  specified in `plotDirectory`. If graphicsoutput=NULL, no plots are
  saved.

- plotName:

  graphical output is stored following the naming convention
  "plotName.graphicsoutput" in `plotDirectory`. Without specifying this
  parameter, plotName is automatically generated following the
  convention "statisticalTestName_varsample_varfactor".

- plotDirectory:

  specifies directory, where generated plots are stored. Default is
  current working directory.

## Value

An object of class `"visstat"` containing the results of the
automatically selected statistical test. The specific contents depend on
which test was performed. Additionally, the returned object includes two
attributes:

- `plot_paths`: Character vector of file paths where plots were saved
  (if `graphicsoutput` was specified)

- `captured_plots`: List of captured plot objects for programmatic
  access

## Details

The decision logic for selecting a statistical test is described below.
For more details, please refer to the package's
[`vignette("visStatistics")`](https://shhschilling.github.io/visStatistics/articles/visStatistics.md).
Throughout, data of class `numeric` or `integer` are referred to as
numeric, while data of class `factor` are referred to as categorical.
The significance level `alpha` is defined as one minus the confidence
level, given by the argument `conf.level`. Assumptions of normality and
homoscedasticity are considered met when the corresponding test yields a
p-value greater than `alpha = 1 - conf.level`. The choice of statistical
tests performed by `visstat_core()` depends on whether the data are
numeric or categorical, the number of levels in the categorical
variable, the distribution of the data, and the chosen `conf.level`. The
function prioritises interpretable visual output and tests that remain
valid under their assumptions, following the logic below:

\(1\) When the response is numerical and the predictor is categorical,
tests of central tendencies are performed. For the decision logic,
please refer to the packages vignette
[`vignette("visStatistics")`](https://shhschilling.github.io/visStatistics/articles/visStatistics.md)

(2): When both the response and predictor are numerical, a linear model
[`lm()`](https://rdrr.io/r/stats/lm.html) is fitted, with residual
diagnostics and a confidence band plot.

(3): When both variables are categorical, `visstat_core()` uses
[`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) or
[`fisher.test()`](https://rdrr.io/r/stats/fisher.test.html) depending on
expected counts, following Cochran's rule (Cochran (1954)
\<doi:10.2307/3001666\>).

Implemented main tests:

[`t.test()`](https://rdrr.io/r/stats/t.test.html),
[`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html),
[`aov()`](https://rdrr.io/r/stats/aov.html),
[`oneway.test()`](https://rdrr.io/r/stats/oneway.test.html),
[`lm()`](https://rdrr.io/r/stats/lm.html),
[`kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html),
[`fisher.test()`](https://rdrr.io/r/stats/fisher.test.html),
[`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html).

Implemented tests for assumptions:

- Normality:
  [`shapiro.test()`](https://rdrr.io/r/stats/shapiro.test.html) and
  `ad.test()`

- Heteroscedasticity:
  [`bartlett.test()`](https://rdrr.io/r/stats/bartlett.test.html) and
  [`levene.test()`](https://shhschilling.github.io/visStatistics/reference/levene.test.md)
  and `bp_test()`

Implemented post hoc tests:

- [`TukeyHSD()`](https://rdrr.io/r/stats/TukeyHSD.html) for
  [`aov()`](https://rdrr.io/r/stats/aov.html)

- `games.howell` for
  [`oneway.test()`](https://rdrr.io/r/stats/oneway.test.html)

- [`pairwise.wilcox.test()`](https://rdrr.io/r/stats/pairwise.wilcox.test.html)
  for [`kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html)

## See also

The package's vignette
[`vignette("visStatistics")`](https://shhschilling.github.io/visStatistics/articles/visStatistics.md)
for a description of the decision logic, illustrated with numerous
examples. The package is accompanied by its webpage
<https://shhschilling.github.io/visStatistics/>. The main function
[`visstat`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
for a detailed description of the return value.

## Examples

``` r
# Welch Two Sample t-test (t.test())
visstat_core(mtcars, "mpg", "am")


#> Warning: Statistical assumptions violated:
#> Homoscedasticity violated (Breusch-Pagan p = 0.0242 )
#> Analysis proceeded but interpret results cautiously.
#> RECOMMENDATION: Consider exploring alternatives outside visstat() such as data transformations, generalised linear models, or robust regression. For an assumption-free, non-causal alternative consider rerunning with correlation = TRUE.

## Wilcoxon rank sum test (wilcox.test())
grades_gender <- data.frame(
  Sex = as.factor(c(rep("Girl", 20), rep("Boy", 20))),
  Grade = c(
    19.3, 18.1, 15.2, 18.3, 7.9, 6.2, 19.4,
    20.3, 9.3, 11.3, 18.2, 17.5, 10.2, 20.1, 13.3, 17.2, 15.1, 16.2, 17.3,
    16.5, 5.1, 15.3, 17.1, 14.8, 15.4, 14.4, 7.5, 15.5, 6.0, 17.4,
    7.3, 14.3, 13.5, 8.0, 19.5, 13.4, 17.9, 17.7, 16.4, 15.6
  )
)
visstat_core(grades_gender, "Grade", "Sex")



## Welch's oneway ANOVA not assuming equal variances (oneway.test())
anova_npk <- visstat_core(npk, "yield", "block")


anova_npk # prints summary of tests
#> Object of class 'visstat'
#> 
#> Available components:
#> [1] "summary statistics of ANOVA" "post-hoc analysis "         
#> [3] "conf.level"                 

## Kruskal-Wallis rank sum test (kruskal.test())
visstat_core(iris, "Petal.Width", "Species")


visstat_core(InsectSprays, "count", "spray")



## Simple linear regression  (lm())
visstat_core(trees, "Girth", "Height", conf.level = 0.99)



## Pearson's Chi-squared test (chisq.test())
### Transform array to data.frame
HairEyeColorDataFrame <- counts_to_cases(as.data.frame(HairEyeColor))
visstat_core(HairEyeColorDataFrame, "Hair", "Eye")



## Fisher's exact test (fisher.test())
HairEyeColorMaleFisher <- HairEyeColor[, , 1]
### slicing out a 2 x2 contingency table
blackBrownHazelGreen <- HairEyeColorMaleFisher[1:2, 3:4]
blackBrownHazelGreen <- counts_to_cases(as.data.frame(blackBrownHazelGreen))
fisher_stats <- visstat_core(blackBrownHazelGreen, "Hair", "Eye")

```
