# Automated Visualization of Statistical Hypothesis Testing

`visstat_core()` implements the decision tree used by
[`visstat`](https://shhschilling.github.io/visStatistics/reference/visstat.md).
It receives a `data.frame` and two column names, determines the
corresponding analysis route, creates the diagnostic and result plots,
and returns the selected test results as a `visstat` object.

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
  group_test = NULL,
  graphicsoutput = NULL,
  plotName = NULL,
  plotDirectory = getwd(),
  plot_args = list()
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

- group_test:

  Optional character. For Route 1 only, `NULL` keeps the default
  assumption gates, `"welch"` forces Welch-type mean tests, and `"rank"`
  forces Wilcoxon/Kruskal-Wallis rank tests.

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

- plot_args:

  Optional named list of base graphics parameters.

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

The decision logic is organised into four routes. Route 1 handles a
numeric response with a categorical predictor. By default, Route 1 uses
residual-based test selection: Shapiro–Wilk on model residuals gates
mean-based versus rank-based analysis, and Levene gates equal-variance
versus Welch-type mean tests inside the mean branch. Alternatively,
`group_test = "welch"` forces Welch-type mean tests, and
`group_test = "rank"` forces Wilcoxon/Kruskal–Wallis tests.

Route 2 handles ordered responses with categorical predictors by
converting the ordered response to integer level codes and applying
Wilcoxon or Kruskal–Wallis tests. Route 3 handles two numeric variables
by fitting [`lm()`](https://rdrr.io/r/stats/lm.html) by default, or
Spearman rank correlation when `correlation = TRUE`. Route 4 handles two
unordered factors with Pearson's \\\chi^2\\ test or Fisher's exact test,
depending on expected counts. If both variables are ordered and
`correlation = TRUE`, Kendall's \\\tau_b\\ is used.

The significance level `alpha` is defined as `1 - conf.level`.
Assumption tests are interpreted relative to this threshold.

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

For the general linear model the Shapiro-Wilk, Anderson-Darling, Levene
and Bartlett tests are applied to the internally studentised residuals
r_i = e_i / (SE_res sqrt(1 - h_i)), which remove the leverage-dependent
variance of the raw residuals (Var(e_i) = sigma^2 (1 - h_i)).

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
provides a detailed description of the return value.

## Examples

``` r
old_qq_nsim <- getOption("visStatistics.qq_nsim")
options(visStatistics.qq_nsim = 100L)

# Welch Two Sample t-test (t.test())
visstat_core(mtcars, "mpg", "am")


#> Warning: Statistical assumptions violated:
#> Homoscedasticity violated (Breusch-Pagan p = 0.0242 )
#> Analysis proceeded but interpret results cautiously.
#> RECOMMENDATION: Consider exploring alternatives outside visstat() such as data transformations,
#> generalised linear models, or robust regression. For a non-causal alternative
#> consider rerunning with correlation = TRUE.

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
#> [3] "conf.level"                  "effect_size"                

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


options(visStatistics.qq_nsim = old_qq_nsim)
```
