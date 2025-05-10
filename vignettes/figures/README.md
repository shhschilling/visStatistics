# visStatistics

`visStatistics` is an R package for rapid visualization and statistical analysis of raw data. It automatically selects and applies the most appropriate hypothesis test to evaluate the relationship between a response (`varsample`) and a feature (`varfactor`) within a `data.frame`.

A minimal function call looks like:

```r
visstat(dataframe, varsample = "response", varfactor = "feature")
```

The input must be a column-based `data.frame`, and `varsample` and `varfactor` are character strings naming columns of that data frame. The significance level $\alpha$, used throughout for hypothesis testing, is defined as `1 - conf.level`, where `conf.level` is user-adjustable (default: `0.95`).

Test selection is fully automatic and based on the types of the response and feature variables, the number of factor levels, and distributional assumptions. Depending on the input, the function may apply:

- Welch’s *t*-test or Wilcoxon rank-sum test for numerical response and binary factors
- ANOVA, Welch’s one-way test, or Kruskal-Wallis test for factors with more than two levels
- Linear regression for two numerical variables
- Chi-squared test or Fisher’s exact test for two categorical variables

The selected test is visualized with interpretable plots, such as box plots with confidence intervals, regression lines, or mosaic plots with residuals.

For a detailed description of the test selection logic, see:

```r
vignette("visStatistics")
```

### Example plot

![Decision tree](man/figures/decision-tree.png)
