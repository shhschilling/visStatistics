# Visualize Numeric Relationships: Regression or Correlation Analysis

This function provides unified visualization for numeric relationships
between two continuous variables. It can perform either regression
analysis (with confidence and prediction bands) or Spearman rank
correlation analysis with appropriate visualizations and statistical
output. For regression, statistical assumptions are checked and warnings
are issued if violated, but analysis proceeds.

## Usage

``` r
vis_numeric(
  y,
  x,
  correlation = FALSE,
  conf.level = 0.95,
  name_of_factor = character(),
  name_of_sample = character()
)
```

## Arguments

- y:

  Numeric vector. The response variable (dependent variable) for
  regression analysis, or the y-axis variable for correlation analysis.

- x:

  Numeric vector. The predictor variable (independent variable) for
  regression analysis, or the x-axis variable for correlation analysis.
  Must have the same length as y.

- correlation:

  Logical. If FALSE (default), performs regression analysis with
  confidence and prediction bands. If TRUE, performs Spearman rank
  correlation analysis.

- conf.level:

  Numeric. Confidence level for statistical tests and intervals. Must be
  between 0 and 1. Default is 0.95 (95 percent confidence level).

- name_of_factor:

  Character string. Label for the x-axis (independent variable). If
  empty, defaults to the variable name.

- name_of_sample:

  Character string. Label for the y-axis (dependent variable). If empty,
  defaults to the variable name.

## Value

A list containing analysis results and assumption checks. Content
depends on analysis type. For regression analysis: analysis_type,
summary_regression, assumptions, warnings, r_squared, adj_r_squared. For
correlation analysis: analysis_type, correlation_test,
correlation_coefficient, assumptions, warnings, method_used.

## Details

Statistical Assumptions Checked: Regression: Normality of residuals
(Shapiro-Wilk test) and homoscedasticity (Breusch-Pagan test). All
regression analyses proceed even if assumptions are violated, but
appropriate warnings are issued. Correlation: Spearman rank correlation
requires no distributional assumptions.

## See also

[`cor.test`](https://rdrr.io/r/stats/cor.test.html),
[`lm`](https://rdrr.io/r/stats/lm.html),
[`shapiro.test`](https://rdrr.io/r/stats/shapiro.test.html)

## Author

Sabine Schilling

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate sample data
set.seed(123)
x <- rnorm(50, mean = 10, sd = 2)
y <- 2 * x + rnorm(50, mean = 0, sd = 1)

# Regression analysis (default)
result1 <- vis_numeric(y, x, 
                      name_of_factor = "Predictor", 
                      name_of_sample = "Response")

# Spearman rank correlation
result2 <- vis_numeric(y, x, correlation = TRUE)
} # }
```
