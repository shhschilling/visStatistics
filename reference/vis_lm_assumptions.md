# Visualisation of the normality distribution of the standardised residuals

Checks for normality of the standardised residuals in the general linear
model Student's t-test (t.test,var=EQUAL) Fisher oneway ANOVA (aov) or
simple linear regression. Performs the Shapiro-Wilk test and
Anderson-Darling test for normality and, if not a regression, also the
Brown-Forsythe Levene-type and Bartlett's tests for homogeneity of
variances. It produces a histogram with normal overlay, a residuals vs
fitted plot, and a normal Q-Q plot.

## Usage

``` r
vis_lm_assumptions(samples, fact, cex = 1, correlation = FALSE)
```

## Arguments

- samples:

  Numeric vector; the dependent variable.

- fact:

  Factor; the independent variable.

- cex:

  Numeric; scaling factor for plot text and symbols (default: 1).

- correlation:

  Logical. If `FALSE` and `fact` is numeric, regression diagnostics are
  shown. If `TRUE`, no regression diagnostics are shown. Default is
  `FALSE`.

## Value

A list with elements:

- summary_anova:

  Summary of the ANOVA model.

- shapiro_test:

  Result from
  [`shapiro.test()`](https://rdrr.io/r/stats/shapiro.test.html).

- ad_test:

  Result from
  [`nortest::ad.test()`](https://rdrr.io/pkg/nortest/man/ad.test.html)
  or a character message if n \< 7.

- levene_test:

  Result from
  [`levene.test()`](https://shhschilling.github.io/visStatistics/reference/levene.test.md)
  (grouped diagnostics only).

- bartlett_test:

  Result from
  [`bartlett.test()`](https://rdrr.io/r/stats/bartlett.test.html)
  (grouped diagnostics only).

- bp_test:

  Result from
  [`bp.test()`](https://shhschilling.github.io/visStatistics/reference/bp.test.md)
  (regression diagnostics only).

## Examples

``` r
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
vis_lm_assumptions(ToothGrowth$len, ToothGrowth$dose)

```
