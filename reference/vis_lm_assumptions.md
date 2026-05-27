# Visualisation of linear model assumption diagnostics

Checks the residual diagnostics in the general linear model Student's
t-test (t.test,var=EQUAL) Fisher oneway ANOVA (aov) or simple linear
regression. Performs the Shapiro-Wilk test and Anderson-Darling test for
normality and, if not a regression, also Levene's and Bartlett's tests
for homogeneity of variances. Formal p-values are computed from raw
model residuals. The plots display the same residuals divided by the
residual standard error. In regression mode, Cook's distance contours
are transformed to this residual scale.

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

## Details

In regression mode, the leverage panel does not use internally
studentised residuals as in `plot.lm()`. With \\z_i = e_i / SE_res\\,
where \\SE_res\\ is the residual standard error, Cook's distance
contours are drawn as \$\$D_i = z_i^2 h_i / (k(1 - h_i)^2),\$\$ with
simple-regression leverage \$\$h_i = 1/N + (x_i - \bar{x})^2 /
\sum\_{r=1}^{N}(x_r - \bar{x})^2.\$\$ Here \\x_i\\ is the predictor
value of observation \\i\\, \\N\\ is the total sample size, and \\k =
2\\ is the number of fitted model parameters.

## Examples

``` r
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
vis_lm_assumptions(ToothGrowth$len, ToothGrowth$dose)

```
