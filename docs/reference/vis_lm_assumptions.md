# Visualisation of linear model assumption diagnostics

Checks the residual diagnostics in the general linear model Student's
t-test (t.test,var=EQUAL) Fisher oneway ANOVA (aov) or simple linear
regression. Performs the Shapiro-Wilk and Anderson-Darling tests for
normality, and for grouped data also Levene's and Bartlett's tests for
homogeneity of variances. For simple linear regression,
heteroscedasticity is assessed with the Breusch-Pagan test
\[@Koenker:1981\], which regresses squared raw residuals on fitted
values. The normality tests, the grouped variance tests, and the
histogram and Q-Q panels are computed from the internally studentised
residuals r_i = e_i / (SE_res sqrt(1 - h_i)), which remove the
leverage-dependent variance of the raw residuals (Var(e_i) = sigma^2
(1 - h_i)). The residuals-vs-fitted panel (regression mode) uses the
z-residuals z_i = e_i / SE_res, which retain the leverage-dependent
spread.

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
