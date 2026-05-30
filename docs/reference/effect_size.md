# Compute an effect-size estimate for a visstat result

`effect_size()` returns the effect-size estimate associated with a
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
result. If `result$effect_size` is already present, it is returned
unchanged. Otherwise, the estimate is computed from the test object
stored in `result`; for some base R stats results, it is extracted
directly from the returned object.

## Usage

``` r
effect_size(result, x = NULL, y = NULL)
```

## Arguments

- result:

  A list returned by
  [`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
  or a compatible test result object.

- x:

  First input vector, matching the first argument of `visstat(x, y)`.
  Required when the effect size cannot be extracted from `result` alone.

- y:

  Second input vector, matching the second argument of `visstat(x, y)`.
  Required when the effect size cannot be extracted from `result` alone.

## Value

A list with components `name`, `estimate`, `effect_size_method`, and
optionally `conf.int`.

## Details

Notation used below: \\x\\ and \\y\\ are the two variables entering the
selected analysis, \\N\\ is the total number of non-missing
observations, \\n_j\\ is the sample size in group \\j\\, \\k\\ is the
number of groups, \\\bar{y}\_j\\ is the mean of numeric vector \\y\\ in
group \\j\\, and \\s_j^2\\ is the variance of numeric vector \\y\\ in
group \\j\\.

The following estimates are computed internally:

- Student's two-sample `t.test(..., var.equal = TRUE)`: Hedges'
  \\g\_{s_p} = J(N-2)(\bar{y}\_1-\bar{y}\_2)/s_p\\, where \\s_p =
  \sqrt{((n_1-1)s_1^2+(n_2-1)s_2^2)/(N-2)}\\ and \\J(\nu) =
  \Gamma(\nu/2)/(\sqrt{\nu/2}\Gamma((\nu-1)/2))\\.

- Welch's two-sample `t.test(..., var.equal = FALSE)`: Hedges'
  \\g\_{s^\*} = J(\nu^\*)(\bar{y}\_1-\bar{y}\_2)/s^\*\\, where \\s^\* =
  \sqrt{(s_1^2+s_2^2)/2}\\ and \\\nu^\* =
  ((n_1-1)(n_2-1)(s_1^2+s_2^2)^2)/ ((n_2-1)s_1^4+(n_1-1)s_2^4)\\.

- Wilcoxon rank-sum test: signed rank-biserial correlation \\r = 2W/(n_1
  n_2)-1\\, where \\W\\ is the statistic returned by
  [`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) for the
  first group.

- Fisher's one-way ANOVA: omega-squared \\\omega^2 =
  \nu_1(F-1)/(\nu_1F+\nu_2+1)\\, where \\F\\ is the ordinary one-way
  ANOVA statistic, \\\nu_1=k-1\\, and \\\nu_2=N-k\\. Negative estimates
  are truncated to zero.

- Welch's one-way test: approximate omega-squared-type estimate
  \\\nu_1(F_W-1)/(\nu_1F_W+\nu_2+1)\\, where \\F_W\\ is the Welch ANOVA
  statistic, \\\nu_1=k-1\\, and \\\nu_2\\ is the usually fractional
  denominator degree of freedom returned by
  [`oneway.test()`](https://rdrr.io/r/stats/oneway.test.html). Negative
  estimates are truncated to zero.

- Kruskal-Wallis test: Kelley-adjusted eta-squared based on \\H\\,
  \\\eta_H^2=(H-k+1)/(N-k)\\, where \\H\\ is the Kruskal-Wallis
  statistic. Negative estimates are truncated to zero.

- Pearson's chi-squared test: Cramer's \\V\\ for general \\R\times C\\
  tables, \\V=\sqrt{\chi^2/(N\cdot(\min(R,C)-1))}\\, where \\R\\ and
  \\C\\ are the numbers of rows and columns. For \\2\times 2\\ tables
  this is phi, \\\sqrt{\chi^2/N}\\. The chi-squared statistic is used as
  supplied by [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html).

The following estimates are extracted from existing result objects:

- \\R^2\\ from `summary(lm())$r.squared`.

- Spearman's \\\rho\\ from `cor.test(method = "spearman")$estimate`.

- Kendall's \\\tau_b\\ from `cor.test(method = "kendall")$estimate`.

- The conditional maximum-likelihood odds ratio from
  `fisher.test()$estimate` and its confidence interval from
  `fisher.test()$conf.int` for \\2\times 2\\ tables.

## References

Hedges, L. V. (1981). Distribution theory for Glass's estimator of
effect size and related estimators. *Journal of Educational Statistics*,
6(2), 107–128. doi:10.3102/10769986006002107.

Delacre, M., Lakens, D., Ley, C., Liu, L., & Leys, C. (2021). Why
Hedges' g\*s based on the non-pooled standard deviation should be
reported with Welch's t-test. *PsyArXiv*. doi:10.31234/osf.io/tu6mp.

Kerby, D. S. (2014). The simple difference formula: An approach to
teaching nonparametric correlation. *Comprehensive Psychology*, 3.
doi:10.2466/11.IT.3.1.

Albers, C., & Lakens, D. (2018). When power analyses based on pilot data
are biased: Inaccurate effect size estimators and follow-up bias.
*Journal of Experimental Social Psychology*, 74, 187–195.
doi:10.1016/j.jesp.2017.09.004.

Kelley, T. L. (1935). An unbiased correlation ratio measure.
*Proceedings of the National Academy of Sciences*, 21(9), 554–559.
doi:10.1073/pnas.21.9.554.

Cohen, J. (2013). *Statistical power analysis for the behavioral
sciences*. Routledge. doi:10.4324/9780203771587.

## Examples

``` r
x <- ToothGrowth$supp
y <- ToothGrowth$len
tt <- list("t-test-statistics" = t.test(y ~ x, var.equal = TRUE))
effect_size(tt, x = x, y = y)
#> $name
#> [1] "Hedges' g"
#> 
#> $estimate
#> [1] 0.4880931
#> 
#> $effect_size_method
#> [1] "Hedges' g using pooled standard deviation"
#> 

kw <- list(
  "Kruskal Wallis rank sum test" = kruskal.test(Petal.Width ~ Species,
                                               data = iris)
)
effect_size(kw, x = iris$Species, y = iris$Petal.Width)
#> $name
#> [1] "eta-squared based on H"
#> 
#> $estimate
#> [1] 0.8788121
#> 
#> $effect_size_method
#> [1] "Eta-squared based on H for Kruskal-Wallis rank sum test"
#> 

tab <- matrix(c(10, 5, 4, 12), nrow = 2)
effect_size(chisq.test(tab))
#> $name
#> [1] "phi"
#> 
#> $estimate
#> [1] 0.3535596
#> 
#> $effect_size_method
#> [1] "Phi coefficient for 2 x 2 contingency table"
#> 

if (FALSE) { # \dontrun{
## Large-sample example with a statistically significant Student's
## t-test p-value but a small effect size, measured by Hedges' g
## using the pooled standard deviation. A small mean shift is added
## to noisy normal data. Because N is large, the t-test p-value
## becomes small, while Hedges' g remains close to zero.
## The residual Shapiro-Wilk p-value in the diagnostic panel is NA
## because shapiro.test() is limited to n <= 5000.
set.seed(20260525)
n <- 2501
mean_shift <- 0.1
group <- factor(rep(c("control", "treatment"), each = n))
response <- rnorm(2 * n) + rep(c(0, mean_shift), each = n)
res <- visstat(group, response)
res[["t-test-statistics"]]$method
res[["t-test-statistics"]]$p.value
res$effect_size$effect_size_method
res$effect_size$estimate
} # }
```
