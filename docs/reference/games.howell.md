# Games-Howell Post-Hoc Test

Performs pairwise comparisons using the Games-Howell test, which does
not assume equal variances or equal sample sizes. This is the
appropriate post-hoc test to use after a significant Welch's ANOVA.

## Usage

``` r
games.howell(samples, groups, conf.level = 0.95)
```

## Arguments

- samples:

  Numeric vector; the dependent variable.

- groups:

  Factor or vector; the grouping variable.

- conf.level:

  Numeric; confidence level for confidence intervals (default: 0.95).

## Value

A data frame with columns:

- group1:

  First group in comparison

- group2:

  Second group in comparison

- mean_diff:

  Difference in means (group1 - group2)

- se:

  Standard error of the difference

- t:

  t-statistic

- df:

  Degrees of freedom (Welch-Satterthwaite)

- p_value:

  Unadjusted p-value

- p_adj:

  Holm-adjusted p-value for multiple comparisons

- ci_lower:

  Lower bound of confidence interval

- ci_upper:

  Upper bound of confidence interval

- significant:

  Logical; TRUE if p_adj \< (1 - conf.level)

## Details

The Games-Howell test uses the Welch-Satterthwaite approximation for
degrees of freedom and does not pool variances. P-values are adjusted
using the Holm method to control family-wise error rate.

## Examples

``` r
# Convert dose to factor
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

# Perform Games-Howell test
result <- games.howell(ToothGrowth$len, ToothGrowth$dose)
print(result)
#> 
#> Games-Howell Post-Hoc Test
#> ==========================
#> 
#>  group1 group2 mean_diff     se        t    df   p_value ci_lower ci_upper
#>     0.5      1    -9.130 1.4097  -6.4766 37.99 1.268e-07 -11.9838  -6.2762
#>     0.5      2   -15.495 1.3132 -11.7990 36.88 4.398e-14 -18.1562 -12.8338
#>       1      2    -6.365 1.2989  -4.9005 37.10 1.906e-05  -8.9965  -3.7335
#>      p_adj significant sig
#>  2.537e-07        TRUE ***
#>  1.319e-13        TRUE ***
#>  1.906e-05        TRUE ***
#> 
#> ---
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> P-values adjusted using Holm method
#> 
```
