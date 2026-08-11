# Dunn's Post-Hoc Test for Kruskal-Wallis

Performs pairwise comparisons on the rank sums of a single, combined
ranking of all groups, as proposed by Dunn (1964). It is the post-hoc
procedure matched to
[`kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html): both rank
the observations globally, so a pairwise decision here concerns the same
quantity the omnibus test rejected.

## Usage

``` r
dunn.test(samples, groups, conf.level = 0.95)
```

## Arguments

- samples:

  numeric vector; the dependent variable.

- groups:

  factor or vector; the grouping variable.

- conf.level:

  numeric; confidence level (default: 0.95). Used only for the
  `significant` column; the p-values do not depend on it.

## Value

A data frame with columns:

- group1:

  First group in comparison

- group2:

  Second group in comparison

- mean_rank_diff:

  Difference in mean ranks (group1 - group2)

- se:

  Standard error of the difference in mean ranks

- z:

  Standard normal test statistic

- p_value:

  Unadjusted two-sided p-value

- p_adj:

  Holm-adjusted p-value for multiple comparisons

- significant:

  Logical; TRUE if p_adj \< (1 - conf.level)

## Details

All \\k\\ samples are combined and ranked from smallest to largest, ties
receiving the average rank. Writing \\\bar R_i\\ for the mean rank of
group \\i\\ and \\N\\ for the total number of observations, the
statistic for groups \\i\\ and \\j\\ is \$\$z\_{ij} = (\bar R_i - \bar
R_j) / \sigma\_{ij},\$\$ with \$\$\sigma\_{ij}^2 =
\left\[\frac{N(N+1)}{12} - \frac{\sum\_{s=1}^{r}(t_s^3 -
t_s)}{12(N-1)}\right\] \left(\frac{1}{n_i} + \frac{1}{n_j}\right),\$\$
where the \\r\\ groups of tied scores contain \\t_s\\ observations each;
the subtracted term is zero without ties. This is Eq. (3) of Dunn
(1964). The function returns two-sided p-values adjusted by Holm's
step-down procedure over all \\p = k(k-1)/2\\ pairwise comparisons. Note
that all pairwise comparisons are performed, so \\p\\ is not chosen in
advance as Dunn's formulation assumes.

## References

Dunn, O. J. (1964). Multiple Comparisons Using Rank Sums.
*Technometrics*, 6(3), 241-252. doi:10.1080/00401706.1964.10490181.

## Examples

``` r
# Convert dose to factor
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

# Perform Dunn's test
result <- dunn.test(ToothGrowth$len, ToothGrowth$dose)
print(result)
#> 
#> Dunn's Post-Hoc Test (Holm-adjusted)
#> Global ranking of all groups; matched to kruskal.test()
#> 
#>  group1 group2 mean_rank_diff     se       z p_value p_adj significant
#>     0.5      1        -19.625 5.5205 -3.5549   4e-04 8e-04        TRUE
#>     0.5      2        -35.125 5.5205 -6.3626   0e+00 0e+00        TRUE
#>       1      2        -15.500 5.5205 -2.8077   5e-03 5e-03        TRUE
```
