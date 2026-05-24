# Mean-centred Levene Test for Homogeneity of Variance

Performs Levene's original mean-centred test. It tests the null
hypothesis that all groups have equal variances by testing whether the
absolute deviations from group means are equal across groups. The
function reproduces the behaviour of leveneTest(y, g, center = mean,
...) of the car package.

## Usage

``` r
levene.test(y, g, data = NULL)
```

## Arguments

- y:

  A numeric response vector.

- g:

  A grouping factor.

- data:

  Optional data frame containing \`y\` and \`g\`.

## Value

An object of class `"htest"` with components:

- statistic:

  the value of the F-statistic.

- parameter:

  degrees of freedom: df1=k-1, df3=N-k, where k is the number of groups
  and N the total sample size

- p.value:

  the p-value of the test.

- method:

  a character string indicating the test performed.

- data.name:

  a character string giving the name(s) of the data.

## Details

For each observation \\y\_{ij}\\ in group \\i\\, compute the absolute
deviation from the group mean:

\$\$z\_{ij} = \|y\_{ij} - \bar{y}\_i\|\$\$

where \\\bar{y}\_i\\ is the mean of group \\i\\.

The test statistic is the F-statistic from a one-way ANOVA on the
\\z\_{ij}\\ values:

\$\$F = \frac{(N-k) \sum\_{i=1}^{k} n_i (\bar{z}\_i - \bar{z})^2}{(k-1)
\sum\_{i=1}^{k} \sum\_{j=1}^{n_i} (z\_{ij} - \bar{z}\_i)^2}\$\$

where:

- \\k\\ = number of groups

- \\N\\ = total sample size

- \\n_i\\ = sample size of group \\i\\

- \\\bar{z}\_i\\ = mean of absolute deviations in group \\i\\

- \\\bar{z}\\ = overall mean of all absolute deviations

Under the null hypothesis of equal variances, the test statistic follows
an F-distribution: \\F \sim F(k-1, N-k)\\.

## References

Levene, H. (1960). Robust tests for equality of variances. In I. Olkin
(Ed.), Contributions to Probability and Statistics (pp. 278-292).
Stanford University Press.

## Examples

``` r
set.seed(123)
y <- c(rnorm(10), rnorm(10, sd = 2), rnorm(10, sd = 0.5))
g <- factor(rep(1:3, each = 10))
levene.test(y, g)
#> 
#>  Mean-centred Levene Test
#> 
#> data:  absolute deviations from group means
#> F = 4.3375, df1 = 2, df2 = 27, p-value = 0.02325
#> 

# Usage with data frame
df <- data.frame(response = y, group = g)
levene.test(response, group, data = df)
#> 
#>  Mean-centred Levene Test
#> 
#> data:  absolute deviations from group means
#> F = 4.3375, df1 = 2, df2 = 27, p-value = 0.02325
#> 

# Example with unequal variances (should reject null hypothesis)
set.seed(456)
y_unequal <- c(rnorm(15, sd = 1), rnorm(15, sd = 5), rnorm(15, sd = 0.2))
g_unequal <- factor(rep(c("A", "B", "C"), each = 15))
levene.test(y_unequal, g_unequal)
#> 
#>  Mean-centred Levene Test
#> 
#> data:  absolute deviations from group means
#> F = 35.937, df1 = 2, df2 = 42, p-value = 8.004e-10
#> 


```
