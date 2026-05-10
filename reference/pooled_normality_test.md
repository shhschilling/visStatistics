# Test normality using pooled standardized residuals

Standardizes values within each group and applies a single normality
test to the pooled standardized values. This avoids multiple testing
issues when deciding between parametric and non-parametric methods.

## Usage

``` r
pooled_normality_test(y, g, test = c("shapiro", "ad"), min_n = 3L)
```

## Arguments

- y:

  Numeric vector of response values

- g:

  Factor or vector defining groups (must have at least 2 levels)

- test:

  Character, either "shapiro" or "ad" for Anderson-Darling

- min_n:

  Minimum sample size per group required (default 3)

## Value

A list with test results (statistic, p.value, method, data.name)
