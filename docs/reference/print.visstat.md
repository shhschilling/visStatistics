# Print method for visstat objects

Displays a brief summary of the statistical test results and, if
available, assumption tests and post hoc comparisons.

## Usage

``` r
# S3 method for class 'visstat'
print(x, ...)
```

## Arguments

- x:

  An object of class `"visstat"`, returned by
  [`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md).

- ...:

  Currently unused. Included for S3 method compatibility.

## Value

The object `x`, invisibly.

## Details

Quick overview of the statistical analysis results.

## See also

[`summary.visstat`](https://shhschilling.github.io/visStatistics/reference/summary.visstat.md),
[`plot.visstat`](https://shhschilling.github.io/visStatistics/reference/plot.visstat.md),
[`visstat`](https://shhschilling.github.io/visStatistics/reference/visstat.md)

## Examples

``` r
anova <- visstat(npk$block, npk$yield)


print(anova)
#> Object of class 'visstat'
#> 
#> Available components:
#> [1] "summary statistics of ANOVA" "post-hoc analysis "         
#> [3] "conf.level"                  "effect_size"                
```
