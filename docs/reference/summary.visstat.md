# Summary method for visstat objects

Displays the full statistical test results and, if available, assumption
tests and post hoc comparisons.

## Usage

``` r
# S3 method for class 'visstat'
summary(object, ...)
```

## Arguments

- object:

  An object of class `"visstat"`, returned by
  [`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md).

- ...:

  Currently unused. Included for S3 method compatibility.

## Value

The object `object`, invisibly.

## Details

This method provides a full textual report of the statistical test
results returned by
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md),
and prints the contents of `posthoc_summary` if present.

## See also

[`print.visstat`](https://shhschilling.github.io/visStatistics/reference/print.visstat.md),
[`visstat`](https://shhschilling.github.io/visStatistics/reference/visstat.md)

## Examples

``` r
anova <- visstat(npk$block, npk$yield)


summary(anova)
#> Summary of visstat object
#> 
#> --- Named components ---
#> [1] "summary statistics of ANOVA" "post-hoc analysis "         
#> [3] "conf.level"                  "effect_size"                
#> 
#> --- Contents ---
#> 
#> $summary statistics of ANOVA:
#> 
#>  One-way analysis of means (not assuming equal variances)
#> 
#> data:  samples and fact
#> F = 6.2463, num df = 5.0000, denom df = 8.0508, p-value = 0.01178
#> 
#> 
#> $post-hoc analysis :
#> $fact
#>       diff         lwr       upr      p adj
#> 2-1 -3.425 -14.5683168  7.718317 1.00000000
#> 3-1 -6.750 -18.9339339  5.433934 1.00000000
#> 4-1  3.900  -9.5032841 17.303284 1.00000000
#> 5-1  3.500  -7.8081398 14.808140 1.00000000
#> 6-1 -2.325 -13.3624780  8.712478 1.00000000
#> 3-2 -3.325 -13.6960897  7.046090 1.00000000
#> 4-2  7.325  -5.2411856 19.891186 1.00000000
#> 5-2  6.925   3.7607460 10.089254 0.03099071
#> 6-2  1.100  -2.8180392  5.018039 1.00000000
#> 4-3 10.650  -2.4317408 23.731741 1.00000000
#> 5-3 10.250  -0.2843401 20.784340 0.69713546
#> 6-3  4.425  -5.8476436 14.697644 1.00000000
#> 5-4 -0.400 -13.1296857 12.329686 1.00000000
#> 6-4 -6.225 -18.6775994  6.227599 1.00000000
#> 6-5 -5.825  -9.5001111 -2.149889 0.13518658
#> 
#> 
#> $conf.level:
#> [1] 0.95
#> 
#> $effect_size:
#> $name
#> [1] "approximate omega-squared-type"
#> 
#> $estimate
#> [1] 0.6511908
#> 
#> $effect_size_method
#> [1] "Approximate omega-squared-type measure for Welch's one-way test, computed from F, df1, and df2"
#> 
```
