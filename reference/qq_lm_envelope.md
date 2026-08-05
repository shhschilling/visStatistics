# Simulated Q-Q envelopes for linear model residuals

Computes pointwise and simultaneous Q-Q envelopes for internally
studentised residuals from an unweighted `lm` or `aov` object by
repeatedly simulating responses from the fitted normal-error model and
refitting the model. The simultaneous envelope follows the Monte Carlo
tolerance-band idea of Schützenmeister et al. (2012); see
[`vis_lm_assumptions`](https://shhschilling.github.io/visStatistics/reference/vis_lm_assumptions.md)
and
[`visstat`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
for where these bands are drawn.

## Usage

``` r
qq_lm_envelope(
  model,
  conf.level = 0.95,
  nsim = getOption("visStatistics.qq_nsim", 5000L),
  q.type = 2L,
  tol = 1e-04,
  max.iter = 100L
)
```

## Arguments

- model:

  An unweighted `lm` or `aov` object.

- conf.level:

  Numeric confidence level for the envelopes.

- nsim:

  Integer number of simulated refits.

- q.type:

  Integer quantile type passed to
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html).

- tol:

  Numeric tolerance for the simultaneous-band coverage search.

- max.iter:

  Integer maximum number of bisection iterations.

## Value

A list of class `qq_lm_envelope` with the observed sorted residuals,
theoretical quantiles, pointwise bounds, simultaneous bounds, the
simulated sorted residual matrix, and the achieved simultaneous
coverage.

## References

Schützenmeister, A., Jensen, U., & Piepho, H.-P. (2012). Checking
Normality and Homoscedasticity in the General Linear Model Using
Diagnostic Plots. *Communications in Statistics - Simulation and
Computation*, 41(2). doi:10.1080/03610918.2011.582560.

## Examples

``` r
fit <- lm(mpg ~ wt, data = mtcars)
env <- qq_lm_envelope(fit, nsim = 100)
str(env)
#> List of 10
#>  $ expected          : num [1:32] -2.15 -1.68 -1.42 -1.23 -1.08 ...
#>  $ observed          : Named num [1:32] -1.52 -1.31 -1.24 -1.16 -1.07 ...
#>   ..- attr(*, "names")= chr [1:32] "Ford Pantera L" "Duster 360" "AMC Javelin" "Camaro Z28" ...
#>  $ sim_orders        : num [1:100, 1:32] -1.76 -2.16 -2.37 -1.92 -2.06 ...
#>  $ conf.level        : num 0.95
#>  $ nsim              : int 100
#>  $ q.type            : int 2
#>  $ pointwise         : num [1:2, 1:32] -2.87 -1.65 -2.21 -1.28 -1.81 ...
#>  $ global            : num [1:2, 1:32] -3.22 -1.39 -2.32 -1.21 -1.87 ...
#>  $ global_coverage   : num 1
#>  $ global_local_alpha: num 0.02
#>  - attr(*, "class")= chr "qq_lm_envelope"
```
