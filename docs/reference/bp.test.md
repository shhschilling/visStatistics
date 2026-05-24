# Breusch-Pagan Test for Heteroscedasticity

Performs the Breusch-Pagan test for heteroscedasticity in linear
regression models. Tests the null hypothesis that the error variance is
constant (homoscedasticity) against the alternative that the error
variance depends on the fitted values.

## Usage

``` r
bp.test(model)
```

## Arguments

- model:

  A fitted linear model object (from
  [`lm()`](https://rdrr.io/r/stats/lm.html)).

## Value

An object of class `"htest"` with components:

- statistic:

  the value of the chi-squared test statistic.

- parameter:

  degrees of freedom.

- p.value:

  the p-value of the test.

- method:

  a character string indicating the test performed.

- data.name:

  a character string giving the name of the model.

## Details

Implements the Koenker variant of the Breusch-Pagan test, which
regresses the squared raw residuals on the fitted values. The test
statistic is:

\$\$BP = n \cdot R^2\$\$

where:

- \\n\\ = sample size

- \\R^2\\ = coefficient of determination from auxiliary regression of
  \\e_i^2\\ on \\\hat{y}\_i\\

Under the null hypothesis of homoscedasticity, the test statistic
follows a chi-squared distribution: \\BP \sim \chi^2(k-1)\\ where \\k\\
is the number of parameters in the model (including intercept).

Large values of the test statistic (small p-values) provide evidence
against homoscedasticity.

## References

Breusch, T. S., and Pagan, A. R. (1979). A simple test for
heteroscedasticity and random coefficient variation. Econometrica,
47(5), 1287-1294. DOI: 10.2307/1911963

Koenker, R. (1981). A note on studentizing a test for
heteroscedasticity. Journal of Econometrics, 17(1), 107-112. DOI:
10.1016/0304-4076(81)90062-2

## Examples

``` r
# Example with homoscedastic errors
set.seed(123)
x <- runif(100)
y <- 2 + 3*x + rnorm(100, sd = 1)
model1 <- lm(y ~ x)
bp.test(model1)  # Should not reject (p > 0.05)
#> 
#>  Breusch-Pagan test for heteroscedasticity
#> 
#> data:  model1
#> BP = 0.025212, df = 1, p-value = 0.8738
#> 

# Example with heteroscedastic errors (variance increases with x)
set.seed(456)
x <- runif(100)
y <- 2 + 3 *x + rnorm(100, sd = 0.5 + 2*x)
model2 <- lm(y ~ x)
bp.test(model2)  # Should reject (p < 0.05)
#> 
#>  Breusch-Pagan test for heteroscedasticity
#> 
#> data:  model2
#> BP = 20.435, df = 1, p-value = 6.169e-06
#> 
```
