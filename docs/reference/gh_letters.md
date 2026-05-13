# Get compact letter display from Games-Howell results

Converts Games-Howell test results into compact letter display using
multcompView. Groups sharing a letter are not significantly different.

## Usage

``` r
gh_letters(x, alpha = 0.05)
```

## Arguments

- x:

  A games.howell object from
  [`games.howell()`](https://shhschilling.github.io/visStatistics/reference/games.howell.md)

- alpha:

  Significance level (default: 0.05)

## Value

A named vector with group names and their letter codes

## Examples

``` r
# Convert dose to factor
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
result <- games.howell(ToothGrowth$len, ToothGrowth$dose)
letters <- gh_letters(result)
print(letters)
#> 0.5   1   2 
#> "a" "b" "c" 
```
