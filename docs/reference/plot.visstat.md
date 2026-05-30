# Plot method for visstat objects

Replays captured plots or reports saved plot file paths from a `visstat`
object.

## Usage

``` r
# S3 method for class 'visstat'
plot(x, which = NULL, ...)
```

## Arguments

- x:

  An object of class `"visstat"`, returned by
  [`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md).

- which:

  Integer selecting a single plot to display (1, 2, ...). If `NULL` (the
  default), all available plots are listed without being drawn.

- ...:

  Currently unused. Included for S3 method compatibility.

## Value

The object `x`, invisibly. Called for its side effect.

## Details

When called without `which`, the method lists all available plots
(either as file paths or as indices of captured plots). When called with
`which`, the selected plot is displayed: for file-based output the
stored path is printed, for captured plots the plot is replayed via
[`replayPlot()`](https://rdrr.io/r/grDevices/recordplot.html).

## See also

[`print.visstat`](https://shhschilling.github.io/visStatistics/reference/print.visstat.md),
[`summary.visstat`](https://shhschilling.github.io/visStatistics/reference/summary.visstat.md),
[`visstat`](https://shhschilling.github.io/visStatistics/reference/visstat.md)

## Examples

``` r
# File-based output: plot() lists stored paths
anova_path <- visstat(
  npk$block,
  npk$yield,
  graphicsoutput = "png",
  plotDirectory = tempdir()
)

plot(anova_path)
#> Plot [1] stored in /var/folders/5c/n85wqnh95l50qbp3s9l0rp_w0000gn/T//RtmpYKlrsH/glm_assumptions_yield_block.png
#> Plot [2] stored in /var/folders/5c/n85wqnh95l50qbp3s9l0rp_w0000gn/T//RtmpYKlrsH/anova_yield_block.png

# Interactive output: plot() lists available plots,
# plot(obj, which = n) replays a specific one
linreg <- visstat(trees$Height, trees$Girth)
#> Warning: Statistical assumptions violated:
#> Normality of residuals violated (Shapiro-Wilk p = 0.0373 )
#> Homoscedasticity violated (Breusch-Pagan p = 0.0158 )
#> Analysis proceeded but interpret results cautiously.
#> RECOMMENDATION: Consider exploring alternatives outside visstat() such as data transformations,
#> generalised linear models, or robust regression. For a non-causal alternative
#> consider rerunning with correlation = TRUE.

plot(linreg)
#> Plot [1] captured. Use plot(obj, which = 1) to display.
#> Plot [2] captured. Use plot(obj, which = 2) to display.
plot(linreg, which = 2)
#> Warning: calling par(new=TRUE) with no plot
```
