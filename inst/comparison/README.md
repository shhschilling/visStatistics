# Branching comparison against related automatic-test packages

Evidence for the claim in the vignette section "Packages with related scope":
among the automated test-selection packages, only `visStatistics` bases the
central-tendency route on the residuals of the fitted linear model, whereas the
others test the response either as a whole or separately within groups.

Locate the directory from R with

```r
system.file("comparison", package = "visStatistics")
```

and copy `compare_competitor_branching.R` to a writable working directory before
running it; it writes plots to `competitor_branching_plots/` in the current
working directory.

## What the script does

It searches for datasets on which the residual-based route and the competitors'
routes disagree: four groups with shifted means and a common $t_8$ error
distribution, so that the pooled response is non-normal only because the group
means differ, while the model residuals are not. It then reports the branch each
logic selects, and calls the installed packages themselves.

## What it requires

`automatedtests`, `autotestR`, `boxTest` and `compareGroups`, none of which is a
dependency of `visStatistics`. Each is guarded by `requireNamespace()`, so the
script runs with whatever subset is installed and reports which were available.

## Results with the versions tested

Run against `compareGroups` 4.10.2, `boxTest` 0.1.0, `autotestR` 1.2.15 and
`automatedtests` 0.1.2:

| package | function | normality assessed on | branch selected |
|---|---|---|---|
| `visStatistics` | `visstat()` | standardised residuals of `lm(y ~ group)`, $p_{SW} = 0.878$ | Welch ANOVA |
| `automatedtests` | `automatical_test()` | ungrouped response, $p_{SW} = 0.00022$ | Kruskal-Wallis |
| `compareGroups` | `compareGroups(method = NA)` | ungrouped response, $p_{SW} = 0.00022$ | continuous non-normal |
| `autotestR` | `pre.test()`, `test.anova()` | each group separately, $p_{SW} = 0.657, 0.683, 0.106, 0.017$ | Kruskal-Wallis with Dunn post hoc |
| `boxTest` | `compare_two_groups()` | each group separately, $p_{SW} = 0.811, 0.028$ | Mann-Whitney U |

The `autotestR` and `boxTest` rows use the two-group and four-group examples in
which group-wise normality is rejected for one group while the pooled residuals
pass; in those cases `visstat()` selects Fisher's ANOVA and Student's t-test
respectively. `compareGroups` reports medians and interquartile ranges rather
than means and standard deviations, which is how its choice of the non-normal
method is visible in the printed table.
