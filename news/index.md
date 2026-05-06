# Changelog

## visStatistics 0.1.8

### Major changes

- Included new function `levene.test()` implementing the
  Levene-Brown-Forsythe Test for homogeneity of variance (center =
  median). It mimics the default behaviour of `leveneTest` in the
  `car` - package .
- Decision test logic for numerical response vector and categorical
  predictor is now based on `shapiro,test()` and
  `levene.test(). Described in detail in`vignette(“visStatistics”).

### Structural Improvements

- The class `"visstat"` now includes a plot-method: `plot.visstat()`.
- All tests for comparing central tendencies show plots for testing the
  normality assumption.
- Diagnostic plots for normality now include histogram overlaid by
  normal distribution.
- All assumption plots are now saved with the prefix “assumption”
  followed by plot name.

### Minor Improvements

In `vignette`: - Paragraph on the assumption checking, based on
hypothesis tests and visual inspection, extended. - Complemented
bibliography of `vignette`. - \`

## visStatistics 0.1.6

### Major changes

- The
  [`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
  function interface has been updated to accept two vectors directly,
  enabling a more concise and idiomatic usage. For example:

  visstat(trees$`Girth, trees`$Height)

  yields the same result as the original form:

  visstat(trees, “Height”, “Girth”)

  This change aligns with standard R conventions. Both calling styles
  remain supported for backwards compatibility.

  See
  [`?visstat`](https://shhschilling.github.io/visStatistics/reference/visstat.md),
  the README, or
  [`vignette("visStatistics")`](https://shhschilling.github.io/visStatistics/articles/visStatistics.md)
  for details.

.

### Structural Improvements

- The
  [`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
  function now returns an object of class `"visstat"`, enabling
  consistent method dispatch.
- New S3 methods added:
  - `print.visstat()` – shows a concise summary,
  - `summary.visstat()` – prints the full test and post hoc summaries.

## visStatistics 0.1.5

CRAN release: 2025-05-24

### News

- Extended vignette: all implemented tests are explained in greater
  detail.
- Graphical output displays the corresponding test statistics, in
  addition to p-values, where appropriate.
- Internal helper function `get_samples_fact_inputfile()` no longer
  exported to NAMESPACE.

### Bug fixes

- The legend for the Šidák-corrected confidence interval no longer
  incorrectly states that it displays group means.

## visStatistics 0.1.3

CRAN release: 2025-05-12

### News

- Added vignette `visStatistics.Rmd` documenting the statistical
  decision logic, with reproducible examples illustrating each test
  case.
- Added a graphical summary of the decision logic to the README and
  vignette.

### Improvements

- Extended `README.html` and the `@details` section of the main function
  [`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md):
  - More precise parameter descriptions.
  - Clearer presentation of the decision logic.

### Change in decision logic

- Welch’s t-test ([`t.test()`](https://rdrr.io/r/stats/t.test.html)) is
  now applied when both groups have more than 30 observations (previous
  threshold was 100).

### Bug fixes

- Confidence and prediction bands in regression now correctly reflect
  the specified `conf.level` rather than defaulting to 0.95.
- Post hoc analysis in the Kruskal–Wallis test
  ([`pairwise.wilcox.test()`](https://rdrr.io/r/stats/pairwise.wilcox.test.html))
  now uses the specified `conf.level`.
- Switching to
  [`fisher.test()`](https://rdrr.io/r/stats/fisher.test.html) now
  correctly follows the expected cell count thresholds.
