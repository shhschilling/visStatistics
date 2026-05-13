# Changelog

## visStatistics 0.2.0

CRAN release: 2026-05-12

### Changes in decision logic

- **Unified normality testing across all branches.** The two-group case
  (t-test vs Wilcoxon) and the multi-group case (ANOVA vs
  Kruskal-Wallis) now share the same normality test:
  [`shapiro.test()`](https://rdrr.io/r/stats/shapiro.test.html) applied
  to the internally studentized residuals from `lm(y ~ x)` via
  [`rstandard()`](https://rdrr.io/r/stats/influence.measures.html).

- **Variance homogeneity test changed from
  [`bartlett.test()`](https://rdrr.io/r/stats/bartlett.test.html) to
  [`levene.test()`](https://shhschilling.github.io/visStatistics/reference/levene.test.md).**
  The Levene-Brown-Forsythe test (using the median) is more robust to
  non-normality than Bartlett’s test. The test now determines whether
  Student’s t-test (`t.test(var.equal = TRUE)`) or Welch’s t-test is
  used in the two-group case, and whether
  [`aov()`](https://rdrr.io/r/stats/aov.html) or
  [`oneway.test()`](https://rdrr.io/r/stats/oneway.test.html) is used in
  the multi-group case.

- **Large-sample threshold changed from 30 to 50 per group.** When every
  group exceeds 50 observations, normality testing is skipped and
  parametric tests are applied directly (justified by the central limit
  theorem).

- **Post-hoc test selection.** When
  [`oneway.test()`](https://rdrr.io/r/stats/oneway.test.html) (Welch’s
  ANOVA) is used for unequal variances,
  [`games.howell()`](https://shhschilling.github.io/visStatistics/reference/games.howell.md)
  is the appropriate post-hoc test, as it does not assume equal
  variances. [`TukeyHSD()`](https://rdrr.io/r/stats/TukeyHSD.html)
  remains for [`aov()`](https://rdrr.io/r/stats/aov.html) (Student’s
  ANOVA).

- **Rank correlation (`correlation = TRUE`).** Selects the most
  appropriate rank correlation for the data type: Spearman’s $`\rho`$
  for two numeric variables, Kendall’s $`\tau_b`$ when both variables
  are ordered factors. This is the only test decision not made
  automatically; it requires an explicit user choice.

### New features

- **Formula interface:** `visstat(y ~ x, data = df)` is now supported
  alongside the existing `visstat(x, y)` and
  `visstat(dataframe, "namey", "namex")` forms.

- **Ordered factor responses:** When the response is of class `ordered`,
  [`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
  converts it to numeric ranks and applies Wilcoxon or Kruskal-Wallis.
  When both variables are ordered and `correlation = TRUE`, Kendall’s
  $`\tau_b`$ is used instead.

- **Correlation analysis:** New parameter `correlation` in
  [`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md).
  When set to `TRUE`, selects Spearman rank correlation for numeric
  variables or Kendall’s $`\tau_b`$ for two ordered factors, instead of
  fitting a regression model.

- **New exported functions:**

  - [`levene.test()`](https://shhschilling.github.io/visStatistics/reference/levene.test.md):
    Levene-Brown-Forsythe test for homogeneity of variance (center =
    median), mimicking the default behaviour of `leveneTest()` in the
    `car` package.
  - [`bp.test()`](https://shhschilling.github.io/visStatistics/reference/bp.test.md):
    Breusch-Pagan test for heteroscedasticity in linear regression
    models.
  - [`games.howell()`](https://shhschilling.github.io/visStatistics/reference/games.howell.md):
    Games-Howell post-hoc test for pairwise comparisons following
    Welch’s ANOVA.
  - [`vis_numeric()`](https://shhschilling.github.io/visStatistics/reference/vis_numeric.md):
    Visualisation of numeric-numeric relationships (regression or
    correlation).
  - [`vis_group_normality()`](https://shhschilling.github.io/visStatistics/reference/vis_group_normality.md):
    Diagnostic plots for the Welch t-test / Welch ANOVA branch.
  - [`vis_lm_assumptions()`](https://shhschilling.github.io/visStatistics/reference/vis_lm_assumptions.md):
    Renamed from
    [`vis_anova_assumptions()`](https://shhschilling.github.io/visStatistics/reference/vis_lm_assumptions.md),
    now provides unified assumption diagnostics for the general linear
    model (t-test, ANOVA, regression).

### Breaking changes

- [`vis_anova_assumptions()`](https://shhschilling.github.io/visStatistics/reference/vis_lm_assumptions.md)
  has been removed and replaced by
  [`vis_lm_assumptions()`](https://shhschilling.github.io/visStatistics/reference/vis_lm_assumptions.md).
  The new function handles both ANOVA and regression diagnostics
  (controlled by the `regression` parameter). For regression, it shows a
  Residuals vs Leverage plot (with Cook’s distance contours) and the
  Breusch-Pagan test instead of the Bartlett test.

### Deprecated

- [`vis_anova_assumptions()`](https://shhschilling.github.io/visStatistics/reference/vis_lm_assumptions.md)
  is provided as a deprecated wrapper for
  [`vis_lm_assumptions()`](https://shhschilling.github.io/visStatistics/reference/vis_lm_assumptions.md).
  It will be removed in a future version.

### Improvements

- Diagnostic plot title now reads “Linear model assumptions” with all
  test p-values on two lines.
- [`plot.visstat()`](https://shhschilling.github.io/visStatistics/reference/plot.visstat.md)
  method added to the `visstat` class.
- Diagnostic plots for normality now include a histogram overlaid with
  the normal density curve.
- All assumption plots are saved with the prefix “assumption”.

### Documentation

- Vignette substantially revised:

  - Spearman correlation section rewritten, justification for
    Spearman-only approach added.
  - Complete decision logic rewritten to reflect the new test selection
    algorithm.
  - The general linear model framework (Appendix A) introduced.
  - Clarified that
    [`rstandard()`](https://rdrr.io/r/stats/influence.measures.html)
    computes internally studentized residuals, with reference to Cook
    and Weisberg (1982).
  - Bibliography extended.

- DESCRIPTION rewritten to reflect the updated test selection algorithm.

## visStatistics 0.1.7

CRAN release: 2025-05-28

- No user-visible changes relative to 0.1.6.

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

### Structural Improvements

- The
  [`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
  function now returns an object of class `"visstat"`, enabling
  consistent method dispatch.
- New S3 methods added:
  - [`print.visstat()`](https://shhschilling.github.io/visStatistics/reference/print.visstat.md)
    – shows a concise summary,
  - [`summary.visstat()`](https://shhschilling.github.io/visStatistics/reference/summary.visstat.md)
    – prints the full test and post hoc summaries.

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
