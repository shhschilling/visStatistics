---
editor_options: 
  markdown: 
    wrap: 72
---

# visStatistics 0.2.1

## API

- Reduced the exported standalone API to user-facing functions. Internal
  routing and plotting helpers are no longer exported or documented as
  standalone functions.
- `vis_anova()`, `vis_numeric()`, `vis_group_normality()`,
  `gh_letters()`, and `pooled_normality_test()` are now internal helpers.
- `vis_anova_assumptions()` remains as an internal deprecated wrapper for
  `vis_lm_assumptions()`, but is no longer exported or documented.
- `effect_size()` is now exported for supported `visstat()` result
  objects.

## Effect sizes

+- Added effect-size output to `visstat()` results for the implemented
+  test branches. The returned `effect_size` field includes the
+  effect-size name, estimate, and method description.

# visStatistics 0.2.0

## Changes in decision logic

- **Unified normality testing across all branches.** The two-group case
  (t-test vs Wilcoxon) and the multi-group case (ANOVA vs
  Kruskal-Wallis) now share the same normality test: `shapiro.test()`
  applied to the internally studentized residuals from `lm(y ~ x)` via
  `rstandard()`.

- **Variance homogeneity test changed from `bartlett.test()` to
  `levene.test()`.** The Brown-Forsythe Levene-type test (using the median)
  is more robust to non-normality than Bartlett's test. The test now
  determines whether Student's t-test (`t.test(var.equal = TRUE)`) or
  Welch's t-test is used in the two-group case, and whether `aov()` or
  `oneway.test()` is used in the multi-group case.

- **Large-sample threshold changed from 30 to 50 per group.** When every
  group exceeds 50 observations, normality testing is skipped and
  parametric tests are applied directly (justified by the central limit
  theorem).

- **Post-hoc test selection.** When `oneway.test()` (Welch's ANOVA) is
  used for unequal variances, `games.howell()` is the appropriate
  post-hoc test, as it does not assume equal variances. `TukeyHSD()`
  remains for `aov()` (Student's ANOVA).

- **Rank correlation (`correlation = TRUE`).** Selects the most
  appropriate rank correlation for the data type: Spearman's $\rho$ for
  two numeric variables, Kendall's $\tau_b$ when both variables are
  ordered factors. This is the only test decision not made automatically;
  it requires an explicit user choice.

## New features

- **Formula interface:** `visstat(y ~ x, data = df)` is now supported
  alongside the existing `visstat(x, y)` and
  `visstat(dataframe, "namey", "namex")` forms.

- **Ordered factor responses:** When the response is of class `ordered`,
  `visstat()` converts it to numeric ranks and applies Wilcoxon or
  Kruskal-Wallis. When both variables are ordered and `correlation = TRUE`,
  Kendall's $\tau_b$ is used instead.

- **Correlation analysis:** New parameter `correlation` in `visstat()`.
  When set to `TRUE`, selects Spearman rank correlation for numeric
  variables or Kendall's $\tau_b$ for two ordered factors, instead of
  fitting a regression model.

- **New exported functions:**

  - `levene.test()`: Brown-Forsythe Levene-type test for homogeneity of
    variance (center = median), mimicking the default behaviour of
    `leveneTest()` in the `car` package.
  - `bp.test()`: Breusch-Pagan test for heteroscedasticity in linear
    regression models.
  - `games.howell()`: Games-Howell post-hoc test for pairwise
    comparisons following Welch's ANOVA.
  - `vis_lm_assumptions()`: Renamed from `vis_anova_assumptions()`, now
    provides unified assumption diagnostics for the general linear model
    (t-test, ANOVA, regression).

## Breaking changes

- `vis_anova_assumptions()` has been removed and replaced by
  `vis_lm_assumptions()`. The new function handles both ANOVA and
  regression diagnostics (controlled by the `regression` parameter). For
  regression, it shows a Residuals vs Leverage plot (with Cook's
  distance contours) and the Breusch-Pagan test instead of the Bartlett
  test.

## Deprecated

- `vis_anova_assumptions()` is provided as a deprecated wrapper for
  `vis_lm_assumptions()`. It will be removed in a future version.

## Improvements

- Diagnostic plot title now reads "Linear model assumptions" with all
  test p-values on two lines.
- `plot.visstat()` method added to the `visstat` class.
- Diagnostic plots for normality now include a histogram overlaid with
  the normal density curve.
- All assumption plots are saved with the prefix "assumption".

## Documentation

- Vignette substantially revised:

  - Spearman correlation section rewritten, justification for
    Spearman-only approach added.
  - Complete decision logic rewritten to reflect the new test selection
    algorithm.
  - The general linear model framework (Appendix A) introduced.
  - Clarified that `rstandard()` computes internally studentized
    residuals, with reference to Cook and Weisberg (1982).
  - Bibliography extended.

- DESCRIPTION rewritten to reflect the updated test selection algorithm.

# visStatistics 0.1.7

- No user-visible changes relative to 0.1.6.

# visStatistics 0.1.6

## Major changes

- The `visstat()` function interface has been updated to accept two
  vectors directly, enabling a more concise and idiomatic usage. For
  example:

  visstat(trees$Girth, trees$Height)

  yields the same result as the original form:

  visstat(trees, "Height", "Girth")

  This change aligns with standard R conventions. Both calling styles
  remain supported for backwards compatibility.

## Structural Improvements

- The `visstat()` function now returns an object of class `"visstat"`,
  enabling consistent method dispatch.
- New S3 methods added:
  - `print.visstat()` – shows a concise summary,
  - `summary.visstat()` – prints the full test and post hoc summaries.

# visStatistics 0.1.5

## News

- Extended vignette: all implemented tests are explained in greater
  detail.
- Graphical output displays the corresponding test statistics, in
  addition to p-values, where appropriate.
- Internal helper function `get_samples_fact_inputfile()` no longer
  exported to NAMESPACE.

## Bug fixes

- The legend for the Šidák-corrected confidence interval no longer
  incorrectly states that it displays group means.

# visStatistics 0.1.3

## News

- Added vignette `visStatistics.Rmd` documenting the statistical
  decision logic, with reproducible examples illustrating each test
  case.
- Added a graphical summary of the decision logic to the README and
  vignette.

## Improvements

- Extended `README.html` and the `@details` section of the main function
  `visstat()`:
  - More precise parameter descriptions.
  - Clearer presentation of the decision logic.

## Change in decision logic

- Welch's t-test (`t.test()`) is now applied when both groups have more
  than 30 observations (previous threshold was 100).

## Bug fixes

- Confidence and prediction bands in regression now correctly reflect
  the specified `conf.level` rather than defaulting to 0.95.
- Post hoc analysis in the Kruskal–Wallis test
  (`pairwise.wilcox.test()`) now uses the specified `conf.level`.
- Switching to `fisher.test()` now correctly follows the expected cell
  count thresholds.
