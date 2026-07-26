---
editor_options: 
  markdown: 
    wrap: 72
---

# visStatistics 0.3.0

## Major changes in Route 1 (numeric response, categorical predictor)

- **Two fixed defaults alongside the automated routing.** New argument
  `group_test` in `visstat()`. The default `group_test = NULL` keeps the
  assumption-driven routing. `group_test = "welch"` keeps Route 1 on the
  mean scale and forces Welch-type tests, while `group_test = "rank"`
  forces the rank-based tests. Both bypass the preliminary assumption
  tests.

- **The large-sample gate was dropped.** In version 0.2.0, normality
  testing was skipped and parametric tests applied directly whenever every
  group held more than 50 observations. This bypass is disabled: the
  Shapiro--Wilk test on the studentised residuals now routes the selection
  at all group sizes. Selected tests can therefore differ from 0.2.0 for
  designs in which every group exceeds 50 observations.

The two changes are one shift rather than two: the implicit, sample-size
based override of the assumption tests was replaced by explicit ones in
both directions. The old gate could only push the selection towards the
mean-based tests, and only once every group passed 50 observations;
`group_test` now fixes either branch, `"welch"` on the mean scale or
`"rank"` on the rank scale, at any group size. Users who relied on the
bypass obtain mean-based tests with `group_test = "welch"`.


## Changes to the exported API

- Newly exported: `effect_size()` and `qq_lm_envelope()`.
- No longer exported: `vis_anova()`, `vis_numeric()`, `gh_letters()`,
  `vis_anova_assumptions()` and `vis_group_normality()`. Internal routing
  and plotting helpers are no longer exported or documented as standalone
  functions.
- No longer shipped: `vis_group_normality()` and `pooled_normality_test()`.
- `vis_anova_assumptions()` remains as an internal deprecated wrapper for
  `vis_lm_assumptions()`.


## Effect sizes

- New, exported `effect_size()` function to generate effect-size output.
- Examples, tests, and vignette documentation include now the effect-size
  output.


## Diagnostics

- New, exported `qq_lm_envelope()`. The Q--Q bands for the internally
  studentised residuals of an unweighted `lm()` or `aov()` fit are now
  obtained by Monte Carlo simulation: responses are repeatedly drawn from
  the fitted normal-error model and the model is refitted. Both a
  point-wise and a simultaneous band are returned at the requested
  `conf.level`, the simultaneous band following the tolerance-band
  construction of Schuetzenmeister et al. (2012), together with its
  achieved coverage.

- The number of simulated refits defaults to 5000. `qq_lm_envelope()` and
  `vis_lm_assumptions()` take it as an argument (`nsim` and `qq_nsim`);
  `visstat()` has no such argument, so there it is set session-wide through
  the `visStatistics.qq_nsim` option, for example
  `options(visStatistics.qq_nsim = 1000L)`.

- The diagnostic Q--Q panel of `vis_lm_assumptions()` displays these
  simulated bands.


## Documentation

- The vignette adds Monte Carlo simulations (B = 50,000) quantifying the
  Type I error and power of the default gating against fixed Welch and
  fixed Kruskal--Wallis defaults.

- The documented routing logic for numeric response and categorical
  predictor input has been updated.


# visStatistics 0.2.0

## Changes in decision logic

- **Unified normality testing across all branches.** The two-group case
  (t-test vs Wilcoxon) and the multi-group case (ANOVA vs
  Kruskal-Wallis) now share the same normality test: `shapiro.test()`
  applied to the internally studentised residuals from `lm(y ~ x)` via
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
  - Clarified that `rstandard()` computes internally studentised
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
