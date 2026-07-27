Dear CRAN-maintainer


Below you find the summary of the main changes in visStatistics [version 0.3.0] to the current version 0.2.0 on CRAN:

## CRAN submission for visStatistics [version 0.3.0]

This is a feature release following version 0.2.0.

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

## Test environment

- R 4.6.0, macOS Sequoia 15.7.2 (aarch64-apple-darwin23)

## Check status


- NOTE: Possibly misspelled words in DESCRIPTION: Kruskal, Spearman,
  Wilcoxon. These are the surnames of the statisticians the tests are
  named after, spelled as in the cited literature.

- NOTE: installed size is 10.1Mb, with the sub-directories doc (6.6Mb)
  and help (2.8Mb) above 1Mb. The package selects and *visualises*
  statistical tests, so its documentation is largely graphical: the
  vignette embeds the assumption-diagnostic panels, the result panels of
  every implemented test, and the three Monte Carlo simulation figures it
  discusses, and the help pages embed the corresponding example plots.
  The Monte Carlo results shipped in inst/simulations are the replication
  material for those figures and take 168Kb; the figures themselves are
  not shipped, but rebuilt from the results by the scripts in the same
  directory.

Thank you for reviewing this release.

Best regards, Sabine Schilling
