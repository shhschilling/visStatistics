## CRAN submission for visStatistics 0.1.2

This is a resubmission. The previous version 0.1.1 is already on CRAN.

### Summary of changes

- Added a detailed vignette `visStatistics.Rmd`, documenting the statistical decision logic with examples for each case.
- Extended the `README.md` and `@details` section of `visstat()` with clearer parameter descriptions and logic overview.
- Changed Welch’s t-test decision threshold: now applied when both groups have >30 observations (was 100).
- Grammar, spelling, and formatting improvements throughout the documentation.
- Updated the package description to meet CRAN standards and clarify statistical terminology.
- Adjusted internal logic for tests and post hoc procedures to respect user-defined `conf.level`.

### Bug fixes

- Confidence and prediction bands in regression now depend on `conf.level` instead of defaulting to 0.95.
- Post hoc analysis in Kruskal-Wallis test (`pairwise.wilcox.test()`) now uses the specified `conf.level`.
- Switching to `fisher.test()` now correctly depends on expected cell counts.


