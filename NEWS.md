# visStatistics 0.1.2

## News

-   Added vignette `visStatistics.Rmd` documenting the statistical decision logic and illustrating each case with reproducible examples.
-   Added graphical summary of decision logic to README and vignette

## Improvements

-   Extended `README.html` and the `@details` section of the main function `visstat()`:
    - More precise parameter descriptions and clearer presentation of the decision logic.

## Change in decision logic

-   Welch's t-test (`t.test()`) is now applied when both groups have more than 30 observations (previous threshold was 100).

## Bug fixes

-   Confidence and prediction bands in regression now depend on the specified `conf.level` instead of always defaulting to 0.95.
-   Post hoc analysis in Kruskal-Wallis test (`pairwise.wilcox.test()`) now uses the specified `conf.level` instead of always defaulting to 0.95.
-   Switching to `fisher.test()` now correctly follows expected cell count thresholds.
