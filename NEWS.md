# visStatistics 0.1.2

## News

-   Added vignette

## Bug fixes

-   Regression confidence and prediction bands now depend on `conf.level`.
-   Post-hoc-Analysis in Kruskal-Wallis-test (`pairwise.wilcox.test()`) now depends on `1-conf.int`
-    Switch to `fisher.test()` now depends on expected cell counts
