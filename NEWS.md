# visStatistics 0.1.2

## News

-   Added vignette

## Bug fixes

-   Regression confidence and prediction bands now depend on `conf.level`
-   Post-hoc-Analysis in Kruskal-Wallis-test (`pairwise.wilcox.test()`) now depends on family wise error rate `1-conf.int`
-   Calculation of $alpha_{Sidak}$corrected leading to larger Sidak corrected confidence intervals 
