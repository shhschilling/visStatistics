`visStatistics` provides a workflow for routine two-variable frequentist inference in R.
Given two vectors, `visstat()` first dispatches by variable classes, factor levels, sample sizes, expected cell counts, and explicit user options.

Its assumption-driven branch concerns tests of central tendency for a numeric response grouped by a factor.
There, in the default setting, sample size and residual diagnostics from a fitted linear model choose between rank-based and mean-based tests and, within the latter, between equal-variance and Welch-type variants.

The output is deliberately visual: diagnostic plots are shown together with assumption-test $p$\ values, the selected test, effect size, and post-hoc results where applicable.
This shifts attention from ad-hoc test selection to visual diagnostic assessment and statistical interpretation.

The automated workflow of `visStatistics` is particularly suited for server-side R applications, where users select variables through a web interface and receive a complete visual statistical analysis.
It also supports time-constrained work such as statistical consulting, where less time spent on test selection leaves more room for interpretation.
