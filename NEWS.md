---
editor_options: 
  markdown: 
    wrap: 80
---

# visStatistics 0.1.6

## Major changes

-   The `visstat()` function interface has been updated to accept two vectors
    directly, enabling a more concise and idiomatic usage. For example:

    visstat(trees$Girth, trees$Height)

    yields the same result as the original form:

    visstat(trees, "Height", "Girth")

    This change aligns with standard R conventions. Both
    calling styles remain supported for backwards compatibility.

    See `?visstat`, the README, or `vignette("visStatistics")` for details.

.

## Structural Improvements

-   The `visstat()` function now returns an object of class `"visstat"`,
    enabling consistent method dispatch.
-   New S3 methods added:
    -   `print.visstat()` – shows a concise summary,
    -   `summary.visstat()` – prints the full test and post hoc summaries.

# visStatistics 0.1.5

## News

-   Extended vignette: all implemented tests are explained in greater detail.
-   Graphical output displays the corresponding test statistics, in addition to
    p-values, where appropriate.
-   Internal helper function `get_samples_fact_inputfile()` no longer exported
    to NAMESPACE.

## Bug fixes

-   The legend for the Šidák-corrected confidence interval no longer incorrectly
    states that it displays group means.

# visStatistics 0.1.3

## News

-   Added vignette `visStatistics.Rmd` documenting the statistical decision
    logic, with reproducible examples illustrating each test case.
-   Added a graphical summary of the decision logic to the README and vignette.

## Improvements

-   Extended `README.html` and the `@details` section of the main function
    `visstat()`:
    -   More precise parameter descriptions.
    -   Clearer presentation of the decision logic.

## Change in decision logic

-   Welch’s t-test (`t.test()`) is now applied when both groups have more than
    30 observations (previous threshold was 100).

## Bug fixes

-   Confidence and prediction bands in regression now correctly reflect the
    specified `conf.level` rather than defaulting to 0.95.
-   Post hoc analysis in the Kruskal–Wallis test (`pairwise.wilcox.test()`) now
    uses the specified `conf.level`.
-   Switching to `fisher.test()` now correctly follows the expected cell count
    thresholds.
