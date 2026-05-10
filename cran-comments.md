## CRAN submission for visStatistics [version 0.2.0]

This is a resubmission of the package after addressing the following issues.

## Major changes

- **New unified statistical decision logic:** The algorithm for selecting appropriate hypothesis tests has been substantially revised. See `vignette("visStatistics")` for detailed justification.

- **New post-hoc test:** `games.howell()` is used as the post-hoc test after Welch's ANOVA when variances are unequal, as it correctly handles this case. `TukeyHSD()` remains for Student's ANOVA.

- **Variance homogeneity test:** Changed from Bartlett's test to Levene-Brown-Forsythe test (`levene.test()`).

## Structural Improvements


- **Formula interface:** `visstat()` now accepts the formula interface
`visstat(y ~ x, data = df)` in addition to the existing calling conventions.

- **Correlation analysis:** New parameter `correlation`. When `TRUE`,
  selects Spearman's $\rho$ for two numeric variables or Kendall's
  $\tau_b$ when both variables are ordered factors.

## New features

- **Ordered factor support:** When a variable is of class `ordered`
  (e.g., Likert scales), `visstat()` converts it to numeric ranks and
  applies Wilcoxon or Kruskal-Wallis. When both variables are ordered
  and `correlation = TRUE`, Kendall's $\tau_b$ is used.


- **New exported functions:**
  - `levene.test()`: Levene-Brown-Forsythe test for homogeneity of variance
  - `bp.test()`: Breusch-Pagan test for heteroscedasticity
  - `games.howell()`: Games-Howell post-hoc test
  - `vis_numeric()`: Visualization for numeric-numeric relationships
  - `vis_group_normality()`: Diagnostic plots for Welch t-test/ANOVA

## Documentation improvements

- Vignette substantially revised with updated decision logic, new sections on correlation analysis and the general linear model framework
- README updated to reflect current decision tree


## Test environment

- R CMD check --as-cran: 0 errors, 0 warnings, 1 note

## Note

The CRAN incoming feasibility check flags "lm" and "spearman" as possibly misspelled words in DESCRIPTION. These are legitimate statistical terms:
- **lm**: Linear model (standard R function for fitting linear models)
- **spearman**: Spearman's rank correlation coefficient (non-parametric correlation method)

Both terms are included in the package's WORDLIST and are appropriate technical terminology for a statistics package.

## Changes since previous submission (0.1.7)

- Fixed roxygen documentation compliance issues

- Updated DESCRIPTION to reflect new decision logic

Thank you for reviewing this update.
