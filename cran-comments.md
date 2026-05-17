## CRAN submission for visStatistics [version 0.2.1]

This is a patch release following version 0.2.0.

## Changes since 0.2.0

- Added effect-size output to `visstat()` results for the implemented
  test branches. The returned `effect_size` field includes the
  effect-size name, estimate, and method description.

- Updated `vis_lm_assumptions()` to use the `correlation` argument
  consistently with `visstat()`.

- For numeric-numeric analyses with `correlation = TRUE`, the Spearman
  branch no longer creates the linear-model assumption diagnostic plot.
  The default regression branch remains unchanged and still returns the
  assumption diagnostic plot.

- Updated the corresponding Rd documentation and tests for the changed
  `vis_lm_assumptions()` argument.

- Added a regression test confirming that the default regression branch
  returns two captured plots and the Spearman correlation branch returns
  one captured plot.

- Added `inspectable` to the spelling word list.

- Added `visstatisticsArchive` to `.Rbuildignore` and `.gitignore`.

## Test environment

- R 4.6.0, macOS Sequoia 15.7.2 (aarch64-apple-darwin23)

## Check status

- `R CMD check --no-manual --no-build-vignettes .` confirms that
  code/documentation mismatches are resolved.
- The spelling test reports: `All Done!`

Thank you for reviewing this patch release.
