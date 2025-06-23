## CRAN submission for visStatistics [version 0.1.8]
This is a resubmission of the package after addressing the following issues:

## Major changes
 -  Included new function `levene.test()` implementing the Levene-Brown-Forsythe Test for homogeneity of variance (center = median). It mimics the default behaviour of `leveneTest` in the `car` - package .
 -  Decision test logic now based on `shapiro,test()` and `levene.test(). Described
    in detail in `vignette("visStatistics").
    
## Structural Improvements

-   The class `"visstat"` now includes a plot-method: `plot.visstat()`.
-   All tests for comparing central tendencies show plots for testing the  normality assumption.

-   Diagnostic plots for normality now include histogram overlaid by normal distribution. 
-   All assumption plots are now saved with the prefix "assumption" followed by 
    plot name. 

## Minor Improvements

  Explanation of test logic extended, above all the assumption checking, both pased
  on hypotheis tests and visual inspection. Bibliography of `vignette` extended.
`

### Test environment


- R CMD check --as-cran: 0 errors, 0 warnings, 0– notes

Thank you for reviewing this update.