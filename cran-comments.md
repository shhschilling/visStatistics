## CRAN submission for visStatistics [version 0.1.5]

This is a resubmission of the package after addressing the following issues:
  
  ### What was fixed
  
  - Reduced the package tarball size from 5.3 MB to 3.1 MB by:
- Excluding development artefacts via `.Rbuildignore`, such as:
  - `vignettes/*.html`
  - `vignettes/*.ppt`

- Ensured only valid vignette outputs generated by `devtools::build_vignettes()` are present in `inst/doc/`

- Confirmed all vignettes are reproducible from source (`.Rmd`) and no pre-built HTML is included in `vignettes/`

### Notes

- This package generates plots in `@examples` deliberately, as graphical output is central to its functionality. The `man/figures/` folder is therefore intentionally included.

- The title in `\VignetteIndexEntry{}` is intentionally shorter than the full YAML title. This is suppressed via:
  `options(rmarkdown.html_vignette.check_title = FALSE)`

### Test environment


- R CMD check --as-cran: 0 errors, 0 warnings, 0– notes

Thank you for reviewing this update.