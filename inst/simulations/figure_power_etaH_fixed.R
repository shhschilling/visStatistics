## ---------------------------------------------------------------------------
## REFERENCE FIGURE: Route 1 power with eta_H^2 held FIXED across all designs.
##
## NOT a vignette figure, and the effect size it holds fixed is NOT a citable
## population parameter.
##
## Unlike omega^2, eta_H^2 has no established population definition independent
## of N: ranks have no population-level existence the way (mu_j, sigma_j^2) do,
## because the rank of an observation is defined only relative to the sample it
## sits in. The grid plotted here holds fixed a quadrature-computed value (see
## eta_h_population.R) and that value is reported as "the quantity this
## simulation held constant", not as a population effect size. Nothing in the
## vignette or the manuscript should label a panel with it.
##
## Input : fleishman_4groups_power_etaH_fixed_B50000.csv
##         (written by route1_power_etaH_fixed.R)
## Output: fleishman_4groups_power_etaH_fixed_balanced.png
##         fleishman_4groups_power_etaH_fixed_unbalanced.png
##
## Kept for inspection, as the counterpart to figure_power_omega_fixed.R:
## equalising the rank-based effect size instead of the parametric one flattens
## the rank branch by construction rather than the parametric branch. Comparing
## the two shows which branch each construction immobilises.
## ---------------------------------------------------------------------------

SIMDIR <- local({
  here <- getwd()
  if (file.exists(file.path(here, "power_figure_fixed_effect_size_helpers.R"))) {
    here
  } else {
    installed <- system.file("simulations", package = "visStatistics")
    if (!nzchar(installed)) stop("Run from inst/simulations/ or install visStatistics.")
    installed
  }
})
source(file.path(SIMDIR, "power_figure_fixed_effect_size_helpers.R"))

## Select the eta_H^2 rows computed from THIS grid's own design constants.
ETA_OWN_GRID <- "etaH_fixed"

## es_label = NA: no eta_H^2 number is printed in the row headers, deliberately,
## because no source we have read defines a population counterpart of
## (H - k + 1)/(N - k). A row header states a property of the design, so it may
## carry omega^2, which has cited population definitions, and not eta_H^2.
##
## The sample quantity converges: simulated at n per group 10, 20, 30, 50, 100,
## 200 (balanced equal SD, panel 1, 4000 reps each) it reads 0.0681, 0.0699,
## 0.0701, 0.0700, 0.0697, 0.0702, against the 0.0709 the grid was solved for
## at N = 40000
## (route1_power_etaH_fixed.R:77). Convergence is all that can be shown; the
## limit has no name in the literature, so it is not labelled here.
build_fixed_es_figures(
  results_file = file.path(SIMDIR, "fleishman_4groups_power_etaH_fixed_B50000.csv"),
  es_column    = "eta_h_sq",
  es_label     = NA,
  out_prefix   = "fleishman_4groups_power_etaH_fixed",
  outdir       = "."
)
