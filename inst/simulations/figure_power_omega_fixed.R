## ---------------------------------------------------------------------------
## REFERENCE FIGURE: Route 1 power with omega^2 held FIXED across all designs.
##
## NOT a vignette figure. The vignette's power figure is route1_power_figure.R,
## which uses the same shift vector in every design and lets omega^2 vary as a
## consequence of the allocation.
##
## Input : fleishman_4groups_power_omega_fixed_B50000.csv
##         (written by route1_power_omega_fixed.R; the shifts are rescaled per
##          design by scale_omega_fixed() in omega_scaling_helpers.R so that the
##          population omega^2 equals the balanced homoscedastic baseline
##          0.0725 in every design)
## Output: fleishman_4groups_power_omega_fixed_balanced.png
##         fleishman_4groups_power_omega_fixed_unbalanced.png
##
## Read this figure as: with the effect size equalised, what is left of the
## difference between designs? Note the built-in limitation -- equalising the
## effect size flattens the parametric branch by construction, so this figure
## cannot answer "what does imbalance cost". See the helpers file header.
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
ETA_OWN_GRID <- "omega_fixed"

build_fixed_es_figures(
  results_file = file.path(SIMDIR, "fleishman_4groups_power_omega_fixed_B50000.csv"),
  es_column    = "omega_sq",
  es_label     = "&omega;<sup>2</sup>",
  out_prefix   = "fleishman_4groups_power_omega_fixed",
  outdir       = "."
)
