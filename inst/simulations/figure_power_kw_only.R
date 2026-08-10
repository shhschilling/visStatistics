## ---------------------------------------------------------------------------
## Route 1 power figures as the package actually behaves: Kruskal-Wallis is the
## only rank-based test drawn, because it is the only one visstat_core() can
## select. The pseudo-rank arms (RK, ATS, ATSp) were simulated to decide whether
## that default should change; the conclusion was that it should not
## (NOTE_atsp_routing.md), so they are exploratory and do not belong in a figure
## that documents the implementation.
##
## Six strategies, matching the decision logic:
##   F     Fisher's ANOVA always
##   W     Welch's ANOVA always
##   L     Levene-gated Fisher/Welch
##   KW    Kruskal-Wallis always
##   SW    Shapiro-Wilk routed Welch/KW
##   SW+L  Shapiro-Wilk plus Levene   <- what visstat() does
##
## The simulated grid is the one that answers "what does the design cost":
## the shift vector is IDENTICAL in every design and omega^2 is reported as the
## consequence of the allocation, not held fixed. The two rescaled grids
## (figure_power_omega_fixed.R, figure_power_etaH_fixed.R) pin one branch's
## effect size and starve the other, and are reference material only.
##
## Input : fleishman_4groups_power.rds
##         effect_sizes_by_design_panel_legacy.csv (omega^2 per design)
## Output: fleishman_4groups_power_kw_balanced.png
##         fleishman_4groups_power_kw_unbalanced.png
##
## The distinct prefix is deliberate: the full figures carrying every simulated
## arm (fleishman_4groups_power_balanced.png / _unbalanced.png) are reference
## material and must not be overwritten by this variant.
## ---------------------------------------------------------------------------

SIMDIR <- local({
  here <- getwd()
  if (file.exists(file.path(here, "route1_power_figure.R"))) {
    here
  } else {
    installed <- system.file("simulations", package = "visStatistics")
    if (!nzchar(installed)) stop("Run from inst/simulations/ or install visStatistics.")
    installed
  }
})

## Both knobs are read by route1_power_figure.R and must be set before sourcing.
POWER_INCLUDE_PSEUDORANK <- FALSE
POWER_OUT_PREFIX <- "fleishman_4groups_power_kw"

source(file.path(SIMDIR, "route1_power_figure.R"))
