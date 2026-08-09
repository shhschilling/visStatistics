## ---------------------------------------------------------------------------
## Population effect sizes of every (design, panel) cell of the Route 1 power
## grid, so that the power figures can state both of them.
##
## A power panel cannot be read without them: a strategy that rejects rarely in
## a given column may be a weak test, or it may be a test facing a nearly zero
## effect. Only omega^2 and eta_H^2 together distinguish the two.
##
##   omega^2   parametric effect size, closed form (omega_scaling_helpers.R).
##             Depends on the design only: the Fleishman panels are all
##             standardised to variance 1, so the panel does not enter.
##   eta_H^2   rank-based effect size, the large-sample limit of
##             (H - k + 1)/(N - k), computed EXACTLY by quadrature from
##             12 * sum_j p_j (r_j - 1/2)^2 (eta_h_population.R). Depends on
##             the design AND the panel, because it is driven by the shape of
##             the input distribution, not only by the shifts.
##
## Both columns are population parameters, not simulation estimates, and are
## named without a hat accordingly. eta_h_sq_mc / eta_h_sq_mc_se repeat the
## Monte Carlo estimate purely as a check on the quadrature; they are not what
## the figures display.
##
## Usage:  Rscript effect_sizes_by_design_panel.R [scaling] [reps] [base]
##   scaling  "legacy"      sqrt(mean(sd^2)), what route1_simulations.R uses and
##                          therefore what fleishman_4groups_power.csv contains
##            "omega_fixed" the corrected factor that holds omega^2 constant
##            "typeI"       all shifts zero, i.e. the equal-means grid of
##                          route1_typeI_figures.R. omega^2 is then 0 by
##                          construction, but eta_H^2 need not be: scaling a
##                          skewed distribution by different SDs moves its
##                          median, so the groups still differ on the rank
##                          scale even though their means agree. That is what
##                          separates a Kruskal-Wallis rejection rate above
##                          alpha caused by a false rank null from one caused
##                          by the chi-square approximation failing.
## ---------------------------------------------------------------------------

SIMDIR <- local({
  here <- getwd()
  if (file.exists(file.path(here, "fleishman_route1_residual_helpers.R"))) here
  else {
    installed <- system.file("simulations", package = "visStatistics")
    if (!nzchar(installed)) stop("Run from inst/simulations/.")
    installed
  }
})
source(file.path(SIMDIR, "fleishman_route1_residual_helpers.R"))
source(file.path(SIMDIR, "omega_scaling_helpers.R"))
source(file.path(SIMDIR, "eta_h_population.R"))

args <- commandArgs(trailingOnly = TRUE)
SCALING <- if (length(args) >= 1) args[1] else "legacy"
REPS <- if (length(args) >= 2) as.integer(args[2]) else 20L
BASE <- if (length(args) >= 3) as.integer(args[3]) else 10000L
stopifnot(SCALING %in% c("legacy", "omega_fixed", "typeI"))

PANELS <- 1:5
BASE_SHIFTS <- c(0, 0.25, 0.50, 0.75)
POWER_DESIGNS <- list(
  list(design = "balanced n, equal SD",   multipliers = c(1, 1, 1, 1),         sd = c(1, 1, 1, 1)),
  list(design = "unbalanced n, equal SD", multipliers = c(0.5, 0.8, 1.2, 1.5), sd = c(1, 1, 1, 1)),
  list(design = "balanced n, unequal SD", multipliers = c(1, 1, 1, 1),         sd = c(1, 1.3, 1.7, 2.2)),
  list(design = "unbalanced n, larger n with larger SD",
       multipliers = c(0.5, 0.8, 1.2, 1.5), sd = c(1, 1.3, 1.7, 2.2)),
  list(design = "unbalanced n, larger n with smaller SD",
       multipliers = c(0.5, 0.8, 1.2, 1.5), sd = c(2.2, 1.7, 1.3, 1))
)

## "typeI" is the equal-means grid: the scale is 0, so every shift is 0 and
## omega^2 comes out 0 from the same closed form used for the power grids.
scale_fun <- switch(SCALING,
  legacy      = scale_legacy,
  omega_fixed = scale_omega_fixed,
  typeI       = function(multipliers, sd_vec, shifts) 0
)
set.seed(20260814)

eta_h_sq_mc <- function(panel, multipliers, sd_vec, shifts) {
  n_vec <- as.integer(round(BASE * multipliers))
  k <- length(n_vec); N <- sum(n_vec)
  g <- factor(rep(seq_len(k), times = n_vec))
  v <- vapply(seq_len(REPS), function(r) {
    y <- unlist(lapply(seq_len(k), function(j) {
      sd_vec[j] * draw_fleishman_panel(n_vec[j], panel) + shifts[j]
    }))
    H <- unname(stats::kruskal.test(y ~ g)$statistic)
    (H - k + 1) / (N - k)
  }, numeric(1))
  c(eta_h_sq = mean(v), eta_h_sq_mc_se = sd(v) / sqrt(REPS))
}

rows <- list(); idx <- 1L
for (pd in POWER_DESIGNS) {
  cscale <- scale_fun(pd$multipliers, pd$sd, BASE_SHIFTS)
  shifts <- BASE_SHIFTS * cscale
  omega_sq <- population_omega_sq(pd$multipliers, pd$sd, BASE_SHIFTS, cscale)
  for (panel in PANELS) {
    one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
    eta_exact <- population_eta_h_sq(pd$multipliers, pd$sd, shifts, panel)
    e <- eta_h_sq_mc(panel, pd$multipliers, pd$sd, shifts)
    rows[[idx]] <- data.frame(
      scaling = SCALING,
      design = pd$design,
      panel = panel,
      skew = one$skew,
      excess_kurtosis = one$excess_kurtosis,
      shift_scale = cscale,
      group_mean_offsets = paste(format(round(shifts, 4), nsmall = 2), collapse = ", "),
      sd_per_group = paste(format(pd$sd, nsmall = 1), collapse = ", "),
      omega_sq = omega_sq,
      eta_h_sq = eta_exact,
      eta_h_sq_mc = unname(e[["eta_h_sq"]]),
      eta_h_sq_mc_se = unname(e[["eta_h_sq_mc_se"]]),
      row.names = NULL
    )
    idx <- idx + 1L
    cat(sprintf("%-38s panel=%d  omega^2=%.5f  eta_H^2=%.5f  [MC %.5f +- %.5f]\n",
                pd$design, panel, omega_sq, eta_exact,
                e[["eta_h_sq"]], e[["eta_h_sq_mc_se"]]))
  }
}

out <- do.call(rbind, rows)
outfile <- sprintf("effect_sizes_by_design_panel_%s.csv", SCALING)
write.csv(out, outfile, row.names = FALSE)
message("Wrote ", outfile)
