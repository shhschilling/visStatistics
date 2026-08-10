## ---------------------------------------------------------------------------
## Pseudo-rank arm for the design variants of route1_power_design_variants.R.
##
## Computes, on the SAME design grid and the same DESIGN_SET flag:
##   ps_kw    rankFD(effect = "unweighted", hypothesis = "H0F"), pseudo-rank
##            Kruskal-Wallis, chi-square reference          -> "RK"
##   ps_ats   rankFD(effect = "unweighted", hypothesis = "H0F"), ANOVA-type
##            statistic, F reference                        -> "ATS"
##   ats_h0p  rankFD(effect = "unweighted", hypothesis = "H0p"), ANOVA-type
##            statistic under the nonparametric Behrens-Fisher null
##                                                          -> "ATSp"
##
## All three in one pass, because each cell's data generation dominates the cost
## and the two rankFD fits per replicate share it. The existing scripts
## rankfd_route1_power.R and rankfd_route1_power_h0p.R are NOT touched and keep
## producing the CSVs the current figures use.
##
## Usage:
##   Rscript rankfd_route1_power_design_variants.R <NREP> <NCORES> <DESIGN_SET>
##   e.g. Rscript rankfd_route1_power_design_variants.R 50000 8 brunner
##
## Output: rankfd_route1_power_design_<DESIGN_SET>_B<NREP>.csv
##         joined to the parametric grid on (design, n_per_group, panel).
##
## Requires rankFD, which visStatistics does not depend on.
##
## The RNG seed differs from route1_power_design_variants.R so the two arms draw
## independent streams, matching how the existing rankfd_* scripts relate to
## route1_simulations.R. Cells are checkpointed and skipped on restart, with the
## stream advanced identically for skipped cells.
## ---------------------------------------------------------------------------

if (!requireNamespace("rankFD", quietly = TRUE)) {
  stop("Package 'rankFD' is required.")
}

SIMDIR <- local({
  here <- getwd()
  if (file.exists(file.path(here, "fleishman_route1_residual_helpers.R"))) {
    here
  } else {
    installed <- system.file("simulations", package = "visStatistics")
    if (!nzchar(installed)) stop("Run from inst/simulations/.")
    installed
  }
})
source(file.path(SIMDIR, "fleishman_route1_residual_helpers.R"))
source(file.path(SIMDIR, "omega_scaling_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)
NREP <- if (length(args) >= 1) as.integer(args[1]) else 50000L
NCORES <- if (length(args) >= 2) as.integer(args[2]) else 8L
DESIGN_SET <- if (length(args) >= 3) args[3] else "brunner"
ALPHA <- 0.05
if (!DESIGN_SET %in% c("legacy", "brunner")) {
  stop("DESIGN_SET must be \"legacy\" or \"brunner\", not \"", DESIGN_SET, "\"")
}

## Same grid as route1_power_design_variants.R. Kept literal rather than sourced
## so that this arm cannot silently change if that script is edited; a stopifnot
## below checks the two agree on what was simulated.
PANELS <- 1:5
POWER_NS <- c(10, 20, 30, 50, 100, 200)
BASE_SHIFTS <- c(0, 0.25, 0.50, 0.75)
NMULT_BAL <- c(1, 1, 1, 1)
NMULT_UNBAL <- c(0.5, 0.8, 1.2, 1.5)

SD_SETS <- list(
  legacy  = list(eq = c(1, 1, 1, 1), pos = c(1, 1.3, 1.7, 2.2), neg = c(2.2, 1.7, 1.3, 1)),
  brunner = list(eq = c(1, 1, 1, 1), pos = c(1, sqrt(2), 2, sqrt(5)),
                 neg = c(sqrt(5), 2, sqrt(2), 1))
)
SDS <- SD_SETS[[DESIGN_SET]]

POWER_DESIGNS <- list(
  list(design = "balanced n, equal SD",                   multipliers = NMULT_BAL,   sd = SDS$eq),
  list(design = "unbalanced n, equal SD",                 multipliers = NMULT_UNBAL, sd = SDS$eq),
  list(design = "balanced n, unequal SD",                 multipliers = NMULT_BAL,   sd = SDS$pos),
  list(design = "unbalanced n, larger n with larger SD",  multipliers = NMULT_UNBAL, sd = SDS$pos),
  list(design = "unbalanced n, larger n with smaller SD", multipliers = NMULT_UNBAL, sd = SDS$neg)
)

shift_scale_for <- function(sd_vec) {
  if (DESIGN_SET == "legacy") sqrt(mean(sd_vec^2)) else 1
}

## As in route1_power_design_variants.R: the two homoscedastic designs have
## inputs identical to the existing grid (SD = (1,1,1,1) gives scale 1 under
## both variants), and their RK/ATS values are already in
## rankfd_route1_power_B50000.csv and rankfd_route1_power_h0p_B50000.csv.
## Only the three heteroscedastic designs are simulated: 90 cells, not 150.
## Pass "FALSE" as the fourth argument to simulate all five anyway.
SKIP_UNCHANGED <- if (length(args) >= 4) !identical(toupper(args[4]), "FALSE") else TRUE

unchanged_from_existing <- function(pd) {
  legacy_eq <- SD_SETS[["legacy"]]$eq
  isTRUE(all.equal(pd$sd, legacy_eq)) &&
    isTRUE(all.equal(shift_scale_for(pd$sd), sqrt(mean(legacy_eq^2))))
}

RNGkind("L'Ecuyer-CMRG")
set.seed(if (DESIGN_SET == "brunner") 20260920L else 20260921L)
.rng_stream <- .Random.seed
cell_seeds <- function(n) {
  seeds <- vector("list", n)
  for (i in seq_len(n)) {
    .rng_stream <<- parallel::nextRNGStream(.rng_stream)
    seeds[[i]] <- .rng_stream
  }
  seeds
}

make_shift_data <- function(panel, n_vec, shifts, sd_vec) {
  k <- length(shifts)
  g <- factor(rep(seq_len(k), times = n_vec))
  y <- unlist(lapply(seq_len(k), function(i) {
    sd_vec[i] * draw_fleishman_panel(n_vec[i], panel) + shifts[i]
  }))
  list(y = y, g = g)
}

run_cell <- function(panel, n_vec, shifts, sd_vec) {
  seeds <- cell_seeds(NREP)
  out <- parallel::mclapply(seq_len(NREP), function(i) {
    assign(".Random.seed", seeds[[i]], envir = globalenv())
    dat <- make_shift_data(panel, n_vec, shifts, sd_vec)
    d <- data.frame(y = dat$y, g = dat$g)
    f_f <- rankFD::rankFD(y ~ g, data = d, effect = "unweighted", hypothesis = "H0F")
    f_p <- rankFD::rankFD(y ~ g, data = d, effect = "unweighted", hypothesis = "H0p")
    c(
      ps_kw   = f_f$Kruskal.Wallis.Test[1, "p-Value"] < ALPHA,
      ps_ats  = f_f$ANOVA.Type.Statistic[1, "p-Value"] < ALPHA,
      ats_h0p = f_p$ANOVA.Type.Statistic[1, "p-Value"] < ALPHA
    )
  }, mc.cores = NCORES)
  colMeans(do.call(rbind, out))
}

OUTFILE <- sprintf("rankfd_route1_power_design_%s_B%d.csv", DESIGN_SET, NREP)
done_keys <- character(0)
if (file.exists(OUTFILE)) {
  ex <- read.csv(OUTFILE, stringsAsFactors = FALSE)
  done_keys <- paste(ex$design, ex$n_per_group, ex$panel)
  message(sprintf("Resuming: %d cells already in %s", length(done_keys), OUTFILE))
}

message(sprintf("DESIGN_SET = %s | B = %d | %d cells (RK, ATS, ATSp)", DESIGN_SET, NREP,
                length(POWER_DESIGNS) * length(POWER_NS) * length(PANELS)))

for (pd in POWER_DESIGNS) {
  cscale <- shift_scale_for(pd$sd)
  shifts <- BASE_SHIFTS * cscale
  omega_sq <- population_omega_sq(pd$multipliers, pd$sd, shifts, 1)
  regime <- omega_sq_regime(pd$multipliers, pd$sd)
  if (SKIP_UNCHANGED && unchanged_from_existing(pd)) {
    message(sprintf("skipping %-38s : inputs identical to the existing rankfd grids",
                    pd$design))
    for (n in POWER_NS) for (panel in PANELS) cell_seeds(NREP)
    next
  }
  for (n in POWER_NS) {
    n_vec <- as.integer(round(n * pd$multipliers))
    stopifnot(mean(n_vec) == n)
    for (panel in PANELS) {
      key <- paste(pd$design, n, panel)
      if (key %in% done_keys) { cell_seeds(NREP); next }
      one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
      res <- run_cell(panel, n_vec, shifts, pd$sd)
      row <- data.frame(
        design_set = DESIGN_SET,
        design = pd$design,
        omega_sq_regime = regime,
        omega_sq = omega_sq,
        shift_scale = cscale,
        group_mean_offsets = paste(format(round(shifts, 4), nsmall = 2), collapse = ", "),
        sd_per_group = paste(format(round(pd$sd, 4), nsmall = 1), collapse = ", "),
        n_vector = paste(n_vec, collapse = ", "),
        n_per_group = n,
        distribution = one$distribution,
        panel = panel,
        skew = one$skew,
        excess_kurtosis = one$excess_kurtosis,
        ps_kw_power = unname(res[["ps_kw"]]),
        ps_kw_mc_se = sqrt(res[["ps_kw"]] * (1 - res[["ps_kw"]]) / NREP),
        ps_ats_power = unname(res[["ps_ats"]]),
        ps_ats_mc_se = sqrt(res[["ps_ats"]] * (1 - res[["ps_ats"]]) / NREP),
        ats_h0p_power = unname(res[["ats_h0p"]]),
        ats_h0p_mc_se = sqrt(res[["ats_h0p"]] * (1 - res[["ats_h0p"]]) / NREP),
        row.names = NULL
      )
      write.table(row, OUTFILE, sep = ",", row.names = FALSE,
                  col.names = !file.exists(OUTFILE), append = file.exists(OUTFILE))
      cat(sprintf("done: %-38s | n=%3d | panel=%d | w2=%.4f | RK=%.4f ATS=%.4f ATSp=%.4f\n",
                  pd$design, n, panel, omega_sq,
                  res[["ps_kw"]], res[["ps_ats"]], res[["ats_h0p"]]))
      utils::flush.console()
    }
  }
}
message("Results saved to ", OUTFILE)
