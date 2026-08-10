## ---------------------------------------------------------------------------
## Route 1 power grid under a SELECTABLE set of design inputs.
##
## route1_simulations.R is NOT touched by this script and remains the source of
## fleishman_4groups_power.rds, so every figure currently in the repository stays
## reproducible. This script writes its own CSVs under its own names.
##
## Usage:
##   Rscript route1_power_design_variants.R <NREP> <NCORES> <DESIGN_SET>
##   e.g. Rscript route1_power_design_variants.R 50000 8 brunner
##
## DESIGN_SET selects the inputs:
##
##   "legacy"   reproduces what route1_simulations.R does, for reference:
##              SD = (1, 1.3, 1.7, 2.2) and its reverse, and the common shift
##              vector rescaled per design by sqrt(mean(SD^2)).
##
##   "brunner"  SD taken verbatim from Brunner, Konietschke, Pauly and Puri
##              (2017), JRSS-B 79(5), Table 2, p. 1477: the scaling vectors are
##              (1, 1, 1, 1), (1, sqrt(2), 2, sqrt(5)) and (sqrt(5), 2, sqrt(2), 1).
##              Written as roots because the variances behind them are the
##              integers sigma^2 = (1, 2, 4, 5); the paper states the vector but
##              not that rationale, so the reading is ours.
##              The shift vector is IDENTICAL in every design and omega^2 is
##              reported as the consequence of the design, not held fixed.
##
## Why the rescaling is dropped in "brunner"
## -----------------------------------------
## route1_simulations.R:40-42 states that scaling the shifts by sqrt(mean(SD^2))
## makes "omega^2 match the homoscedastic blocks". It does not: with
## SD = (1, 1.3, 1.7, 2.2) the factor is 1.614 and the balanced heteroscedastic
## design reaches omega^2 = 0.0803 against the homoscedastic 0.0725. The average
## of the variances is not the quantity that enters omega^2, which uses the
## inverse-variance-weighted grand mean (see omega_scaling_helpers.R and
## Eq. (omega-sq-population-heteroscedastic) of _effect_size_table.Rmd). Holding
## the shifts identical and reporting omega^2 is the comparison that isolates the
## design, and it is the same principle already used for the balance comparison.
##
## Sample-size scaling stays as the package does it: fixed allocation ratios
## scaled by n_bar. Brunner instead ADDS a constant m to every group, so his
## ratios move with sample size; that scheme is deliberately not adopted here.
##
## Shift vector: Brunner's power alternatives (p. 1480) are generated from
## X_ik ~ N(mu_i, 1) with a one-point alternative mu = (0,0,0,delta) or an
## increasing trend mu = delta*(1/4, 1/2, 3/4, 1). At delta = 1 the trend equals
## the package's (0, 0.25, 0.50, 0.75) plus the constant 0.25 in every group,
## which is a location shift of the whole experiment: no test sees it and
## omega^2 is unchanged (verified to six decimals). The two are therefore the
## same alternative and the package's vector is kept.
##
## Output (never overwrites an existing grid):
##   fleishman_4groups_power_design_<DESIGN_SET>_B<NREP>.csv
##
## Checkpointing: every completed cell is appended immediately and skipped on
## restart, with the RNG stream advanced identically for skipped cells, so an
## interrupted run resumes without changing any result.
## ---------------------------------------------------------------------------

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

## route_once() calls levene.test() from the package.
library(visStatistics)

args <- commandArgs(trailingOnly = TRUE)
NREP <- if (length(args) >= 1) as.integer(args[1]) else 50000L
NCORES <- if (length(args) >= 2) as.integer(args[2]) else 8L
DESIGN_SET <- if (length(args) >= 3) args[3] else "brunner"
ALPHA <- 0.05
if (!DESIGN_SET %in% c("legacy", "brunner")) {
  stop("DESIGN_SET must be \"legacy\" or \"brunner\", not \"", DESIGN_SET, "\"")
}

## Pull the routing machinery out of route1_simulations.R without running it:
## evaluate only its top-level `name <- ...` assignments, skipping the loops,
## write.csv() and saveRDS() calls. Assignments that depend on results the loops
## would have produced fail harmlessly and are skipped.
local({
  exprs <- parse(file.path(SIMDIR, "route1_simulations.R"))
  for (e in exprs) {
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]])) {
      nm <- as.character(e[[2]])
      if (nm %in% c("NREP", "NCORES", "ALPHA", "OUTDIR", "POWER_DESIGNS")) next
      try(eval(e, envir = globalenv()), silent = TRUE)
    }
  }
})
stopifnot(is.function(route_once), is.function(summarise_binary))

PANELS <- 1:5
POWER_NS <- c(10, 20, 30, 50, 100, 200)
BASE_SHIFTS <- c(0, 0.25, 0.50, 0.75)
SCENARIO <- "moderate ordered effect: 0, 0.25, 0.50, 0.75 SD"

NMULT_BAL <- c(1, 1, 1, 1)
NMULT_UNBAL <- c(0.5, 0.8, 1.2, 1.5)

## The only thing DESIGN_SET changes: the SD vectors, and whether the shifts are
## rescaled per design. Design NAMES are identical in both variants so the two
## grids can be joined and compared cell by cell.
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

## "legacy" keeps route1_simulations.R:303 verbatim; "brunner" uses the same
## shifts everywhere.
shift_scale_for <- function(sd_vec) {
  if (DESIGN_SET == "legacy") sqrt(mean(sd_vec^2)) else 1
}

## The two homoscedastic designs are NOT rerun by default: with SD = (1,1,1,1)
## the legacy scale factor is sqrt(mean(1)) = 1, so their inputs -- SD vector,
## shifts and multipliers -- are already exactly those of the grid in
## fleishman_4groups_power.rds. Only the three heteroscedastic designs differ,
## which is 90 cells instead of 150. Pass "FALSE" as the fourth argument to
## simulate all five anyway.
##
## Skipped cells still advance the RNG stream, exactly as the checkpoint skip
## does, so the cells that ARE simulated reproduce bit-identically whether or
## not the skip is enabled.
SKIP_UNCHANGED <- if (length(args) >= 4) !identical(toupper(args[4]), "FALSE") else TRUE

unchanged_from_existing <- function(pd) {
  legacy_sd <- SD_SETS[["legacy"]]
  matches_eq <- isTRUE(all.equal(pd$sd, legacy_sd$eq))
  matches_eq && isTRUE(all.equal(shift_scale_for(pd$sd), sqrt(mean(legacy_sd$eq^2))))
}

## Own seed, so these streams cannot collide with any existing grid.
RNGkind("L'Ecuyer-CMRG")
set.seed(if (DESIGN_SET == "brunner") 20260910L else 20260911L)
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

run_power_cell <- function(panel, n_vec, shifts, sd_vec) {
  seeds <- cell_seeds(NREP)
  out <- parallel::mclapply(seq_len(NREP), function(i) {
    assign(".Random.seed", seeds[[i]], envir = globalenv())
    dat <- make_shift_data(panel, n_vec, shifts, sd_vec)
    route_once(dat$y, dat$g, alpha = ALPHA)
  }, mc.cores = NCORES)
  c(
    fisher = summarise_binary(vapply(out, `[[`, numeric(1), "fisher_reject") > 0.5),
    welch  = summarise_binary(vapply(out, `[[`, numeric(1), "welch_reject") > 0.5),
    mean   = summarise_binary(vapply(out, `[[`, numeric(1), "levene_route_reject") > 0.5),
    rank   = summarise_binary(vapply(out, `[[`, numeric(1), "rank_reject") > 0.5),
    sw     = summarise_binary(vapply(out, `[[`, numeric(1), "sw_reject_final") > 0.5),
    gate   = summarise_binary(vapply(out, `[[`, numeric(1), "sw_gate_reject") > 0.5),
    route_rank   = summarise_binary(vapply(out, `[[`, numeric(1), "route_rank") > 0.5),
    route_fisher = summarise_binary(vapply(out, `[[`, numeric(1), "route_fisher") > 0.5),
    route_welch  = summarise_binary(vapply(out, `[[`, numeric(1), "route_welch") > 0.5)
  )
}

OUTFILE <- sprintf("fleishman_4groups_power_design_%s_B%d.csv", DESIGN_SET, NREP)
done_keys <- character(0)
if (file.exists(OUTFILE)) {
  ex <- read.csv(OUTFILE, stringsAsFactors = FALSE)
  done_keys <- paste(ex$design, ex$n_per_group, ex$panel)
  message(sprintf("Resuming: %d cells already in %s", length(done_keys), OUTFILE))
}

message(sprintf("DESIGN_SET = %s | B = %d | %d cells", DESIGN_SET, NREP,
                length(POWER_DESIGNS) * length(POWER_NS) * length(PANELS)))

for (pd in POWER_DESIGNS) {
  cscale <- shift_scale_for(pd$sd)
  shifts <- BASE_SHIFTS * cscale
  ## Reported, never targeted: the population omega^2 this design actually has,
  ## from the same function used everywhere else in the package.
  omega_sq <- population_omega_sq(pd$multipliers, pd$sd, shifts, 1)
  regime <- omega_sq_regime(pd$multipliers, pd$sd)
  if (SKIP_UNCHANGED && unchanged_from_existing(pd)) {
    message(sprintf("skipping %-38s : inputs identical to fleishman_4groups_power.rds",
                    pd$design))
    for (n in POWER_NS) for (panel in PANELS) cell_seeds(NREP)
    next
  }
  for (n in POWER_NS) {
    n_vec <- as.integer(round(n * pd$multipliers))
    for (panel in PANELS) {
      key <- paste(pd$design, n, panel)
      if (key %in% done_keys) { cell_seeds(NREP); next }
      one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
      res <- run_power_cell(panel, n_vec, shifts, pd$sd)
      row <- data.frame(
        design_set = DESIGN_SET,
        design = pd$design,
        omega_sq_regime = regime,
        effect_size = SCENARIO,
        shift_scale = cscale,
        omega_sq = omega_sq,
        group_mean_offsets = paste(format(round(shifts, 4), nsmall = 2), collapse = ", "),
        sd_per_group = paste(format(round(pd$sd, 4), nsmall = 1), collapse = ", "),
        n_vector = paste(n_vec, collapse = ", "),
        n_per_group = n,
        distribution = one$distribution,
        panel = panel,
        skew = one$skew,
        excess_kurtosis = one$excess_kurtosis,
        fisher_power = unname(res[["fisher.rate"]]),
        welch_power  = unname(res[["welch.rate"]]),
        mean_power   = unname(res[["mean.rate"]]),
        rank_power   = unname(res[["rank.rate"]]),
        sw_power     = unname(res[["sw.rate"]]),
        gate_power   = unname(res[["gate.rate"]]),
        fisher_mc_se = unname(res[["fisher.mc_se"]]),
        welch_mc_se  = unname(res[["welch.mc_se"]]),
        mean_mc_se   = unname(res[["mean.mc_se"]]),
        rank_mc_se   = unname(res[["rank.mc_se"]]),
        sw_mc_se     = unname(res[["sw.mc_se"]]),
        gate_mc_se   = unname(res[["gate.mc_se"]]),
        route_fisher_probability = unname(res[["route_fisher.rate"]]),
        route_welch_probability  = unname(res[["route_welch.rate"]]),
        route_rank_probability   = unname(res[["route_rank.rate"]]),
        row.names = NULL
      )
      write.table(row, OUTFILE, sep = ",", row.names = FALSE,
                  col.names = !file.exists(OUTFILE), append = file.exists(OUTFILE))
      cat(sprintf("done: %-38s | n=%3d | panel=%d | w2=%.4f (%s) | F=%.4f W=%.4f KW=%.4f SW+L=%.4f\n",
                  pd$design, n, panel, omega_sq, regime,
                  res[["fisher.rate"]], res[["welch.rate"]],
                  res[["rank.rate"]], res[["gate.rate"]]))
      utils::flush.console()
    }
  }
}
message("Results saved to ", OUTFILE)
