## ---------------------------------------------------------------------------
## Route 1 power grid rerun with the shift scale that actually holds the
## population omega^2 constant across all five designs.
##
## route1_simulations.R:303 uses shift_scale = sqrt(mean(sd_vec^2)). That holds
## omega^2 at the balanced homoscedastic baseline only when the group sizes are
## equal: both the effect variance and the error variance of the population
## omega^2 are weighted by the allocation fractions p_j = n_j/N, and the
## unweighted mean ignores those weights. In the three unbalanced designs of
## POWER_DESIGNS the achieved omega^2 is therefore not the baseline value
## (0.0725) but 0.0627, 0.0525 and 0.0778 respectively.
##
## This script reruns the same grid, same designs, same tests, changing only
## the scale factor (see omega_scaling_helpers.R). Comparing its output with
## fleishman_4groups_power.csv isolates how much of the reported power
## difference between balanced and unbalanced designs was an effect-size
## difference rather than a design effect.
##
## Nothing already computed is overwritten: output goes to its own file, and
## the RNG seed is this script's own.
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
ALPHA <- 0.05

## Pull the routing machinery out of route1_simulations.R without running it:
## evaluate only its top-level `name <- ...` assignments, skipping the loops,
## write.csv() and saveRDS() calls. Assignments that depend on results the
## loops would have produced fail harmlessly and are skipped.
local({
  exprs <- parse(file.path(SIMDIR, "route1_simulations.R"))
  for (e in exprs) {
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]])) {
      nm <- as.character(e[[2]])
      if (nm %in% c("NREP", "NCORES", "ALPHA", "OUTDIR")) next
      try(eval(e, envir = globalenv()), silent = TRUE)
    }
  }
})
stopifnot(is.function(route_once), is.function(summarise_binary),
          is.function(standardised_residuals))

PANELS <- 1:5
POWER_NS <- c(10, 20, 30, 50, 100, 200)
POWER_DESIGNS <- list(
  list(design = "balanced n, equal SD",   multipliers = c(1, 1, 1, 1),         sd = c(1, 1, 1, 1)),
  list(design = "unbalanced n, equal SD", multipliers = c(0.5, 0.8, 1.2, 1.5), sd = c(1, 1, 1, 1)),
  list(design = "balanced n, unequal SD", multipliers = c(1, 1, 1, 1),         sd = c(1, 1.3, 1.7, 2.2)),
  list(design = "unbalanced n, larger n with larger SD",
       multipliers = c(0.5, 0.8, 1.2, 1.5), sd = c(1, 1.3, 1.7, 2.2)),
  list(design = "unbalanced n, larger n with smaller SD",
       multipliers = c(0.5, 0.8, 1.2, 1.5), sd = c(2.2, 1.7, 1.3, 1))
)
BASE_SHIFTS <- c(0, 0.25, 0.50, 0.75)
SCENARIO <- "moderate ordered effect: 0, 0.25, 0.50, 0.75 SD"

RNGkind("L'Ecuyer-CMRG")
set.seed(20260811)
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

OUTFILE <- sprintf("fleishman_4groups_power_omega_fixed_B%d.csv", NREP)
done_keys <- character(0)
if (file.exists(OUTFILE)) {
  ex <- read.csv(OUTFILE, stringsAsFactors = FALSE)
  done_keys <- paste(ex$design, ex$n_per_group, ex$panel)
  message(sprintf("Resuming: %d cells already in %s", length(done_keys), OUTFILE))
}

for (pd in POWER_DESIGNS) {
  cscale <- scale_omega_fixed(pd$multipliers, pd$sd, BASE_SHIFTS)
  shifts <- BASE_SHIFTS * cscale
  omega_sq <- population_omega_sq(pd$multipliers, pd$sd, BASE_SHIFTS, cscale)
  for (n in POWER_NS) {
    n_vec <- as.integer(round(n * pd$multipliers))
    for (panel in PANELS) {
      key <- paste(pd$design, n, panel)
      if (key %in% done_keys) { cell_seeds(NREP); next }
      one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
      res <- run_power_cell(panel, n_vec, shifts, pd$sd)
      row <- data.frame(
        design = pd$design,
        effect_size = SCENARIO,
        shift_scale = cscale,
        omega_sq = omega_sq,
        group_mean_offsets = paste(format(round(shifts, 4), nsmall = 2), collapse = ", "),
        sd_per_group = paste(format(pd$sd, nsmall = 1), collapse = ", "),
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
      cat(sprintf("done: %-38s | n=%3d | panel=%d | w2=%.4f | F=%.4f W=%.4f KW=%.4f SW+L=%.4f\n",
                  pd$design, n, panel, omega_sq,
                  res[["fisher.rate"]], res[["welch.rate"]],
                  res[["rank.rate"]], res[["gate.rate"]]))
    }
  }
}
message("Results saved to ", OUTFILE)
