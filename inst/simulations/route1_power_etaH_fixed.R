## ---------------------------------------------------------------------------
## The mirror image of route1_power_omega_fixed.R: instead of holding the
## parametric effect size omega^2 constant across designs and input
## distributions, hold the rank-based effect size eta_H^2 constant.
##
## Motivation. omega^2 is a variance ratio of the observations, so scaling the
## shift vector to fix it says nothing about how separated the groups are on
## the rank scale, and eta_H^2 then varies from panel to panel (it depends on
## the shape of the input distribution, not only on the mean shifts). Fixing
## eta_H^2 instead puts the rank-based tests in front of the same task in every
## cell, and lets omega^2 vary. Comparing the two runs shows which of the two
## effect sizes the reported power differences were actually tracking.
##
## Target. eta_H^2 of the balanced homoscedastic design with the unscaled base
## shifts on the normal panel (panel 1), i.e. the cell that is identical in
## both runs and in the original fleishman_4groups_power.csv.
##
## Scale search. eta_H^2 has no closed form here, so for each (design, panel)
## the scale c is found by uniroot() on a large pre-drawn sample: the
## standardised Fleishman deviates are drawn once with a fixed seed and reused
## for every candidate c, so the objective is a deterministic, monotone
## function of c and the root is reproducible.
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
library(visStatistics)

args <- commandArgs(trailingOnly = TRUE)
NREP <- if (length(args) >= 1) as.integer(args[1]) else 50000L
NCORES <- if (length(args) >= 2) as.integer(args[2]) else 8L
ALPHA <- 0.05

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
stopifnot(is.function(route_once), is.function(summarise_binary))

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
SCENARIO <- "moderate ordered effect: 0, 0.25, 0.50, 0.75 SD (eta_H^2 fixed)"

## ---- population eta_H^2 on fixed, pre-drawn large samples ------------------
## ETA_REPS independent samples of ETA_BASE * sum(multipliers) observations are
## drawn once per (panel, design) and reused for every candidate c, so the
## objective is a deterministic, monotone function of c and uniroot() is
## reproducible. Averaging over ETA_REPS samples reduces the Monte Carlo error
## of the target; the eta_H^2 actually achieved is written to the CSV for every
## cell, so how constant it really is can be checked rather than assumed.
ETA_BASE <- 10000L   # per multiplier unit; multipliers sum to 4, so N = 40000
ETA_REPS <- 8L
ETA_SEED <- 20260812L

make_eta_objective <- function(panel, multipliers, sd_vec) {
  n_vec <- as.integer(round(ETA_BASE * multipliers))
  k <- length(n_vec)
  N <- sum(n_vec)
  g <- factor(rep(seq_len(k), times = n_vec))
  set.seed(ETA_SEED + panel)
  reps <- lapply(seq_len(ETA_REPS), function(r) {
    lapply(seq_len(k), function(j) draw_fleishman_panel(n_vec[j], panel))
  })
  function(c) {
    mean(vapply(reps, function(z) {
      y <- unlist(lapply(seq_len(k), function(j) sd_vec[j] * z[[j]] + c * BASE_SHIFTS[j]))
      H <- unname(stats::kruskal.test(y ~ g)$statistic)
      (H - k + 1) / (N - k)
    }, numeric(1)))
  }
}

## Target: balanced homoscedastic, normal panel, unscaled shifts -- the one
## cell that is identical here, in route1_power_omega_fixed.R and in the
## original fleishman_4groups_power.csv.
ETA_TARGET <- make_eta_objective(1L, c(1, 1, 1, 1), c(1, 1, 1, 1))(1)
message(sprintf("eta_H^2 target (balanced homoscedastic, panel 1, c=1) = %.6f", ETA_TARGET))

## ---- scale table, computed once -------------------------------------------
scale_tab <- list()
for (pd in POWER_DESIGNS) {
  for (panel in PANELS) {
    f <- make_eta_objective(panel, pd$multipliers, pd$sd)
    cc <- uniroot(function(c) f(c) - ETA_TARGET, interval = c(0.05, 20),
                  tol = 1e-3, extendInt = "upX")$root
    scale_tab[[paste(pd$design, panel)]] <- list(
      c = cc,
      eta_h_sq = f(cc),
      omega_sq = population_omega_sq(pd$multipliers, pd$sd, BASE_SHIFTS, cc)
    )
    message(sprintf("scale: %-38s panel=%d  c=%.4f  eta_H^2=%.5f  omega^2=%.5f",
                    pd$design, panel, cc,
                    scale_tab[[paste(pd$design, panel)]]$eta_h_sq,
                    scale_tab[[paste(pd$design, panel)]]$omega_sq))
  }
}

## ---- power grid ------------------------------------------------------------
RNGkind("L'Ecuyer-CMRG")
set.seed(20260813)
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

OUTFILE <- sprintf("fleishman_4groups_power_etaH_fixed_B%d.csv", NREP)
done_keys <- character(0)
if (file.exists(OUTFILE)) {
  ex <- read.csv(OUTFILE, stringsAsFactors = FALSE)
  done_keys <- paste(ex$design, ex$n_per_group, ex$panel)
  message(sprintf("Resuming: %d cells already in %s", length(done_keys), OUTFILE))
}

for (pd in POWER_DESIGNS) {
  for (n in POWER_NS) {
    n_vec <- as.integer(round(n * pd$multipliers))
    for (panel in PANELS) {
      key <- paste(pd$design, n, panel)
      if (key %in% done_keys) { cell_seeds(NREP); next }
      st <- scale_tab[[paste(pd$design, panel)]]
      shifts <- BASE_SHIFTS * st$c
      one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
      res <- run_power_cell(panel, n_vec, shifts, pd$sd)
      row <- data.frame(
        design = pd$design,
        effect_size = SCENARIO,
        shift_scale = st$c,
        eta_h_sq = st$eta_h_sq,
        omega_sq = st$omega_sq,
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
      cat(sprintf("done: %-38s | n=%3d | panel=%d | c=%.3f etaH=%.4f w2=%.4f | F=%.4f W=%.4f KW=%.4f SW+L=%.4f\n",
                  pd$design, n, panel, st$c, st$eta_h_sq, st$omega_sq,
                  res[["fisher.rate"]], res[["welch.rate"]],
                  res[["rank.rate"]], res[["gate.rate"]]))
    }
  }
}
message("Results saved to ", OUTFILE)
