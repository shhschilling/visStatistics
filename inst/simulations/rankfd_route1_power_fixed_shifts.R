## ---------------------------------------------------------------------------
## Simplified power simulation: fixed shifts and SDs, varying only balance.
##
## Design: the homoscedastic baseline shifts (0, 0.25, 0.50, 0.75 SD) and SDs
## (all 1) are held fixed; only the sample-size multipliers vary (balanced vs.
## two unbalanced pairings). Unlike fleishman_4groups_power.csv, shifts are
## NOT rescaled to hold omega^2 fixed, so omega^2 differs across designs by
## construction of unequal group sizes -- this isolates the effect of
## imbalance alone. eta_H^2, like omega^2, is not a fixed population quantity
## either (Brunner et al. 2017, JRSSB, p. 1464): both are computed per design
## and reported, not held constant.
##
## Uses its own RNG seed (not the shared route1_simulations.R stream chain),
## since this is a separate simulation with a different cell count.
##
##   ps_kw   rankFD(effect = "unweighted"), pseudo-ranks, chi-square reference
##   ps_ats  rankFD(effect = "unweighted"), ANOVA-type statistic, F reference
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

args <- commandArgs(trailingOnly = TRUE)
NREP <- if (length(args) >= 1) as.integer(args[1]) else 50000L
NCORES <- if (length(args) >= 2) {
  as.integer(args[2])
} else {
  max(1L, min(8L, parallel::detectCores(logical = FALSE) - 1L))
}
ALPHA <- 0.05

## Fixed SDs and shifts (homoscedastic baseline, matching fleishman_4groups_power.csv panel B)
FIXED_SDS <- c(1, 1, 1, 1)
FIXED_SHIFTS <- c(0, 0.25, 0.50, 0.75)

## Balance multiplier variants
BALANCE_DESIGNS <- list(
  list(design = "balanced",           multipliers = c(1, 1, 1, 1)),
  list(design = "unbalanced, ascending",  multipliers = c(0.5, 0.8, 1.2, 1.5)),
  list(design = "unbalanced, descending", multipliers = c(1.5, 1.2, 0.8, 0.5))
)

PANELS <- 1:5
POWER_NS <- c(10, 20, 30, 50, 100, 200)

RNGkind("L'Ecuyer-CMRG")
set.seed(20260810)
.rng_stream <- .Random.seed

cell_seeds <- function(n) {
  seeds <- list()
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

## eta_H^2 for the population implied by (shifts, sd_vec, multipliers): the
## classical H statistic's expectation depends on the relative sizes n_i/N
## (Brunner et al. 2017, p. 1464), which are preserved under uniform scaling,
## so a large, ratio-preserving sample gives a stable Monte Carlo estimate.
population_eta_h_sq <- function(panel, multipliers, shifts, sd_vec, scale = 200) {
  n_vec <- pmax(as.integer(round(multipliers * scale)), 2L)
  dat <- make_shift_data(panel, n_vec, shifts, sd_vec)
  k <- length(n_vec)
  N <- sum(n_vec)
  H_stat <- unname(stats::kruskal.test(dat$y ~ dat$g)$statistic)
  (H_stat - k + 1) / (N - k)
}

run_cell <- function(panel, n_vec, shifts, sd_vec) {
  seeds <- cell_seeds(NREP)
  out <- parallel::mclapply(seq_len(NREP), function(i) {
    assign(".Random.seed", seeds[[i]], envir = globalenv())
    dat <- make_shift_data(panel, n_vec, shifts, sd_vec)
    fit <- rankFD::rankFD(y ~ g, data = data.frame(y = dat$y, g = dat$g),
                          effect = "unweighted", hypothesis = "H0F")
    c(
      ps_kw = fit$Kruskal.Wallis.Test[1, "p-Value"] < ALPHA,
      ps_ats = fit$ANOVA.Type.Statistic[1, "p-Value"] < ALPHA
    )
  }, mc.cores = NCORES)
  colMeans(do.call(rbind, out))
}

## Checkpointing: the mclapply worker pool has twice been killed mid-run by
## something outside R (all 8 workers fail with the identical SIGPIPE
## "sendMaster" error at the same instant -- the signature of the parent
## losing its background session, not a per-replicate data bug). Cells
## already written to OUTFILE are skipped on restart; the RNG stream is
## still advanced for skipped cells so later cells draw the same streams
## as an uninterrupted run.
OUTFILE <- "rankfd_route1_power_fixed_shifts_B50000.csv"
cell_key <- function(design, n, panel) paste(design, n, panel)
done_keys <- character(0)
if (file.exists(OUTFILE)) {
  existing <- read.csv(OUTFILE, stringsAsFactors = FALSE)
  done_keys <- cell_key(existing$design, existing$n_per_group, existing$panel)
  message(sprintf("Resuming: %d cells already in %s", length(done_keys), OUTFILE))
}

append_row <- function(row) {
  write.table(row, OUTFILE, sep = ",", row.names = FALSE,
              col.names = !file.exists(OUTFILE), append = file.exists(OUTFILE))
}

for (design in BALANCE_DESIGNS) {
  for (n in POWER_NS) {
    n_vec <- as.integer(round(n * design$multipliers))
    for (panel in PANELS) {
      key <- cell_key(design$design, n, panel)
      if (key %in% done_keys) {
        cell_seeds(NREP)  # keep the RNG stream aligned with an uninterrupted run
        next
      }

      one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]

      group_means <- FIXED_SHIFTS
      grand_mean <- mean(group_means)
      sigma_sq_effect <- mean((group_means - grand_mean)^2)
      sigma_sq_error <- mean(FIXED_SDS^2)
      omega_sq <- sigma_sq_effect / (sigma_sq_effect + sigma_sq_error)

      eta_h_sq <- population_eta_h_sq(panel, design$multipliers, FIXED_SHIFTS, FIXED_SDS)

      res <- run_cell(panel, n_vec, FIXED_SHIFTS, FIXED_SDS)

      row <- data.frame(
        design = design$design,
        n_per_group = n,
        n_vector = paste(n_vec, collapse = ", "),
        multipliers = paste(design$multipliers, collapse = ", "),
        sd_per_group = paste(FIXED_SDS, collapse = ", "),
        shifts = paste(FIXED_SHIFTS, collapse = ", "),
        panel = panel,
        distribution = one$distribution,
        skew = one$skew,
        excess_kurtosis = one$excess_kurtosis,
        omega_sq = omega_sq,
        eta_h_sq = eta_h_sq,
        ps_kw_power = unname(res[["ps_kw"]]),
        ps_kw_mc_se = sqrt(res[["ps_kw"]] * (1 - res[["ps_kw"]]) / NREP),
        ps_ats_power = unname(res[["ps_ats"]]),
        ps_ats_mc_se = sqrt(res[["ps_ats"]] * (1 - res[["ps_ats"]]) / NREP),
        row.names = NULL
      )
      append_row(row)

      cat(sprintf("done: %-24s | n=%3d | panel=%d | omega_sq=%.4f eta_h_sq=%.4f | ps_kw=%.4f ps_ats=%.4f\n",
        design$design, n, panel, omega_sq, eta_h_sq,
        res[["ps_kw"]], res[["ps_ats"]]))
    }
  }
}

message("Results saved to ", OUTFILE)
