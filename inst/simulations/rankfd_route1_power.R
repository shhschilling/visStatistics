## ---------------------------------------------------------------------------
## Pseudo-rank arm for the Route 1 power grid.
##
## Companion to rankfd_route1_typeI.R. Only the two pseudo-rank procedures are
## simulated; nothing already in fleishman_4groups_power.csv is recomputed.
##
##   ps_kw   rankFD(effect = "unweighted"), pseudo-ranks, chi-square reference
##   ps_ats  rankFD(effect = "unweighted"), ANOVA-type statistic, F reference
##
## Stream bookkeeping. route1_simulations.R draws NREP L'Ecuyer streams per
## cell, in this order:
##
##   cells      0 -  149   Type I grid   (6 sizes x 5 designs x 5 panels)
##   cells    150 -  299   power grid    (5 designs x 6 sizes x 5 panels)
##
## rankfd_route1_typeI.R then continues at cell 300 and consumes the next 150.
## This script therefore starts at cell 450, so the four runs form one
## continuous, reproducible stream sequence and no cell is ever drawn twice.
##
## The design list, size vector, shift scaling and draw order are copied
## verbatim from route1_simulations.R; changing any of them breaks the
## correspondence with the saved power results.
##
## Requires rankFD, which visStatistics does not depend on.
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

## ---- copied verbatim from route1_simulations.R -----------------------------
PANELS <- 1:5
POWER_NS <- c(10, 20, 30, 50, 100, 200)

POWER_DESIGNS <- list(
  list(design = "balanced n, equal SD",   multipliers = c(1, 1, 1, 1),        sd = c(1, 1, 1, 1)),
  list(design = "unbalanced n, equal SD", multipliers = c(0.5, 0.8, 1.2, 1.5), sd = c(1, 1, 1, 1)),
  list(design = "balanced n, unequal SD", multipliers = c(1, 1, 1, 1),        sd = c(1, 1.3, 1.7, 2.2)),
  list(design = "unbalanced n, larger n with larger SD",
       multipliers = c(0.5, 0.8, 1.2, 1.5), sd = c(1, 1.3, 1.7, 2.2)),
  list(design = "unbalanced n, larger n with smaller SD",
       multipliers = c(0.5, 0.8, 1.2, 1.5), sd = c(2.2, 1.7, 1.3, 1))
)
SHIFT_SCENARIOS <- list(
  "moderate ordered effect: 0, 0.25, 0.50, 0.75 SD" = c(0, 0.25, 0.50, 0.75)
)

RNGkind("L'Ecuyer-CMRG")
set.seed(20260615)
.rng_stream <- .Random.seed

## 150 Type I cells + 150 power cells of route1_simulations.R, then the 150
## Type I cells of rankfd_route1_typeI.R.
CELLS_ALREADY_RUN <- 450L
for (i in seq_len(CELLS_ALREADY_RUN * NREP)) {
  .rng_stream <- parallel::nextRNGStream(.rng_stream)
}

cell_seeds <- function(n) {
  seeds <- vector("list", n)
  for (i in seq_len(n)) {
    seeds[[i]] <- .rng_stream
    .rng_stream <<- parallel::nextRNGStream(.rng_stream)
  }
  seeds
}

make_shift_data <- function(panel, n_vec, shifts, sd_vec = rep(1, length(shifts))) {
  k <- length(shifts)
  stopifnot(length(n_vec) == k, length(sd_vec) == k)
  g <- factor(rep(seq_len(k), times = n_vec))
  y <- unlist(lapply(seq_len(k), function(i) {
    sd_vec[i] * draw_fleishman_panel(n_vec[i], panel) + shifts[i]
  }))
  list(y = y, g = g)
}
## ---- end of the copied block -----------------------------------------------

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

saved <- read.csv(file.path(SIMDIR, "fleishman_4groups_power.csv"))

rows <- list()
idx <- 1
for (pdesign in POWER_DESIGNS) {
  for (n in POWER_NS) {
    n_vec <- as.integer(round(n * pdesign$multipliers))
    stopifnot(mean(n_vec) == n)
    for (panel in PANELS) {
      for (scenario_name in names(SHIFT_SCENARIOS)) {
        sd_vec <- pdesign$sd
        shift_scale <- sqrt(mean(sd_vec^2))
        shifts <- SHIFT_SCENARIOS[[scenario_name]] * shift_scale
        res <- run_cell(panel, n_vec, shifts, sd_vec)
        one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
        ref <- saved$rank_power[saved$design == pdesign$design &
                                  saved$n_per_group == n &
                                  saved$panel == panel]
        rows[[idx]] <- data.frame(
          design = pdesign$design,
          effect_size = scenario_name,
          group_mean_offsets = paste(format(round(shifts, 3), nsmall = 2), collapse = ", "),
          sd_per_group = paste(format(sd_vec, nsmall = 1), collapse = ", "),
          n_vector = paste(n_vec, collapse = ", "),
          n_per_group = n,
          distribution = one$distribution,
          panel = panel,
          skew = one$skew,
          excess_kurtosis = one$excess_kurtosis,
          rank_power = ref,
          ps_kw_power = unname(res[["ps_kw"]]),
          ps_kw_mc_se = sqrt(res[["ps_kw"]] * (1 - res[["ps_kw"]]) / NREP),
          ps_ats_power = unname(res[["ps_ats"]]),
          ps_ats_mc_se = sqrt(res[["ps_ats"]] * (1 - res[["ps_ats"]]) / NREP),
          row.names = NULL
        )
        cat(sprintf("done: n=%3d | %-38s | panel=%d | kw(saved)=%.4f ps_kw=%.4f ps_ats=%.4f\n",
                    n, pdesign$design, panel, ref,
                    res[["ps_kw"]], res[["ps_ats"]]))
        utils::flush.console()
        idx <- idx + 1
      }
    }
  }
}

result <- do.call(rbind, rows)
outfile <- sprintf("rankfd_route1_power_B%d.csv", NREP)
write.csv(result, outfile, row.names = FALSE)
message("Wrote: ", outfile)

## Balanced designs: pseudo-ranks and ranks are the same procedure, so the two
## columns must agree to within Monte Carlo error.
bal <- result[grepl("^balanced", result$design), ]
message(sprintf("balanced designs: max |ps_kw - rank_power| = %.5f (MC SE about %.5f)",
                max(abs(bal$ps_kw_power - bal$rank_power)),
                sqrt(2 * 0.25 / NREP)))
