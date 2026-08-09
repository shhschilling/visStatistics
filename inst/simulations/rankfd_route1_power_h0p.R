## ---------------------------------------------------------------------------
## Pseudo-rank arm for the Route 1 power grid.
##
## Companion to rankfd_route1_typeI.R. Only the two pseudo-rank procedures are
## simulated; nothing already in fleishman_4groups_power.csv is recomputed.
##
##   ats_h0p  rankFD(effect = "unweighted", hypothesis = "H0p"), ANOVA-type
##            statistic for the relative-effect null, which allows the group
##            variances to differ under the null and so covers the
##            nonparametric Behrens-Fisher situation. There is no
##            Kruskal-Wallis element under this option: that statistic is
##            derived under H0F.
##
## Stream bookkeeping. Continues after route1_simulations.R (cells 0-299), the
## H0F Type I arm (300-449), the H0F power arm (450-599), and the H0p Type I
## arm (600-749), each drawing NREP streams per cell. This script therefore
## starts at cell 750, so all five runs form one continuous, reproducible
## stream sequence and no cell is ever drawn twice.
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
CELLS_ALREADY_RUN <- 750L
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
                          effect = "unweighted", hypothesis = "H0p")
    c(ats_h0p = fit$ANOVA.Type.Statistic[1, "p-Value"] < ALPHA)
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
          ats_h0p_power = unname(res[["ats_h0p"]]),
          ats_h0p_mc_se = sqrt(res[["ats_h0p"]] * (1 - res[["ats_h0p"]]) / NREP),
          row.names = NULL
        )
        cat(sprintf("done: n=%3d | %-38s | panel=%d | kw(saved)=%.4f ats_h0p=%.4f\n",
                    n, pdesign$design, panel, ref, res[["ats_h0p"]]))
        utils::flush.console()
        idx <- idx + 1
      }
    }
  }
}

result <- do.call(rbind, rows)
outfile <- sprintf("rankfd_route1_power_h0p_B%d.csv", NREP)
write.csv(result, outfile, row.names = FALSE)
message("Wrote: ", outfile)

