## ---------------------------------------------------------------------------
## H0p arm for the Route 1 equal-means grid.
##
## Adds one more test situation to the simulation of route1_simulations.R and
## nothing else. The designs, the sizes and the draw order are copied verbatim
## from that script, and the L'Ecuyer stream continues at the position that run
## left off: route1_simulations.R consumes NREP streams for each of its 150
## Type I and 150 power cells, so this script skips 300 * NREP streams before
## it starts. The two runs therefore form one reproducible stream sequence and
## nothing already computed is computed again -- in particular
## stats::kruskal.test() is not rerun; the saved rank_rejection column is
## carried along as the classical baseline for the same designs.
##
##   ats_h0p  rankFD(effect = "unweighted", hypothesis = "H0p"), ANOVA-type
##            statistic for the relative-effect null, which allows the group
##            variances to differ under the null and so covers the
##            nonparametric Behrens-Fisher situation. There is no
##            Kruskal-Wallis element under this option: that statistic is
##            derived under H0F.
##
## Pseudo-ranks and ranks coincide for two groups and for equal group sizes
## (Zimmermann et al. 2022, pp. 125-126), so in the two balanced designs ps_kw
## must agree with the saved rank_rejection to within Monte Carlo error, the
## samples being independent; only the three unbalanced designs can differ
## systematically.
##
## Any change to MEAN_NS, to make_conditions(), to the loop order or to the draw
## order in make_equal_mean_data() breaks the correspondence with the saved
## results.
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
MEAN_NS <- c(10, 20, 30, 50, 100, 200)

RNGkind("L'Ecuyer-CMRG")
set.seed(20260615)
.rng_stream <- .Random.seed

## Continue after route1_simulations.R (cells 0-299), the H0F Type I arm
## (300-449) and the H0F power arm (450-599), each drawing NREP streams per
## cell.
CELLS_ALREADY_RUN <- 600L
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

make_conditions <- function(mean_n) {
  balanced_n <- rep(mean_n, 4)
  unbalanced_n <- as.integer(round(mean_n * c(0.5, 0.8, 1.2, 1.5)))
  stopifnot(mean(unbalanced_n) == mean_n)
  list(
    list(design = "balanced n, equal SD", n = balanced_n, sd = c(1, 1, 1, 1)),
    list(design = "balanced n, unequal SD", n = balanced_n, sd = c(1, 1.3, 1.7, 2.2)),
    list(design = "unbalanced n, equal SD", n = unbalanced_n, sd = c(1, 1, 1, 1)),
    list(design = "unbalanced n, larger n with larger SD", n = unbalanced_n, sd = c(1, 1.3, 1.7, 2.2)),
    list(design = "unbalanced n, larger n with smaller SD", n = unbalanced_n, sd = c(2.2, 1.7, 1.3, 1))
  )
}

make_equal_mean_data <- function(panel, n_vec, sd_vec) {
  g <- factor(rep(seq_along(n_vec), times = n_vec))
  y <- unlist(lapply(seq_along(n_vec), function(i) {
    sd_vec[i] * draw_fleishman_panel(n_vec[i], panel)
  }))
  list(y = y, g = g)
}
## ---- end of the copied block -----------------------------------------------

run_cell <- function(panel, n_vec, sd_vec) {
  seeds <- cell_seeds(NREP)
  out <- parallel::mclapply(seq_len(NREP), function(i) {
    assign(".Random.seed", seeds[[i]], envir = globalenv())
    dat <- make_equal_mean_data(panel, n_vec, sd_vec)
    fit <- rankFD::rankFD(y ~ g, data = data.frame(y = dat$y, g = dat$g),
                          effect = "unweighted", hypothesis = "H0p")
    c(ats_h0p = fit$ANOVA.Type.Statistic[1, "p-Value"] < ALPHA)
  }, mc.cores = NCORES)
  colMeans(do.call(rbind, out))
}

saved <- read.csv(file.path(SIMDIR, "route1_equal_mean_simulations.csv"))

rows <- list()
idx <- 1
for (mean_n in MEAN_NS) {
  for (condition in make_conditions(mean_n)) {
    for (panel in PANELS) {
      res <- run_cell(panel, condition$n, condition$sd)
      one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
      ref <- saved$rank_rejection[saved$mean_n_per_group == mean_n &
                                    saved$design == condition$design &
                                    saved$panel == panel]
      rows[[idx]] <- data.frame(
        design = condition$design,
        mean_n_per_group = mean_n,
        n_per_group = paste(condition$n, collapse = ", "),
        sd_per_group = paste(format(condition$sd, nsmall = 1), collapse = ", "),
        distribution = one$distribution,
        panel = panel,
        skew = one$skew,
        excess_kurtosis = one$excess_kurtosis,
        rank_null_true = panel %in% c(1, 2),
        rank_rejection = ref,
        ats_h0p_rejection = unname(res[["ats_h0p"]]),
        ats_h0p_mc_se = sqrt(res[["ats_h0p"]] * (1 - res[["ats_h0p"]]) / NREP),
        row.names = NULL
      )
      cat(sprintf("done: mean_n=%3d | %-38s | panel=%d | kw(saved)=%.4f ats_h0p=%.4f\n",
                  mean_n, condition$design, panel, ref, res[["ats_h0p"]]))
      utils::flush.console()
      idx <- idx + 1
    }
  }
}

result <- do.call(rbind, rows)
outfile <- sprintf("rankfd_route1_typeI_h0p_B%d.csv", NREP)
write.csv(result, outfile, row.names = FALSE)
message("Wrote: ", outfile)

