## ---------------------------------------------------------------------------
## Route 1 Type I grid under a SELECTABLE set of design inputs.
##
## route1_simulations.R is NOT touched and remains the source of
## route1_equal_mean_simulations.rds, so every figure currently in the
## repository stays reproducible. This script writes its own CSV.
##
## Usage:
##   Rscript route1_typeI_design_variants.R <NREP> <NCORES> <DESIGN_SET>
##   e.g. Rscript route1_typeI_design_variants.R 50000 8 brunner
##
## DESIGN_SET selects the scaling vectors:
##
##   "legacy"   what route1_simulations.R:158 uses:
##              SD = (1, 1.3, 1.7, 2.2) and its reverse.
##
##   "brunner"  Brunner, Konietschke, Pauly and Puri (2017), JRSS-B 79(5),
##              Table 2, p. 1477: sigma = (1, 1, 1, 1), (1, sqrt(2), 2, sqrt(5))
##              and (sqrt(5), 2, sqrt(2), 1). The roots are on the variance
##              scale: sigma^2 = (1, 2, 4, 5). The paper states the vectors but
##              gives no rationale for that particular sequence, so the reading
##              of the roots is ours.
##
## Why rerun the Type I grid at all
## --------------------------------
## The legacy vector (1, 1.3, 1.7, 2.2) has no source. Its design NAMES follow
## Brunner's Table 2 exactly -- balanced/unbalanced homoscedastic, balanced
## heteroscedastic, positive and negative pairing -- but the numbers are
## near-misses of the table they are named after. Type I error is where the
## design genuinely matters and cannot be reduced to an effect size, since every
## effect size is zero under the null, so it is worth having on the sourced
## vectors rather than on rounded approximations of them.
##
## Sample-size scaling stays as the package does it: fixed allocation ratios
## (0.5, 0.8, 1.2, 1.5) scaled by n_bar. Brunner instead ADDS a constant m to
## every group, so his ratios move with sample size; that scheme is deliberately
## not adopted here, as for the power grid.
##
## All group means are zero, exactly as in the legacy grid: this is the
## equal-means design, testing the parametric null. Under symmetric inputs the
## rank null holds too; under skewed inputs it does not, and the rank rows are
## then measuring power. eta_h_own_derivation.R quantifies which is which.
##
## Output (never overwrites an existing grid):
##   route1_typeI_design_<DESIGN_SET>_B<NREP>.csv
##
## Checkpointing: every completed cell is appended immediately and skipped on
## restart, with the RNG stream advanced identically for skipped cells.
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
## evaluate only its top-level `name <- ...` assignments, skipping the loops and
## the write.csv()/saveRDS() calls.
local({
  exprs <- parse(file.path(SIMDIR, "route1_simulations.R"))
  for (e in exprs) {
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]])) {
      nm <- as.character(e[[2]])
      if (nm %in% c("NREP", "NCORES", "ALPHA", "OUTDIR", "make_conditions")) next
      try(eval(e, envir = globalenv()), silent = TRUE)
    }
  }
})
stopifnot(is.function(route_once), is.function(summarise_binary),
          is.function(standardised_residuals))

PANELS <- 1:5
MEAN_NS <- c(10, 20, 30, 50, 100, 200)
NMULT_UNBAL <- c(0.5, 0.8, 1.2, 1.5)

## The only thing DESIGN_SET changes.
SD_SETS <- list(
  legacy  = list(eq = c(1, 1, 1, 1), pos = c(1, 1.3, 1.7, 2.2), neg = c(2.2, 1.7, 1.3, 1)),
  brunner = list(eq = c(1, 1, 1, 1), pos = c(1, sqrt(2), 2, sqrt(5)),
                 neg = c(sqrt(5), 2, sqrt(2), 1))
)
SDS <- SD_SETS[[DESIGN_SET]]

make_conditions <- function(mean_n) {
  balanced_n <- rep(mean_n, 4)
  unbalanced_n <- as.integer(round(mean_n * NMULT_UNBAL))
  stopifnot(mean(unbalanced_n) == mean_n)
  list(
    list(design = "balanced n, equal SD",   n = balanced_n,   sd = SDS$eq),
    list(design = "balanced n, unequal SD", n = balanced_n,   sd = SDS$pos),
    list(design = "unbalanced n, equal SD", n = unbalanced_n, sd = SDS$eq),
    list(design = "unbalanced n, larger n with larger SD",
         n = unbalanced_n, sd = SDS$pos),
    list(design = "unbalanced n, larger n with smaller SD",
         n = unbalanced_n, sd = SDS$neg)
  )
}

## Own seed, so these streams cannot collide with any existing grid.
RNGkind("L'Ecuyer-CMRG")
set.seed(if (DESIGN_SET == "brunner") 20260930L else 20260931L)
.rng_stream <- .Random.seed
cell_seeds <- function(n) {
  seeds <- vector("list", n)
  for (i in seq_len(n)) {
    .rng_stream <<- parallel::nextRNGStream(.rng_stream)
    seeds[[i]] <- .rng_stream
  }
  seeds
}

## Equal means throughout: only the scale differs between groups.
make_equal_mean_data <- function(panel, n_vec, sd_vec) {
  k <- length(n_vec)
  g <- factor(rep(seq_len(k), times = n_vec))
  y <- unlist(lapply(seq_len(k), function(i) {
    sd_vec[i] * draw_fleishman_panel(n_vec[i], panel)
  }))
  list(y = y, g = g)
}

run_type1_cell <- function(panel, n_vec, sd_vec) {
  seeds <- cell_seeds(NREP)
  out <- parallel::mclapply(seq_len(NREP), function(i) {
    assign(".Random.seed", seeds[[i]], envir = globalenv())
    dat <- make_equal_mean_data(panel, n_vec, sd_vec)
    route_once(dat$y, dat$g, alpha = ALPHA)
  }, mc.cores = NCORES)
  pick <- function(nm) vapply(out, `[[`, numeric(1), nm) > 0.5
  c(
    fisher = summarise_binary(pick("fisher_reject")),
    welch  = summarise_binary(pick("welch_reject")),
    mean   = summarise_binary(pick("levene_route_reject")),
    rank   = summarise_binary(pick("rank_reject")),
    sw     = summarise_binary(pick("sw_reject_final")),
    gate   = summarise_binary(pick("sw_gate_reject")),
    route_rank   = summarise_binary(pick("route_rank")),
    route_fisher = summarise_binary(pick("route_fisher")),
    route_welch  = summarise_binary(pick("route_welch"))
  )
}

OUTFILE <- sprintf("route1_typeI_design_%s_B%d.csv", DESIGN_SET, NREP)
done_keys <- character(0)
if (file.exists(OUTFILE)) {
  ex <- read.csv(OUTFILE, stringsAsFactors = FALSE)
  done_keys <- paste(ex$design, ex$mean_n_per_group, ex$panel)
  message(sprintf("Resuming: %d cells already in %s", length(done_keys), OUTFILE))
}

message(sprintf("DESIGN_SET = %s | B = %d | %d cells", DESIGN_SET, NREP,
                5 * length(MEAN_NS) * length(PANELS)))

for (mean_n in MEAN_NS) {
  for (condition in make_conditions(mean_n)) {
    for (panel in PANELS) {
      key <- paste(condition$design, mean_n, panel)
      if (key %in% done_keys) { cell_seeds(NREP); next }
      one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
      res <- run_type1_cell(panel, condition$n, condition$sd)
      row <- data.frame(
        design_set = DESIGN_SET,
        design = condition$design,
        mean_n_per_group = mean_n,
        n_per_group = paste(condition$n, collapse = ", "),
        sd_per_group = paste(format(round(condition$sd, 4), nsmall = 1), collapse = ", "),
        distribution = one$distribution,
        panel = panel,
        skew = one$skew,
        excess_kurtosis = one$excess_kurtosis,
        groups = 4,
        group_means = "0, 0, 0, 0",
        ## The rank null holds only where the inputs are symmetric; scaling a
        ## skewed distribution moves its mass even at a fixed mean.
        rank_null_true = panel %in% c(1, 2),
        fisher_rejection = unname(res[["fisher.rate"]]),
        fisher_mc_se = unname(res[["fisher.mc_se"]]),
        welch_rejection = unname(res[["welch.rate"]]),
        welch_mc_se = unname(res[["welch.mc_se"]]),
        levene_route_rejection = unname(res[["mean.rate"]]),
        levene_route_mc_se = unname(res[["mean.mc_se"]]),
        rank_rejection = unname(res[["rank.rate"]]),
        rank_mc_se = unname(res[["rank.mc_se"]]),
        sw_rejection = unname(res[["sw.rate"]]),
        sw_mc_se = unname(res[["sw.mc_se"]]),
        sw_gate_rejection = unname(res[["gate.rate"]]),
        sw_gate_mc_se = unname(res[["gate.mc_se"]]),
        route_fisher_probability = unname(res[["route_fisher.rate"]]),
        route_welch_probability = unname(res[["route_welch.rate"]]),
        route_rank_probability = unname(res[["route_rank.rate"]]),
        row.names = NULL
      )
      write.table(row, OUTFILE, sep = ",", row.names = FALSE,
                  col.names = !file.exists(OUTFILE), append = file.exists(OUTFILE))
      cat(sprintf("done: n=%3d | %-38s | panel=%d | F=%.4f W=%.4f KW=%.4f SW+L=%.4f\n",
                  mean_n, condition$design, panel,
                  res[["fisher.rate"]], res[["welch.rate"]],
                  res[["rank.rate"]], res[["gate.rate"]]))
      utils::flush.console()
    }
  }
}
message("Results saved to ", OUTFILE)
