## ---------------------------------------------------------------------------
## Power against delta at fixed n, following SPEC_power_design.md.
##
##   balanced, sigma = 1 in every group, n in {10, 20, 50, 100}
##   delta swept 0, 0.2, ..., 1.6 as the horizontal axis
##   alternative (a)  mu = (0, 0, 0, delta)
##   alternative (b)  mu = (delta/4, delta/2, 3delta/4, delta)
##   five Fleishman panels; panel 1 is the normal case of Brunner's Table 5
##
## Output is a table, not a figure: route1_power_delta_sweep_B<NREP>.csv
##
## Usage:
##   Rscript route1_power_delta_sweep.R [NREP] [NCORES]
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
## route_once() calls levene.test() from the package.
library(visStatistics)

## route_once() and summarise_binary() live in route1_simulations.R, which runs
## its own grids when sourced. Take only its top-level function definitions, the
## same way route1_power_design_variants.R does.
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

args <- commandArgs(trailingOnly = TRUE)
NREP <- if (length(args) >= 1) as.integer(args[1]) else 50000L
NCORES <- if (length(args) >= 2) as.integer(args[2]) else max(1L, parallel::detectCores() - 2L)
stopifnot(NREP > 0, NCORES > 0)
ALPHA <- 0.05

NS <- c(10L, 20L, 50L, 100L)
DELTAS <- seq(0, 1.6, by = 0.2)
PANELS <- 1:5
ALTS <- list(onepoint = function(d) c(0, 0, 0, d),
             trend    = function(d) c(d / 4, d / 2, 3 * d / 4, d))

RNGkind("L'Ecuyer-CMRG")
set.seed(20260912L)
.rng_stream <- .Random.seed
cell_seeds <- function(n) {
  seeds <- vector("list", n)
  for (i in seq_len(n)) {
    .rng_stream <<- parallel::nextRNGStream(.rng_stream)
    seeds[[i]] <- .rng_stream
  }
  seeds
}

run_cell <- function(panel, n, shifts, seeds) {
  out <- parallel::mclapply(seeds, function(s) {
    assign(".Random.seed", s, envir = globalenv())
    y <- unlist(lapply(seq_along(shifts),
                       function(i) draw_fleishman_panel(n, panel) + shifts[i]))
    g <- factor(rep(seq_along(shifts), each = n))
    route_once(y, g, alpha = ALPHA)
  }, mc.cores = NCORES)
  pick <- function(nm) vapply(out, `[[`, numeric(1), nm)
  c(fisher = summarise_binary(pick("fisher_reject")),
    welch  = summarise_binary(pick("welch_reject")),
    mean   = summarise_binary(pick("levene_route_reject")),
    rank   = summarise_binary(pick("rank_reject")),
    sw     = summarise_binary(pick("sw_reject_final")),
    gate   = summarise_binary(pick("sw_gate_reject")))
}

OUTFILE <- sprintf("route1_power_delta_sweep_B%d.csv", NREP)
done_keys <- if (file.exists(OUTFILE)) {
  d <- read.csv(OUTFILE, stringsAsFactors = FALSE)
  paste(d$alternative, d$n_per_group, d$delta, d$panel)
} else character(0)

message(sprintf("delta sweep | B = %d | %d cells", NREP,
                length(ALTS) * length(NS) * length(DELTAS) * length(PANELS)))

for (alt in names(ALTS)) {
  for (n in NS) {
    for (dl in DELTAS) {
      shifts <- ALTS[[alt]](dl)
      for (panel in PANELS) {
        seeds <- cell_seeds(NREP)
        key <- paste(alt, n, dl, panel)
        if (key %in% done_keys) next
        res <- run_cell(panel, n, shifts, seeds)
        one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
        row <- data.frame(
          alternative = alt,
          n_per_group = n,
          delta = dl,
          panel = panel,
          skew = one$skew,
          excess_kurtosis = one$excess_kurtosis,
          group_means = paste(sprintf("%.3f", shifts), collapse = ", "),
          fisher_power = unname(res[["fisher.rate"]]),
          welch_power  = unname(res[["welch.rate"]]),
          mean_power   = unname(res[["mean.rate"]]),
          rank_power   = unname(res[["rank.rate"]]),
          sw_power     = unname(res[["sw.rate"]]),
          gate_power   = unname(res[["gate.rate"]]),
          rank_mc_se   = unname(res[["rank.mc_se"]]),
          row.names = NULL
        )
        write.table(row, OUTFILE, sep = ",", row.names = FALSE,
                    col.names = !file.exists(OUTFILE), append = file.exists(OUTFILE))
        cat(sprintf("done: %-8s n=%2d delta=%.1f panel=%d | F=%.4f KW=%.4f SW+L=%.4f\n",
                    alt, n, dl, panel, res[["fisher.rate"]], res[["rank.rate"]],
                    res[["gate.rate"]]))
        utils::flush.console()
      }
    }
  }
}
message("Results saved to ", OUTFILE)
