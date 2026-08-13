## ---------------------------------------------------------------------------
## Supplementary tables: the simulated results that no figure shows.
##
## Layout follows Brunner, Konietschke, Pauly & Puri (2017), JRSS-B, Tables 3-5,
## p. 1478-1479: rows are the input distribution crossed with the sample size,
## columns are the competing strategies. No Monte Carlo standard errors, as in
## those tables. Rates are given to three decimals rather than his four, which
## is where the Monte Carlo error of B = 50,000 puts the last honest digit.
##
## Tables produced:
##   1  power, unbalanced homoscedastic, increasing trend alternative
##   2  power, unbalanced homoscedastic, one-point alternative
##   3  power against delta at fixed n            (written only if the grid exists)
##
## The vignette's power figure shows the balanced homoscedastic design only, so
## the unbalanced rows are simulated but never plotted. The delta sweep has no
## figure at all.
##
## Usage:
##   Rscript supplement_tables.R [DELTA]
## Output: supplement_table_<n>.tex and .csv
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

args <- commandArgs(trailingOnly = TRUE)
DELTA <- if (length(args) >= 1) as.numeric(args[1]) else 0.5
DELTA_TAG <- sub("\\.", "", format(DELTA, nsmall = 2))

STRATS <- c(fisher_power = "F", welch_power = "W", mean_power = "L",
            rank_power = "KW", sw_power = "SW", gate_power = "SW+L")

## The panel labels of the figures, so a reader can match table to figure.
panel_label <- function(p) {
  one <- fleishman_cases[fleishman_cases$panel == p, , drop = FALSE]
  if (p == 1) return("normal")
  sprintf("skew %s, excess kurtosis %s", one$skew, one$excess_kurtosis)
}

## Three decimals: at B = 50,000 the Monte Carlo standard error is 0.00098 at
## p = 0.05 and 0.0022 at p = 0.5, so a fourth decimal reports noise.
fmt <- function(x) formatC(x, format = "f", digits = 3)

## Only the data is written. The vignette child _supplementary_tables.Rmd
## renders it, so the numbers exist in exactly one place and the table markup
## follows the same LaTeX/HTML branch as the effect-size table.
write_table <- function(d, file_stem, caption, row_vars, row_names) {
  d <- d[do.call(order, unname(d[row_vars])), , drop = FALSE]
  out <- data.frame(d[row_vars], lapply(d[names(STRATS)], fmt),
                    check.names = FALSE, stringsAsFactors = FALSE)
  names(out) <- c(row_names, unname(STRATS))
  utils::write.csv(out, paste0(file_stem, ".csv"), row.names = FALSE)
  message("wrote ", file_stem, ".csv (", nrow(out), " rows)")
}

## ---- 1: trend alternative, unbalanced homoscedastic -------------------------
trend <- readRDS(file.path(SIMDIR, "fleishman_4groups_power.rds"))
trend <- trend[trend$design == "unbalanced n, equal SD", , drop = FALSE]
trend$distribution <- vapply(trend$panel, panel_label, character(1))
write_table(
  trend, "supplement_table_1_power_trend_unbalanced",
  paste("Power of the six strategies under the increasing trend alternative,",
        "mean shifts $(0, 0.25, 0.50, 0.75)$, unbalanced homoscedastic design,",
        "$\\bar n(0.5, 0.8, 1.2, 1.5)$, $\\sigma_i = 1$.",
        "$B = 50{,}000$ replications per cell."),
  c("distribution", "n_per_group"), c("Distribution", "$\\bar n$"))

## ---- 2: one-point alternative, unbalanced homoscedastic ---------------------
pf <- file.path(SIMDIR, sprintf(
  "fleishman_4groups_power_design_brunner_onepoint_d%s_B50000.csv", DELTA_TAG))
if (file.exists(pf)) {
  point <- read.csv(pf, stringsAsFactors = FALSE)
  point <- point[point$design == "unbalanced n, equal SD", , drop = FALSE]
  point$distribution <- vapply(point$panel, panel_label, character(1))
  write_table(
    point, sprintf("supplement_table_2_power_onepoint_d%s_unbalanced", DELTA_TAG),
    sprintf(paste("Power of the six strategies under the one-point alternative,",
                  "$\\mu = (0, 0, 0, %.2f)$, unbalanced homoscedastic design,",
                  "$\\bar n(0.5, 0.8, 1.2, 1.5)$, $\\sigma_i = 1$.",
                  "$B = 50{,}000$ replications per cell."), DELTA),
    c("distribution", "n_per_group"), c("Distribution", "$\\bar n$"))
} else {
  message("skipping table 2: ", basename(pf), " not found")
}

## ---- 3: power against delta at fixed n --------------------------------------
sf <- file.path(SIMDIR, "route1_power_delta_sweep_B50000.csv")
if (file.exists(sf)) {
  sweep <- read.csv(sf, stringsAsFactors = FALSE)
  if (nrow(sweep) < 360) {
    stop("the delta sweep holds ", nrow(sweep), " of 360 cells; ",
         "finish the run before writing the supplement table")
  }
  sweep$distribution <- vapply(sweep$panel, panel_label, character(1))
  sweep$alt <- ifelse(sweep$alternative == "onepoint",
                      "one-point", "increasing trend")
  write_table(
    sweep, "supplement_table_3_power_delta_sweep",
    paste("Power against $\\delta$ at fixed balanced $n$, $\\sigma_i = 1$,",
          "under the one-point alternative $\\mu = (0,0,0,\\delta)$ and the",
          "increasing trend alternative $\\mu = (\\delta/4, \\delta/2,",
          "3\\delta/4, \\delta)$, following Brunner et al. (2017), p. 1480.",
          "$B = 50{,}000$ replications per cell."),
    c("alt", "distribution", "n_per_group", "delta"),
    c("Alternative", "Distribution", "$n$", "$\\delta$"))
} else {
  message("skipping table 3: the delta sweep grid is not complete")
}
