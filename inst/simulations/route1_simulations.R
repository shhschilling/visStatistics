## Route 1 Fleishman type-I and power simulations.
##
## This keeps the Gamma simulation routing logic, but draws centred,
## standardised Fleishman residual distributions.

## Reproducible parallel streams: set.seed() alone does not reach the workers
## forked by mclapply(). With L'Ecuyer-CMRG each replication is given its own
## stream, drawn in the parent, so results do not depend on the number of cores.
RNGkind("L'Ecuyer-CMRG")
set.seed(20260615)
.rng_stream <- .Random.seed

cell_seeds <- function(n) {
  seeds <- vector("list", n)
  for (i in seq_len(n)) {
    seeds[[i]] <- .rng_stream
    .rng_stream <<- parallel::nextRNGStream(.rng_stream)
  }
  seeds
}

args <- commandArgs(trailingOnly = TRUE)
NREP <- if (length(args) >= 1) as.integer(args[1]) else 50000
NCORES <- if (length(args) >= 2) {
  as.integer(args[2])
} else {
  max(1L, min(8L, parallel::detectCores(logical = FALSE) - 1L))
}
ALPHA <- 0.05
PANELS <- 1:5
MEAN_NS <- c(10, 20, 30, 50, 100, 200)
POWER_NS <- c(10, 20, 30, 50, 100, 200)

## Power designs. The multipliers sum to 4, so mean(n_vec) == mean_n exactly and
## both series share an x-axis tick. The Fleishman panels are drawn with unit
## variance, so the four groups stay identically distributed under H0, the rank
## branch's null is exactly true, and a single density heads each column of the
## figure. The only thing that differs between the two designs is the imbalance.
## Appended designs keep the stream positions of the first two, so their cells
## reproduce bit-identically. The heteroscedastic blocks hold the effect size
## fixed: shifts are scaled by sqrt(mean(sd^2)), so omega^2 matches the
## homoscedastic blocks and the comparison isolates unequal variances.
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
OUTDIR <- sprintf("fleishman_route1_power_B%d_outputs", NREP)
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

stopifnot(NREP > 0)
stopifnot(NCORES > 0)
library(visStatistics)
source("fleishman_route1_residual_helpers.R")

standardised_residuals <- function(y, g) {
  model <- stats::aov(y ~ g)
  raw <- stats::residuals(model)
  rs <- suppressWarnings(stats::rstandard(model))
  if (any(!is.finite(rs))) {
    rs <- raw / max(stats::sigma(model), 1e-8)
  }
  list(model = model, rs = rs)
}

levene_p <- function(rs, g) {
  levene.test(rs, g)$p.value
}

normality_p <- function(rs) {
  if (length(rs) < 3) {
    return(NA_real_)
  }
  stats::shapiro.test(rs)$p.value
}

## Sample skewness and excess kurtosis of the standardised residuals, the
## vector the Shapiro-Wilk gate is applied to. A scale mixture of symmetric
## distributions induces excess kurtosis but no skewness; skewed input carries
## both, so the two channels are recorded separately.
resid_moments <- function(x) {
  x <- x - mean(x)
  n <- length(x)
  m2 <- sum(x^2) / n
  if (m2 <= 0) return(c(skewness = NA_real_, excess_kurtosis = NA_real_))
  c(
    skewness = (sum(x^3) / n) / m2^1.5,
    excess_kurtosis = (sum(x^4) / n) / m2^2 - 3
  )
}

route_once <- function(y, g, alpha = ALPHA) {
  g <- factor(g)
  fit <- standardised_residuals(y, g)
  p_norm <- normality_p(fit$rs)
  normality_met <- is.na(p_norm) || p_norm >= alpha
  p_lev <- levene_p(fit$rs, g)
  equal_var <- is.na(p_lev) || p_lev >= alpha

  p_fisher <- summary(fit$model)[[1]][["Pr(>F)"]][1]
  p_welch <- stats::oneway.test(y ~ g, var.equal = FALSE)$p.value
  p_levene_route <- if (equal_var) p_fisher else p_welch
  p_rank <- stats::kruskal.test(y, g)$p.value
  p_sw <- if (normality_met) p_welch else p_rank
  p_gate <- if (normality_met) p_levene_route else p_rank

  mom <- resid_moments(fit$rs)

  c(
    resid_skewness = unname(mom["skewness"]),
    resid_excess_kurtosis = unname(mom["excess_kurtosis"]),
    fisher_reject = p_fisher < alpha,
    welch_reject = p_welch < alpha,
    levene_route_reject = p_levene_route < alpha,
    rank_reject = p_rank < alpha,
    sw_reject_final = p_sw < alpha,
    sw_gate_reject = p_gate < alpha,
    sw_route_welch = normality_met,
    sw_route_rank = !normality_met,
    route_fisher = normality_met && equal_var,
    route_welch = normality_met && !equal_var,
    route_rank = !normality_met,
    levene_select_fisher = equal_var,
    levene_select_welch = !equal_var,
    sw_reject = !normality_met,
    levene_reject = !equal_var
  )
}

summarise_binary <- function(values) {
  p <- mean(values)
  se <- sqrt(p * (1 - p) / length(values))
  c(rate = p, mc_se = se)
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

run_type1_cell <- function(panel, n_vec, sd_vec) {
  seeds <- cell_seeds(NREP)
  out <- parallel::mclapply(seq_len(NREP), function(i) {
    assign(".Random.seed", seeds[[i]], envir = globalenv())
    dat <- make_equal_mean_data(panel, n_vec, sd_vec)
    route_once(dat$y, dat$g)
  }, mc.cores = NCORES)
  names_out <- names(out[[1]])
  moment_names <- c("resid_skewness", "resid_excess_kurtosis")
  flag_names <- setdiff(names_out, moment_names)
  stats <- lapply(flag_names, function(nm) {
    summarise_binary(vapply(out, `[[`, numeric(1), nm) > 0.5)
  })
  names(stats) <- flag_names
  moments <- vapply(moment_names, function(nm) {
    mean(vapply(out, `[[`, numeric(1), nm), na.rm = TRUE)
  }, numeric(1))
  c(unlist(stats), moments)
}

case_label <- function(panel) {
  fleishman_panel_short_label(panel)
}

case_levels <- vapply(PANELS, case_label, character(1))

type1_rows <- list()
idx <- 1
for (mean_n in MEAN_NS) {
  for (condition in make_conditions(mean_n)) {
    for (panel in PANELS) {
      res <- run_type1_cell(panel, condition$n, condition$sd)
      one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
      type1_rows[[idx]] <- data.frame(
        design = condition$design,
        mean_n_per_group = mean_n,
        n_per_group = paste(condition$n, collapse = ", "),
        sd_per_group = paste(format(condition$sd, nsmall = 1), collapse = ", "),
        distribution = one$distribution,
        panel = panel,
        skew = one$skew,
        excess_kurtosis = one$excess_kurtosis,
        groups = 4,
        group_means = "0, 0, 0, 0",
        resid_skewness = res["resid_skewness"],
        resid_excess_kurtosis = res["resid_excess_kurtosis"],
        fisher_rejection = res["fisher_reject.rate"],
        fisher_mc_se = res["fisher_reject.mc_se"],
        welch_rejection = res["welch_reject.rate"],
        welch_mc_se = res["welch_reject.mc_se"],
        levene_route_rejection = res["levene_route_reject.rate"],
        levene_route_mc_se = res["levene_route_reject.mc_se"],
        rank_rejection = res["rank_reject.rate"],
        rank_mc_se = res["rank_reject.mc_se"],
        sw_rejection = res["sw_reject_final.rate"],
        sw_mc_se = res["sw_reject_final.mc_se"],
        sw_gate_rejection = res["sw_gate_reject.rate"],
        sw_gate_mc_se = res["sw_gate_reject.mc_se"],
        sw_route_welch_probability = res["sw_route_welch.rate"],
        sw_route_welch_mc_se = res["sw_route_welch.mc_se"],
        sw_route_rank_probability = res["sw_route_rank.rate"],
        sw_route_rank_mc_se = res["sw_route_rank.mc_se"],
        route_fisher_probability = res["route_fisher.rate"],
        route_fisher_mc_se = res["route_fisher.mc_se"],
        route_welch_probability = res["route_welch.rate"],
        route_welch_mc_se = res["route_welch.mc_se"],
        route_rank_probability = res["route_rank.rate"],
        route_rank_mc_se = res["route_rank.mc_se"],
        levene_select_fisher_probability = res["levene_select_fisher.rate"],
        levene_select_welch_probability = res["levene_select_welch.rate"],
        sw_reject_probability = res["sw_reject.rate"],
        levene_reject_probability = res["levene_reject.rate"],
        row.names = NULL
      )
      cat(sprintf(
        "type I done: mean_n=%d | %s | panel=%d\n",
        mean_n, condition$design, panel
      ))
      idx <- idx + 1
    }
  }
}

type1 <- do.call(rbind, type1_rows)
type1$design <- factor(type1$design, levels = unique(type1$design))
type1$skew_label <- factor(vapply(type1$panel, case_label, character(1)),
  levels = case_levels
)
write.csv(type1, file.path(OUTDIR, "route1_equal_mean_simulations.csv"),
  row.names = FALSE
)
saveRDS(type1, file.path(OUTDIR, "route1_equal_mean_simulations.rds"), version = 2)

make_shift_data <- function(panel, n_vec, shifts, sd_vec = rep(1, length(shifts))) {
  k <- length(shifts)
  stopifnot(length(n_vec) == k, length(sd_vec) == k)
  g <- factor(rep(seq_len(k), times = n_vec))
  y <- unlist(lapply(seq_len(k), function(i) {
    sd_vec[i] * draw_fleishman_panel(n_vec[i], panel) + shifts[i]
  }))
  list(y = y, g = g)
}

run_power_cell <- function(panel, n_vec, shifts, sd_vec = rep(1, length(shifts))) {
  seeds <- cell_seeds(NREP)
  out <- parallel::mclapply(seq_len(NREP), function(i) {
    assign(".Random.seed", seeds[[i]], envir = globalenv())
    dat <- make_shift_data(panel, n_vec, shifts, sd_vec)
    route_once(dat$y, dat$g)
  }, mc.cores = NCORES)
  c(
    fisher = summarise_binary(vapply(out, `[[`, numeric(1), "fisher_reject") > 0.5),
    welch = summarise_binary(vapply(out, `[[`, numeric(1), "welch_reject") > 0.5),
    mean = summarise_binary(vapply(out, `[[`, numeric(1), "levene_route_reject") > 0.5),
    rank = summarise_binary(vapply(out, `[[`, numeric(1), "rank_reject") > 0.5),
    sw = summarise_binary(vapply(out, `[[`, numeric(1), "sw_reject_final") > 0.5),
    gate = summarise_binary(vapply(out, `[[`, numeric(1), "sw_gate_reject") > 0.5),
    sw_route_welch = summarise_binary(vapply(out, `[[`, numeric(1), "sw_route_welch") > 0.5),
    sw_route_rank = summarise_binary(vapply(out, `[[`, numeric(1), "sw_route_rank") > 0.5),
    route_rank = summarise_binary(vapply(out, `[[`, numeric(1), "route_rank") > 0.5),
    route_fisher = summarise_binary(vapply(out, `[[`, numeric(1), "route_fisher") > 0.5),
    route_welch = summarise_binary(vapply(out, `[[`, numeric(1), "route_welch") > 0.5),
    mean_route_fisher = summarise_binary(vapply(out, `[[`, numeric(1), "levene_select_fisher") > 0.5),
    mean_route_welch = summarise_binary(vapply(out, `[[`, numeric(1), "levene_select_welch") > 0.5),
    levene_reject = summarise_binary(vapply(out, `[[`, numeric(1), "levene_reject") > 0.5),
    resid_skewness = mean(vapply(out, `[[`, numeric(1), "resid_skewness"), na.rm = TRUE),
    resid_excess_kurtosis = mean(vapply(out, `[[`, numeric(1), "resid_excess_kurtosis"), na.rm = TRUE)
  )
}

power_rows <- list()
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
      res <- run_power_cell(panel, n_vec, shifts, sd_vec)
      one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
      power_rows[[idx]] <- data.frame(
        design = pdesign$design,
        scenario = paste("four groups with", scenario_name),
        effect_size = scenario_name,
        group_mean_offsets = paste(format(round(shifts, 3), nsmall = 2), collapse = ", "),
        sd_per_group = paste(format(sd_vec, nsmall = 1), collapse = ", "),
        shift_scale = shift_scale,
        n_vector = paste(n_vec, collapse = ", "),
        distribution = one$distribution,
        panel = panel,
        skew = one$skew,
        excess_kurtosis = one$excess_kurtosis,
        n_per_group = n,
        groups = 4,
        resid_skewness = res["resid_skewness"],
        resid_excess_kurtosis = res["resid_excess_kurtosis"],
        fisher_power = res["fisher.rate"],
        fisher_mc_se = res["fisher.mc_se"],
        welch_power = res["welch.rate"],
        welch_mc_se = res["welch.mc_se"],
        mean_power = res["mean.rate"],
        mean_mc_se = res["mean.mc_se"],
        rank_power = res["rank.rate"],
        rank_mc_se = res["rank.mc_se"],
        sw_power = res["sw.rate"],
        sw_mc_se = res["sw.mc_se"],
        gate_power = res["gate.rate"],
        gate_mc_se = res["gate.mc_se"],
        sw_route_welch_probability = res["sw_route_welch.rate"],
        sw_route_welch_mc_se = res["sw_route_welch.mc_se"],
        sw_route_rank_probability = res["sw_route_rank.rate"],
        sw_route_rank_mc_se = res["sw_route_rank.mc_se"],
        route_rank_probability = res["route_rank.rate"],
        route_rank_mc_se = res["route_rank.mc_se"],
        route_fisher_probability = res["route_fisher.rate"],
        route_fisher_mc_se = res["route_fisher.mc_se"],
        route_welch_probability = res["route_welch.rate"],
        route_welch_mc_se = res["route_welch.mc_se"],
        mean_route_fisher_probability = res["mean_route_fisher.rate"],
        mean_route_fisher_mc_se = res["mean_route_fisher.mc_se"],
        mean_route_welch_probability = res["mean_route_welch.rate"],
        mean_route_welch_mc_se = res["mean_route_welch.mc_se"],
        levene_reject_probability = res["levene_reject.rate"],
        levene_reject_mc_se = res["levene_reject.mc_se"],
        row.names = NULL
      )
      cat(sprintf("power done: %s | n=%d (%s) | panel=%d\n",
                  pdesign$design, n, paste(n_vec, collapse = ","), panel))
      idx <- idx + 1
    }
  }
 }
}

power <- do.call(rbind, power_rows)
power$design <- factor(power$design,
  levels = vapply(POWER_DESIGNS, `[[`, character(1), "design")
)
power$skew_label <- factor(vapply(power$panel, case_label, character(1)),
  levels = case_levels
)
power$n_label <- factor(paste0("n = ", power$n_per_group),
  levels = paste0("n = ", POWER_NS)
)
power$effect_size <- factor(power$effect_size, levels = names(SHIFT_SCENARIOS))
write.csv(power, file.path(OUTDIR, "fleishman_4groups_power.csv"),
  row.names = FALSE
)
saveRDS(power, file.path(OUTDIR, "fleishman_4groups_power.rds"), version = 2)

writeLines(
  c(
    "Route 1 Fleishman residual simulations",
    paste("NREP per cell:", NREP),
    paste("cores:", NCORES),
    sprintf(
      "Maximum Monte Carlo SE for a percentage estimate: %.2f percentage points",
      100 * sqrt(0.25 / NREP)
    ),
    "Panels:",
    sprintf(
      "  %d: skew %.2f, excess kurtosis %.2f",
      fleishman_cases$panel,
      fleishman_cases$skew,
      fleishman_cases$excess_kurtosis
    )
  ),
  file.path(OUTDIR, "README.txt")
)

message("Outputs written to: ", OUTDIR)
