## ---------------------------------------------------------------------------
## Route 1 equal-mean rejection simulation inspired by Blanca:2017 and
## Zimmerman:2004.
##
## Ground truth:
##   all four group means are equal.
##
## Design principles read from the local PDFs:
##   Blanca:2017: zero group effect, one-way design, balanced and unbalanced
##     group sizes, non-normal distributions, empirical Type I error.
##   Zimmerman:2004: zero group effect, unequal variances, variance-size
##     pairing, unconditional tests versus Levene-conditioned choice.
##
## This script adapts those principles to the Route 1 four-group setting.
## ---------------------------------------------------------------------------

set.seed(20260603)

args <- commandArgs(trailingOnly = TRUE)
NREP <- if (length(args) >= 1) as.integer(args[1]) else 5000
ALPHA <- 0.05
SKEWS <- c(0, 0.5, 1, 2, 4)
MEAN_NS <- c(10, 20, 50, 100)
OUTDIR <- file.path(
  "dev",
  "codexsimulation20160606_route1_equal_mean_blanca_zimmerman_skew4_outputs"
)
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

stopifnot(NREP > 0)
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required.")
}
if (!requireNamespace("scales", quietly = TRUE)) {
  stop("Package 'scales' is required.")
}
if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Package 'pkgload' is required.")
}
pkgload::load_all(".", quiet = TRUE)

make_conditions <- function(mean_n) {
  balanced_n <- rep(mean_n, 4)
  unbalanced_n <- as.integer(round(mean_n * c(0.5, 0.8, 1.2, 1.5)))
  stopifnot(mean(unbalanced_n) == mean_n)

  list(
    list(
      design = "balanced n, equal SD",
      n = balanced_n,
      sd = c(1, 1, 1, 1)
    ),
    list(
      design = "balanced n, unequal SD",
      n = balanced_n,
      sd = c(1, 1.3, 1.7, 2.2)
    ),
    list(
      design = "unbalanced n, equal SD",
      n = unbalanced_n,
      sd = c(1, 1, 1, 1)
    ),
    list(
      design = "unbalanced n, larger n with larger SD",
      n = unbalanced_n,
      sd = c(1, 1.3, 1.7, 2.2)
    ),
    list(
      design = "unbalanced n, larger n with smaller SD",
      n = unbalanced_n,
      sd = c(2.2, 1.7, 1.3, 1)
    )
  )
}

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
  if (length(rs) < 3) return(NA_real_)
  stats::shapiro.test(rs)$p.value
}

shape_from_skew <- function(skew) {
  if (skew == 0) Inf else (2 / skew)^2
}

draw_gamma_or_normal <- function(n, skew) {
  if (skew == 0) return(stats::rnorm(n))
  shape <- shape_from_skew(skew)
  (stats::rgamma(n, shape = shape, scale = 1) - shape) / sqrt(shape)
}

make_equal_mean_data <- function(skew, n_vec, sd_vec) {
  g <- factor(rep(seq_along(n_vec), times = n_vec))
  y <- unlist(lapply(seq_along(n_vec), function(i) {
    sd_vec[i] * draw_gamma_or_normal(n_vec[i], skew)
  }))
  list(y = y, g = g)
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
  p_sw_gate <- if (normality_met) p_levene_route else p_rank

  c(
    fisher_reject = p_fisher < alpha,
    welch_reject = p_welch < alpha,
    levene_route_reject = p_levene_route < alpha,
    rank_reject = p_rank < alpha,
    sw_reject_final = p_sw < alpha,
    sw_gate_reject = p_sw_gate < alpha,
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

run_cell <- function(skew, n_vec, sd_vec) {
  out <- replicate(NREP, {
    dat <- make_equal_mean_data(skew, n_vec, sd_vec)
    route_once(dat$y, dat$g)
  }, simplify = FALSE)

  names_out <- names(out[[1]])
  stats <- lapply(names_out, function(nm) {
    summarise_binary(vapply(out, `[[`, logical(1), nm))
  })
  names(stats) <- names_out
  unlist(stats)
}

rows <- list()
idx <- 1
for (mean_n in MEAN_NS) {
  for (condition in make_conditions(mean_n)) {
    for (skew in SKEWS) {
      res <- run_cell(skew, condition$n, condition$sd)
      shape <- shape_from_skew(skew)
      rows[[idx]] <- data.frame(
        design = condition$design,
        mean_n_per_group = mean_n,
        n_per_group = paste(condition$n, collapse = ", "),
        sd_per_group = paste(format(condition$sd, nsmall = 1), collapse = ", "),
        distribution = if (skew == 0) "normal" else "standardised Gamma",
        skew = skew,
        gamma_shape = if (is.infinite(shape)) NA_real_ else shape,
        excess_kurtosis = if (skew == 0) 0 else 1.5 * skew^2,
        groups = 4,
        group_means = "0, 0, 0, 0",
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
        "done: mean_n=%d | %s | skew=%s\n",
        mean_n,
        condition$design,
        skew
      ))
      idx <- idx + 1
    }
  }
}

sim <- do.call(rbind, rows)
design_levels <- unique(sim$design)
sim$design <- factor(sim$design, levels = design_levels)
sim$skew_label <- ifelse(
  sim$skew == 0,
  "normal\nskew = 0\nexcess kurtosis = 0",
  sprintf(
    "Gamma\nskew = %.1f\nexcess kurtosis = %.2f",
    sim$skew,
    sim$excess_kurtosis
  )
)
skew_levels <- ifelse(
  SKEWS == 0,
  "normal\nskew = 0\nexcess kurtosis = 0",
  sprintf("Gamma\nskew = %.1f\nexcess kurtosis = %.2f", SKEWS, 1.5 * SKEWS^2)
)
sim$skew_label <- factor(sim$skew_label, levels = skew_levels)

write.csv(
  sim,
  file.path(OUTDIR, "route1_equal_mean_blanca_zimmerman.csv"),
  row.names = FALSE
)
saveRDS(sim, file.path(OUTDIR, "route1_equal_mean_blanca_zimmerman.rds"))

readme <- c(
  "Route 1 equal-mean rejection simulation inspired by Blanca:2017 and Zimmerman:2004",
  paste("NREP per cell:", NREP),
  sprintf(
    "Maximum Monte Carlo SE for a percentage estimate: %.2f percentage points",
    100 * sqrt(0.25 / NREP)
  ),
  "",
  "Local PDFs read before simulation:",
  "  Blanca:2017, DOI 10.7334/psicothema2016.383",
  "  Zimmerman:2004, DOI 10.1348/000711004849222",
  "",
  "Ground truth:",
  "  all four group means are equal.",
  "",
  "Design adaptation:",
  "  Blanca-style: zero group effect, balanced/unbalanced one-way layouts,",
  "  non-normal distributions, empirical rejection rates.",
  "  Zimmerman-style: unconditional Fisher/Welch tests versus Levene-conditioned",
  "  choice, unequal variances, and variance-size pairing.",
  "",
  "Gamma caveat:",
  "  skewness and excess kurtosis vary together on the Gamma path.",
  "",
  "Files:",
  "  route1_equal_mean_blanca_zimmerman.csv",
  "  route1_equal_mean_blanca_zimmerman.rds",
  "  route1_equal_mean_gate_heatmap.png",
  "  route1_equal_mean_strategy_rejection.png"
)
writeLines(readme, file.path(OUTDIR, "README.txt"))

ggplot2 <- asNamespace("ggplot2")

strategy_labels <- c(
  "1. Fisher always" = "1. Fisher's one-way ANOVA",
  "2. Welch always" = "2. Welch's heteroscedastic one-way ANOVA",
  "3. Levene-gated Fisher/Welch" = "3. Levene-gated Fisher/Welch route",
  "4. Shapiro-Wilk routed procedure" = "4. Shapiro-Wilk routed Route 1 test",
  "5. Kruskal-Wallis always" = "5. Kruskal-Wallis rank sum test"
)

to_long <- function(dat) {
  out <- rbind(
    transform(dat, strategy = "1. Fisher always", rejection = fisher_rejection),
    transform(dat, strategy = "2. Welch always", rejection = welch_rejection),
    transform(dat, strategy = "3. Levene-gated Fisher/Welch",
              rejection = levene_route_rejection),
    transform(dat, strategy = "4. Shapiro-Wilk routed procedure",
              rejection = sw_gate_rejection),
    transform(dat, strategy = "5. Kruskal-Wallis always",
              rejection = rank_rejection)
  )
  out$strategy <- factor(out$strategy, levels = names(strategy_labels))
  out
}

sim_long <- to_long(sim)
sim_sw <- subset(sim_long, strategy == "4. Shapiro-Wilk routed procedure")
sim_other <- subset(sim_long, strategy != "4. Shapiro-Wilk routed procedure")

gate <- sim
gate$gate_label <- sprintf(
  "F %.0f%%\nW %.0f%%\nK %.0f%%",
  100 * gate$route_fisher_probability,
  100 * gate$route_welch_probability,
  100 * gate$route_rank_probability
)
gate$rejection_label <- sprintf("%.1f%%\n%s", 100 * gate$sw_gate_rejection,
                                gate$gate_label)

p_gate <- ggplot2$ggplot(
  gate,
  ggplot2$aes(x = skew_label, y = factor(mean_n_per_group),
              fill = sw_gate_rejection)
) +
  ggplot2$geom_tile(colour = "white", linewidth = 0.7) +
  ggplot2$geom_text(ggplot2$aes(label = rejection_label), size = 2.25,
                    lineheight = 0.82) +
  ggplot2$facet_wrap(stats::as.formula("~ design"), ncol = 1) +
  ggplot2$scale_fill_gradient2(
    low = "#2c7bb6",
    mid = "#ffffbf",
    high = "#d7191c",
    midpoint = ALPHA,
    labels = scales::percent,
    name = "Final-test\nrejection"
  ) +
  ggplot2$labs(
    title = "Rejection rate of the selected final Route 1 test in equal-population-mean simulations",
    subtitle = "Cell top: final-test rejection rate; below: F/W/K route probabilities",
    x = "distribution",
    y = "mean n per group",
    caption = paste(
      "Samples are drawn from distributions with equal population means.",
      "F = Fisher route, W = Welch route, K = Kruskal-Wallis route.",
      "Gamma caveat: skewness and excess kurtosis vary together."
    )
  ) +
  ggplot2$theme_minimal(base_size = 10) +
  ggplot2$theme(
    axis.text.x = ggplot2$element_text(angle = 45, hjust = 1),
    panel.grid = ggplot2$element_blank(),
    plot.caption = ggplot2$element_text(hjust = 0, size = 8.5)
  )

p_strategy <- ggplot2$ggplot() +
  ggplot2$geom_hline(yintercept = ALPHA, colour = "grey25",
                     linewidth = 0.35, linetype = "dashed") +
  ggplot2$geom_hline(yintercept = c(0.025, 0.075), colour = "grey70",
                     linewidth = 0.3, linetype = "dotted") +
  ggplot2$geom_vline(xintercept = MEAN_NS, colour = "grey88",
                     linewidth = 0.35) +
  ggplot2$geom_point(
    data = sim_other,
    ggplot2$aes(x = mean_n_per_group, y = rejection, colour = strategy,
                shape = strategy),
    size = 2.7,
    alpha = 0.55
  ) +
  ggplot2$geom_point(
    data = sim_sw,
    ggplot2$aes(x = mean_n_per_group, y = rejection, colour = strategy,
                shape = strategy),
    size = 2.7,
    stroke = 1.0,
    alpha = 0.75
  ) +
  ggplot2$facet_grid(stats::as.formula("design ~ skew_label"),
                     scales = "free_y") +
  ggplot2$scale_y_continuous(labels = scales::percent,
                             expand = ggplot2$expansion(mult = c(0.02, 0.08))) +
  ggplot2$scale_x_log10(breaks = MEAN_NS) +
  ggplot2$scale_shape_manual(
    values = c(
      "1. Fisher always" = 16,
      "2. Welch always" = 17,
      "3. Levene-gated Fisher/Welch" = 15,
      "4. Shapiro-Wilk routed procedure" = 5,
      "5. Kruskal-Wallis always" = 18
    ),
    breaks = names(strategy_labels),
    labels = strategy_labels
  ) +
  ggplot2$scale_colour_discrete(
    breaks = names(strategy_labels),
    labels = strategy_labels
  ) +
  ggplot2$labs(
    title = "Equal-mean rejection rates across Route 1 strategies",
    subtitle = "Dashed line: alpha = 5%; dotted lines: Bradley 2.5%-7.5% interval",
    x = "mean n per group",
    y = "simulated rejection rate",
    colour = "test strategy",
    shape = "test strategy",
    caption = paste(
      "All four group means are equal.",
      "Kruskal-Wallis rejection is not a Type I error for its own null when rank distributions differ."
    )
  ) +
  ggplot2$theme_minimal(base_size = 9) +
  ggplot2$theme(
    legend.position = "right",
    axis.text.x = ggplot2$element_text(angle = 45, hjust = 1),
    panel.grid.major.x = ggplot2$element_blank(),
    panel.grid.minor.x = ggplot2$element_blank(),
    panel.grid.major.y = ggplot2$element_blank(),
    panel.grid.minor.y = ggplot2$element_blank(),
    panel.border = ggplot2$element_rect(colour = "black", fill = NA, linewidth = 0.25),
    plot.caption = ggplot2$element_text(hjust = 0, size = 8)
  )

ggplot2$ggsave(file.path(OUTDIR, "route1_equal_mean_gate_heatmap.png"),
               p_gate, width = 12, height = 15, dpi = 180)
ggplot2$ggsave(file.path(OUTDIR, "route1_equal_mean_strategy_rejection.png"),
               p_strategy, width = 18, height = 13, dpi = 180)

message("Outputs written to: ", OUTDIR)
