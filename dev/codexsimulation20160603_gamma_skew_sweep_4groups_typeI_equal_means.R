## ---------------------------------------------------------------------------
## Four-group Gamma/normal skew sweep for equal-mean rejection rates.
##
## Question tested:
##   rejection probability when all group means are equal.
##
## Design:
##   four groups with equal means;
##   skew values: 0, 0.1, 0.5, 1, 2, 6;
##   variance scenarios:
##     equal variances: sd = 1, 1, 1, 1
##     unequal variances: sd = 1, 1.5, 2, 2.5
##
## Caveat:
##   In the unequal-variance row, Kruskal-Wallis is not testing equality of
##   means. Its rejection rate is therefore an equal-mean rejection rate, not a
##   Type I error rate for the Kruskal-Wallis null.
## ---------------------------------------------------------------------------

set.seed(20260603)

args <- commandArgs(trailingOnly = TRUE)
NREP <- if (length(args) >= 1) as.integer(args[1]) else 4000
ALPHA <- 0.05
SKEWS <- c(0, 0.1, 0.5, 1, 2, 6)
NS <- c(10, 20, 50, 100)
VARIANCE_SCENARIOS <- list(
  "equal variances\nSD: 1, 1, 1, 1" = c(1, 1, 1, 1),
  "unequal variances\nSD: 1, 1.5, 2, 2.5" = c(1, 1.5, 2, 2.5)
)
OUTDIR <- file.path(
  "dev",
  "codexsimulation20160603_gamma_skew_sweep_4groups_typeI_outputs"
)
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

stopifnot(NREP > 0)
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required.")
}
if (!requireNamespace("scales", quietly = TRUE)) {
  stop("Package 'scales' is required.")
}

levene_p <- function(y, g) {
  g <- factor(g)
  z <- abs(y - ave(y, g, FUN = mean))
  stats::anova(stats::lm(z ~ g))[["Pr(>F)"]][1]
}

normality_p <- function(y, g) {
  model <- stats::lm(y ~ factor(g))
  raw <- stats::residuals(model)
  rs <- suppressWarnings(stats::rstandard(model))
  if (any(!is.finite(rs))) {
    rs <- raw / max(stats::sigma(model), 1e-8)
  }
  rs <- rs[is.finite(rs)]
  if (length(rs) < 3) return(NA_real_)
  stats::shapiro.test(rs)$p.value
}

route_once <- function(y, g, alpha = ALPHA) {
  g <- factor(g)
  p_norm <- normality_p(y, g)
  normality_met <- is.na(p_norm) || p_norm >= alpha
  p_lev <- levene_p(y, g)
  equal_var <- is.na(p_lev) || p_lev >= alpha

  p_fisher <- stats::anova(stats::lm(y ~ g))[["Pr(>F)"]][1]
  p_welch <- stats::oneway.test(y ~ g, var.equal = FALSE)$p.value
  p_mean <- if (equal_var) p_fisher else p_welch
  p_rank <- stats::kruskal.test(y, g)$p.value
  p_gate <- if (normality_met) p_mean else p_rank

  c(
    fisher_reject = p_fisher < alpha,
    welch_reject = p_welch < alpha,
    mean_reject = p_mean < alpha,
    rank_reject = p_rank < alpha,
    gate_reject = p_gate < alpha,
    route_rank = !normality_met,
    route_fisher = normality_met && equal_var,
    route_welch = normality_met && !equal_var
  )
}

shape_from_skew <- function(skew) {
  if (skew == 0) Inf else (2 / skew)^2
}

draw_gamma_or_normal <- function(n, skew) {
  if (skew == 0) return(stats::rnorm(n))
  shape <- shape_from_skew(skew)
  (stats::rgamma(n, shape = shape, scale = 1) - shape) / sqrt(shape)
}

make_equal_mean_data <- function(skew, n, sds) {
  k <- length(sds)
  g <- factor(rep(seq_len(k), each = n))
  y <- unlist(lapply(seq_len(k), function(i) {
    sds[i] * draw_gamma_or_normal(n, skew)
  }))
  list(y = y, g = g)
}

summarise_binary <- function(values) {
  p <- mean(values)
  se <- sqrt(p * (1 - p) / length(values))
  c(rate = p, mc_se = se)
}

run_cell <- function(skew, n, sds) {
  out <- replicate(NREP, {
    dat <- make_equal_mean_data(skew, n, sds)
    route_once(dat$y, dat$g)
  }, simplify = FALSE)

  fisher_reject <- vapply(out, `[[`, logical(1), "fisher_reject")
  welch_reject <- vapply(out, `[[`, logical(1), "welch_reject")
  mean_reject <- vapply(out, `[[`, logical(1), "mean_reject")
  rank_reject <- vapply(out, `[[`, logical(1), "rank_reject")
  gate_reject <- vapply(out, `[[`, logical(1), "gate_reject")
  route_rank <- vapply(out, `[[`, logical(1), "route_rank")
  route_fisher <- vapply(out, `[[`, logical(1), "route_fisher")
  route_welch <- vapply(out, `[[`, logical(1), "route_welch")

  c(
    fisher = summarise_binary(fisher_reject),
    welch = summarise_binary(welch_reject),
    mean = summarise_binary(mean_reject),
    rank = summarise_binary(rank_reject),
    gate = summarise_binary(gate_reject),
    route_rank = summarise_binary(route_rank),
    route_fisher = summarise_binary(route_fisher),
    route_welch = summarise_binary(route_welch)
  )
}

rows <- list()
idx <- 1
for (variance_name in names(VARIANCE_SCENARIOS)) {
  sds <- VARIANCE_SCENARIOS[[variance_name]]
  for (n in NS) {
    for (skew in SKEWS) {
      res <- run_cell(skew, n, sds)
      shape <- shape_from_skew(skew)
      rows[[idx]] <- data.frame(
        variance_scenario = variance_name,
        group_sds = paste(format(sds, nsmall = 1), collapse = ", "),
        distribution = if (skew == 0) "normal" else "standardised Gamma",
        skew = skew,
        gamma_shape = if (is.infinite(shape)) NA_real_ else shape,
        excess_kurtosis = if (skew == 0) 0 else 1.5 * skew^2,
        n_per_group = n,
        groups = 4,
        group_means = "0, 0, 0, 0",
        fisher_rejection = res["fisher.rate"],
        fisher_mc_se = res["fisher.mc_se"],
        welch_rejection = res["welch.rate"],
        welch_mc_se = res["welch.mc_se"],
        mean_route_rejection = res["mean.rate"],
        mean_route_mc_se = res["mean.mc_se"],
        rank_rejection = res["rank.rate"],
        rank_mc_se = res["rank.mc_se"],
        gate_rejection = res["gate.rate"],
        gate_mc_se = res["gate.mc_se"],
        route_rank_probability = res["route_rank.rate"],
        route_rank_mc_se = res["route_rank.mc_se"],
        route_fisher_probability = res["route_fisher.rate"],
        route_fisher_mc_se = res["route_fisher.mc_se"],
        route_welch_probability = res["route_welch.rate"],
        route_welch_mc_se = res["route_welch.mc_se"],
        row.names = NULL
      )
      cat(sprintf("done: %s n=%d skew=%s\n", variance_name, n, skew))
      idx <- idx + 1
    }
  }
}

type1 <- do.call(rbind, rows)
type1$skew_label <- ifelse(
  type1$skew == 0,
  "normal\nskew = 0\nexcess kurtosis = 0",
  sprintf(
    "Gamma\nskew = %.1f\nexcess kurtosis = %.2f",
    type1$skew,
    type1$excess_kurtosis
  )
)
skew_levels <- ifelse(
  SKEWS == 0,
  "normal\nskew = 0\nexcess kurtosis = 0",
  sprintf("Gamma\nskew = %.1f\nexcess kurtosis = %.2f", SKEWS, 1.5 * SKEWS^2)
)
type1$skew_label <- factor(type1$skew_label, levels = skew_levels)
type1$variance_scenario <- factor(
  type1$variance_scenario,
  levels = names(VARIANCE_SCENARIOS)
)

write.csv(
  type1,
  file.path(OUTDIR, "gamma_skew_sweep_4groups_typeI_equal_means.csv"),
  row.names = FALSE
)
saveRDS(
  type1,
  file.path(OUTDIR, "gamma_skew_sweep_4groups_typeI_equal_means.rds")
)

readme <- c(
  "Four-group Gamma/normal skew sweep for equal-mean rejection rates",
  paste("NREP per cell:", NREP),
  sprintf(
    "Maximum Monte Carlo SE for a percentage estimate: %.2f percentage points",
    100 * sqrt(0.25 / NREP)
  ),
  "",
  "All group means are equal.",
  "Rows compare equal variances with unequal variances.",
  "Bottom labels in the main plot show gate route probabilities:",
  "  F = Fisher route, W = Welch route, K = Kruskal-Wallis route.",
  "",
  "Important interpretation caveat:",
  "  In the unequal-variance row, Kruskal-Wallis is not testing equality of means.",
  "  Its rejection rate is an equal-mean rejection rate, not a Type I error",
  "  rate for the Kruskal-Wallis null.",
  "",
  "Files:",
  "  gamma_skew_sweep_4groups_typeI_equal_means.csv",
  "  gamma_skew_sweep_4groups_typeI_equal_means.rds",
  "  gamma_skew_sweep_4groups_typeI_equal_means.png",
  "  gamma_skew_sweep_4groups_typeI_gate_routes.png"
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
              rejection = mean_route_rejection),
    transform(dat, strategy = "4. Shapiro-Wilk routed procedure",
              rejection = gate_rejection),
    transform(dat, strategy = "5. Kruskal-Wallis always",
              rejection = rank_rejection)
  )
  out$strategy <- factor(out$strategy, levels = names(strategy_labels))
  out
}

type1_long <- to_long(type1)
type1_sw <- subset(type1_long, strategy == "4. Shapiro-Wilk routed procedure")
type1_other <- subset(type1_long, strategy != "4. Shapiro-Wilk routed procedure")
gate_labels <- type1
gate_labels$gate_label <- sprintf(
  "F %.0f%%\nW %.0f%%\nK %.0f%%",
  100 * gate_labels$route_fisher_probability,
  100 * gate_labels$route_welch_probability,
  100 * gate_labels$route_rank_probability
)
p_type1 <- ggplot2$ggplot() +
  ggplot2$geom_hline(
    yintercept = ALPHA,
    colour = "grey25",
    linewidth = 0.35,
    linetype = "dashed"
  ) +
  ggplot2$geom_vline(
    xintercept = NS,
    colour = "grey88",
    linewidth = 0.35
  ) +
  ggplot2$geom_point(
    data = type1_other,
    ggplot2$aes(x = n_per_group, y = rejection, colour = strategy,
                shape = strategy),
    size = 3.2,
    alpha = 0.55
  ) +
  ggplot2$geom_point(
    data = type1_sw,
    ggplot2$aes(x = n_per_group, y = rejection, colour = strategy,
                shape = strategy),
    size = 3.2,
    stroke = 1.1,
    alpha = 0.75
  ) +
  ggplot2$geom_text(
    data = gate_labels,
    ggplot2$aes(x = n_per_group, y = 0.012, label = gate_label),
    colour = "grey25",
    size = 2.0,
    lineheight = 0.82
  ) +
  ggplot2$facet_grid(
    stats::as.formula("variance_scenario ~ skew_label"),
    scales = "free_y"
  ) +
  ggplot2$scale_y_continuous(
    labels = scales::percent,
    expand = ggplot2$expansion(mult = c(0.02, 0.08))
  ) +
  ggplot2$scale_x_log10(breaks = NS) +
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
    title = "Equal-mean rejection rates under Route 1 gate simulation",
    subtitle = "Dashed line marks alpha = 5%; all four group means are equal",
    x = "n per group",
    y = "simulated rejection rate",
    colour = "test strategy",
    shape = "test strategy",
    caption = paste(
      "Bottom labels show gate decisions: F = Fisher, W = Welch, K = Kruskal-Wallis.",
      "In the unequal-variance row, Kruskal-Wallis rejection is not a Type I error for its own null, because the rank distributions differ."
    )
  ) +
  ggplot2$theme_minimal(base_size = 10) +
  ggplot2$theme(
    legend.position = "right",
    axis.text.x = ggplot2$element_text(angle = 45, hjust = 1),
    panel.grid.major.x = ggplot2$element_blank(),
    panel.grid.minor.x = ggplot2$element_blank(),
    panel.grid.major.y = ggplot2$element_blank(),
    panel.grid.minor.y = ggplot2$element_blank(),
    panel.border = ggplot2$element_rect(colour = "black", fill = NA, linewidth = 0.25),
    plot.caption = ggplot2$element_text(hjust = 0, size = 8.5)
  )

p_route <- ggplot2$ggplot(
  type1,
  ggplot2$aes(x = skew_label, y = factor(n_per_group), fill = route_rank_probability)
) +
  ggplot2$geom_tile(colour = "white", linewidth = 0.7) +
  ggplot2$geom_text(
    ggplot2$aes(label = sprintf("%.0f%%", 100 * route_rank_probability)),
    size = 3.0
  ) +
  ggplot2$scale_fill_gradient(
    limits = c(0, 1),
    low = "#edf8fb",
    high = "#006d2c",
    labels = scales::percent,
    name = "% routed\nto Kruskal-Wallis"
  ) +
  ggplot2$facet_wrap(stats::as.formula("~ variance_scenario"), nrow = 2) +
  ggplot2$labs(
    title = "Route 1 probability of taking the Kruskal-Wallis branch",
    subtitle = "Cells show how often Shapiro-Wilk rejects residual normality",
    x = "residual distribution",
    y = "n per group"
  ) +
  ggplot2$theme_minimal(base_size = 11) +
  ggplot2$theme(
    axis.text.x = ggplot2$element_text(angle = 45, hjust = 1),
    panel.grid = ggplot2$element_blank()
  )

ggplot2$ggsave(
  file.path(OUTDIR, "gamma_skew_sweep_4groups_typeI_equal_means.png"),
  p_type1,
  width = 16,
  height = 8.8,
  dpi = 180
)
ggplot2$ggsave(
  file.path(OUTDIR, "gamma_skew_sweep_4groups_typeI_gate_routes.png"),
  p_route,
  width = 12,
  height = 7.2,
  dpi = 180
)

message("Outputs written to: ", OUTDIR)
