## ---------------------------------------------------------------------------
## Four-group Gamma/normal skew sweep for Route 1 power.
##
## Question tested:
##   power of the omnibus group comparison to detect an ordered group pattern.
##
## Design:
##   four unbalanced groups with ordered mean offsets:
##     moderate: 0, 0.25, 0.50, 0.75 SD.
##     strong:   0, 0.50, 1.00, 1.50 SD.
##   group sizes: n_i = round(mean_n * c(0.5, 0.8, 1.2, 1.5)).
##   skew values: 0, 0.5, 1, 2.
##
## Tests:
##   F: Fisher's one-way ANOVA.
##   W: Welch's heteroscedastic ANOVA.
##   L: mean-centred Levene selects F vs W.
##   KW: Kruskal-Wallis branch.
##   SW: Shapiro-Wilk selects W vs KW.
##   SW+L: Shapiro-Wilk selects KW or, if parametric branch is retained,
##         Levene selects F vs W.
##
## Caveat:
##   For skew > 0, data are standardised Gamma draws. Along this Gamma path,
##   skewness and excess kurtosis vary together.
## ---------------------------------------------------------------------------

set.seed(20260603)

args <- commandArgs(trailingOnly = TRUE)
NREP <- if (length(args) >= 1) as.integer(args[1]) else 50000
ALPHA <- 0.05
SHIFT_SCENARIOS <- list(
  "moderate ordered effect: 0, 0.25, 0.50, 0.75 SD" = c(0, 0.25, 0.50, 0.75)
  ## Uncomment for a stronger, near-trivial power condition.
  ## ,
  ## "strong ordered effect: 0, 0.50, 1.00, 1.50 SD" = c(0, 0.50, 1.00, 1.50)
)
OUTDIR <- file.path("dev", "codexsimulation20160606_gamma_skew_sweep_4groups_power_skew4_unbalanced_B50000_outputs")
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

stopifnot(NREP > 0)
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required.")
}
if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Package 'pkgload' is required.")
}
pkgload::load_all(".", quiet = TRUE)

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

route_once <- function(y, g, alpha = ALPHA) {
  g <- factor(g)
  fit <- standardised_residuals(y, g)
  p_norm <- normality_p(fit$rs)
  normality_met <- is.na(p_norm) || p_norm >= alpha
  p_lev <- levene_p(fit$rs, g)
  equal_var <- is.na(p_lev) || p_lev >= alpha

  p_fisher <- summary(fit$model)[[1]][["Pr(>F)"]][1]
  p_welch <- stats::oneway.test(y ~ g, var.equal = FALSE)$p.value
  p_mean <- if (equal_var) p_fisher else p_welch
  p_rank <- stats::kruskal.test(y, g)$p.value
  p_sw <- if (normality_met) p_welch else p_rank
  p_gate <- if (normality_met) p_mean else p_rank

  c(fisher_reject = p_fisher < alpha,
    welch_reject = p_welch < alpha,
    mean_reject = p_mean < alpha,
    rank_reject = p_rank < alpha,
    sw_reject = p_sw < alpha,
    gate_reject = p_gate < alpha,
    sw_route_welch = normality_met,
    sw_route_rank = !normality_met,
    route_rank = !normality_met,
    route_fisher = normality_met && equal_var,
    route_welch = normality_met && !equal_var,
    mean_route_fisher = equal_var,
    mean_route_welch = !equal_var,
    fisher_selected_reject = equal_var && p_fisher < alpha,
    welch_selected_reject = !equal_var && p_welch < alpha,
    levene_reject = !equal_var)
}

shape_from_skew <- function(skew) {
  if (skew == 0) Inf else (2 / skew)^2
}

draw_gamma_or_normal <- function(n, skew) {
  if (skew == 0) return(stats::rnorm(n))
  shape <- shape_from_skew(skew)
  (stats::rgamma(n, shape = shape, scale = 1) - shape) / sqrt(shape)
}

make_shift_data <- function(skew, n_vec, shifts) {
  k <- length(shifts)
  g <- factor(rep(seq_len(k), times = n_vec))
  y <- unlist(lapply(seq_len(k), function(i) {
    draw_gamma_or_normal(n_vec[i], skew) + shifts[i]
  }))
  list(y = y, g = g)
}

summarise_binary <- function(values) {
  p <- mean(values)
  se <- sqrt(p * (1 - p) / length(values))
  c(rate = p, mc_se = se)
}

run_cell <- function(skew, n_vec, shifts) {
  out <- replicate(NREP, {
    dat <- make_shift_data(skew, n_vec, shifts)
    route_once(dat$y, dat$g)
  }, simplify = FALSE)

  fisher_reject <- vapply(out, `[[`, logical(1), "fisher_reject")
  welch_reject <- vapply(out, `[[`, logical(1), "welch_reject")
  mean_reject <- vapply(out, `[[`, logical(1), "mean_reject")
  rank_reject <- vapply(out, `[[`, logical(1), "rank_reject")
  sw_reject <- vapply(out, `[[`, logical(1), "sw_reject")
  gate_reject <- vapply(out, `[[`, logical(1), "gate_reject")
  sw_route_welch <- vapply(out, `[[`, logical(1), "sw_route_welch")
  sw_route_rank <- vapply(out, `[[`, logical(1), "sw_route_rank")
  route_rank <- vapply(out, `[[`, logical(1), "route_rank")
  route_fisher <- vapply(out, `[[`, logical(1), "route_fisher")
  route_welch <- vapply(out, `[[`, logical(1), "route_welch")
  mean_route_fisher <- vapply(out, `[[`, logical(1), "mean_route_fisher")
  mean_route_welch <- vapply(out, `[[`, logical(1), "mean_route_welch")
  fisher_selected_reject <- vapply(out, `[[`, logical(1), "fisher_selected_reject")
  welch_selected_reject <- vapply(out, `[[`, logical(1), "welch_selected_reject")
  levene_reject <- vapply(out, `[[`, logical(1), "levene_reject")

  c(fisher = summarise_binary(fisher_reject),
    welch = summarise_binary(welch_reject),
    mean = summarise_binary(mean_reject),
    rank = summarise_binary(rank_reject),
    sw = summarise_binary(sw_reject),
    gate = summarise_binary(gate_reject),
    sw_route_welch = summarise_binary(sw_route_welch),
    sw_route_rank = summarise_binary(sw_route_rank),
    route_rank = summarise_binary(route_rank),
    route_fisher = summarise_binary(route_fisher),
    route_welch = summarise_binary(route_welch),
    mean_route_fisher = summarise_binary(mean_route_fisher),
    mean_route_welch = summarise_binary(mean_route_welch),
    fisher_selected_reject = summarise_binary(fisher_selected_reject),
    welch_selected_reject = summarise_binary(welch_selected_reject),
    levene_reject = summarise_binary(levene_reject))
}

skews <- c(0, 0.5, 1, 2)
ns <- c(10, 20, 50, 100, 200)

rows <- list()
idx <- 1
for (n in ns) {
  n_vec <- as.integer(round(n * c(0.5, 0.8, 1.2, 1.5)))
  for (skew in skews) {
    for (scenario_name in names(SHIFT_SCENARIOS)) {
      shifts <- SHIFT_SCENARIOS[[scenario_name]]
      res <- run_cell(skew, n_vec, shifts)
      shape <- shape_from_skew(skew)
      rows[[idx]] <- data.frame(
        scenario = paste("four groups with", scenario_name),
        effect_size = scenario_name,
        group_mean_offsets = paste(format(shifts, nsmall = 2), collapse = ", "),
        distribution = if (skew == 0) "normal" else "standardised Gamma",
        skew = skew,
        gamma_shape = if (is.infinite(shape)) NA_real_ else shape,
        excess_kurtosis = if (skew == 0) 0 else 1.5 * skew^2,
        n_per_group = n,
        group_sizes = paste(n_vec, collapse = ", "),
        groups = 4,
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
        fisher_selected_reject_probability = res["fisher_selected_reject.rate"],
        fisher_selected_reject_mc_se = res["fisher_selected_reject.mc_se"],
        welch_selected_reject_probability = res["welch_selected_reject.rate"],
        welch_selected_reject_mc_se = res["welch_selected_reject.mc_se"],
        levene_reject_probability = res["levene_reject.rate"],
        levene_reject_mc_se = res["levene_reject.mc_se"],
        row.names = NULL
      )
      cat(sprintf("done: n=%d skew=%s %s\n", n, skew, scenario_name))
      idx <- idx + 1
    }
  }
}

power <- do.call(rbind, rows)
skew_label <- function(skew) {
  if (skew == 0) {
    return("Normal(mu=0, sigma^2=1)\nskew=0, excess kurtosis=0")
  }
  shape <- shape_from_skew(skew)
  sprintf(
    "standardised Gamma(\u03b1=%s, \u03b8=1)\nskew=%s, excess kurtosis=%s",
    format(shape, trim = TRUE, scientific = FALSE),
    format(skew, trim = TRUE, scientific = FALSE),
    format(round(6 / shape, 3), trim = TRUE, scientific = FALSE)
  )
}
power$skew_label <- vapply(power$skew, skew_label, character(1))
skew_levels <- vapply(skews, skew_label, character(1))
power$skew_label <- factor(power$skew_label, levels = skew_levels)
power$n_label <- factor(paste0("n = ", power$n_per_group),
                        levels = paste0("n = ", ns))
power$effect_size <- factor(power$effect_size, levels = names(SHIFT_SCENARIOS))

write.csv(power, file.path(OUTDIR, "gamma_skew_sweep_4groups_power.csv"),
          row.names = FALSE)
saveRDS(power, file.path(OUTDIR, "gamma_skew_sweep_4groups_power.rds"))

readme <- c(
  "Four-group Gamma/normal skew sweep for Route 1 power",
  paste("NREP per cell:", NREP),
  sprintf("Maximum Monte Carlo SE for a percentage estimate: %.2f percentage points",
          100 * sqrt(0.25 / NREP)),
  paste("alpha:", ALPHA),
  "Group mean offsets:",
  paste(" ", names(SHIFT_SCENARIOS)),
  "",
  "Power means: probability of p < alpha for detecting the omnibus group pattern.",
  "Group sizes: n_i = round(mean_n * c(0.5, 0.8, 1.2, 1.5)); equal SD.",
  "F: Fisher's one-way ANOVA.",
  "W: Welch's heteroscedastic ANOVA.",
  "L: mean-centred Levene selects F vs W.",
  "KW: Kruskal-Wallis branch.",
  "SW: Shapiro-Wilk selects W vs KW.",
  "SW+L: Shapiro-Wilk selects KW or, if parametric branch is retained,",
  "  Levene selects F vs W.",
  "In this homoscedastic design, Welch selections are false-positive Levene routes.",
  "",
  "Skew = 0 is normal. Skew > 0 uses the standardised Gamma path.",
  "Gamma path caveat: skewness and excess kurtosis vary together.",
  "",
  "Files:",
  "  gamma_skew_sweep_4groups_power.csv",
  "  gamma_skew_sweep_4groups_power.rds",
  "  gamma_skew_sweep_4groups_power.png",
  "  gamma_skew_sweep_4groups_parametric_branch_power.png",
  "  gamma_skew_sweep_4groups_route_probability.png",
  "  gamma_skew_sweep_4groups_fisher_welch_route_probability.png"
)
writeLines(readme, file.path(OUTDIR, "README.txt"))

ggplot2 <- asNamespace("ggplot2")

to_long <- function(dat) {
  out <- rbind(
    transform(dat, strategy = "1. Fisher always", power = fisher_power),
    transform(dat, strategy = "2. Welch always", power = welch_power),
    transform(dat, strategy = "3. Levene-gated Fisher/Welch", power = mean_power),
    transform(dat, strategy = "4. Kruskal-Wallis always", power = rank_power),
    transform(dat, strategy = "5. Shapiro-Wilk routed Welch/KW", power = sw_power),
    transform(dat, strategy = "6. Shapiro-Wilk plus Levene", power = gate_power)
  )
  out$strategy <- factor(
    out$strategy,
    levels = c(
      "1. Fisher always",
      "2. Welch always",
      "3. Levene-gated Fisher/Welch",
      "4. Kruskal-Wallis always",
      "5. Shapiro-Wilk routed Welch/KW",
      "6. Shapiro-Wilk plus Levene"
    )
  )
  out
}

power_long <- to_long(power)
power_plot <- power_long[power_long$n_per_group %in% c(10, 20, 50, 100), ,
                         drop = FALSE]
power_sw <- subset(power_plot, strategy %in% c(
  "5. Shapiro-Wilk routed Welch/KW",
  "6. Shapiro-Wilk plus Levene"
))
power_sw_only <- subset(
  power_plot,
  strategy == "5. Shapiro-Wilk routed Welch/KW"
)
power_sw_l <- subset(
  power_plot,
  strategy == "6. Shapiro-Wilk plus Levene"
)
power_other <- subset(power_plot, !(strategy %in% c(
  "5. Shapiro-Wilk routed Welch/KW",
  "6. Shapiro-Wilk plus Levene"
)))
gate_labels <- power[power$n_per_group %in% c(10, 20, 50, 100), ,
                     drop = FALSE]
gate_labels$gate_label <- sprintf(
  "F %.0f%%\nW %.0f%%\nKW %.0f%%",
  100 * gate_labels$route_fisher_probability,
  100 * gate_labels$route_welch_probability,
  100 * gate_labels$route_rank_probability
)

parametric_long <- rbind(
  transform(power, strategy = "Fisher always", power = fisher_power),
  transform(power, strategy = "Welch always", power = welch_power),
  transform(power, strategy = "Levene-gated Fisher/Welch", power = mean_power)
)
parametric_long$strategy <- factor(
  parametric_long$strategy,
  levels = c("Fisher always", "Welch always", "Levene-gated Fisher/Welch")
)

p_power <- ggplot2$ggplot() +
  ggplot2$geom_vline(
    xintercept = c(10, 20, 50, 100),
    colour = "grey88",
    linewidth = 0.35
  ) +
  ggplot2$geom_point(
    data = power_other,
    ggplot2$aes(x = n_per_group, y = power, colour = strategy,
                shape = strategy, group = strategy),
    size = 3.4,
    alpha = 0.55
  ) +
  ggplot2$geom_point(
    data = power_sw_only,
    ggplot2$aes(x = n_per_group, y = power, colour = strategy,
                shape = strategy, group = strategy),
    size = 3.4,
    stroke = 1.1,
    alpha = 0.75
  ) +
  ggplot2$geom_point(
    data = power_sw_l,
    ggplot2$aes(x = n_per_group, y = power, colour = strategy,
                shape = strategy, group = strategy),
    size = 5.0,
    stroke = 1.2,
    alpha = 0.75
  ) +
  ggplot2$geom_text(
    data = gate_labels,
    ggplot2$aes(x = n_per_group, y = 0.06, label = gate_label),
    colour = "grey25",
    size = 2.1,
    lineheight = 0.82
  ) +
  ggplot2$facet_grid(stats::as.formula("effect_size ~ skew_label")) +
  ggplot2$scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  ggplot2$scale_x_log10(breaks = c(10, 20, 50, 100)) +
  ggplot2$scale_shape_manual(
    values = c("1. Fisher always" = 16,
               "2. Welch always" = 17,
               "3. Levene-gated Fisher/Welch" = 15,
               "4. Kruskal-Wallis always" = 18,
               "5. Shapiro-Wilk routed Welch/KW" = 1,
               "6. Shapiro-Wilk plus Levene" = 5),
    breaks = c(
      "1. Fisher always",
      "2. Welch always",
      "3. Levene-gated Fisher/Welch",
      "4. Kruskal-Wallis always",
      "5. Shapiro-Wilk routed Welch/KW",
      "6. Shapiro-Wilk plus Levene"
    ),
    labels = c(
      "1. Fisher always" = "F",
      "2. Welch always" = "W",
      "3. Levene-gated Fisher/Welch" = "L",
      "4. Kruskal-Wallis always" = "KW",
      "5. Shapiro-Wilk routed Welch/KW" = "SW",
      "6. Shapiro-Wilk plus Levene" = "SW+L"
    )
  ) +
  ggplot2$scale_colour_discrete(
    breaks = c(
      "1. Fisher always",
      "2. Welch always",
      "3. Levene-gated Fisher/Welch",
      "4. Kruskal-Wallis always",
      "5. Shapiro-Wilk routed Welch/KW",
      "6. Shapiro-Wilk plus Levene"
    ),
    labels = c(
      "1. Fisher always" = "F",
      "2. Welch always" = "W",
      "3. Levene-gated Fisher/Welch" = "L",
      "4. Kruskal-Wallis always" = "KW",
      "5. Shapiro-Wilk routed Welch/KW" = "SW",
      "6. Shapiro-Wilk plus Levene" = "SW+L"
    )
  ) +
  ggplot2$labs(
    title = "Five testing strategies under additive mean shifts",
    subtitle = "Parametric H0: mu1 = ... = mu4; Kruskal-Wallis H0: equal rank distributions",
    x = "n per group",
    y = "simulated rejection rate",
    colour = "test strategy",
    shape = "test strategy"
  ) +
  ggplot2$theme_minimal(base_size = 11) +
  ggplot2$theme(
    legend.position = "right",
    axis.text.x = ggplot2$element_text(angle = 45, hjust = 1),
    panel.grid.major.x = ggplot2$element_blank(),
    panel.grid.minor.x = ggplot2$element_blank(),
    panel.grid.major.y = ggplot2$element_blank(),
    panel.grid.minor.y = ggplot2$element_blank(),
    panel.border = ggplot2$element_rect(colour = "black", fill = NA, linewidth = 0.25),
    plot.caption = ggplot2$element_text(hjust = 0, size = 9)
  )

p_route <- ggplot2$ggplot(
  power,
  ggplot2$aes(x = skew_label, y = n_label, fill = route_rank_probability)
) +
  ggplot2$geom_tile(colour = "white", linewidth = 0.7) +
  ggplot2$geom_text(
    ggplot2$aes(label = sprintf("%.0f%%", 100 * route_rank_probability)),
    size = 3.1
  ) +
  ggplot2$scale_fill_gradient(
    limits = c(0, 1),
    low = "#edf8fb",
    high = "#006d2c",
    labels = scales::percent,
    name = "% routed\nto rank test"
  ) +
  ggplot2$facet_wrap(stats::as.formula("~ effect_size"), nrow = 2) +
  ggplot2$labs(
    title = "Four-group Route 1 probability of using Kruskal-Wallis",
    subtitle = "Cells show how often Shapiro-Wilk rejects standardised residual normality",
    x = "distribution shape",
    y = "n per group",
    caption = paste(
      "Two ordered-effect conditions are shown.",
      "Gamma caveat: skewness and excess kurtosis vary together."
    )
  ) +
  ggplot2$theme_minimal(base_size = 11) +
  ggplot2$theme(
    axis.text.x = ggplot2$element_text(size = 7.5),
    legend.position = "right",
    plot.caption = ggplot2$element_text(hjust = 0, size = 9)
  )

p_parametric <- ggplot2$ggplot(
  parametric_long,
  ggplot2$aes(x = n_per_group, y = power, colour = strategy,
              linetype = strategy, shape = strategy, group = strategy)
) +
  ggplot2$geom_line(linewidth = 0.85) +
  ggplot2$geom_point(size = 2.4, fill = "white", stroke = 0.9) +
  ggplot2$facet_grid(stats::as.formula("effect_size ~ skew_label")) +
  ggplot2$scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  ggplot2$scale_x_log10(breaks = ns) +
  ggplot2$scale_linetype_manual(
    values = c("Fisher always" = "solid",
               "Welch always" = "longdash",
               "Levene-gated Fisher/Welch" = "dotted")
  ) +
  ggplot2$scale_shape_manual(
    values = c("Fisher always" = 21,
               "Welch always" = 24,
               "Levene-gated Fisher/Welch" = 22)
  ) +
  ggplot2$labs(
    title = "Parametric branch power under true homoscedasticity",
    subtitle = "Fisher always, Welch always, and Levene-gated Fisher/Welch",
    x = "n per group",
    y = "simulated rejection rate",
    colour = "parametric strategy",
    linetype = "parametric strategy",
    shape = "parametric strategy",
    caption = paste(
      "The simulated groups have equal variances; Welch selections are false-positive Levene routes.",
      "Skew = 0 is normal; skew > 0 uses standardised Gamma groups."
    )
  ) +
  ggplot2$theme_minimal(base_size = 11) +
  ggplot2$theme(
    legend.position = "right",
    axis.text.x = ggplot2$element_text(angle = 45, hjust = 1),
    plot.caption = ggplot2$element_text(hjust = 0, size = 9)
  )

ggplot2$ggsave(file.path(OUTDIR, "gamma_skew_sweep_4groups_power.png"),
               p_power, width = 14, height = 7.8, dpi = 180)
ggplot2$ggsave(file.path(OUTDIR, "gamma_skew_sweep_4groups_parametric_branch_power.png"),
               p_parametric, width = 14, height = 7.8, dpi = 180)
ggplot2$ggsave(file.path(OUTDIR, "gamma_skew_sweep_4groups_route_probability.png"),
               p_route, width = 12, height = 7.2, dpi = 180)

p_fisher_welch <- ggplot2$ggplot(
  power,
  ggplot2$aes(x = skew_label, y = n_label, fill = route_welch_probability)
) +
  ggplot2$geom_tile(colour = "white", linewidth = 0.7) +
  ggplot2$geom_text(
    ggplot2$aes(label = sprintf("%.1f%%", 100 * route_welch_probability)),
    size = 3.1
  ) +
  ggplot2$scale_fill_gradient(
    limits = c(0, 0.20),
    low = "#f7fbff",
    high = "#08519c",
    labels = scales::percent,
    name = "% routed\nto Welch"
  ) +
  ggplot2$labs(
    title = "False Welch routing in the homoscedastic simulation",
    subtitle = "Cells show Levene false positives after passing the Shapiro-Wilk gate",
    x = "standardised residual distribution",
    y = "n per group",
    caption = paste(
      "The simulated groups have equal variances.",
      "Welch routing therefore indicates Levene rejection under true homoscedasticity."
    )
  ) +
  ggplot2$theme_minimal(base_size = 11) +
  ggplot2$theme(
    axis.text.x = ggplot2$element_text(size = 7.5),
    legend.position = "right",
    plot.caption = ggplot2$element_text(hjust = 0, size = 9)
  )

ggplot2$ggsave(file.path(OUTDIR, "gamma_skew_sweep_4groups_fisher_welch_route_probability.png"),
               p_fisher_welch, width = 12, height = 5.4, dpi = 180)

cat("Wrote outputs to:", OUTDIR, "\n")
