## Plot-only script for codexsimulation20160603_gamma_skew_sweep_4groups_power.R

OUTDIR <- file.path("dev", "codexsimulation20160606_gamma_skew_sweep_4groups_power_skew4_B50000_outputs")
RESULTS <- file.path(OUTDIR, "gamma_skew_sweep_4groups_power.rds")

## ---------------------------------------------------------------------------
## Edit labels here for publication figures. The simulation is read from RESULTS
## and is not rerun by this script.
## ---------------------------------------------------------------------------

POWER_TITLE <- "b) Power simulations"
POWER_SUBTITLE <- NULL
POWER_X <- "n per group"
POWER_Y <- "simulated rejection rate"
POWER_CAPTION <- NULL
POWER_LEGEND_TITLE <- "test strategy"

PARAMETRIC_TITLE <- "Parametric branch power under true homoscedasticity"
PARAMETRIC_SUBTITLE <- "Fisher always, Welch always, and Levene-gated Fisher/Welch"
PARAMETRIC_X <- "n per group"
PARAMETRIC_Y <- "simulated rejection rate"
PARAMETRIC_CAPTION <- paste(
  "The simulated groups have equal variances; Welch selections are false-positive Levene routes.",
  "Skew = 0 is normal; skew > 0 uses standardised Gamma groups."
)

ROUTE_TITLE <- "Probability of routing to the non-parametric branch"
ROUTE_SUBTITLE <- "Cells show how often Shapiro-Wilk rejects residual normality"
ROUTE_X <- "residual distribution"
ROUTE_Y <- "n per group"
ROUTE_CAPTION <- paste(
  "Group mean offsets are 0, 0.25, 0.50, and 0.75 after standardisation.",
  "Gamma caveat: skewness and excess kurtosis vary together."
)

WELCH_ROUTE_TITLE <- "False Welch routing in the homoscedastic simulation"
WELCH_ROUTE_SUBTITLE <- "Cells show Levene false positives after passing the Shapiro-Wilk gate"
WELCH_ROUTE_X <- "residual distribution"
WELCH_ROUTE_Y <- "n per group"
WELCH_ROUTE_CAPTION <- paste(
  "The simulated groups have equal variances.",
  "Welch routing therefore indicates Levene rejection under true homoscedasticity."
)

STRATEGY_LABELS <- c(
  "1. Fisher always" = "F",
  "2. Welch always" = "W",
  "3. Levene-gated Fisher/Welch" = "L",
  "4. Kruskal-Wallis always" = "KW",
  "5. Shapiro-Wilk routed Welch/KW" = "SW",
  "6. Shapiro-Wilk plus Levene" = "SW+L"
)
STRATEGY_SHAPES <- c(
  "1. Fisher always" = 0,
  "2. Welch always" = 2,
  "3. Levene-gated Fisher/Welch" = 5,
  "4. Kruskal-Wallis always" = 4,
  "5. Shapiro-Wilk routed Welch/KW" = 1,
  "6. Shapiro-Wilk plus Levene" = 1
)
STRATEGY_SIZES <- c(
  "1. Fisher always" = 4.0,
  "2. Welch always" = 3.2,
  "3. Levene-gated Fisher/Welch" = 3.6,
  "4. Kruskal-Wallis always" = 3.8,
  "5. Shapiro-Wilk routed Welch/KW" = 5.8,
  "6. Shapiro-Wilk plus Levene" = 7.2
)
STRATEGY_COLOURS <- c(
  "1. Fisher always" = "#B79F00",
  "2. Welch always" = "#56B4E9",
  "3. Levene-gated Fisher/Welch" = "#009E73",
  "4. Kruskal-Wallis always" = "#000000",
  "5. Shapiro-Wilk routed Welch/KW" = "#D55E00",
  "6. Shapiro-Wilk plus Levene" = "#0072B2"
)
NS_TO_PLOT <- c(10, 20, 50, 100)
SKEWS_TO_PLOT <- c(0, 0.5, 1, 2)

EFFECT_SIZE_LABELS <- c(
  "moderate ordered effect: 0, 0.25, 0.50, 0.75 SD" =
    "moderate ordered effect: 0, 0.25, 0.50, 0.75 SD",
  "strong ordered effect: 0, 0.50, 1.00, 1.50 SD" =
    "strong ordered effect: 0, 0.50, 1.00, 1.50 SD"
)
EFFECTS_TO_PLOT <- c(
  "moderate ordered effect: 0, 0.25, 0.50, 0.75 SD"
  ## Uncomment to include the stronger, near-trivial power condition.
  ## ,
  ## "strong ordered effect: 0, 0.50, 1.00, 1.50 SD"
)

POWER_FILE <- "gamma_skew_sweep_4groups_power.png"
PARAMETRIC_FILE <- "gamma_skew_sweep_4groups_parametric_branch_power.png"
ROUTE_FILE <- "gamma_skew_sweep_4groups_route_probability.png"
WELCH_ROUTE_FILE <- "gamma_skew_sweep_4groups_fisher_welch_route_probability.png"
POWER_WIDTH <- 14
POWER_HEIGHT <- 7.8
PARAMETRIC_WIDTH <- 14
PARAMETRIC_HEIGHT <- 7.8
ROUTE_WIDTH <- 12
ROUTE_HEIGHT <- 7.2
WELCH_ROUTE_WIDTH <- 12
WELCH_ROUTE_HEIGHT <- 5.4
DPI <- 360

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required.")
}

power <- readRDS(RESULTS)
power <- power[power$skew %in% SKEWS_TO_PLOT, , drop = FALSE]
ns <- sort(unique(power$n_per_group))
skews <- SKEWS_TO_PLOT
if (!"effect_size" %in% names(power)) {
  power$effect_size <- "moderate ordered effect: 0, 0.25, 0.50, 0.75 SD"
}
effect_raw <- as.character(power$effect_size)
power <- power[effect_raw %in% EFFECTS_TO_PLOT, , drop = FALSE]
effect_raw <- as.character(power$effect_size)
effect_mapped <- EFFECT_SIZE_LABELS[effect_raw]
power$effect_size <- ifelse(is.na(effect_mapped), effect_raw, effect_mapped)

power$skew_label <- ifelse(
  power$skew == 0,
  "normal\nskew = 0\nexcess kurtosis = 0",
  sprintf("Gamma\nskew = %.1f\nexcess kurtosis = %.2f",
          power$skew, power$excess_kurtosis)
)
skew_levels <- ifelse(
  skews == 0,
  "normal\nskew = 0\nexcess kurtosis = 0",
  sprintf("Gamma\nskew = %.1f\nexcess kurtosis = %.2f",
          skews, 1.5 * skews^2)
)
power$skew_label <- factor(power$skew_label, levels = skew_levels)
panel_levels <- paste0(seq_along(skew_levels), ")")
names(panel_levels) <- skew_levels
power$power_panel <- factor(
  panel_levels[as.character(power$skew_label)],
  levels = panel_levels
)
power$n_label <- factor(paste0("n = ", power$n_per_group),
                        levels = paste0("n = ", ns))
power$effect_size <- factor(power$effect_size, levels = unique(power$effect_size))

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
    levels = names(STRATEGY_LABELS)
  )
  out
}

power_long <- to_long(power)
power_plot <- power_long[power_long$n_per_group %in% NS_TO_PLOT, ,
                         drop = FALSE]
gate_labels <- power[power$n_per_group %in% NS_TO_PLOT, , drop = FALSE]
gate_labels$gate_label <- sprintf(
  "F %.0f%%\nW %.0f%%\nKW %.0f%%",
  100 * gate_labels$route_fisher_probability,
  100 * gate_labels$route_welch_probability,
  100 * gate_labels$route_rank_probability
)
gate_rate_values <- rbind(
  transform(gate_labels, gate_row = "F", gate_y = 0.085,
            gate_rate = route_fisher_probability),
  transform(gate_labels, gate_row = "W", gate_y = 0.055,
            gate_rate = route_welch_probability),
  transform(gate_labels, gate_row = "KW", gate_y = 0.025,
            gate_rate = route_rank_probability)
)
gate_rate_values$gate_rate_label <- sprintf("%.0f", 100 * gate_rate_values$gate_rate)
gate_rate_rows <- unique(gate_rate_values[c("power_panel", "gate_row", "gate_y")])
gate_rate_title <- unique(gate_labels["power_panel"])
gate_rate_title$gate_title <- "SW+L selection (%)"

if (all(c("fisher_power", "welch_power") %in% names(power))) {
  parametric_long <- rbind(
    transform(power, strategy = "Fisher always", power = fisher_power),
    transform(power, strategy = "Welch always", power = welch_power),
    transform(power, strategy = "Levene-gated Fisher/Welch", power = mean_power)
  )
  parametric_long$strategy <- factor(
    parametric_long$strategy,
    levels = c("Fisher always", "Welch always", "Levene-gated Fisher/Welch")
  )
}

p_power <- ggplot2$ggplot() +
  ggplot2$geom_vline(
    xintercept = NS_TO_PLOT,
    colour = "grey88",
    linewidth = 0.35
  ) +
  ggplot2$geom_point(
    data = power_plot,
    ggplot2$aes(x = n_per_group, y = power, colour = strategy,
                shape = strategy, size = strategy, group = strategy),
    stroke = 1.15
  ) +
  ggplot2$geom_text(
    data = gate_rate_title,
    ggplot2$aes(x = 26, y = 0.125, label = gate_title),
    colour = "grey25",
    family = "serif",
    size = 2.35
  ) +
  ggplot2$geom_text(
    data = gate_rate_rows,
    ggplot2$aes(x = 5.8, y = gate_y, label = gate_row),
    colour = "grey25",
    family = "serif",
    hjust = 0,
    size = 2.35
  ) +
  ggplot2$geom_text(
    data = gate_rate_values,
    ggplot2$aes(x = n_per_group, y = gate_y, label = gate_rate_label),
    colour = "grey25",
    family = "serif",
    size = 2.35
  ) +
  ggplot2$facet_grid(stats::as.formula(". ~ power_panel")) +
  ggplot2$scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  ggplot2$scale_x_log10(breaks = NS_TO_PLOT, limits = c(5.5, 130)) +
  ggplot2$scale_shape_manual(
    values = STRATEGY_SHAPES,
    breaks = names(STRATEGY_LABELS),
    labels = STRATEGY_LABELS
  ) +
  ggplot2$scale_size_manual(
    values = STRATEGY_SIZES,
    breaks = names(STRATEGY_LABELS),
    labels = STRATEGY_LABELS
  ) +
  ggplot2$scale_colour_manual(
    values = STRATEGY_COLOURS,
    breaks = names(STRATEGY_LABELS),
    labels = STRATEGY_LABELS
  ) +
  ggplot2$labs(
    title = POWER_TITLE,
    subtitle = POWER_SUBTITLE,
    x = POWER_X,
    y = POWER_Y,
    colour = POWER_LEGEND_TITLE,
    shape = POWER_LEGEND_TITLE,
    size = POWER_LEGEND_TITLE,
    caption = POWER_CAPTION
  ) +
  ggplot2$theme_minimal(base_size = 11, base_family = "serif") +
  ggplot2$theme(
    legend.position = "right",
    axis.text.x = ggplot2$element_text(angle = 45, hjust = 1),
    panel.grid.major.x = ggplot2$element_blank(),
    panel.grid.minor.x = ggplot2$element_blank(),
    panel.grid.major.y = ggplot2$element_blank(),
    panel.grid.minor.y = ggplot2$element_blank(),
    panel.border = ggplot2$element_rect(colour = "black", fill = NA, linewidth = 0.25),
    strip.background = ggplot2$element_blank(),
    strip.text.x = ggplot2$element_text(size = 8, face = "bold",
                                        family = "serif", hjust = 0),
    plot.title = ggplot2$element_text(hjust = 0),
    plot.title.position = "plot",
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
    name = "% routed\nto non-parametric\nbranch"
  ) +
  ggplot2$facet_wrap(stats::as.formula("~ effect_size"), nrow = 2) +
  ggplot2$labs(
    title = ROUTE_TITLE,
    subtitle = ROUTE_SUBTITLE,
    x = ROUTE_X,
    y = ROUTE_Y,
    caption = ROUTE_CAPTION
  ) +
  ggplot2$theme_minimal(base_size = 11, base_family = "serif") +
  ggplot2$theme(
    axis.text.x = ggplot2$element_text(size = 7.5),
    legend.position = "right",
    plot.caption = ggplot2$element_text(hjust = 0, size = 9)
  )

if (exists("parametric_long", inherits = FALSE)) {
  p_parametric <- ggplot2$ggplot(
    parametric_long,
    ggplot2$aes(x = n_per_group, y = power, colour = strategy,
                shape = strategy)
  ) +
    ggplot2$geom_point(size = 2.4, fill = "white", stroke = 0.9) +
    ggplot2$facet_grid(stats::as.formula("effect_size ~ skew_label")) +
    ggplot2$scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    ggplot2$scale_x_log10(breaks = ns) +
    ggplot2$scale_shape_manual(
      values = c("Fisher always" = 21,
                 "Welch always" = 24,
                 "Levene-gated Fisher/Welch" = 22)
    ) +
    ggplot2$labs(
      title = PARAMETRIC_TITLE,
      subtitle = PARAMETRIC_SUBTITLE,
      x = PARAMETRIC_X,
      y = PARAMETRIC_Y,
      colour = "parametric strategy",
      shape = "parametric strategy",
      caption = PARAMETRIC_CAPTION
    ) +
    ggplot2$theme_minimal(base_size = 11, base_family = "serif") +
    ggplot2$theme(
      legend.position = "right",
      axis.text.x = ggplot2$element_text(angle = 45, hjust = 1),
      plot.caption = ggplot2$element_text(hjust = 0, size = 9)
    )
}

ggplot2$ggsave(file.path(OUTDIR, POWER_FILE),
               p_power, width = POWER_WIDTH, height = POWER_HEIGHT, dpi = DPI)
if (exists("p_parametric", inherits = FALSE)) {
  ggplot2$ggsave(file.path(OUTDIR, PARAMETRIC_FILE),
                 p_parametric, width = PARAMETRIC_WIDTH,
                 height = PARAMETRIC_HEIGHT, dpi = DPI)
} else {
  message("Skipping parametric branch plot: fisher_power/welch_power not in RESULTS.")
}
ggplot2$ggsave(file.path(OUTDIR, ROUTE_FILE),
               p_route, width = ROUTE_WIDTH, height = ROUTE_HEIGHT, dpi = DPI)

if ("route_welch_probability" %in% names(power)) {
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
      title = WELCH_ROUTE_TITLE,
      subtitle = WELCH_ROUTE_SUBTITLE,
      x = WELCH_ROUTE_X,
      y = WELCH_ROUTE_Y,
      caption = WELCH_ROUTE_CAPTION
    ) +
    ggplot2$theme_minimal(base_size = 11, base_family = "serif") +
    ggplot2$theme(
      axis.text.x = ggplot2$element_text(size = 7.5),
      legend.position = "right",
      plot.caption = ggplot2$element_text(hjust = 0, size = 9)
    )

  ggplot2$ggsave(file.path(OUTDIR, WELCH_ROUTE_FILE),
                 p_fisher_welch, width = WELCH_ROUTE_WIDTH,
                 height = WELCH_ROUTE_HEIGHT, dpi = DPI)
} else {
  message("Skipping Welch-route plot: route_welch_probability is not in RESULTS.")
}

cat("Updated plots in:", OUTDIR, "\n")
