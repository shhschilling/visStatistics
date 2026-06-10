## Plot-only script for codexsimulation20160603_gamma_skew_sweep_4groups_typeI_equal_means.R

OUTDIR <- file.path(
  "dev",
  "codexsimulation20160603_gamma_skew_sweep_4groups_typeI_outputs"
)
RESULTS <- file.path(OUTDIR, "gamma_skew_sweep_4groups_typeI_equal_means.rds")
NS <- c(10, 20, 50, 100)
ALPHA <- 0.05

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required.")
}
if (!requireNamespace("scales", quietly = TRUE)) {
  stop("Package 'scales' is required.")
}

type1 <- readRDS(RESULTS)
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
      "In the unequal-variance row, Kruskal-Wallis rejection is not a Type I error for its own null, because the rank distributions differ.",
      "The two rows use different y-axis scales."
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

message("Updated plots in: ", OUTDIR)
