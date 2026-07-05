## Fleishman Route 1 power figures for the 50k simulation run.

if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
if (!requireNamespace("patchwork", quietly = TRUE)) stop("Package 'patchwork' is required.")
if (!requireNamespace("scales", quietly = TRUE)) stop("Package 'scales' is required.")
if (!requireNamespace("ggtext", quietly = TRUE)) stop("Package 'ggtext' is required.")

source(file.path("dev", "fleishman_route1_residual_helpers.R"))
source(file.path("dev", "fleishman_figure_typography.R"))

OUTDIR <- file.path("dev", "fleishman_route1_power_B50000_outputs")
FIGDIR <- file.path("vignettes", "figures")
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIGDIR, showWarnings = FALSE, recursive = TRUE)

RESULTS <- file.path(OUTDIR, "fleishman_4groups_power.rds")
power <- readRDS(RESULTS)

ggplot2 <- asNamespace("ggplot2")
patchwork <- asNamespace("patchwork")
scales <- asNamespace("scales")
group_cols <- fleishman_group_cols

panel_title <- function(panel) {
  one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
  if (nrow(one) != 1) stop("Unknown Fleishman panel: ", panel)
  if (panel == 1) {
    return("N(0, 1)\n\n\nskew = 0\nexcess kurtosis = 0")
  }
  sprintf(
    paste(
      "Fleishman polynomial",
      "a = %.3f, b = %.3f",
      "c = -a, d = %.3f",
      "skew = %s",
      "excess kurtosis = %s",
      sep = "\n"
    ),
    one$a,
    one$b,
    one$d,
    format(one$skew, trim = TRUE, scientific = FALSE),
    format(one$excess_kurtosis, trim = TRUE, scientific = FALSE)
  )
}

power$panel <- as.integer(power$panel)
power$skew_label <- factor(
  vapply(power$panel, function(panel) {
    one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
    if (panel == 1) {
      return("Normal distribution\nskew = 0; excess kurtosis = 0")
    }
    sprintf(
      "F.P.\nskew = %s\nexcess kurtosis = %s",
      format(one$skew, trim = TRUE, scientific = FALSE),
      format(one$excess_kurtosis, trim = TRUE, scientific = FALSE)
    )
  }, character(1)),
  levels = vapply(fleishman_cases$panel, function(panel) {
    one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
    if (panel == 1) {
      return("Normal distribution\nskew = 0; excess kurtosis = 0")
    }
    sprintf(
      "F.P.\nskew = %s\nexcess kurtosis = %s",
      format(one$skew, trim = TRUE, scientific = FALSE),
      format(one$excess_kurtosis, trim = TRUE, scientific = FALSE)
    )
  }, character(1))
)

skew_levels <- levels(power$skew_label)
panel_levels <- paste0(seq_along(skew_levels), ")")

group_means <- as.numeric(strsplit(power$group_mean_offsets[1], ", ")[[1]])
groups <- names(group_cols)[seq_along(group_means)]
group_legend_labels <- sprintf("%s (mean shift = %.2f)", groups, group_means)
names(group_legend_labels) <- groups
xlim <- c(-2.5, 5)
y_cap <- 0.7

make_density <- function(panel, shift) {
  x <- seq(xlim[1], xlim[2], length.out = 700)
  density <- fleishman_scaled_density(x, panel, sd = 1, shift = shift)
  density[!is.finite(density)] <- NA_real_
  data.frame(x = x, density = density, piece = "density")
}

pdf_rows <- list()
idx <- 1
for (panel in sort(unique(power$panel))) {
  for (i in seq_along(group_means)) {
    curve <- make_density(panel, group_means[i])
    pdf_rows[[idx]] <- data.frame(
      panel = panel,
      distribution = panel_title(panel),
      group = groups[i],
      shift = group_means[i],
      x = curve$x,
      density = curve$density,
      piece = curve$piece,
      stringsAsFactors = FALSE
    )
    idx <- idx + 1
  }
}
pdf_data <- do.call(rbind, pdf_rows)
pdf_data$distribution <- factor(
  pdf_data$distribution,
  levels = vapply(sort(unique(power$panel)), panel_title, character(1))
)
pdf_panel_numbers <- data.frame(
  distribution = factor(
    vapply(sort(unique(power$panel)), panel_title, character(1)),
    levels = levels(pdf_data$distribution)
  ),
  number = paste0(sort(unique(power$panel)), ")"),
  x = xlim[1],
  y = y_cap,
  stringsAsFactors = FALSE
)
reference_lines <- do.call(rbind, lapply(sort(unique(power$panel)), function(panel) {
  one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
  data.frame(
    distribution = factor(panel_title(panel), levels = levels(pdf_data$distribution)),
    group = factor(rep(groups, 2), levels = groups),
    value = c(group_means, one$a + group_means),
    line_type = factor(
      rep(c("mean", "median"), each = length(groups)),
      levels = c("mean", "median")
    )
  )
}))

p_pdf <- ggplot2$ggplot(
  pdf_data,
  ggplot2$aes(x = x, y = density, colour = group, group = interaction(group, piece))
) +
  ggplot2$geom_vline(
    data = reference_lines,
    ggplot2$aes(xintercept = value, colour = group, linetype = line_type),
    linewidth = 0.42,
    alpha = 0.75,
    inherit.aes = FALSE,
    show.legend = c(colour = FALSE, linetype = TRUE)
  ) +
  ggplot2$geom_line(linewidth = 0.7, na.rm = TRUE) +
  ggplot2$geom_text(
    data = pdf_panel_numbers,
    ggplot2$aes(x = x, y = y, label = number),
    inherit.aes = FALSE,
    family = FLEISHMAN_FONT_FAMILY,
    fontface = "plain",
    hjust = 0,
    vjust = -2.0,
    size = FLEISHMAN_GEOM_TEXT$panel_number
  ) +
  ggplot2$facet_wrap(~distribution, nrow = 1) +
  ggplot2$coord_cartesian(xlim = xlim, ylim = c(0, y_cap), clip = "off") +
  ggplot2$scale_colour_manual(
    values = group_cols,
    breaks = groups,
    labels = group_legend_labels,
    name = "group,\ngroup mean offset"
  ) +
  ggplot2$scale_linetype_manual(
    values = c(mean = "dashed", median = "dotted"),
    name = "reference line"
  ) +
  ggplot2$guides(
    colour = ggplot2$guide_legend(
      order = 1
    ),
    linetype = ggplot2$guide_legend(
      order = 2,
      override.aes = list(colour = "grey25")
    )
  ) +
  ggplot2$labs(
    title = fleishman_panel_title("A", "input distributions"),
    subtitle = NULL,
    x = "Response value with group shift",
    y = "Theoretical density"
  ) +
  ggplot2$theme_minimal(
    base_size = FLEISHMAN_TEXT$section_title,
    base_family = FLEISHMAN_FONT_FAMILY
  ) +
  ggplot2$theme(
    panel.grid.minor = ggplot2$element_blank(),
    panel.border = ggplot2$element_rect(colour = "grey35", fill = NA, linewidth = 0.35),
    strip.text = ggplot2$element_text(
      size = FLEISHMAN_TEXT$panel_title,
      family = FLEISHMAN_FONT_FAMILY,
      lineheight = FLEISHMAN_LINEHEIGHT$panel_title
    ),
    strip.background = ggplot2$element_blank(),
    legend.position = "right",
    legend.title = ggplot2$element_text(size = FLEISHMAN_TEXT$legend),
    legend.text = ggplot2$element_text(size = FLEISHMAN_TEXT$legend),
    axis.title.x = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_title),
    axis.title.y = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_title),
    plot.title = ggtext::element_markdown(
      hjust = 0,
      size = FLEISHMAN_TEXT$panel_letter,
      family = FLEISHMAN_FONT_FAMILY
    ),
    plot.subtitle = ggplot2$element_text(size = FLEISHMAN_TEXT$section_title),
    plot.title.position = "plot",
    plot.margin = ggplot2$margin(14, 5.5, 5.5, 5.5)
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
EFFECTS_TO_PLOT <- "moderate ordered effect: 0, 0.25, 0.50, 0.75 SD"

power$effect_size <- factor(power$effect_size, levels = EFFECTS_TO_PLOT)
power$power_panel <- factor(paste0(power$panel, ")"), levels = panel_levels)
power$n_label <- factor(paste0("n = ", power$n_per_group),
  levels = paste0("n = ", sort(unique(power$n_per_group)))
)

to_long <- function(dat) {
  out <- rbind(
    transform(dat, strategy = "1. Fisher always", power = fisher_power),
    transform(dat, strategy = "2. Welch always", power = welch_power),
    transform(dat, strategy = "3. Levene-gated Fisher/Welch", power = mean_power),
    transform(dat, strategy = "4. Kruskal-Wallis always", power = rank_power),
    transform(dat, strategy = "5. Shapiro-Wilk routed Welch/KW", power = sw_power),
    transform(dat, strategy = "6. Shapiro-Wilk plus Levene", power = gate_power)
  )
  out$strategy <- factor(out$strategy, levels = names(STRATEGY_LABELS))
  out
}

power_long <- to_long(power)
power_plot <- subset(power_long, n_per_group %in% NS_TO_PLOT)

gate_labels <- subset(power, n_per_group %in% NS_TO_PLOT)
gate_labels$gate_title <- "SW+L selection (%)"
gate_labels$gate_row <- "SW+L"
gate_labels$gate_y <- 0.085

gate_rate_values <- rbind(
  transform(gate_labels,
    gate_row = "F", gate_y = 0.085,
    gate_rate = route_fisher_probability
  ),
  transform(gate_labels,
    gate_row = "W", gate_y = 0.055,
    gate_rate = route_welch_probability
  ),
  transform(gate_labels,
    gate_row = "KW", gate_y = 0.025,
    gate_rate = route_rank_probability
  )
)
gate_rate_values$gate_rate_label <- sprintf("%.0f", 100 * gate_rate_values$gate_rate)
gate_rate_rows <- unique(gate_rate_values[c("power_panel", "gate_row", "gate_y")])
gate_rate_title <- unique(gate_labels["power_panel"])
gate_rate_title$gate_title <- "SW+L selection (%)"

p_power <- ggplot2$ggplot() +
  ggplot2$geom_vline(xintercept = NS_TO_PLOT, colour = "grey88", linewidth = 0.35) +
  ggplot2$geom_point(
    data = power_plot,
    ggplot2$aes(
      x = n_per_group, y = power, colour = strategy,
      shape = strategy, size = strategy, group = strategy
    ),
    stroke = 1.15
  ) +
  ggplot2$geom_text(
    data = gate_rate_title,
    ggplot2$aes(x = 26, y = 0.125, label = gate_title),
    colour = "grey25",
    family = FLEISHMAN_FONT_FAMILY,
    size = FLEISHMAN_GEOM_TEXT$inset
  ) +
  ggplot2$geom_text(
    data = gate_rate_rows,
    ggplot2$aes(x = 5.8, y = gate_y, label = gate_row),
    colour = "grey25",
    family = FLEISHMAN_FONT_FAMILY,
    hjust = 0,
    size = FLEISHMAN_GEOM_TEXT$inset
  ) +
  ggplot2$geom_text(
    data = gate_rate_values,
    ggplot2$aes(x = n_per_group, y = gate_y, label = gate_rate_label),
    colour = "grey25",
    family = FLEISHMAN_FONT_FAMILY,
    size = FLEISHMAN_GEOM_TEXT$inset
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
    title = fleishman_panel_title("B", "power simulations"),
    x = "n per group",
    y = "simulated rejection rate",
    colour = "test strategy",
    shape = "test strategy",
    size = "test strategy"
  ) +
  ggplot2$theme_minimal(
    base_size = FLEISHMAN_TEXT$section_title,
    base_family = FLEISHMAN_FONT_FAMILY
  ) +
  ggplot2$theme(
    legend.position = "right",
    legend.title = ggplot2$element_text(size = FLEISHMAN_TEXT$legend),
    legend.text = ggplot2$element_text(size = FLEISHMAN_TEXT$legend),
    axis.title.x = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_title),
    axis.title.y = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_title),
    axis.text = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_text),
    axis.text.x = ggplot2$element_text(angle = 45, hjust = 1),
    panel.grid.major.x = ggplot2$element_blank(),
    panel.grid.minor.x = ggplot2$element_blank(),
    panel.grid.major.y = ggplot2$element_blank(),
    panel.grid.minor.y = ggplot2$element_blank(),
    panel.border = ggplot2$element_rect(colour = "black", fill = NA, linewidth = 0.25),
    strip.background = ggplot2$element_blank(),
    strip.text.x = ggplot2$element_text(
      size = FLEISHMAN_TEXT$power_strip,
      face = "plain",
      family = FLEISHMAN_FONT_FAMILY,
      hjust = 0
    ),
    plot.title = ggtext::element_markdown(
      hjust = 0,
      size = FLEISHMAN_TEXT$panel_letter,
      family = FLEISHMAN_FONT_FAMILY
    ),
    plot.title.position = "plot"
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

p_parametric <- ggplot2$ggplot(
  parametric_long,
  ggplot2$aes(x = n_per_group, y = power, colour = strategy, shape = strategy)
) +
  ggplot2$geom_point(size = 2.4, fill = "white", stroke = 0.9) +
  ggplot2$facet_grid(stats::as.formula("effect_size ~ skew_label")) +
  ggplot2$scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  ggplot2$scale_x_log10(breaks = sort(unique(power$n_per_group))) +
  ggplot2$scale_shape_manual(values = c(
    "Fisher always" = 21,
    "Welch always" = 24,
    "Levene-gated Fisher/Welch" = 22
  )) +
  ggplot2$labs(
    title = "Parametric branch power under true homoscedasticity",
    subtitle = "Fisher always, Welch always, and Levene-gated Fisher/Welch",
    x = "n per group",
    y = "simulated rejection rate",
    colour = "parametric strategy",
    shape = "parametric strategy"
  ) +
  ggplot2$theme_minimal(base_size = 11, base_family = "serif") +
  ggplot2$theme(
    legend.position = "right",
    axis.text.x = ggplot2$element_text(angle = 45, hjust = 1),
    plot.title = ggplot2$element_text(hjust = 0),
    plot.title.position = "plot"
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
    title = "Probability of routing to the non-parametric branch",
    subtitle = "Cells show how often Shapiro-Wilk rejects residual normality",
    x = "residual distribution",
    y = "n per group"
  ) +
  ggplot2$theme_minimal(base_size = 11, base_family = "serif") +
  ggplot2$theme(
    axis.text.x = ggplot2$element_text(size = 7.5),
    legend.position = "right"
  )

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
    x = "residual distribution",
    y = "n per group"
  ) +
  ggplot2$theme_minimal(base_size = 11, base_family = "serif") +
  ggplot2$theme(
    axis.text.x = ggplot2$element_text(size = 7.5),
    legend.position = "right"
  )

combined <- patchwork$wrap_plots(p_pdf, p_power, ncol = 1, heights = c(1, 2.35))

ggplot2$ggsave(file.path(OUTDIR, "fleishman_4groups_power_pdf.png"),
  p_pdf,
  width = 20, height = 5.2, dpi = 360
)
ggplot2$ggsave(file.path(FIGDIR, "fleishman_4groups_power_pdf.png"),
  p_pdf,
  width = 20, height = 5.2, dpi = 360
)
ggplot2$ggsave(file.path(OUTDIR, "fleishman_4groups_power.png"),
  combined,
  width = 20, height = 15.2, dpi = 360
)
ggplot2$ggsave(file.path(FIGDIR, "fleishman_4groups_power.png"),
  combined,
  width = 20, height = 15.2, dpi = 360
)
ggplot2$ggsave(file.path(OUTDIR, "fleishman_4groups_power_with_pdf.png"),
  combined,
  width = 20, height = 15.2, dpi = 360
)
ggplot2$ggsave(file.path(FIGDIR, "fleishman_4groups_power_with_pdf.png"),
  combined,
  width = 20, height = 15.2, dpi = 360
)
ggplot2$ggsave(file.path(OUTDIR, "fleishman_4groups_parametric_branch_power.png"),
  p_parametric,
  width = 14, height = 7.8, dpi = 360
)
ggplot2$ggsave(file.path(FIGDIR, "fleishman_4groups_parametric_branch_power.png"),
  p_parametric,
  width = 14, height = 7.8, dpi = 360
)
ggplot2$ggsave(file.path(OUTDIR, "fleishman_4groups_route_probability.png"),
  p_route,
  width = 12, height = 7.2, dpi = 360
)
ggplot2$ggsave(file.path(FIGDIR, "fleishman_4groups_route_probability.png"),
  p_route,
  width = 12, height = 7.2, dpi = 360
)
ggplot2$ggsave(file.path(OUTDIR, "fleishman_4groups_fisher_welch_route_probability.png"),
  p_fisher_welch,
  width = 12, height = 5.4, dpi = 360
)
ggplot2$ggsave(file.path(FIGDIR, "fleishman_4groups_fisher_welch_route_probability.png"),
  p_fisher_welch,
  width = 12, height = 5.4, dpi = 360
)

message("Updated plots in: ", OUTDIR)
