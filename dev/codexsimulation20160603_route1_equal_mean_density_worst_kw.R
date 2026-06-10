## ---------------------------------------------------------------------------
## Theoretical population densities for Route 1 equal-mean cells with high
## Kruskal-Wallis rejection.
##
## All group means are zero. Only population SD and skew differ by condition.
## ---------------------------------------------------------------------------

OUTDIR <- file.path(
  "dev",
  "codexsimulation20160603_route1_equal_mean_blanca_zimmerman_outputs"
)
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required.")
}

skews <- c(2, 6)
designs <- list(
  "balanced n, unequal SD\nn: 50, 50, 50, 50; SD: 1, 1.3, 1.7, 2.2" =
    list(n = c(50, 50, 50, 50), sd = c(1, 1.3, 1.7, 2.2)),
  "unbalanced n, larger n with larger SD\nn: 25, 40, 60, 75; SD: 1, 1.3, 1.7, 2.2" =
    list(n = c(25, 40, 60, 75), sd = c(1, 1.3, 1.7, 2.2)),
  "unbalanced n, larger n with smaller SD\nn: 25, 40, 60, 75; SD: 2.2, 1.7, 1.3, 1" =
    list(n = c(25, 40, 60, 75), sd = c(2.2, 1.7, 1.3, 1))
)
group_names <- LETTERS[1:4]
x_grid <- seq(-3, 14, length.out = 2200)

shape_from_skew <- function(skew) {
  (2 / skew)^2
}

standardised_gamma_density <- function(z, skew) {
  shape <- shape_from_skew(skew)
  stats::dgamma(z * sqrt(shape) + shape, shape = shape, scale = 1) *
    sqrt(shape)
}

scaled_gamma_density <- function(x, skew, sd) {
  standardised_gamma_density(x / sd, skew) / sd
}

rows <- list()
idx <- 1
for (design_name in names(designs)) {
  design <- designs[[design_name]]
  for (skew in skews) {
    for (i in seq_along(group_names)) {
      rows[[idx]] <- data.frame(
        design = design_name,
        group = group_names[i],
        n = design$n[i],
        sd = design$sd[i],
        mean = 0,
        skew = skew,
        excess_kurtosis = 1.5 * skew^2,
        x = x_grid,
        density = scaled_gamma_density(x_grid, skew, design$sd[i])
      )
      idx <- idx + 1
    }
  }
}

density_data <- do.call(rbind, rows)
density_data$group_label <- sprintf(
  "%s",
  density_data$group
)
density_data$group_detail <- sprintf(
  "%s: n=%s, SD=%.1f",
  density_data$group,
  density_data$n,
  density_data$sd
)
density_data$skew_label <- sprintf(
  "Gamma\nskew = %.0f\nexcess kurtosis = %.0f",
  density_data$skew,
  density_data$excess_kurtosis
)
density_data$skew_label <- factor(
  density_data$skew_label,
  levels = sprintf("Gamma\nskew = %.0f\nexcess kurtosis = %.0f",
                   skews, 1.5 * skews^2)
)
density_data$design <- factor(density_data$design, levels = names(designs))

write.csv(
  density_data,
  file.path(OUTDIR, "route1_equal_mean_density_worst_kw.csv"),
  row.names = FALSE
)

ggplot2 <- asNamespace("ggplot2")

p <- ggplot2$ggplot(
  density_data,
  ggplot2$aes(x = x, y = density, colour = group_label)
) +
  ggplot2$geom_line(linewidth = 0.85) +
  ggplot2$geom_vline(xintercept = 0, linetype = "dashed",
                     linewidth = 0.35, colour = "grey20") +
  ggplot2$facet_grid(stats::as.formula("design ~ skew_label"),
                     scales = "free") +
  ggplot2$labs(
    title = "Theoretical distributions in high Kruskal-Wallis rejection cells",
    subtitle = "All population means are equal at zero; groups differ in spread",
    x = "value",
    y = "density",
    colour = "group",
    caption = paste(
      "Dashed vertical line marks the common population mean.",
      "Distributions are standardised Gamma variables rescaled by group SD.",
      "Gamma caveat: skewness and excess kurtosis vary together."
    )
  ) +
  ggplot2$theme_minimal(base_size = 11) +
  ggplot2$theme(
    legend.position = "right",
    panel.grid.minor = ggplot2$element_blank(),
    panel.border = ggplot2$element_rect(colour = "black", fill = NA, linewidth = 0.25),
    plot.caption = ggplot2$element_text(hjust = 0, size = 9)
  )

ggplot2$ggsave(
  file.path(OUTDIR, "route1_equal_mean_density_worst_kw.png"),
  p,
  width = 14,
  height = 9,
  dpi = 180
)

p_bulk <- p +
  ggplot2$coord_cartesian(xlim = c(-2, 4), ylim = c(0, 2.2)) +
  ggplot2$labs(
    title = "Bulk view of theoretical distributions in high Kruskal-Wallis rejection cells",
    subtitle = "Same population densities; axes zoomed to show the central mass"
  )

ggplot2$ggsave(
  file.path(OUTDIR, "route1_equal_mean_density_worst_kw_bulk.png"),
  p_bulk,
  width = 14,
  height = 9,
  dpi = 180
)

prob_grid <- seq(0.001, 0.999, length.out = 1600)
q_rows <- list()
idx <- 1
for (design_name in names(designs)) {
  design <- designs[[design_name]]
  for (skew in skews) {
    shape <- shape_from_skew(skew)
    for (i in seq_along(group_names)) {
      q_values <- (stats::qgamma(prob_grid, shape = shape, scale = 1) -
                     shape) / sqrt(shape) * design$sd[i]
      q_rows[[idx]] <- data.frame(
        design = design_name,
        group = group_names[i],
        n = design$n[i],
        sd = design$sd[i],
        skew = skew,
        excess_kurtosis = 1.5 * skew^2,
        x = q_values,
        cdf = prob_grid
      )
      idx <- idx + 1
    }
  }
}

cdf_data <- do.call(rbind, q_rows)
cdf_data$group_label <- cdf_data$group
cdf_data$group_detail <- sprintf(
  "%s: n=%s, SD=%.1f",
  cdf_data$group,
  cdf_data$n,
  cdf_data$sd
)
cdf_data$skew_label <- sprintf(
  "Gamma\nskew = %.0f\nexcess kurtosis = %.0f",
  cdf_data$skew,
  cdf_data$excess_kurtosis
)
cdf_data$skew_label <- factor(
  cdf_data$skew_label,
  levels = sprintf("Gamma\nskew = %.0f\nexcess kurtosis = %.0f",
                   skews, 1.5 * skews^2)
)
cdf_data$design <- factor(cdf_data$design, levels = names(designs))

quantile_probs <- c(0.25, 0.50, 0.75)
quantile_rows <- list()
idx <- 1
for (design_name in names(designs)) {
  design <- designs[[design_name]]
  for (skew in skews) {
    shape <- shape_from_skew(skew)
    for (i in seq_along(group_names)) {
      q <- (stats::qgamma(quantile_probs, shape = shape, scale = 1) -
              shape) / sqrt(shape) * design$sd[i]
      quantile_rows[[idx]] <- data.frame(
        design = design_name,
        group = group_names[i],
        skew = skew,
        skew_label = sprintf("Gamma\nskew = %.0f\nexcess kurtosis = %.0f",
                             skew, 1.5 * skew^2),
        q25 = q[1],
        median = q[2],
        q75 = q[3],
        mean = 0
      )
      idx <- idx + 1
    }
  }
}
quantile_data <- do.call(rbind, quantile_rows)
quantile_data$skew_label <- factor(
  quantile_data$skew_label,
  levels = sprintf("Gamma\nskew = %.0f\nexcess kurtosis = %.0f",
                   skews, 1.5 * skews^2)
)
quantile_data$design <- factor(quantile_data$design, levels = names(designs))

write.csv(
  cdf_data,
  file.path(OUTDIR, "route1_equal_mean_cdf_worst_kw.csv"),
  row.names = FALSE
)
write.csv(
  quantile_data,
  file.path(OUTDIR, "route1_equal_mean_quantiles_worst_kw.csv"),
  row.names = FALSE
)

p_cdf <- ggplot2$ggplot(
  cdf_data,
  ggplot2$aes(x = x, y = cdf, colour = group_label)
) +
  ggplot2$geom_line(linewidth = 0.85) +
  ggplot2$geom_vline(xintercept = 0, linetype = "dashed",
                     linewidth = 0.35, colour = "grey20") +
  ggplot2$geom_segment(
    data = quantile_data,
    ggplot2$aes(x = q25, xend = q75, y = 0.5, yend = 0.5,
                colour = group),
    inherit.aes = FALSE,
    linewidth = 1.2,
    alpha = 0.75
  ) +
  ggplot2$geom_point(
    data = quantile_data,
    ggplot2$aes(x = median, y = 0.5, colour = group),
    inherit.aes = FALSE,
    size = 1.8
  ) +
  ggplot2$facet_grid(stats::as.formula("design ~ skew_label")) +
  ggplot2$coord_cartesian(xlim = c(-2, 8)) +
  ggplot2$scale_y_continuous(labels = scales::percent) +
  ggplot2$labs(
    title = "Theoretical CDFs in high Kruskal-Wallis rejection cells",
    subtitle = "All population means are equal at zero; CDFs differ because group spreads differ",
    x = "value",
    y = "cumulative probability",
    colour = "group",
    caption = paste(
      "Dashed vertical line marks the common population mean.",
      "Horizontal segments mark Q1-Q3 at the median level; points mark medians.",
      "Kruskal-Wallis reacts to these rank-distribution differences, not to unequal means."
    )
  ) +
  ggplot2$theme_minimal(base_size = 11) +
  ggplot2$theme(
    legend.position = "right",
    panel.grid.minor = ggplot2$element_blank(),
    panel.border = ggplot2$element_rect(colour = "black", fill = NA, linewidth = 0.25),
    plot.caption = ggplot2$element_text(hjust = 0, size = 9)
  )

ggplot2$ggsave(
  file.path(OUTDIR, "route1_equal_mean_cdf_worst_kw.png"),
  p_cdf,
  width = 14,
  height = 9,
  dpi = 180
)

cat("Wrote density and CDF figures to:", OUTDIR, "\n")
