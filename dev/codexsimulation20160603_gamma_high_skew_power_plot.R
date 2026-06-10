## Plot-only script for codexsimulation20160603_gamma_high_skew_power.R

OUTDIR <- file.path("dev", "codexsimulation20160603_gamma_high_skew_power_outputs")
RESULTS <- file.path(OUTDIR, "gamma_high_skew_power.rds")

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required.")
}

power <- readRDS(RESULTS)
ns <- sort(unique(power$n_per_group))

power$skew_label <- sprintf(
  "skew = %.0f\nshape = %.2f\nexcess kurtosis = %.1f",
  power$skew,
  power$gamma_shape,
  power$excess_kurtosis
)
power$n_label <- factor(paste0("n = ", power$n_per_group),
                        levels = paste0("n = ", ns))
power$groups_label <- paste0(power$groups, " groups")
power$rank_minus_mean <- power$rank_power - power$mean_power
power$gate_minus_rank <- power$gate_power - power$rank_power

to_long <- function(dat) {
  out <- rbind(
    transform(dat, strategy = "mean test", power = mean_power),
    transform(dat, strategy = "rank test", power = rank_power),
    transform(dat, strategy = "Shapiro-gated route", power = gate_power)
  )
  out$strategy <- factor(
    out$strategy,
    levels = c("mean test", "rank test", "Shapiro-gated route")
  )
  out
}

power_long <- to_long(power)
ggplot2 <- asNamespace("ggplot2")

p_power <- ggplot2$ggplot(
  power_long,
  ggplot2$aes(x = n_label, y = power, colour = strategy, linetype = strategy,
              shape = strategy, group = strategy)
) +
  ggplot2$geom_line(linewidth = 0.75) +
  ggplot2$geom_point(size = 2.1) +
  ggplot2$facet_grid(groups_label ~ skew_label) +
  ggplot2$scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  ggplot2$scale_linetype_manual(
    values = c("mean test" = "solid",
               "rank test" = "dashed",
               "Shapiro-gated route" = "dotted")
  ) +
  ggplot2$scale_shape_manual(
    values = c("mean test" = 16,
               "rank test" = 17,
               "Shapiro-gated route" = 15)
  ) +
  ggplot2$labs(
    title = "Power to detect a 0.5 SD group shift under Gamma skew",
    subtitle = "Mean test vs rank test vs Route 1 Shapiro-gated route",
    x = "n per group",
    y = "power: percentage with p < 0.05",
    colour = "test strategy",
    linetype = "test strategy",
    shape = "test strategy",
    caption = paste(
      "Data: standardised Gamma groups; last group shifted by 0.5 SD.",
      "Gamma caveat: skewness and excess kurtosis vary together."
    )
  ) +
  ggplot2$theme_minimal(base_size = 11) +
  ggplot2$theme(
    legend.position = "right",
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
  ggplot2$facet_wrap(stats::as.formula("~ groups_label"), nrow = 1) +
  ggplot2$scale_fill_gradient(
    limits = c(0, 1),
    low = "#edf8fb",
    high = "#006d2c",
    labels = scales::percent,
    name = "% routed\nto rank test"
  ) +
  ggplot2$labs(
    title = "Route 1 probability of using the rank test",
    subtitle = "Cells show how often Shapiro-Wilk rejects residual normality",
    x = "Gamma skewness",
    y = "n per group",
    caption = paste(
      "Data: standardised Gamma groups; last group shifted by 0.5 SD.",
      "Gamma caveat: skewness and excess kurtosis vary together."
    )
  ) +
  ggplot2$theme_minimal(base_size = 11) +
  ggplot2$theme(
    axis.text.x = ggplot2$element_text(size = 7.5),
    legend.position = "right",
    plot.caption = ggplot2$element_text(hjust = 0, size = 9)
  )

p_overlay <- ggplot2$ggplot(
  power,
  ggplot2$aes(x = rank_power, y = gate_power, colour = groups_label,
              group = groups_label)
) +
  ggplot2$geom_abline(slope = 1, intercept = 0, colour = "grey40",
                      linewidth = 0.5, linetype = "dashed") +
  ggplot2$geom_point(size = 2.3, alpha = 0.9) +
  ggplot2$facet_wrap(stats::as.formula("~ skew_label"), nrow = 1) +
  ggplot2$coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  ggplot2$labs(
    title = "Shapiro-gated route closely follows the rank test under Gamma skew",
    subtitle = "Points near the dashed line mean gate power equals rank-test power",
    x = "rank-test power",
    y = "Shapiro-gated route power",
    colour = "design",
    caption = paste(
      "Power means detecting a 0.5 SD shift in the last group.",
      "Data: standardised Gamma groups; skewness and excess kurtosis vary together."
    )
  ) +
  ggplot2$theme_minimal(base_size = 11) +
  ggplot2$theme(
    legend.position = "right",
    plot.caption = ggplot2$element_text(hjust = 0, size = 9)
  )

p_gain <- ggplot2$ggplot(
  power,
  ggplot2$aes(x = n_label, y = rank_minus_mean, colour = groups_label,
              group = groups_label)
) +
  ggplot2$geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.45) +
  ggplot2$geom_line(linewidth = 0.65) +
  ggplot2$geom_point(size = 2.0) +
  ggplot2$facet_wrap(stats::as.formula("~ skew_label"), nrow = 1) +
  ggplot2$scale_y_continuous(labels = scales::percent) +
  ggplot2$labs(
    title = "Power advantage of the rank test over the mean test",
    subtitle = "Positive values mean the rank test detects the 0.5 SD shift more often",
    x = "n per group",
    y = "rank power minus mean-test power",
    colour = "design",
    caption = paste(
      "Power means detecting a 0.5 SD shift in the last group.",
      "Data: standardised Gamma groups; skewness and excess kurtosis vary together."
    )
  ) +
  ggplot2$theme_minimal(base_size = 11) +
  ggplot2$theme(
    legend.position = "right",
    plot.caption = ggplot2$element_text(hjust = 0, size = 9)
  )

ggplot2$ggsave(file.path(OUTDIR, "gamma_high_skew_power.png"),
               p_power, width = 12, height = 6.2, dpi = 180)
ggplot2$ggsave(file.path(OUTDIR, "gamma_high_skew_route_probability.png"),
               p_route, width = 12, height = 5.6, dpi = 180)
ggplot2$ggsave(file.path(OUTDIR, "gamma_high_skew_gate_rank_overlay.png"),
               p_overlay, width = 12, height = 4.7, dpi = 180)
ggplot2$ggsave(file.path(OUTDIR, "gamma_high_skew_rank_power_gain.png"),
               p_gain, width = 12, height = 4.8, dpi = 180)

cat("Updated plots in:", OUTDIR, "\n")
