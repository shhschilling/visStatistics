## ---------------------------------------------------------------------------
## Raw two-group examples for the mild-skew Gamma simulations.
##
## Shows what the actual sampled values look like for two identical groups
## drawn from the standardised Gamma path used in the routing stress test.
##
## Data-generating process:
##   group A and group B are independently sampled from the same distribution;
##   no shift; equal means and equal ordering are true.
## ---------------------------------------------------------------------------

set.seed(20260602)

OUTDIR <- file.path("dev", "codexsimulation20160602_mild_skew_gamma_raw_examples_outputs")
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required.")
}

n_per_group <- 100
skews <- c(0.1, 0.3, 0.5)

shape_from_skew <- function(skew) (2 / skew)^2

std_gamma <- function(m, skew) {
  shape <- shape_from_skew(skew)
  (stats::rgamma(m, shape = shape, scale = 1) - shape) / sqrt(shape)
}

rows <- list()
i <- 1
for (skew in skews) {
  shape <- shape_from_skew(skew)
  for (group in c("A", "B")) {
    rows[[i]] <- data.frame(
      group = group,
      skew = skew,
      shape = shape,
      excess_kurtosis = 1.5 * skew^2,
      value = std_gamma(n_per_group, skew)
    )
    i <- i + 1
  }
}
raw_data <- do.call(rbind, rows)
raw_data$skew_label <- sprintf(
  "skew = %.1f\nGamma shape = %.1f\nexcess kurtosis = %.2f",
  raw_data$skew,
  raw_data$shape,
  raw_data$excess_kurtosis
)

summary_data <- stats::aggregate(
  value ~ group + skew + shape + excess_kurtosis + skew_label,
  raw_data,
  function(x) c(mean = mean(x), median = stats::median(x))
)
summary_data <- do.call(data.frame, summary_data)
names(summary_data)[names(summary_data) == "value.mean"] <- "mean"
names(summary_data)[names(summary_data) == "value.median"] <- "median"

density_markers <- rbind(
  transform(summary_data, statistic = "mean", marker_value = mean),
  transform(summary_data, statistic = "median", marker_value = median)
)

write.csv(raw_data, file.path(OUTDIR, "mild_skew_gamma_raw_two_groups.csv"),
          row.names = FALSE)
saveRDS(raw_data, file.path(OUTDIR, "mild_skew_gamma_raw_two_groups.rds"))

readme <- c(
  "Raw two-group examples for mild-skew Gamma simulations",
  paste("n per group:", n_per_group),
  "Data-generating process:",
  "  group A and group B are independently sampled from the same standardised Gamma distribution.",
  "  There is no group shift; equal means and equal ordering are true.",
  "  Gamma shape = (2 / skew)^2, scale = 1; values are standardised to mean 0 and SD 1.",
  "  Along this Gamma path, skewness and excess kurtosis vary together.",
  "",
  "Files:",
  "  mild_skew_gamma_raw_two_groups.csv",
  "  mild_skew_gamma_raw_two_groups.rds",
  "  mild_skew_gamma_raw_two_groups_points.png",
  "  mild_skew_gamma_raw_two_groups_density.png",
  "",
  "Plot markers:",
  "  points plot: diamond = sample mean; horizontal bar = sample median.",
  "  density plot: solid vertical line = sample mean; dashed vertical line = sample median."
)
writeLines(readme, file.path(OUTDIR, "README.txt"))

ggplot2 <- asNamespace("ggplot2")

p_points <- ggplot2$ggplot(
  raw_data,
  ggplot2$aes(x = group, y = value, colour = group)
) +
  ggplot2$geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.4) +
  ggplot2$geom_boxplot(width = 0.42, outlier.shape = NA, alpha = 0.15,
                       colour = "grey25") +
  ggplot2$geom_jitter(width = 0.16, height = 0, alpha = 0.62, size = 1.7) +
  ggplot2$geom_point(
    data = summary_data,
    ggplot2$aes(y = mean, shape = "mean"),
    size = 3.0,
    colour = "black",
    fill = "white",
    stroke = 1.0
  ) +
  ggplot2$geom_point(
    data = summary_data,
    ggplot2$aes(y = median, shape = "median"),
    size = 5.2,
    colour = "black",
    stroke = 1.1
  ) +
  ggplot2$facet_wrap(stats::as.formula("~ skew_label"), nrow = 1) +
  ggplot2$scale_shape_manual(
    name = "summary",
    values = c(mean = 23, median = 95)
  ) +
  ggplot2$labs(
    title = "Raw two-group samples from identical mild-skew Gamma distributions",
    subtitle = "Each panel: group A and B have the same distribution, no shift, n = 100 per group",
    x = "group",
    y = "standardised raw value",
    caption = "Gamma path limitation: skewness and excess kurtosis vary together."
  ) +
  ggplot2$theme_minimal(base_size = 11) +
  ggplot2$theme(
    legend.position = "right",
    plot.caption = ggplot2$element_text(hjust = 0, size = 9)
  )

p_density <- ggplot2$ggplot(
  raw_data,
  ggplot2$aes(x = value, colour = group, fill = group)
) +
  ggplot2$geom_density(alpha = 0.18, linewidth = 0.8) +
  ggplot2$geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.4) +
  ggplot2$geom_vline(
    data = density_markers,
    ggplot2$aes(xintercept = marker_value, colour = group,
                linetype = statistic),
    linewidth = 0.65,
    alpha = 0.85
  ) +
  ggplot2$facet_wrap(stats::as.formula("~ skew_label"), nrow = 1) +
  ggplot2$scale_linetype_manual(
    name = "summary",
    values = c(mean = "solid", median = "dashed")
  ) +
  ggplot2$labs(
    title = "Density view of the same raw two-group Gamma samples",
    subtitle = "Groups are independently sampled from the same standardised Gamma distribution",
    x = "standardised raw value",
    y = "sample density",
    caption = "Gamma path limitation: skewness and excess kurtosis vary together."
  ) +
  ggplot2$theme_minimal(base_size = 11) +
  ggplot2$theme(plot.caption = ggplot2$element_text(hjust = 0, size = 9))

ggplot2$ggsave(file.path(OUTDIR, "mild_skew_gamma_raw_two_groups_points.png"),
               p_points, width = 10, height = 5.8, dpi = 180)
ggplot2$ggsave(file.path(OUTDIR, "mild_skew_gamma_raw_two_groups_density.png"),
               p_density, width = 10, height = 5.8, dpi = 180)

cat("Wrote outputs to:", OUTDIR, "\n")
