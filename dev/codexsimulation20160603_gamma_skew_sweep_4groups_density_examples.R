## ---------------------------------------------------------------------------
## Theoretical density examples for the four-group Gamma/normal skew sweep.
##
## Four group means are offset by either
##   0, 0.25, 0.50, 0.75 SD or 0, 0.50, 1.00, 1.50 SD.
## This shows the population distributions behind the power simulation.
## ---------------------------------------------------------------------------

OUTDIR <- file.path("dev", "codexsimulation20160603_gamma_skew_sweep_4groups_power_outputs")
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required.")
}
source(file.path("dev", "codexsimulation20160607_gamma_density_helpers.R"))

skews <- c(0, 0.1, 0.5, 1, 2, 6)
x_grid <- seq(-3, 8, length.out = 1600)
shift_scenarios <- list(
  "moderate ordered effect: 0, 0.25, 0.50, 0.75 SD" =
    c(A = 0, B = 0.25, C = 0.50, D = 0.75)
  ## Uncomment for a stronger, near-trivial power condition.
  ## ,
  ## "strong ordered effect: 0, 0.50, 1.00, 1.50 SD" =
  ##   c(A = 0, B = 0.50, C = 1.00, D = 1.50)
)

shape_from_skew <- function(skew) {
  if (skew == 0) Inf else (2 / skew)^2
}

standardised_density <- function(x, skew) {
  if (skew == 0) return(stats::dnorm(x))
  shape <- shape_from_skew(skew)
  standardised_gamma_density(x, alpha = shape, shift = 0)
}

rows <- list()
idx <- 1
for (skew in skews) {
  for (scenario_name in names(shift_scenarios)) {
    mean_offsets <- shift_scenarios[[scenario_name]]
    for (group in names(mean_offsets)) {
      offset <- unname(mean_offsets[group])
      shape <- shape_from_skew(skew)
      rows[[idx]] <- data.frame(
        effect_size = scenario_name,
        group = group,
        mean_offset = offset,
        skew = skew,
        gamma_shape = if (is.infinite(shape)) NA_real_ else shape,
        excess_kurtosis = if (skew == 0) 0 else 1.5 * skew^2,
        value = x_grid + offset,
        density = standardised_density(x_grid, skew)
      )
      idx <- idx + 1
    }
  }
}

density_data <- do.call(rbind, rows)
density_data$skew_label <- ifelse(
  density_data$skew == 0,
  "normal\nskew = 0\nexcess kurtosis = 0",
  sprintf(
    "Gamma\nskew = %.1f\nexcess kurtosis = %.2f",
    density_data$skew,
    density_data$excess_kurtosis
  )
)
skew_levels <- ifelse(
  skews == 0,
  "normal\nskew = 0\nexcess kurtosis = 0",
  sprintf("Gamma\nskew = %.1f\nexcess kurtosis = %.2f",
          skews, 1.5 * skews^2)
)
density_data$skew_label <- factor(density_data$skew_label, levels = skew_levels)
density_data$effect_size <- factor(density_data$effect_size,
                                   levels = names(shift_scenarios))

write.csv(density_data,
          file.path(OUTDIR, "gamma_skew_sweep_4groups_density_examples.csv"),
          row.names = FALSE)

ggplot2 <- asNamespace("ggplot2")

p_density <- ggplot2$ggplot(
  density_data,
  ggplot2$aes(x = value, y = density, colour = group)
) +
  ggplot2$geom_line(linewidth = 0.75, na.rm = TRUE) +
  ggplot2$geom_vline(
    data = unique(density_data[c("effect_size", "group", "mean_offset", "skew_label")]),
    ggplot2$aes(xintercept = mean_offset, colour = group),
    linewidth = 0.5,
    linetype = "dashed",
    show.legend = FALSE
  ) +
  ggplot2$facet_grid(stats::as.formula("effect_size ~ skew_label"),
                     scales = "free_y") +
  ggplot2$labs(
    title = "Theoretical four-group distributions behind the power simulation",
    subtitle = "Group mean offsets are 0, 0.25, 0.50, and 0.75 SD",
    x = "standardised value after group mean offset",
    y = "density",
    colour = "group",
    caption = paste(
      "Dashed vertical lines mark the group mean offsets.",
      "Skew = 0 is normal; skew > 0 uses standardised Gamma groups.",
      "Gamma caveat: skewness and excess kurtosis vary together."
    )
  ) +
  ggplot2$theme_minimal(base_size = 11) +
  ggplot2$theme(
    legend.position = "right",
    plot.caption = ggplot2$element_text(hjust = 0, size = 9)
  )

ggplot2$ggsave(
  file.path(OUTDIR, "gamma_skew_sweep_4groups_density_examples.png"),
  p_density,
  width = 14,
  height = 7.8,
  dpi = 180
)

p_density_bulk <- p_density +
  ggplot2$coord_cartesian(xlim = c(-3, 5), ylim = c(0, 1.05)) +
  ggplot2$labs(
    title = "Bulk view of the theoretical four-group distributions",
    subtitle = "Same distributions; x-axis and y-axis capped to compare the central mass"
  )

ggplot2$ggsave(
  file.path(OUTDIR, "gamma_skew_sweep_4groups_density_examples_bulk.png"),
  p_density_bulk,
  width = 14,
  height = 7.8,
  dpi = 180
)

cat("Wrote density examples to:", OUTDIR, "\n")
