## Generate power figure from fixed-shifts simulation results: same shifts
## and SDs held fixed across all three designs, only the sample-size
## allocation (balance) varies. omega^2 therefore differs by design (not
## held artificially equal), shown once per row. eta_H^2 is not shown: it
## has no established population definition independent of N.

if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
if (!requireNamespace("patchwork", quietly = TRUE)) stop("Package 'patchwork' is required.")
if (!requireNamespace("scales", quietly = TRUE)) stop("Package 'scales' is required.")
if (!requireNamespace("ggtext", quietly = TRUE)) stop("Package 'ggtext' is required.")

SIMDIR <- local({
  here <- getwd()
  if (file.exists(file.path(here, "fleishman_route1_residual_helpers.R"))) {
    here
  } else {
    installed <- system.file("simulations", package = "visStatistics")
    if (!nzchar(installed)) {
      stop("Cannot locate the simulations directory: run from inst/simulations/ ",
           "or install visStatistics.")
    }
    installed
  }
})

source(file.path(SIMDIR, "fleishman_route1_residual_helpers.R"))
source(file.path(SIMDIR, "fleishman_figure_typography.R"))

OUTDIR <- "."
FIGDIR <- "."
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIGDIR, showWarnings = FALSE, recursive = TRUE)

RESULTS_FILE <- file.path(SIMDIR, "rankfd_route1_power_fixed_shifts_B50000.csv")
if (!file.exists(RESULTS_FILE)) {
  stop("Results file not found: ", RESULTS_FILE)
}

power <- read.csv(RESULTS_FILE)

## One row per design. The design specification (n multipliers, SD vector,
## shift vector) is shown explicitly, not just its name -- shifts and SDs
## are identical across rows by construction (that is the whole point of
## this comparison), only the n multipliers differ. omega^2 is shown once
## per row since it is constant within a design across n and panel here.
power$header <- with(power,
  paste0(design,
         "  |  (n₁,n₂,n₃,n₄)=n̄(", multipliers, ")",
         "  |  (SD₁,...,SD₄)=(", sd_per_group, ")",
         "  |  shifts=(", shifts, ")",
         "  |  ω²=", format(round(omega_sq, 4), nsmall = 4))
)
power$header <- factor(power$header, levels = unique(power$header))

## Create figure
to_long <- function(dat) {
  rows <- list(
    transform(dat, strategy = "Pseudo-rank KW", power = ps_kw_power),
    transform(dat, strategy = "ANOVA-type stat", power = ps_ats_power)
  )
  out <- do.call(rbind, rows)
  out$strategy <- factor(out$strategy, levels = c("Pseudo-rank KW", "ANOVA-type stat"))
  out
}

power_long <- to_long(power)

NS_TO_PLOT <- sort(unique(power$n_per_group))

ggplot2 <- asNamespace("ggplot2")
scales <- asNamespace("scales")

fig <- ggplot2$ggplot(power_long, ggplot2$aes(x = n_per_group, y = power, colour = strategy, shape = strategy)) +
  ggplot2$geom_point(size = 3, stroke = 1.2) +
  ggplot2$geom_line(size = 0.8, alpha = 0.7) +
  ggplot2$facet_grid(header ~ factor(panel, levels = 1:5), scales = "free_x") +
  ggplot2$scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), labels = scales::percent) +
  ggplot2$scale_x_log10(breaks = NS_TO_PLOT) +
  ggplot2$scale_colour_manual(
    values = c("Pseudo-rank KW" = "#999999", "ANOVA-type stat" = "#CC79A7"),
    name = "Test"
  ) +
  ggplot2$scale_shape_manual(
    values = c("Pseudo-rank KW" = 3, "ANOVA-type stat" = 6),
    name = "Test"
  ) +
  ggplot2$labs(
    title = "Rank-based power, same distributions across designs: balance effect only",
    x = "Sample size per group (log scale)",
    y = "Simulated rejection rate (power)",
    subtitle = "Shifts and SDs held identical across rows; only the sample-size allocation differs"
  ) +
  ggplot2$theme_minimal(base_size = 10) +
  ggplot2$theme(
    strip.text = ggplot2$element_text(size = 8, face = "plain"),
    legend.position = "bottom",
    panel.grid.major = ggplot2$element_line(colour = "grey90"),
    panel.grid.minor = ggplot2$element_blank()
  )

ggplot2$ggsave(file.path(OUTDIR, "rankfd_route1_power_fixed_shifts.png"),
  fig, width = 16, height = 10, dpi = 150)

message("Figure saved to rankfd_route1_power_fixed_shifts.png")
