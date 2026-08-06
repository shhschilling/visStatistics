## ---------------------------------------------------------------------------
## Why the Route 1 normality gate is not replaced by a sample-size rule.
##
## The central limit theorem requires a finite second moment, not a large n.
## The Cauchy distribution has none: the variance integral diverges because the
## density decays like 1/(pi*x^2), so no amount of averaging concentrates the
## sample mean. A rule of the form "all groups larger than n, therefore trust
## the mean-based tests" cannot see this, because the sample standard deviation
## is finite in every sample and simply grows with n. A gate applied to the
## residual distribution can.
##
## Panel A contrasts the Cauchy and normal densities on a linear scale, where
## they look unremarkable, with the right tail on a log-log scale, where the
## Cauchy sits on its power law and the normal falls away.
## Panel B is the consequence for the Route 1 tests: the four-group design of
## the power simulation, drawn from Cauchy instead of Fleishman variates.
##
## Uses no saved Monte Carlo output; the small simulation in panel B runs here.
## ---------------------------------------------------------------------------

if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
if (!requireNamespace("patchwork", quietly = TRUE)) stop("Package 'patchwork' is required.")
if (!requireNamespace("ggtext", quietly = TRUE)) stop("Package 'ggtext' is required.")

## Locate the shared helpers, whether this script is sourced from inside
## inst/simulations/ or from anywhere else with the package installed.
SIMDIR <- local({
  here <- getwd()
  if (file.exists(file.path(here, "fleishman_figure_typography.R"))) {
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

ggplot2 <- asNamespace("ggplot2")
patchwork <- asNamespace("patchwork")

## Replications and sizes of the small simulation in panel B. Set before
## sourcing to run it longer.
if (!exists("NREP_CAUCHY")) NREP_CAUCHY <- 3000L
if (!exists("NS_CAUCHY")) NS_CAUCHY <- c(10, 30, 100, 300, 1000)
ALPHA <- 0.05

## The two curves keep the group palette of the other figures: the Cauchy takes
## the colour of the widest group, the normal that of the narrowest.
COL_CAUCHY <- unname(fleishman_group_cols["D"])
COL_NORMAL <- unname(fleishman_group_cols["B"])
COL_LAW <- "grey35"

panel_header <- function(letter, description) {
  ggplot2$ggplot() +
    ggplot2$labs(title = fleishman_panel_title(letter, description)) +
    ggplot2$theme_void() +
    ggplot2$theme(
      plot.title = ggtext::element_markdown(
        size = FLEISHMAN_TEXT$panel_letter,
        margin = ggplot2$margin(0, 0, 0, 0)
      ),
      plot.title.position = "plot",
      plot.margin = ggplot2$margin(2, 4, 0, 4)
    )
}

fleishman_theme <- function() {
  ggplot2$theme_minimal(
    base_size = FLEISHMAN_TEXT$legend,
    base_family = FLEISHMAN_FONT_FAMILY
  ) +
    ggplot2$theme(
      plot.title = ggplot2$element_text(
        size = FLEISHMAN_TEXT$panel_title, hjust = 0.5, face = "plain"
      ),
      axis.title = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_title),
      axis.text = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_text),
      legend.text = ggplot2$element_text(size = FLEISHMAN_TEXT$legend),
      legend.title = ggplot2$element_text(size = FLEISHMAN_TEXT$legend),
      panel.grid.minor = ggplot2$element_blank()
    )
}

## ---- A, left: the densities on a linear scale ------------------------------
x_lin <- seq(-6, 6, length.out = 1200)
dens_lin <- rbind(
  data.frame(x = x_lin, density = dcauchy(x_lin), law = "Cauchy(0, 1)"),
  data.frame(x = x_lin, density = dnorm(x_lin), law = "N(0, 1)")
)
dens_lin$law <- factor(dens_lin$law, levels = c("Cauchy(0, 1)", "N(0, 1)"))

p_linear <- ggplot2$ggplot(
  dens_lin, ggplot2$aes(x = x, y = density, colour = law)
) +
  ggplot2$geom_line(linewidth = 0.7) +
  ggplot2$scale_colour_manual(
    values = c("Cauchy(0, 1)" = COL_CAUCHY, "N(0, 1)" = COL_NORMAL),
    name = NULL
  ) +
  ggplot2$labs(
    x = "Response value", y = "Theoretical density",
    title = "linear scale: the two densities look comparable"
  ) +
  fleishman_theme() +
  ggplot2$theme(legend.position = c(0.83, 0.85))

## ---- A, right: the right tail on a log-log scale ---------------------------
x_tail <- 10^seq(0, 2, length.out = 700)
dens_tail <- rbind(
  data.frame(x = x_tail, density = dcauchy(x_tail), law = "Cauchy(0, 1)"),
  data.frame(x = x_tail, density = dnorm(x_tail), law = "N(0, 1)"),
  data.frame(x = x_tail, density = 1 / (pi * x_tail^2), law = "1/(pi*x^2)")
)
dens_tail$law <- factor(
  dens_tail$law, levels = c("Cauchy(0, 1)", "N(0, 1)", "1/(pi*x^2)")
)
## Drop what falls below the panel, so the normal curve leaves the frame where
## it decays instead of being drawn along the lower limit.
dens_tail <- dens_tail[dens_tail$density >= 1e-12, , drop = FALSE]

p_tail <- ggplot2$ggplot(
  dens_tail,
  ggplot2$aes(x = x, y = density, colour = law, linetype = law)
) +
  ggplot2$geom_line(linewidth = 0.7, na.rm = TRUE) +
  ggplot2$scale_colour_manual(
    values = c("Cauchy(0, 1)" = COL_CAUCHY, "N(0, 1)" = COL_NORMAL,
               "1/(pi*x^2)" = COL_LAW),
    name = NULL,
    labels = c("Cauchy(0, 1)", "N(0, 1)", expression(1 / (pi * x^2)))
  ) +
  ggplot2$scale_linetype_manual(
    values = c("Cauchy(0, 1)" = "solid", "N(0, 1)" = "solid",
               "1/(pi*x^2)" = "dashed"),
    name = NULL,
    labels = c("Cauchy(0, 1)", "N(0, 1)", expression(1 / (pi * x^2)))
  ) +
  ggplot2$scale_x_log10() +
  ggplot2$scale_y_log10(limits = c(1e-12, 0.5)) +
  ggplot2$labs(
    x = "Response value", y = "Theoretical density",
    title = "right tail, log-log: the Cauchy sits on its power law"
  ) +
  fleishman_theme() +
  ggplot2$theme(legend.position = c(0.24, 0.24))

## ---- B: the Route 1 tests on Cauchy input ----------------------------------
## Four groups, the mean shifts of the power simulation, Cauchy instead of
## Fleishman variates. The rank branch's alternative is true throughout.
SHIFTS <- c(0, 0.25, 0.50, 0.75)

run_cauchy_cell <- function(n) {
  k <- length(SHIFTS)
  out <- replicate(NREP_CAUCHY, {
    y <- unlist(lapply(seq_len(k), function(i) rcauchy(n, SHIFTS[i], 1)))
    g <- factor(rep(seq_len(k), each = n))
    p_fisher <- summary(stats::aov(y ~ g))[[1]][1, "Pr(>F)"]
    p_welch <- stats::oneway.test(y ~ g, var.equal = FALSE)$p.value
    p_rank <- stats::kruskal.test(y, g)$p.value
    rs <- stats::rstandard(stats::lm(y ~ g))
    rs <- rs[is.finite(rs)]
    p_sw <- stats::shapiro.test(rs[seq_len(min(5000L, length(rs)))])$p.value
    c(p_fisher < ALPHA, p_welch < ALPHA, p_rank < ALPHA, p_sw < ALPHA)
  })
  data.frame(
    n_per_group = n,
    strategy = c("F", "W", "KW", "SW rejects normality"),
    rate = rowMeans(out)
  )
}

set.seed(20260805)
cauchy_power <- do.call(rbind, lapply(NS_CAUCHY, run_cauchy_cell))
cauchy_power$strategy <- factor(
  cauchy_power$strategy,
  levels = c("F", "W", "KW", "SW rejects normality")
)
write.csv(
  cauchy_power, file.path(OUTDIR, "cauchy_route1_rejection_rates.csv"),
  row.names = FALSE
)

STRATEGY_COLS <- c(
  "F" = unname(fleishman_group_cols["A"]),
  "W" = COL_NORMAL,
  "KW" = unname(fleishman_group_cols["C"]),
  "SW rejects normality" = COL_CAUCHY
)

p_power <- ggplot2$ggplot(
  cauchy_power,
  ggplot2$aes(x = n_per_group, y = rate, colour = strategy, shape = strategy)
) +
  ggplot2$geom_hline(yintercept = ALPHA, linewidth = 0.35,
                     linetype = "dashed", colour = "grey45") +
  ggplot2$geom_line(linewidth = 0.6) +
  ggplot2$geom_point(size = 2.4) +
  ggplot2$scale_colour_manual(values = STRATEGY_COLS, name = "test strategy") +
  ggplot2$scale_shape_manual(
    values = c("F" = 15, "W" = 17, "KW" = 4, "SW rejects normality" = 1),
    name = "test strategy"
  ) +
  ggplot2$scale_x_log10(breaks = NS_CAUCHY) +
  ggplot2$scale_y_continuous(
    limits = c(0, 1), breaks = seq(0, 1, by = 0.1),
    labels = scales::percent
  ) +
  ggplot2$labs(
    x = "n per group",
    y = "simulated rejection rate",
    title = paste0(
      "four Cauchy groups, mean shifts (0, 0.25, 0.50, 0.75); ",
      format(NREP_CAUCHY, big.mark = ","), " replications, alpha = 5%"
    )
  ) +
  fleishman_theme()

## ---- assembly --------------------------------------------------------------
combined <- patchwork$wrap_plots(
  panel_header("A", "Cauchy and normal densities"),
  patchwork$wrap_plots(p_linear, p_tail, nrow = 1),
  panel_header(
    "B",
    "Route 1 rejection rates under a location shift the mean cannot detect"
  ),
  p_power,
  ncol = 1,
  heights = c(0.08, 1, 0.08, 1)
) &
  ggplot2$theme(plot.margin = ggplot2$margin(2, 12, 6, 12))

outfile <- file.path(OUTDIR, "cauchy_heavy_tail_gate.png")
ggplot2$ggsave(outfile, combined, width = 16, height = 12,
               dpi = FLEISHMAN_DPI)
message("Wrote: ", outfile)
