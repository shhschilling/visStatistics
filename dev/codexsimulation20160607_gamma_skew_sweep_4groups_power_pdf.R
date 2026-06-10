## Theoretical input PDFs for the Route 1 power simulation.
##
## Matches:
##   dev/codexsimulation20160606_gamma_skew_sweep_4groups_power_skew4_B50000.R

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required.")
}

OUTDIR <- file.path(
  "dev",
  "codexsimulation20160606_gamma_skew_sweep_4groups_power_skew4_B50000_outputs"
)
FIGDIR <- file.path("vignettes", "figures")
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIGDIR, showWarnings = FALSE, recursive = TRUE)

skews <- c(0, 0.1, 0.5, 1, 2, 4)
shifts <- c(0, 0.25, 0.50, 0.75)
groups <- LETTERS[1:4]
group_cols <- c(A = "#1b9e77", B = "#d95f02", C = "#7570b3", D = "#e7298a")

shape_from_skew <- function(skew) {
  if (skew == 0) Inf else (2 / skew)^2
}

facet_label <- function(skew) {
  if (skew == 0) {
    return("N(0, 1)\nskew = 0; excess kurtosis = 0")
  }
  alpha <- shape_from_skew(skew)
  sprintf(
    "standardised Gamma(\u03b1 = %s, \u03b8 = 1)\nskew = %s; excess kurtosis = %s",
    format(alpha, trim = TRUE, scientific = FALSE),
    format(skew, trim = TRUE, scientific = FALSE),
    format(round(6 / alpha, 3), trim = TRUE, scientific = FALSE)
  )
}

standardised_gamma_density <- function(y, alpha, shift) {
  x <- (y - shift) * sqrt(alpha) + alpha
  out <- rep(0, length(y))
  ok <- x > 0
  out[ok] <- stats::dgamma(x[ok], shape = alpha, scale = 1) * sqrt(alpha)
  out
}

make_density <- function(skew) {
  alpha <- shape_from_skew(skew)
  if (skew == 0) {
    xlim <- stats::qnorm(c(0.001, 0.999)) + range(shifts)
  } else {
    xlim <- (stats::qgamma(c(0.003, 0.997), shape = alpha, scale = 1) -
               alpha) / sqrt(alpha) + range(shifts)
  }
  xgrid <- seq(xlim[1], xlim[2], length.out = 700)
  do.call(rbind, lapply(seq_along(shifts), function(i) {
    dens <- if (skew == 0) {
      stats::dnorm(xgrid - shifts[i])
    } else {
      standardised_gamma_density(xgrid, alpha = alpha, shift = shifts[i])
    }
    data.frame(
      x = xgrid,
      density = dens,
      group = groups[i],
      shift = shifts[i],
      skew = skew,
      distribution = facet_label(skew),
      stringsAsFactors = FALSE
    )
  }))
}

dens <- do.call(rbind, lapply(skews, make_density))
dens$density_plot <- pmin(dens$density, 1.1)
dens$distribution <- factor(dens$distribution, levels = vapply(skews, facet_label, character(1)))

p <- ggplot2::ggplot(dens, ggplot2::aes(x = x, y = density_plot, colour = group)) +
  ggplot2::geom_line(linewidth = 0.7) +
  ggplot2::geom_vline(
    data = unique(dens[c("distribution", "group", "shift")]),
    ggplot2::aes(xintercept = shift, colour = group),
    linetype = "dashed",
    linewidth = 0.35,
    show.legend = FALSE
  ) +
  ggplot2::facet_wrap(~ distribution, scales = "free", nrow = 1) +
  ggplot2::scale_colour_manual(values = group_cols, name = "Group") +
  ggplot2::labs(
    title = "a) Input distributions",
    subtitle = "Group means after standardisation: A = 0, B = 0.25, C = 0.50, D = 0.75",
    x = "Response value after standardisation and group shift",
    y = "Theoretical density"
  ) +
  ggplot2::theme_minimal(base_size = 10, base_family = "serif") +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "grey35", fill = NA, linewidth = 0.35),
    strip.text = ggplot2::element_text(size = 8),
    legend.position = "right",
    plot.title = ggplot2::element_text(hjust = 0),
    plot.title.position = "plot"
  )

outfile <- "gamma_skew_sweep_4groups_power_pdf.png"
ggplot2::ggsave(file.path(OUTDIR, outfile), p, width = 14, height = 3.4, dpi = 360)
ggplot2::ggsave(file.path(FIGDIR, outfile), p, width = 14, height = 3.4, dpi = 360)

message("Wrote: ", file.path(OUTDIR, outfile))
message("Wrote: ", file.path(FIGDIR, outfile))
