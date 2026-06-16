## Theoretical input PDFs for the Route 1 power simulation.
##
## Matches:
##   dev/codexsimulation20160606_gamma_skew_sweep_4groups_power_skew4_B50000.R

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required.")
}

source(file.path("dev", "codexsimulation20160607_gamma_density_helpers.R"))

OUTDIR <- file.path(
  "dev",
  "codexsimulation20160606_gamma_skew_sweep_4groups_power_skew4_B50000_outputs"
)
FIGDIR <- file.path("vignettes", "figures")
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIGDIR, showWarnings = FALSE, recursive = TRUE)

skews <- c(0, 0.5, 1, 2)
shifts <- c(0, 0.25, 0.50, 0.75)
groups <- LETTERS[1:4]
group_cols <- gamma_group_cols
DENSITY_CAP <- 1.0

facet_label <- function(skew) {
  if (skew == 0) {
    return("N(0, 1)\nskew = 0; excess kurtosis = 0")
  }
  alpha <- shape_from_skew(skew)
  sprintf(
    "standardised \u0393(\u03b1 = %s, \u03b8 = 1)\nskew = %s; excess kurtosis = %s",
    format(alpha, trim = TRUE, scientific = FALSE),
    format(skew, trim = TRUE, scientific = FALSE),
    format(round(6 / alpha, 3), trim = TRUE, scientific = FALSE)
  )
}

make_density <- function(skew) {
  alpha <- shape_from_skew(skew)
  xlim <- c(-2.2, 4.2)
  xgrid <- seq(xlim[1], xlim[2], length.out = 700)
  do.call(rbind, lapply(seq_along(shifts), function(i) {
    curve <- if (skew == 0) {
      data.frame(
        x = xgrid,
        density = stats::dnorm(xgrid - shifts[i]),
        piece = "density"
      )
    } else {
      standardised_gamma_curve(
        alpha = alpha,
        shift = shifts[i],
        sd = 1,
        xlim = xlim,
        n = 700,
        y_cap = DENSITY_CAP
      )
    }
    out <- data.frame(
      x = curve$x,
      density = curve$density,
      group = groups[i],
      shift = shifts[i],
      skew = skew,
      piece = curve$piece,
      distribution = facet_label(skew),
      stringsAsFactors = FALSE
    )
    out
  }))
}

dens <- do.call(rbind, lapply(skews, make_density))
add_density_cap <- function(dat, cap) {
  pieces <- split(dat, interaction(dat$distribution, dat$group, drop = TRUE))
  do.call(rbind, lapply(pieces, function(sub) {
    sub$density_plot <- sub$density
    over <- which(is.finite(sub$density) & sub$density > cap)
    if (length(over) > 0) {
      after <- which(is.finite(sub$density) &
                       sub$density <= cap &
                       seq_along(sub$density) > max(over))
      if (length(after) > 0) {
        j <- min(after)
        k <- max(over[over < j])
        x_cap <- sub$x[k] +
          (cap - sub$density[k]) * (sub$x[j] - sub$x[k]) /
          (sub$density[j] - sub$density[k])
        cap_row <- sub[j, , drop = FALSE]
        cap_row$x <- x_cap
        cap_row$density <- cap
        cap_row$density_plot <- cap
        sub$density_plot[sub$density > cap] <- NA_real_
        sub <- rbind(sub, cap_row)
        sub <- sub[order(sub$x), , drop = FALSE]
      }
    }
    sub
  }))
}

dens <- add_density_cap(dens, DENSITY_CAP)
dens$distribution <- factor(dens$distribution, levels = vapply(skews, facet_label, character(1)))
scale_anchors <- data.frame(
  distribution = vapply(skews, facet_label, character(1)),
  x = -2.2,
  density_plot = vapply(skews, function(skew) {
    if (skew <= 1) 0.50 else DENSITY_CAP
  }, numeric(1)),
  stringsAsFactors = FALSE
)
scale_anchors$distribution <- factor(scale_anchors$distribution,
                                     levels = levels(dens$distribution))
panel_labels <- do.call(rbind, lapply(skews, function(skew) {
  sub <- dens[dens$skew == skew, , drop = FALSE]
  xr <- range(sub$x)
  alpha <- shape_from_skew(skew)
  line1 <- if (skew == 0) {
    "N(0, 1)"
  } else {
    sprintf(
      "standardised \u0393(\u03b1 = %s, \u03b8 = 1)",
      format(alpha, trim = TRUE, scientific = FALSE)
    )
  }
  line2 <- sprintf(
    "skew = %s; excess kurtosis = %s",
    format(skew, trim = TRUE, scientific = FALSE),
    if (skew == 0) "0" else format(round(6 / alpha, 3), trim = TRUE,
                                    scientific = FALSE)
  )
  data.frame(
    distribution = facet_label(skew),
    number = paste0(match(skew, skews), ")"),
    line1 = line1,
    line2 = line2,
    x_num = xr[1],
    x_text = xr[1] + 0.085 * diff(xr),
    stringsAsFactors = FALSE
  )
}))
panel_labels$distribution <- factor(panel_labels$distribution,
                                    levels = levels(dens$distribution))

p <- ggplot2::ggplot(
  dens,
  ggplot2::aes(
    x = x,
    y = density_plot,
    colour = group,
    group = interaction(group, piece)
  )
) +
  ggplot2::geom_blank(
    data = scale_anchors,
    ggplot2::aes(x = x, y = density_plot),
    inherit.aes = FALSE
  ) +
  ggplot2::geom_line(linewidth = 0.7, na.rm = TRUE) +
  ggplot2::geom_text(
    data = panel_labels,
    ggplot2::aes(x = x_num, y = Inf, label = number),
    inherit.aes = FALSE,
    family = "serif",
    fontface = "bold",
    hjust = 0,
    vjust = -2.0,
    size = 2.8
  ) +
  ggplot2::geom_text(
    data = panel_labels,
    ggplot2::aes(x = x_text, y = Inf, label = line1),
    inherit.aes = FALSE,
    family = "serif",
    hjust = 0,
    vjust = -2.0,
    size = 2.8
  ) +
  ggplot2::geom_text(
    data = panel_labels,
    ggplot2::aes(x = x_num, y = Inf, label = line2),
    inherit.aes = FALSE,
    family = "serif",
    hjust = 0,
    vjust = -0.7,
    size = 2.8
  ) +
  ggplot2::facet_wrap(~ distribution, scales = "free_y", nrow = 1) +
  ggplot2::scale_colour_manual(values = group_cols, name = "Group") +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0.02, 0))
  ) +
  ggplot2::coord_cartesian(clip = "off") +
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
    strip.text = ggplot2::element_text(size = 8, family = "serif",
                                       colour = NA),
    strip.background = ggplot2::element_blank(),
    legend.position = "right",
    plot.title = ggplot2::element_text(hjust = 0),
    plot.title.position = "plot"
  )

outfile <- "gamma_skew_sweep_4groups_power_pdf.png"
ggplot2::ggsave(file.path(OUTDIR, outfile), p, width = 14, height = 3.4, dpi = 360)
ggplot2::ggsave(file.path(FIGDIR, outfile), p, width = 14, height = 3.4, dpi = 360)

message("Wrote: ", file.path(OUTDIR, outfile))
message("Wrote: ", file.path(FIGDIR, outfile))
