## ---------------------------------------------------------------------------
## Route 1 figure variants:
##   1. Equal SD: identical distributions and identical means.
##   2. Unequal SD: identical means, unequal distributions.
##
## Uses saved Monte Carlo output only. Does not rerun the simulation.
## ---------------------------------------------------------------------------

OUTDIR <- file.path(
  "dev",
  "codexsimulation20160606_route1_equal_mean_blanca_zimmerman_skew4_outputs"
)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required.")
}
if (!requireNamespace("patchwork", quietly = TRUE)) {
  stop("Package 'patchwork' is required.")
}
if (!requireNamespace("scales", quietly = TRUE)) {
  stop("Package 'scales' is required.")
}
if (!requireNamespace("colorspace", quietly = TRUE)) {
  stop("Package 'colorspace' is required.")
}

sim <- readRDS(file.path(OUTDIR, "route1_equal_mean_blanca_zimmerman.rds"))

ALPHA <- 0.05
ggplot2 <- asNamespace("ggplot2")
patchwork <- asNamespace("patchwork")
scales <- asNamespace("scales")
source(file.path("dev", "codexsimulation20160607_gamma_density_helpers.R"))

design_levels <- levels(sim$design)
if (is.null(design_levels)) design_levels <- unique(sim$design)
preferred_design_levels <- c(
  "balanced n, equal SD",
  "unbalanced n, equal SD",
  "balanced n, unequal SD",
  "unbalanced n, larger n with larger SD",
  "unbalanced n, larger n with smaller SD"
)
design_levels <- c(
  preferred_design_levels[preferred_design_levels %in% design_levels],
  setdiff(design_levels, preferred_design_levels)
)

equal_designs <- c("balanced n, equal SD", "unbalanced n, equal SD")
unequal_designs <- c(
  "balanced n, unequal SD",
  "unbalanced n, larger n with larger SD",
  "unbalanced n, larger n with smaller SD"
)
equal_designs <- equal_designs[equal_designs %in% design_levels]
unequal_designs <- unequal_designs[unequal_designs %in% design_levels]

skew_levels <- levels(sim$skew_label)
if (is.null(skew_levels)) skew_levels <- unique(sim$skew_label)
base_distribution_labels <- setNames(
  ifelse(
    grepl("^normal", skew_levels),
    "base normal: skew = 0, excess kurtosis = 0",
    sub("Gamma\n", "base Gamma: ", skew_levels)
  ),
  skew_levels
)

standard_density <- function(x, skew) {
  if (skew == 0) return(stats::dnorm(x))
  standardised_gamma_density(x, alpha = shape_from_skew(skew), shift = 0)
}

scaled_density <- function(x, skew, sd) {
  if (skew == 0) return(stats::dnorm(x / sd) / sd)
  shape <- shape_from_skew(skew)
  density <- standardised_gamma_density(x / sd, alpha = shape, shift = 0) / sd
  density[!is.finite(density)] <- NA_real_
  density
}

one_per_distribution <- sim[!duplicated(sim[c("design", "skew_label")]), ]
pdf_rows <- list()
idx <- 1
x_grid <- seq(-2.5, 5, length.out = 700)

for (i in seq_len(nrow(one_per_distribution))) {
  one <- one_per_distribution[i, ]
  sd_vec <- as.numeric(strsplit(one$sd_per_group, ", ")[[1]])
  for (j in seq_along(sd_vec)) {
    pdf_rows[[idx]] <- data.frame(
      design = one$design,
      skew_label = one$skew_label,
      group = LETTERS[j],
      sd = sd_vec[j],
      x = x_grid,
      density = scaled_density(x_grid, one$skew, sd_vec[j]),
      row.names = NULL
    )
    idx <- idx + 1
  }
}

pdf_data <- do.call(rbind, pdf_rows)
pdf_data$design <- factor(pdf_data$design, levels = design_levels)
pdf_data$skew_label <- factor(pdf_data$skew_label, levels = skew_levels)

sim$design <- factor(sim$design, levels = design_levels)
sim$skew_base_label <- factor(
  base_distribution_labels[as.character(sim$skew_label)],
  levels = base_distribution_labels
)

make_pdf_plot <- function(design_name, title, show_group_legend = TRUE,
                          black_curves = FALSE) {
  one_pdf <- subset(pdf_data, design == design_name)
  panels <- vector("list", length(skew_levels))
  for (i in seq_along(skew_levels)) {
    skew_level <- skew_levels[i]
    panel_data <- subset(one_pdf, skew_label == skew_level)
    y_limit <- if (grepl("skew = 6", skew_level, fixed = TRUE)) 3 else 1.2
    line_layer <- if (black_curves) {
      ggplot2$geom_line(colour = "black", linewidth = 0.55, alpha = 0.9,
                         na.rm = TRUE)
    } else {
      ggplot2$geom_line(linewidth = 0.55, alpha = 0.9, na.rm = TRUE)
    }
    panels[[i]] <- ggplot2$ggplot(
      panel_data,
      ggplot2$aes(x = x, y = density, colour = group)
    ) +
      line_layer +
      ggplot2$coord_cartesian(xlim = c(-2.5, 5), ylim = c(0, y_limit)) +
      ggplot2$scale_colour_brewer(palette = "Dark2", name = "group") +
      ggplot2$labs(
        title = base_distribution_labels[[as.character(skew_level)]],
        x = "Response values",
        y = if (i == 1) "Density" else NULL
      ) +
      ggplot2$theme_minimal(base_size = 8.5) +
      ggplot2$theme(
        legend.position = if (show_group_legend && i == length(skew_levels)) {
          "right"
        } else {
          "none"
        },
        legend.box.margin = ggplot2$margin(0, 0, 0, 4),
        panel.grid.minor = ggplot2$element_blank(),
        panel.border = ggplot2$element_rect(colour = "black", fill = NA,
                                            linewidth = 0.2),
        plot.title = ggplot2$element_text(size = 7.5, hjust = 0.5,
                                          lineheight = 0.9)
      )
  }
  patchwork$wrap_plots(panels, nrow = 1) +
    patchwork$plot_annotation(
      title = title,
      subtitle = NULL
    ) &
    ggplot2$theme(
      plot.title = ggplot2$element_text(face = "bold", size = 8.5),
      plot.subtitle = ggplot2$element_text(size = 8)
    )
}

block_title <- function(label) {
  ggplot2$ggplot() +
    ggplot2$annotate(
      "text",
      x = 0,
      y = 0.5,
      label = label,
      hjust = 0,
      vjust = 0.5,
      fontface = "bold",
      size = 3.1,
      lineheight = 0.95
    ) +
    ggplot2$theme_void() +
    ggplot2$coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off")
}

make_rejection_rows <- function(dat, strategies) {
  rows <- list(
    Fisher = transform(dat, strategy = "Fisher", rejection = fisher_rejection),
    Welch = transform(dat, strategy = "Welch", rejection = welch_rejection),
    F = transform(dat, strategy = "F", rejection = fisher_rejection),
    W = transform(dat, strategy = "W", rejection = welch_rejection),
    L = transform(dat, strategy = "L",
                             rejection = levene_route_rejection),
    KW = transform(dat, strategy = "KW",
                                 rejection = rank_rejection),
    SW = transform(dat, strategy = "SW",
                   rejection = sw_rejection),
    `SW+L` = transform(dat, strategy = "SW+L",
                          rejection = sw_gate_rejection)
  )
  out <- do.call(rbind, rows[strategies])
  out$strategy <- factor(out$strategy, levels = strategies)
  out$row_label <- sprintf("%s | %s", out$mean_n_per_group, out$strategy)
  mean_levels <- sort(unique(out$mean_n_per_group))
  out$row_label <- factor(
    out$row_label,
    levels = as.vector(vapply(mean_levels, function(m) {
      sprintf("%s | %s", m, rev(strategies))
    }, character(length(strategies))))
  )
  out$row_axis_label <- ifelse(
    out$strategy == strategies[1],
    sprintf("%s | %s", out$mean_n_per_group, out$strategy),
    as.character(out$strategy)
  )
  out$rejection_label <- sprintf("%.1f", 100 * out$rejection)
  out$gate_bar_label <- ""
  out$split_f_label <- ""
  out$split_w_label <- ""
  out$split_kw_label <- ""
  is_l <- out$strategy == "L"
  out$gate_bar_label[is_l] <- "|"
  out$split_f_label[is_l] <- sprintf(
    "%.0f",
    100 * out$levene_select_fisher_probability[is_l]
  )
  out$split_w_label[is_l] <- sprintf(
    "%.0f",
    100 * out$levene_select_welch_probability[is_l]
  )
  is_sw <- out$strategy == "SW"
  out$gate_bar_label[is_sw] <- "|"
  out$split_w_label[is_sw] <- sprintf(
    "%.0f",
    100 * out$sw_route_welch_probability[is_sw]
  )
  out$split_kw_label[is_sw] <- sprintf(
    "%.0f",
    100 * out$sw_route_rank_probability[is_sw]
  )
  is_sw_l <- out$strategy == "SW+L"
  out$gate_bar_label[is_sw_l] <- "|"
  out$split_f_label[is_sw_l] <- sprintf(
    "%.0f",
    100 * out$route_fisher_probability[is_sw_l]
  )
  out$split_w_label[is_sw_l] <- sprintf(
    "%.0f",
    100 * out$route_welch_probability[is_sw_l]
  )
  out$split_kw_label[is_sw_l] <- sprintf(
    "%.0f",
    100 * out$route_rank_probability[is_sw_l]
  )
  out
}

design_formula_label <- function(design_name, subtitle) {
  if (design_name == "balanced n, equal SD") {
    return(bquote(paste(
      "balanced ", n[i], ", equal SD: ", .(subtitle), "; ",
      n[i] == n, "; ",
      SD[i] == .("(1.0, 1.0, 1.0, 1.0)")
    )))
  }
  if (design_name == "unbalanced n, equal SD") {
    return(bquote(paste(
      "unbalanced ", n[i], ", equal SD: ", .(subtitle), "; ",
      n[i] == ceiling(bar(n) %.% .("(0.5, 0.8, 1.2, 1.5)")), "; ",
      SD[i] == .("(1.0, 1.0, 1.0, 1.0)")
    )))
  }
  if (design_name == "balanced n, unequal SD") {
    return(bquote(paste(
      "balanced ", n[i], ", unequal SD: ", .(subtitle), "; ",
      n[i] == n, "; ",
      SD[i] == .("(1.0, 1.3, 1.7, 2.2)")
    )))
  }
  if (design_name == "unbalanced n, larger n with larger SD") {
    return(bquote(paste(
      "unbalanced ", n[i], ", larger ", n[i], " with larger SD: ",
      .(subtitle), "; ",
      n[i] == ceiling(bar(n) %.% .("(0.5, 0.8, 1.2, 1.5)")), "; ",
      SD[i] == .("(1.0, 1.3, 1.7, 2.2)")
    )))
  }
  if (design_name == "unbalanced n, larger n with smaller SD") {
    return(bquote(paste(
      "unbalanced ", n[i], ", larger ", n[i], " with smaller SD: ",
      .(subtitle), "; ",
      n[i] == ceiling(bar(n) %.% .("(0.5, 0.8, 1.2, 1.5)")), "; ",
      SD[i] == .("(2.2, 1.7, 1.3, 1.0)")
    )))
  }
  ""
}

temporary_caption <- paste(
  "TEMP CAPTION: i = 1, ..., 4 indexes the four groups;",
  "nbar denotes the target mean group size for unbalanced designs.",
  "All numbers are percentages. The first value is the final-test rejection rate.",
  "For gated strategies only, the columns after | are route split rates to F,",
  "W, and KW.",
  sep = " "
)

axis_labels <- function(dat) {
  labels <- unique(dat[c("row_label", "row_axis_label")])
  stats::setNames(labels$row_axis_label, labels$row_label)
}

separator_values <- function(dat, rows_per_n) {
  n_blocks <- length(unique(dat$mean_n_per_group))
  seq(rows_per_n + 0.5, rows_per_n * (n_blocks - 1) + 0.5,
      by = rows_per_n)
}

mean_fill_scale <- function(limit = 1) {
  anchors <- c(0, 0.025, 0.05, 0.065, 0.075, 0.10, 0.20, 0.50, 1.00)
  anchor_cols <- c(
    "#084594", "#6cc5c2", "#4daf4a", "#a8d96a", "#cce88a",
    "#fdae61", "#f46d43", "#d73027", "#9e1b1b"
  )
  keep <- anchors <= limit
  ggplot2$scale_fill_gradientn(
    colours = anchor_cols[keep],
    values = scales$rescale(anchors[keep], from = c(0, limit)),
    limits = c(0, limit),
    oob = scales$squish,
    labels = scales$percent,
    na.value = "white",
    name = "Rejection\nrate"
  )
}

make_rejection_plot <- function(design_name, strategies, subtitle,
                                show_legend = FALSE, fill_limit = 1) {
  one <- subset(
    make_rejection_rows(sim, strategies),
    design == design_name
  )
  one$strategy <- as.character(one$strategy)
  mean_levels <- sort(unique(one$mean_n_per_group), decreasing = TRUE)
  n_pattern <- if (startsWith(design_name, "balanced")) {
    "n[i] == %s"
  } else {
    "bar(n) == %s"
  }
  one$mean_n_label <- factor(
    sprintf(n_pattern, one$mean_n_per_group),
    levels = sprintf(n_pattern, mean_levels)
  )
  one$plot_strategy <- as.character(one$strategy)
  header <- unique(one[c("design", "mean_n_per_group", "skew_base_label",
                         "mean_n_label")])
  header$strategy <- "header"
  header$plot_strategy <- "header"
  header$rejection <- NA_real_
  header$rejection_label <- "rej"
  header$gate_bar_label <- "|"
  header$split_f_label <- "F"
  header$split_w_label <- "W"
  header$split_kw_label <- "KW"
  for (nm in setdiff(names(one), names(header))) {
    header[[nm]] <- NA
  }
  one <- rbind(one, header[names(one)])
  one$plot_strategy <- factor(
    one$plot_strategy,
    levels = c(rev(strategies), "header")
  )
  ggplot2$ggplot(
    one,
    ggplot2$aes(x = skew_base_label, y = plot_strategy, fill = rejection)
  ) +
    ggplot2$geom_tile(colour = NA) +
    ggplot2$geom_text(
      ggplot2$aes(label = rejection_label),
      position = ggplot2$position_nudge(x = -0.34),
      size = 2.8
    ) +
    ggplot2$geom_text(
      ggplot2$aes(label = gate_bar_label),
      position = ggplot2$position_nudge(x = -0.19),
      size = 2.8
    ) +
    ggplot2$geom_text(
      ggplot2$aes(label = split_f_label),
      position = ggplot2$position_nudge(x = -0.04),
      size = 2.8
    ) +
    ggplot2$geom_text(
      ggplot2$aes(label = split_w_label),
      position = ggplot2$position_nudge(x = 0.17),
      size = 2.8
    ) +
    ggplot2$geom_text(
      ggplot2$aes(label = split_kw_label),
      position = ggplot2$position_nudge(x = 0.37),
      size = 2.8
    ) +
    ggplot2$scale_y_discrete(
      labels = c(stats::setNames(strategies, strategies), header = "")
    ) +
    ggplot2$facet_grid(
      stats::as.formula("mean_n_label ~ ."),
      switch = "y",
      labeller = ggplot2$label_parsed
    ) +
    mean_fill_scale(fill_limit) +
    ggplot2$labs(
      x = NULL,
      y = "strategy"
    ) +
    ggplot2$theme_minimal(base_size = 9) +
    ggplot2$theme(
      axis.text.x = ggplot2$element_blank(),
      axis.ticks.x = ggplot2$element_blank(),
      panel.grid = ggplot2$element_blank(),
      panel.spacing.y = grid::unit(0.08, "lines"),
      strip.placement = "outside",
      strip.background.y = ggplot2$element_rect(
        colour = "grey35",
        fill = "grey95",
        linewidth = 0.25
      ),
      strip.text.y.left = ggplot2$element_text(
        angle = 0,
        face = "bold",
        size = 8.5
      ),
      legend.position = if (show_legend) "right" else "none"
    )
}

make_equal_plot <- function() {
  strategies <- c("F", "W", "L", "KW", "SW", "SW+L")
  plot_list <- list()
  height_list <- numeric()
  plot_list[[1]] <- make_pdf_plot(
    "balanced n, equal SD",
    "Identical distributions; mean_i = 0 and SD_i = 1 (i = 1, ..., 4)",
    show_group_legend = FALSE,
    black_curves = TRUE
  )
  height_list[1] <- 0.70
  balanced_plot <- make_rejection_plot(
    "balanced n, equal SD",
    strategies,
    "identical distributions and identical means; empirical Type I error (%)",
    show_legend = TRUE,
    fill_limit = 0.20
  )
  unbalanced_plot <- make_rejection_plot(
    "unbalanced n, equal SD",
    strategies,
    "identical distributions and identical means; empirical Type I error (%)",
    show_legend = FALSE,
    fill_limit = 0.20
  )
  plot_list[[2]] <- block_title(
    "a) balanced: n_i = n for all groups i = 1, ..., 4"
  )
  height_list[2] <- 0.09
  plot_list[[3]] <- balanced_plot
  height_list[3] <- 0.85
  plot_list[[4]] <- patchwork$plot_spacer()
  height_list[4] <- 0.02
  plot_list[[5]] <- block_title(
    "b) unbalanced n_i: n_i = ceiling(nbar * (0.5, 0.8, 1.2, 1.5))"
  )
  height_list[5] <- 0.09
  plot_list[[6]] <- unbalanced_plot
  height_list[6] <- 0.85
  patchwork$wrap_plots(plot_list, ncol = 1, heights = height_list) +
    patchwork$plot_layout(guides = "keep") +
    patchwork$plot_annotation(
      title = "Route 1 simulation: identical distributions; mean_i = 0 and SD_i = 1 (i = 1, ..., 4)",
      caption = temporary_caption
    ) &
    ggplot2$theme(
      plot.caption = ggplot2$element_text(hjust = 0),
      plot.caption.position = "plot",
      plot.margin = ggplot2$margin(8, 14, 8, 14)
    )
}

make_unequal_plot <- function() {
  strategies <- c("F", "W", "L", "SW", "SW+L")
  plot_list <- list()
  height_list <- numeric()
  plot_list[[1]] <- make_pdf_plot(
    "balanced n, unequal SD",
    "Four groups with identical means; SD_i = (1.0, 1.3, 1.7, 2.2)"
  )
  height_list[1] <- 0.70
  plot_list[[2]] <- block_title(
    "a) balanced: n_i = n for all groups i = 1, ..., 4"
  )
  height_list[2] <- 0.09
  plot_list[[3]] <- make_rejection_plot(
    "balanced n, unequal SD",
    strategies,
    "identical means, unequal distributions; rejection rate (%)",
    show_legend = TRUE,
    fill_limit = 1
  )
  height_list[3] <- 0.75
  plot_list[[4]] <- block_title(
    "b) unbalanced: n_i = ceiling(nbar * (0.5, 0.8, 1.2, 1.5)); larger n with larger SD"
  )
  height_list[4] <- 0.09
  plot_list[[5]] <- make_rejection_plot(
    "unbalanced n, larger n with larger SD",
    strategies,
    "identical means, unequal distributions; rejection rate (%)",
    show_legend = FALSE,
    fill_limit = 1
  )
  height_list[5] <- 0.75
  plot_list[[6]] <- block_title(
    "c) unbalanced: n_i = ceiling(nbar * (0.5, 0.8, 1.2, 1.5)); larger n with smaller SD"
  )
  height_list[6] <- 0.09
  plot_list[[7]] <- make_rejection_plot(
    "unbalanced n, larger n with smaller SD",
    strategies,
    "identical means, unequal distributions; rejection rate (%)",
    show_legend = FALSE,
    fill_limit = 1
  )
  height_list[7] <- 0.75
  patchwork$wrap_plots(plot_list, ncol = 1, heights = height_list) +
    patchwork$plot_layout(guides = "keep") +
    patchwork$plot_annotation(
      title = "Route 1 simulation: identical means and unequal distributions; mean_i = 0 (i = 1, ..., 4)",
      caption = temporary_caption
    ) &
    ggplot2$theme(
      plot.caption = ggplot2$element_text(hjust = 0),
      plot.caption.position = "plot",
      plot.margin = ggplot2$margin(8, 14, 8, 14)
    )
}

equal_outfile <- file.path(
  OUTDIR,
  "route1_identical_distributions_typeI_with_kw_skew4.png"
)
ggplot2$ggsave(
  equal_outfile,
  make_equal_plot(),
  width = 15,
  height = 11,
  dpi = 180
)
message("Wrote: ", equal_outfile)

unequal_outfile <- file.path(
  OUTDIR,
  "route1_equal_means_unequal_distributions_without_kw_skew4.png"
)
ggplot2$ggsave(
  unequal_outfile,
  make_unequal_plot(),
  width = 15,
  height = 12,
  dpi = 180
)
message("Wrote: ", unequal_outfile)
