## ---------------------------------------------------------------------------
## Route 1 heat map with theoretical population PDFs.
##
## Uses the saved Monte Carlo output. It does not rerun the simulation.
## ---------------------------------------------------------------------------

OUTDIR <- file.path(
  "dev",
  "codexsimulation20160603_route1_equal_mean_blanca_zimmerman_outputs"
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
source(file.path("dev", "codexsimulation20160607_gamma_density_helpers.R"))

sim <- readRDS(file.path(OUTDIR, "route1_equal_mean_blanca_zimmerman.rds"))

ALPHA <- 0.05
ggplot2 <- asNamespace("ggplot2")
patchwork <- asNamespace("patchwork")
scales <- asNamespace("scales")

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
pdf_blocks <- list(
  list(
    pdf_design = "balanced n, equal SD",
    title = "Equal SD; SD = 1.0, 1.0, 1.0, 1.0",
    heatmap_designs = c("balanced n, equal SD", "unbalanced n, equal SD")
  ),
  list(
    pdf_design = "balanced n, unequal SD",
    title = "Unequal SD; SD values = 1.0, 1.3, 1.7, 2.2",
    heatmap_designs = c(
      "balanced n, unequal SD",
      "unbalanced n, larger n with larger SD",
      "unbalanced n, larger n with smaller SD"
    )
  )
)
pdf_blocks <- lapply(pdf_blocks, function(block) {
  block$heatmap_designs <- block$heatmap_designs[
    block$heatmap_designs %in% design_levels
  ]
  block
})
pdf_blocks <- Filter(function(block) {
  block$pdf_design %in% design_levels && length(block$heatmap_designs) > 0
}, pdf_blocks)
skew_levels <- levels(sim$skew_label)
if (is.null(skew_levels)) skew_levels <- unique(sim$skew_label)
base_distribution_labels <- setNames(
  ifelse(
    grepl("^normal", skew_levels),
    "base normal\nskew = 0",
    sub("Gamma\n", "base Gamma\n", sub("\nexcess kurtosis = .*", "", skew_levels))
  ),
  skew_levels
)

shape_from_skew <- function(skew) {
  if (skew == 0) Inf else (2 / skew)^2
}

standard_density <- function(x, skew) {
  if (skew == 0) return(stats::dnorm(x))
  shape <- shape_from_skew(skew)
  standardised_gamma_density(x, alpha = shape, shift = 0)
}

scaled_density <- function(x, skew, sd) {
  standard_density(x / sd, skew) / sd
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

gate <- sim
gate$design <- factor(gate$design, levels = design_levels)
gate$skew_base_label <- factor(
  base_distribution_labels[as.character(gate$skew_label)],
  levels = base_distribution_labels
)
gate$gate_label <- sprintf(
  "F %.0f%% | W %.0f%% | K %.0f%%",
  100 * gate$route_fisher_probability,
  100 * gate$route_welch_probability,
  100 * gate$route_rank_probability
)
gate$levene_label <- sprintf(
  "F %.0f%% | W %.0f%%",
  100 * gate$levene_select_fisher_probability,
  100 * gate$levene_select_welch_probability
)
gate$rejection_label <- sprintf("%.1f%%\n%s", 100 * gate$sw_gate_rejection,
                                gate$gate_label)

strategy_levels <- c(
  "Fisher",
  "Welch",
  "Levene F/W",
  "SW gate",
  "Kruskal-Wallis"
)

make_strategy_rows <- function(dat) {
  out <- rbind(
    transform(dat, strategy = "Fisher", rejection = fisher_rejection),
    transform(dat, strategy = "Welch", rejection = welch_rejection),
    transform(dat, strategy = "Levene F/W", rejection = levene_route_rejection),
    transform(dat, strategy = "SW gate", rejection = sw_gate_rejection),
    transform(dat, strategy = "Kruskal-Wallis", rejection = rank_rejection)
  )
  out$strategy <- factor(out$strategy, levels = strategy_levels)
  out$cell_label <- sprintf("%.1f%%", 100 * out$rejection)
  out$cell_label[out$strategy == "Levene F/W"] <- sprintf(
    "%.1f%%\n%s",
    100 * out$rejection[out$strategy == "Levene F/W"],
    out$levene_label[out$strategy == "Levene F/W"]
  )
  out$cell_label[out$strategy == "SW gate"] <- sprintf(
    "%.1f%%\n%s",
    100 * out$rejection[out$strategy == "SW gate"],
    out$gate_label[out$strategy == "SW gate"]
  )
  mean_levels <- sort(unique(out$mean_n_per_group))
  row_levels <- as.vector(vapply(mean_levels, function(m) {
    sprintf("%s | %s", m, strategy_levels)
  }, character(length(strategy_levels))))
  out$row_label <- sprintf("%s | %s", out$mean_n_per_group, out$strategy)
  out$row_label <- factor(out$row_label, levels = row_levels)
  out$row_axis_label <- ifelse(
    out$strategy == "Fisher",
    sprintf("%s | %s", out$mean_n_per_group, out$strategy),
    as.character(out$strategy)
  )
  out
}

gate_long <- make_strategy_rows(gate)
best_rows <- do.call(rbind, by(
  gate_long,
  list(gate_long$design, gate_long$mean_n_per_group, gate_long$skew_base_label),
  function(dat) dat[abs(dat$rejection - ALPHA) ==
                      min(abs(dat$rejection - ALPHA)), ]
))
best_rows <- best_rows[order(
  best_rows$design,
  best_rows$mean_n_per_group,
  best_rows$skew,
  best_rows$strategy
), ]

comparison_table <- gate_long[c(
  "design",
  "mean_n_per_group",
  "skew_base_label",
  "strategy",
  "rejection"
)]
comparison_table$abs_deviation_from_alpha <- abs(comparison_table$rejection - ALPHA)
comparison_table$is_closest_to_alpha <- interaction(
  comparison_table$design,
  comparison_table$mean_n_per_group,
  comparison_table$skew_base_label,
  comparison_table$strategy,
  drop = TRUE
) %in% interaction(
  best_rows$design,
  best_rows$mean_n_per_group,
  best_rows$skew_base_label,
  best_rows$strategy,
  drop = TRUE
)
write.csv(
  comparison_table,
  file.path(OUTDIR, "route1_equal_mean_strategy_comparison_table.csv"),
  row.names = FALSE
)

comparison_wide <- reshape(
  comparison_table[c("design", "mean_n_per_group", "skew_base_label",
                     "strategy", "rejection")],
  idvar = c("design", "mean_n_per_group", "skew_base_label"),
  timevar = "strategy",
  direction = "wide"
)
names(comparison_wide) <- sub("^rejection\\.", "", names(comparison_wide))
lowest_table <- aggregate(
  strategy ~ design + mean_n_per_group + skew_base_label,
  subset(comparison_table, is_closest_to_alpha),
  function(x) paste(as.character(x), collapse = "; ")
)
names(lowest_table)[names(lowest_table) == "strategy"] <- "closest_to_alpha_strategy"
comparison_wide <- merge(
  comparison_wide,
  lowest_table,
  by = c("design", "mean_n_per_group", "skew_base_label"),
  all.x = TRUE
)
write.csv(
  comparison_wide,
  file.path(OUTDIR, "route1_equal_mean_strategy_comparison_wide.csv"),
  row.names = FALSE
)

make_pdf_plot <- function(design_name, title = NULL) {
  one_pdf <- subset(pdf_data, design == design_name)
  if (is.null(title)) {
    design_info <- subset(sim, design == design_name)[1, ]
    title <- sprintf(
      "%s; SD = %s",
      as.character(design_name),
      design_info$sd_per_group
    )
  }
  ggplot2$ggplot(
    one_pdf,
    ggplot2$aes(x = x, y = density, colour = group)
  ) +
    ggplot2$geom_line(linewidth = 0.55, alpha = 0.9, na.rm = TRUE) +
    ggplot2$coord_cartesian(xlim = c(-2.5, 5), ylim = c(0, 2.3)) +
    ggplot2$facet_grid(
      stats::as.formula(". ~ skew_label"),
      labeller = ggplot2$labeller(skew_label = base_distribution_labels)
    ) +
    ggplot2$scale_colour_brewer(palette = "Dark2", name = "group") +
    ggplot2$labs(
      title = title,
      subtitle = "Ground-truth population PDFs after group-specific SD scaling",
      x = NULL,
      y = "density"
    ) +
    ggplot2$theme_minimal(base_size = 8.5) +
    ggplot2$theme(
      legend.position = "right",
      panel.grid.minor = ggplot2$element_blank(),
      panel.border = ggplot2$element_rect(colour = "black", fill = NA,
                                          linewidth = 0.2),
      axis.text.x = ggplot2$element_blank(),
      strip.text = ggplot2$element_text(size = 7.5),
      plot.title = ggplot2$element_text(face = "bold")
    )
}

make_gate_plot <- function(design_name, strategies = strategy_levels,
                           show_best = FALSE,
                           subtitle = "Rejection rates for fixed alternatives and the selected Route 1 test") {
  one_gate <- subset(gate_long, design == design_name & strategy %in% strategies)
  one_best <- subset(best_rows, design == design_name)
  one_best <- subset(one_best, strategy %in% strategies)
  axis_label_column <- if (length(strategies) == 1) {
    "mean_n_per_group"
  } else {
    "row_axis_label"
  }
  axis_labels <- unique(one_gate[c("row_label", axis_label_column)])
  axis_labels <- stats::setNames(axis_labels[[axis_label_column]],
                                 axis_labels$row_label)
  n_strategies <- length(strategies)
  n_blocks <- length(unique(one_gate$mean_n_per_group))
  separator_y <- seq(n_strategies + 0.5,
                     n_strategies * (n_blocks - 1) + 0.5,
                     by = n_strategies)
  p <- ggplot2$ggplot(
    one_gate,
    ggplot2$aes(x = skew_base_label, y = row_label, fill = rejection)
  ) +
    ggplot2$geom_tile(colour = "white", linewidth = 0.7)
  if (show_best) {
    p <- p + ggplot2$geom_tile(
      data = one_best,
      ggplot2$aes(x = skew_base_label, y = row_label),
      fill = NA,
      colour = "#008000",
      linewidth = 1.0,
      show.legend = FALSE
    )
  }
  p +
    ggplot2$geom_hline(
      yintercept = separator_y,
      colour = "grey35",
      linewidth = 0.25
    ) +
    ggplot2$geom_text(ggplot2$aes(label = cell_label), size = 1.55,
                      lineheight = 0.82) +
    ggplot2$scale_y_discrete(labels = axis_labels) +
    ggplot2$scale_fill_gradient2(
      low = "#2c7bb6",
      mid = "#ffffbf",
      high = "#d7191c",
      midpoint = ALPHA,
      limits = c(0, 1),
      labels = scales$percent,
      name = "Rejection\nrate"
    ) +
    ggplot2$labs(
      subtitle = subtitle,
      x = NULL,
      y = "mean n | strategy"
    ) +
    ggplot2$theme_minimal(base_size = 9) +
    ggplot2$theme(
      axis.text.x = ggplot2$element_blank(),
      axis.ticks.x = ggplot2$element_blank(),
      panel.grid = ggplot2$element_blank(),
      legend.position = "right"
    )
}

make_combined_plot <- function(strategies = strategy_levels, show_best = FALSE,
                               title, caption_extra = character(),
                               heatmap_subtitle = "Rejection rates for fixed alternatives and the selected Route 1 test",
                               route_caption = "In the Levene and SW gate rows, F/W and F/W/K are the selected-route probabilities.") {
  gate_height <- if (identical(strategies, "SW gate")) 0.55 else 1.45
  plot_list <- list()
  height_list <- numeric()
  idx <- 1
  for (block in pdf_blocks) {
    plot_list[[idx]] <- make_pdf_plot(block$pdf_design, title = block$title)
    height_list[idx] <- 0.8
    idx <- idx + 1
    for (design_name in block$heatmap_designs) {
      plot_list[[idx]] <- make_gate_plot(
        design_name,
        strategies = strategies,
        show_best = show_best,
        subtitle = paste(design_name, heatmap_subtitle, sep = ": ")
      )
      height_list[idx] <- gate_height
      idx <- idx + 1
    }
  }
  patchwork$wrap_plots(
    plot_list,
    ncol = 1,
    heights = height_list
  ) +
    patchwork$plot_layout(guides = "collect") +
    patchwork$plot_annotation(
      title = title,
      caption = paste(
        "Samples are drawn from distributions with equal population means.",
        "The heat-map y-axis gives mean n per group.",
        "Balanced rows use n = m,m,m,m; unbalanced rows use n = round(m * 0.5,0.8,1.2,1.5), where m is mean n per group.",
        "Column labels describe the base generator before group-specific SD scaling.",
        "Group order in PDFs is A-D.",
        "Equal SD: A-D all use SD = 1.",
        "Unequal SD: A-D use SD = 1.0,1.3,1.7,2.2.",
        "For larger n with larger SD, A-D use n = 0.5m,0.8m,1.2m,1.5m.",
        "For larger n with smaller SD, the same n values are paired with SD = 2.2,1.7,1.3,1.0.",
        "PDFs are shown once per SD set; the reversed-SD row uses the same density set in opposite group order.",
        caption_extra,
        route_caption,
        "Dashed PDF line marks the common population mean.",
        "Within unequal-SD rows, group PDFs differ because each base distribution is rescaled by the displayed SD vector."
      )
    )
}

plots_to_save <- list(
  list(
    filename = "route1_equal_mean_pdf_over_heatmap.png",
    plot = make_combined_plot(
      strategies = strategy_levels,
      show_best = TRUE,
      title = "Route 1 simulation: population PDFs shared by matching SD patterns",
      caption_extra = c(
        "Heat-map rows compare Fisher, Welch, Levene-gated Fisher/Welch, SW gate, and Kruskal-Wallis.",
        "Green frames mark the rejection rate closest to alpha = 5% within each distribution and mean-n combination."
      )
    ),
    width = 15,
    height = 30
  ),
  list(
    filename = "route1_equal_mean_pdf_over_heatmap_no_frame.png",
    plot = make_combined_plot(
      strategies = strategy_levels,
      show_best = FALSE,
      title = "Route 1 simulation: rejection rates without closest-to-alpha frames",
      caption_extra = "Heat-map rows compare Fisher, Welch, Levene-gated Fisher/Welch, SW gate, and Kruskal-Wallis."
    ),
    width = 15,
    height = 30
  ),
  list(
    filename = "route1_equal_mean_pdf_over_sw_gate_heatmap.png",
    plot = make_combined_plot(
      strategies = "SW gate",
      show_best = FALSE,
      title = "Route 1 simulation: shared population PDFs over SW-gate rejection rates",
      caption_extra = "The SW gate row shows the final-test rejection rate and F/W/K route probabilities.",
      heatmap_subtitle = "SW-gate final-test rejection rate and selected-route probabilities",
      route_caption = "In the SW gate row, F/W/K are the selected-route probabilities."
    ),
    width = 15,
    height = 18
  )
)

for (spec in plots_to_save) {
  ggplot2$ggsave(
    file.path(OUTDIR, spec$filename),
    spec$plot,
    width = spec$width,
    height = spec$height,
    dpi = 180
  )
  message("Wrote: ", file.path(OUTDIR, spec$filename))
}
