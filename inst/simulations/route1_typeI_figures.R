## ---------------------------------------------------------------------------
## Route 1 figure variants:
##   1. Equal SD: identical distributions and identical means.
##   2. Unequal SD: identical means, unequal distributions.
##
## Uses saved Monte Carlo output only. Does not rerun the simulation.
## ---------------------------------------------------------------------------

OUTDIR <- "."
FIGDIR <- "."
dir.create(FIGDIR, showWarnings = FALSE, recursive = TRUE)

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
if (!requireNamespace("ggtext", quietly = TRUE)) {
  stop("Package 'ggtext' is required.")
}

sim <- readRDS(file.path(OUTDIR, "route1_equal_mean_blanca_zimmerman.rds"))

ALPHA <- 0.05
ggplot2 <- asNamespace("ggplot2")
patchwork <- asNamespace("patchwork")
scales <- asNamespace("scales")
source("fleishman_route1_residual_helpers.R")
source("fleishman_figure_typography.R")
group_cols <- fleishman_group_cols
names(group_cols) <- LETTERS[seq_along(group_cols)]

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
distribution_label <- function(skew_label) {
  one <- sim[sim$skew_label == skew_label, ][1, ]
  if (one$panel == 1) {
    return("N(0, 1)\nskew = 0; excess kurtosis = 0")
  }
  sprintf(
    "F.P.\nskew = %s; excess kurtosis = %s",
    format(one$skew, trim = TRUE, scientific = FALSE),
    format(one$excess_kurtosis, trim = TRUE, scientific = FALSE)
  )
}
base_distribution_labels <- setNames(
  lapply(skew_levels, distribution_label),
  skew_levels
)
numbered_distribution_labels <- setNames(
  paste0(seq_along(skew_levels), ") ", unlist(base_distribution_labels)),
  skew_levels
)
## Column numbers 1) ... 5) repeated above every heatmap block, so that the
## columns of panels B, C and D can be referenced as in panel A.
column_number_labels <- stats::setNames(
  paste0(seq_along(numbered_distribution_labels), ")"),
  numbered_distribution_labels
)

distribution_plot_title <- function(skew_label, number) {
  one <- sim[sim$skew_label == skew_label, ][1, ]
  if (one$panel == 1) {
    return(sprintf(
      "%d) N(0, 1)\n\n\nskew = 0\nexcess kurtosis = 0",
      number
    ))
  }
  case <- fleishman_cases[fleishman_cases$panel == one$panel, , drop = FALSE]
  sprintf(
    paste(
      "%d) Fleishman polynomial",
      "a = %.3f, b = %.3f",
      "c = -a, d = %.3f",
      "skew = %s",
      "excess kurtosis = %s",
      sep = "\n"
    ),
    number, case$a, case$b, case$d,
    format(case$skew, trim = TRUE, scientific = FALSE),
    format(case$excess_kurtosis, trim = TRUE, scientific = FALSE)
  )
}

density_y_limit <- function(skew_label) {
  one <- sim[sim$skew_label == skew_label, ][1, ]
  if (one$panel == 5) 0.75 else 0.70
}

make_density_curve <- function(panel, sd, xlim) {
  x <- seq(xlim[1], xlim[2], length.out = 700)
  density <- fleishman_scaled_density(x, panel, sd = sd, shift = 0)
  density[!is.finite(density)] <- NA_real_
  data.frame(x = x, density = density, piece = "density")
}

one_per_distribution <- sim[!duplicated(sim[c("design", "skew_label")]), ]
pdf_rows <- list()
idx <- 1
x_grid <- seq(-4, 4, length.out = 700)

for (i in seq_len(nrow(one_per_distribution))) {
  one <- one_per_distribution[i, ]
    sd_vec <- as.numeric(strsplit(one$sd_per_group, ", ")[[1]])
    for (j in seq_along(sd_vec)) {
      curve <- make_density_curve(
        one$panel,
        sd_vec[j],
        range(x_grid)
      )
      pdf_row <- data.frame(
        design = one$design,
        skew_label = one$skew_label,
        panel = one$panel,
        group = names(group_cols)[j],
        sd = sd_vec[j],
        x = curve$x,
        density = curve$density,
        piece = curve$piece,
        row.names = NULL
      )
    pdf_rows[[idx]] <- pdf_row
    idx <- idx + 1
  }
}

pdf_data <- do.call(rbind, pdf_rows)
pdf_data$design <- factor(pdf_data$design, levels = design_levels)
pdf_data$skew_label <- factor(pdf_data$skew_label, levels = skew_levels)

sim$design <- factor(sim$design, levels = design_levels)
sim$skew_base_label <- factor(
  numbered_distribution_labels[as.character(sim$skew_label)],
  levels = numbered_distribution_labels
)

make_pdf_plot <- function(design_name, title, show_group_legend = TRUE,
                          black_curves = FALSE, show_sd_mapping = FALSE) {
  one_pdf <- subset(pdf_data, design == design_name)
  panels <- vector("list", length(skew_levels))
  for (i in seq_along(skew_levels)) {
    skew_level <- skew_levels[i]
    panel_data <- subset(one_pdf, skew_label == skew_level)
    ref_case <- fleishman_cases[
      fleishman_cases$panel == unique(panel_data$panel),
      ,
      drop = FALSE
    ]
    mean_line <- data.frame(
      xintercept = 0,
      reference = factor("mean", levels = c("mean", "median"))
    )
    median_lines <- unique(panel_data[c("group", "sd")])
    median_lines$xintercept <- ref_case$a * median_lines$sd
    median_lines$reference <- factor("median", levels = c("mean", "median"))
    if (length(unique(round(median_lines$xintercept, 12))) == 1) {
      median_lines <- median_lines[1, ]
    }
    y_limit <- density_y_limit(skew_level)
    line_layer <- if (black_curves) {
      ggplot2$geom_line(colour = "black", linewidth = 0.55, alpha = 0.9,
                         na.rm = TRUE)
    } else {
      ggplot2$geom_line(linewidth = 0.55, alpha = 0.9, na.rm = TRUE)
    }
    colour_scale <- if (black_curves) {
      NULL
    } else {
      ggplot2$scale_colour_manual(
        values = group_cols,
        name = "group",
        labels = if (show_sd_mapping) {
          c("Group A, SD=1", "Group B, SD=1.3",
            "Group C, SD=1.7", "Group D, SD=2.2")
        } else {
          ggplot2$waiver()
        }
      )
    }
    median_layer <- if (black_curves) {
      ggplot2$geom_vline(
        data = median_lines,
        ggplot2$aes(xintercept = xintercept, linetype = reference),
        colour = "grey25",
        linewidth = 0.45,
        inherit.aes = FALSE
      )
    } else {
      ggplot2$geom_vline(
        data = median_lines,
        ggplot2$aes(xintercept = xintercept, colour = group,
                    linetype = reference),
        linewidth = 0.45,
        inherit.aes = FALSE,
        show.legend = c(colour = FALSE, linetype = TRUE)
      )
    }
    panels[[i]] <- ggplot2$ggplot(
      panel_data,
      ggplot2$aes(x = x, y = density, colour = group,
                  group = interaction(group, piece))
    ) +
      ggplot2$geom_vline(
        data = mean_line,
        ggplot2$aes(xintercept = xintercept, linetype = reference),
        colour = "grey25",
        linewidth = 0.45,
        inherit.aes = FALSE
      ) +
      median_layer +
      line_layer +
      ggplot2$coord_cartesian(xlim = c(-4, 4), ylim = c(0, y_limit)) +
      colour_scale +
      ggplot2$scale_linetype_manual(
        values = c(mean = "dashed", median = "dotted"),
        name = "reference"
      ) +
      ggplot2$guides(
        colour = ggplot2$guide_legend(order = 1),
        linetype = ggplot2$guide_legend(
          order = 2,
          override.aes = list(colour = "grey25")
        )
      ) +
      ggplot2$labs(
        title = distribution_plot_title(as.character(skew_level), i),
        x = "Response values",
        y = if (i == 1) "Density" else NULL
      ) +
      ggplot2$theme_minimal(
        base_size = FLEISHMAN_TEXT$section_title,
        base_family = FLEISHMAN_FONT_FAMILY
      ) +
      ggplot2$theme(
        legend.position = if (i == length(skew_levels)) {
          "right"
        } else {
          "none"
        },
        legend.box.margin = ggplot2$margin(0, 0, 0, 4),
        legend.key.height = grid::unit(1.6, "lines"),
        legend.title = ggplot2$element_text(size = FLEISHMAN_TEXT$legend),
        legend.text = ggplot2$element_text(size = FLEISHMAN_TEXT$legend),
        axis.title.x = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_title),
        axis.title.y = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_title),
        panel.grid.minor = ggplot2$element_blank(),
        panel.border = ggplot2$element_rect(colour = "black", fill = NA,
                                            linewidth = 0.2),
        plot.title = ggplot2$element_text(
          size = FLEISHMAN_TEXT$panel_title,
          hjust = 0.5,
          face = "plain",
          lineheight = FLEISHMAN_LINEHEIGHT$panel_title
        ),
        plot.margin = ggplot2$margin(22, 4, 4, 4)
      )
  }
  patchwork$wrap_plots(panels, nrow = 1) +
    patchwork$plot_annotation(
      title = title,
      subtitle = NULL,
      theme = ggplot2$theme(
        plot.title = ggplot2$element_text(
          face = "plain",
          size = FLEISHMAN_TEXT$main_title
        ),
        plot.subtitle = ggplot2$element_text(
          size = FLEISHMAN_TEXT$section_title
        )
      )
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
      fontface = "plain",
      size = FLEISHMAN_GEOM_TEXT$block_title,
      lineheight = FLEISHMAN_LINEHEIGHT$block_title
    ) +
    ggplot2$theme_void() +
    ggplot2$coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off")
}

panel_header <- function(letter, description) {
  ggplot2$ggplot() +
    ggplot2$labs(
      title = fleishman_panel_title(letter, description)
    ) +
    ggplot2$theme_void() +
    ggplot2$theme(
      plot.title = ggtext::element_markdown(
        hjust = 0,
        size = FLEISHMAN_TEXT$panel_letter,
        family = FLEISHMAN_FONT_FAMILY,
        margin = ggplot2$margin(b = 0)
      ),
      plot.title.position = "plot",
      plot.margin = ggplot2$margin(0, 0, 0, 0)
    )
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
  if (limit <= 0.075) {
    anchors <- c(0.025, 0.05, 0.075)
    anchor_cols <- c("#67a9cf", "#1a9850", "#66bd63")
  } else {
    anchors <- c(0, 0.025, 0.05, 0.075, 0.10, 0.20, 0.50, 1.00)
    anchor_cols <- c(
      "#084594", "#67a9cf", "#1a9850", "#66bd63",
      "#fee08b", "#fdae61", "#d73027", "#9e1b1b"
    )
  }
  keep <- anchors <= limit
  ggplot2$scale_fill_gradientn(
    colours = anchor_cols[keep],
    values = scales$rescale(anchors[keep], from = range(anchors[keep])),
    limits = range(anchors[keep]),
    oob = scales$squish,
    labels = scales$percent,
    na.value = "white",
    name = "rejection\nrate"
  )
}

make_rejection_plot <- function(design_name, strategies, subtitle,
                                show_legend = FALSE, fill_limit = 1) {
  one <- subset(
    make_rejection_rows(sim, strategies),
    design == design_name
  )
  one$strategy <- as.character(one$strategy)
  mean_levels <- sort(unique(one$mean_n_per_group), decreasing = FALSE)
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
  header$rejection_label <- "rej."
  header$gate_bar_label <- "|"
  header$split_f_label <- "F"
  header$split_w_label <- "W"
  header$split_kw_label <- "KW"
  for (nm in setdiff(names(one), names(header))) {
    header[[nm]] <- NA
  }
  one <- rbind(one, header[names(one)])
  strategy_levels <- c(rev(strategies), "header")
  strategy_y <- stats::setNames(seq_along(strategy_levels), strategy_levels)
  strategy_y["header"] <- length(strategies) + 1.25
  one$plot_strategy <- factor(one$plot_strategy, levels = strategy_levels)
  one$plot_y <- unname(strategy_y[as.character(one$plot_strategy)])
  ggplot2$ggplot(
    one,
    ggplot2$aes(x = skew_base_label, y = plot_y, fill = rejection)
  ) +
    ggplot2$geom_tile(colour = NA) +
    ggplot2$geom_text(
      ggplot2$aes(label = rejection_label),
      position = ggplot2$position_nudge(x = -0.34),
      size = FLEISHMAN_GEOM_TEXT$heatmap_cell
    ) +
    ggplot2$geom_text(
      ggplot2$aes(label = gate_bar_label),
      position = ggplot2$position_nudge(x = -0.19),
      size = FLEISHMAN_GEOM_TEXT$heatmap_cell
    ) +
    ggplot2$geom_text(
      ggplot2$aes(label = split_f_label),
      position = ggplot2$position_nudge(x = -0.04),
      size = FLEISHMAN_GEOM_TEXT$heatmap_cell
    ) +
    ggplot2$geom_text(
      ggplot2$aes(label = split_w_label),
      position = ggplot2$position_nudge(x = 0.17),
      size = FLEISHMAN_GEOM_TEXT$heatmap_cell
    ) +
    ggplot2$geom_text(
      ggplot2$aes(label = split_kw_label),
      position = ggplot2$position_nudge(x = 0.37),
      size = FLEISHMAN_GEOM_TEXT$heatmap_cell
    ) +
    ggplot2$scale_y_continuous(
      breaks = unname(strategy_y),
      labels = c(rev(strategies), ""),
      expand = ggplot2$expansion(add = c(0.45, 0.20))
    ) +
    ggplot2$scale_x_discrete(
      position = "top",
      labels = column_number_labels
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
    ggplot2$theme_minimal(
      base_size = FLEISHMAN_TEXT$legend,
      base_family = FLEISHMAN_FONT_FAMILY
    ) +
    ggplot2$theme(
      axis.title.y = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_title),
      axis.text.x = ggplot2$element_text(
        size = FLEISHMAN_TEXT$strip,
        margin = ggplot2$margin(t = 0, b = 0)
      ),
      axis.text.y = ggplot2$element_text(size = FLEISHMAN_TEXT$heatmap_row),
      axis.ticks.x = ggplot2$element_blank(),
        panel.grid = ggplot2$element_blank(),
      panel.spacing.y = grid::unit(0.12, "lines"),
      strip.placement = "outside",
      strip.background.y = ggplot2$element_rect(
        colour = "grey35",
        fill = "grey95",
        linewidth = 0.25
      ),
      strip.text.y.left = ggplot2$element_text(
        angle = 0,
        face = "plain",
        size = FLEISHMAN_TEXT$strip
      ),
      legend.position = if (show_legend) "right" else "none"
    )
}

make_equal_plot <- function() {
  strategies <- c("F", "W", "L", "KW", "SW", "SW+L")
  plot_list <- list()
  height_list <- numeric()
  plot_list[[1]] <- panel_header(
    "A",
    "identical distributions; mean<sub>i</sub> = 0 and SD<sub>i</sub> = 1 (i = 1, ..., 4)"
  )
  height_list[1] <- 0.10
  plot_list[[2]] <- make_pdf_plot(
    "balanced n, equal SD",
    expression(paste("identical distributions; ", mean[i] == 0,
                     " and ", SD[i] == 1, " (i = 1, ..., 4)")),
    show_group_legend = FALSE,
    black_curves = TRUE
  )
  height_list[2] <- 1.15
  balanced_plot <- make_rejection_plot(
    "balanced n, equal SD",
    strategies,
    "identical distributions and identical means; empirical Type I error (%)",
    show_legend = TRUE,
    fill_limit = 0.075
  )
  unbalanced_plot <- make_rejection_plot(
    "unbalanced n, equal SD",
    strategies,
    "identical distributions and identical means; empirical Type I error (%)",
    show_legend = FALSE,
    fill_limit = 0.075
  )
  plot_list[[3]] <- panel_header(
    "B",
    "balanced; (n<sub>1</sub>, n<sub>2</sub>, n<sub>3</sub>, n<sub>4</sub>) = <b>n</b>; (SD<sub>1</sub>, SD<sub>2</sub>, SD<sub>3</sub>, SD<sub>4</sub>) = 1"
  )
  height_list[3] <- 0.03
  plot_list[[4]] <- balanced_plot
  height_list[4] <- 1.00
  plot_list[[5]] <- patchwork$plot_spacer()
  height_list[5] <- 0.06
  plot_list[[6]] <- panel_header(
    "C",
    "unbalanced; (n<sub>1</sub>, n<sub>2</sub>, n<sub>3</sub>, n<sub>4</sub>) = n&#772;(0.5, 0.8, 1.2, 1.5); (SD<sub>1</sub>, SD<sub>2</sub>, SD<sub>3</sub>, SD<sub>4</sub>) = 1"
  )
  height_list[6] <- 0.03
  plot_list[[7]] <- unbalanced_plot
  height_list[7] <- 1.00
  (
    patchwork$wrap_plots(plot_list, ncol = 1, heights = height_list) +
      patchwork$plot_layout(guides = "keep")
  ) &
    ggplot2$theme(
      plot.margin = ggplot2$margin(2, 14, 6, 14)
    )
}

make_unequal_design_plot <- function(design_name, panel_label, show_legend = TRUE) {
  strategies <- c("F", "W", "L", "SW", "SW+L")
  plot_list <- list()
  height_list <- numeric()
  plot_list[[1]] <- make_pdf_plot(
    design_name,
    design_formula_label(design_name, "identical means"),
    show_group_legend = TRUE,
    show_sd_mapping = TRUE
  )
  height_list[1] <- 1.15
  plot_list[[2]] <- block_title(
    panel_label
  )
  height_list[2] <- 0.12
  plot_list[[3]] <- make_rejection_plot(
    design_name,
    strategies,
    "identical means, unequal distributions; rejection rate (%)",
    show_legend = show_legend,
    fill_limit = 1
  )
  height_list[3] <- 0.90
  (
    patchwork$wrap_plots(plot_list, ncol = 1, heights = height_list) +
      patchwork$plot_layout(guides = "keep")
  ) &
    ggplot2$theme(
      plot.margin = ggplot2$margin(2, 14, 6, 14)
    )
}

make_unequal_plot <- function() {
  strategies <- c("F", "W", "L", "SW", "SW+L")
  plot_list <- list()
  height_list <- numeric()
  plot_list[[1]] <- panel_header(
    "A",
    "identical means; (SD<sub>1</sub>, SD<sub>2</sub>, SD<sub>3</sub>, SD<sub>4</sub>) = (1.0, 1.3, 1.7, 2.2)"
  )
  height_list[1] <- 0.10
  plot_list[[2]] <- make_pdf_plot(
    "balanced n, unequal SD",
    expression(paste("identical means; (", SD[1], ", ..., ", SD[4],
                     ") = ", "(1.0, 1.3, 1.7, 2.2)")),
    show_group_legend = TRUE,
    show_sd_mapping = TRUE
  )
  height_list[2] <- 1.15
  plot_list[[3]] <- panel_header(
    "B",
    "balanced; (n<sub>1</sub>, n<sub>2</sub>, n<sub>3</sub>, n<sub>4</sub>) = <b>n</b>; (SD<sub>1</sub>, SD<sub>2</sub>, SD<sub>3</sub>, SD<sub>4</sub>) = (1.0, 1.3, 1.7, 2.2)"
  )
  height_list[3] <- 0.03
  plot_list[[4]] <- make_rejection_plot(
    "balanced n, unequal SD",
    strategies,
    "identical means, unequal distributions; rejection rate (%)",
    show_legend = TRUE,
    fill_limit = 1
  )
  height_list[4] <- 0.90
  plot_list[[5]] <- panel_header(
    "C",
    "unbalanced; (n<sub>1</sub>, n<sub>2</sub>, n<sub>3</sub>, n<sub>4</sub>) = n&#772;(0.5, 0.8, 1.2, 1.5); (SD<sub>1</sub>, SD<sub>2</sub>, SD<sub>3</sub>, SD<sub>4</sub>) = (1.0, 1.3, 1.7, 2.2)"
  )
  height_list[5] <- 0.03
  plot_list[[6]] <- make_rejection_plot(
    "unbalanced n, larger n with larger SD",
    strategies,
    "identical means, unequal distributions; rejection rate (%)",
    show_legend = FALSE,
    fill_limit = 1
  )
  height_list[6] <- 0.90
  plot_list[[7]] <- panel_header(
    "D",
    "unbalanced; (n<sub>1</sub>, n<sub>2</sub>, n<sub>3</sub>, n<sub>4</sub>) = n&#772;(0.5, 0.8, 1.2, 1.5); (SD<sub>1</sub>, SD<sub>2</sub>, SD<sub>3</sub>, SD<sub>4</sub>) = (2.2, 1.7, 1.3, 1.0)"
  )
  height_list[7] <- 0.03
  plot_list[[8]] <- make_rejection_plot(
    "unbalanced n, larger n with smaller SD",
    strategies,
    "identical means, unequal distributions; rejection rate (%)",
    show_legend = FALSE,
    fill_limit = 1
  )
  height_list[8] <- 0.90
  (
    patchwork$wrap_plots(plot_list, ncol = 1, heights = height_list) +
      patchwork$plot_layout(guides = "keep")
  ) &
    ggplot2$theme(
      plot.margin = ggplot2$margin(2, 14, 6, 14)
    )
}

save_plot <- function(filename, plot, width, height, dpi = FLEISHMAN_DPI) {
  outfile <- file.path(OUTDIR, filename)
  figfile <- file.path(FIGDIR, filename)
  ggplot2$ggsave(outfile, plot, width = width, height = height, dpi = dpi)
  message("Wrote: ", outfile)
  if (normalizePath(OUTDIR) != normalizePath(FIGDIR)) {
    file.copy(outfile, figfile, overwrite = TRUE)
    message("Wrote: ", figfile)
  }
}

equal_outfile <- file.path(
  OUTDIR,
  "route1_identical_distributions_typeI_with_kw_fleishman_B50000.png"
)
ggplot2$ggsave(
  equal_outfile,
  make_equal_plot(),
  width = 20,
  height = 22,
  dpi = FLEISHMAN_DPI
)
message("Wrote: ", equal_outfile)

if (normalizePath(OUTDIR) != normalizePath(FIGDIR)) {
  file.copy(
    equal_outfile,
    file.path(FIGDIR, "route1_identical_distributions_typeI_with_kw_fleishman_B50000.png"),
    overwrite = TRUE
  )
}

save_plot(
  "route1_equal_means_unequal_distributions_fleishman_B50000.png",
  make_unequal_plot(),
  width = 20,
  height = 26
)
