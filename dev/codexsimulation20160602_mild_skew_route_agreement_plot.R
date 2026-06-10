## ---------------------------------------------------------------------------
## Plot-only script for codexsimulation20160602_mild_skew_route_agreement.R
##
## Reads saved simulation results and regenerates the figures without rerunning
## the Monte Carlo simulation.
## ---------------------------------------------------------------------------

OUTDIR <- file.path("dev", "codexsimulation20160602_mild_skew_route_agreement_outputs")
RESULTS <- file.path(OUTDIR, "mild_skew_route_agreement_simulation_results.rds")

if (!file.exists(RESULTS)) {
  stop("Missing saved simulation results: ", RESULTS)
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required.")
}

sim <- readRDS(RESULTS)
route_table <- sim$route_table
agreement_table <- sim$agreement_table
ns <- sim$ns

ggplot2 <- asNamespace("ggplot2")

route_table$skew_label <- sprintf("skew = %.1f", route_table$skew)
route_table$n_label <- factor(route_table$n_per_group,
                              levels = rev(ns),
                              labels = paste0("n = ", rev(ns)))
route_table$percent_label <- sprintf("%.0f%%", route_table$route_rank_percent)

p_route_heatmap <- ggplot2$ggplot(
  route_table,
  ggplot2$aes(x = skew_label, y = n_label, fill = route_rank_percent)
) +
  ggplot2$geom_tile(colour = "white", linewidth = 0.8) +
  ggplot2$geom_text(ggplot2$aes(label = percent_label), size = 3.6) +
  ggplot2$facet_wrap(stats::as.formula("~ groups"),
                     labeller = ggplot2$label_both) +
  ggplot2$scale_fill_gradient(low = "#f7fbff", high = "#08519c",
                              limits = c(0, 100),
                              name = "% routed\nto ranks") +
  ggplot2$labs(
    title = "Figure X. Rank routing under identical mild-skew Gamma groups",
    subtitle = "Cells show % routed to ranks. DGP: identical Gamma groups, no shift; skew/kurtosis vary together.",
    x = "target Gamma skewness",
    y = "sample size per group",
    caption = paste(
      "Legend: panels = number of groups; rows = n per group; columns = target Gamma skewness.\n",
      "Cell value/colour = percentage routed to the rank test (Wilcoxon or Kruskal-Wallis)."
    )
  ) +
  ggplot2$theme_minimal(base_size = 11) +
  ggplot2$theme(
    panel.grid = ggplot2$element_blank(),
    plot.caption = ggplot2$element_text(hjust = 0, size = 9),
    axis.text.x = ggplot2$element_text(angle = 30, hjust = 1)
  )

agreement_table$skew_label <- sprintf("skew = %.1f", agreement_table$skew)
agreement_table$n_label <- factor(agreement_table$n_per_group,
                                  levels = rev(ns),
                                  labels = paste0("n = ", rev(ns)))
agreement_table$percent_label <- sprintf("%.0f%%",
                                         agreement_table$agreement_percent)

p_agree_heatmap <- ggplot2$ggplot(
  agreement_table,
  ggplot2$aes(x = skew_label, y = n_label, fill = agreement_percent)
) +
  ggplot2$geom_tile(colour = "white", linewidth = 0.8) +
  ggplot2$geom_text(ggplot2$aes(label = percent_label), size = 3.2) +
  ggplot2$facet_grid(groups ~ shift_sd, labeller = ggplot2$label_both) +
  ggplot2$scale_fill_gradient(low = "#fee5d9", high = "#238b45",
                              limits = c(80, 100),
                              name = "% same\ndecision") +
  ggplot2$labs(
    title = "Figure Y. Agreement of mean and rank decisions under mild skew",
    subtitle = paste("Cells show % same significant/not-significant decision.",
                     "Same Gamma shape in all groups; last group shifted by 0, 0.3, or 0.5 SD."),
    x = "target Gamma skewness",
    y = "sample size per group",
    caption = paste(
      "Legend: rows = n per group; columns = target Gamma skewness; top facets = shift of last group in SD; side facets = number of groups.\n",
      "Cell value/colour = agreement between the mean test (Student/Fisher) and the rank test (Wilcoxon/Kruskal-Wallis)."
    )
  ) +
  ggplot2$theme_minimal(base_size = 10) +
  ggplot2$theme(
    panel.grid = ggplot2$element_blank(),
    plot.caption = ggplot2$element_text(hjust = 0, size = 8.5),
    axis.text.x = ggplot2$element_text(angle = 30, hjust = 1)
  )

ggplot2$ggsave(file.path(OUTDIR, "figure_x_route_to_rank_mild_skew_heatmap.png"),
               p_route_heatmap, width = 10, height = 6.4, dpi = 180)
ggplot2$ggsave(file.path(OUTDIR, "figure_y_mean_rank_agreement_mild_skew_heatmap.png"),
               p_agree_heatmap, width = 11.5, height = 7.2, dpi = 180)

cat("Regenerated figures from saved simulation results in:", OUTDIR, "\n")
