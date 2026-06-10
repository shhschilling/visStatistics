## ---------------------------------------------------------------------------
## Mild-skew stress test for Route 1.
##
## Figure X:
##   Percentage routed to the rank branch by Shapiro-Wilk on internally
##   studentised residuals, under identical standardised Gamma groups.
##
## Figure Y:
##   Agreement between the mean-based and rank-based tests in the same
##   mild-skew Gamma settings.
##
## Data-generating distribution:
##   Gamma(shape = (2 / skew)^2, scale = 1), standardised to mean 0 and SD 1.
##   Target skewness: 0.1, 0.2, 0.3, 0.4, 0.5.
##   This is a one-parameter Gamma path: skewness and kurtosis vary together.
## ---------------------------------------------------------------------------

set.seed(20260602)

args <- commandArgs(trailingOnly = TRUE)
NREP <- if (length(args) >= 1) as.integer(args[1]) else 2000
ALPHA <- 0.05
OUTDIR <- file.path("dev", "codexsimulation20160602_mild_skew_route_agreement_outputs")
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required.")
}

skews <- c(0.1, 0.2, 0.3, 0.4, 0.5)
ns <- c(50, 100, 200)
groups <- c(2, 4)
shifts <- c(0, 0.3, 0.5)

shape_from_skew <- function(skew) (2 / skew)^2

std_gamma <- function(m, skew) {
  shape <- shape_from_skew(skew)
  (stats::rgamma(m, shape = shape, scale = 1) - shape) / sqrt(shape)
}

route_rank_once <- function(y, g, alpha = ALPHA) {
  stats::shapiro.test(stats::rstandard(stats::lm(y ~ factor(g))))$p.value < alpha
}

test_agreement_once <- function(y, g, alpha = ALPHA) {
  g <- factor(g)
  if (nlevels(g) == 2) {
    lev <- levels(g)
    a <- y[g == lev[1]]
    b <- y[g == lev[2]]
    p_mean <- stats::t.test(a, b, var.equal = TRUE)$p.value
    p_rank <- suppressWarnings(stats::wilcox.test(a, b)$p.value)
  } else {
    p_mean <- stats::anova(stats::lm(y ~ g))[["Pr(>F)"]][1]
    p_rank <- stats::kruskal.test(y, g)$p.value
  }
  c(mean_reject = p_mean < alpha, rank_reject = p_rank < alpha)
}

route_cell <- function(skew, n, k, nrep = NREP) {
  g <- factor(rep(seq_len(k), each = n))
  route <- replicate(nrep, {
    y <- std_gamma(n * k, skew)
    route_rank_once(y, g)
  })
  p <- mean(route)
  se <- sqrt(p * (1 - p) / nrep)
  data.frame(
    groups = k,
    n_per_group = n,
    skew = skew,
    route_rank_percent = 100 * p,
    mc_se_percent = 100 * se
  )
}

agreement_cell <- function(skew, n, k, shift, nrep = NREP) {
  g <- factor(rep(seq_len(k), each = n))
  last <- levels(g)[k]
  out <- replicate(nrep, {
    y <- std_gamma(n * k, skew)
    y[g == last] <- y[g == last] + shift
    test_agreement_once(y, g)
  })
  mean_reject <- out[1, ]
  rank_reject <- out[2, ]
  data.frame(
    groups = k,
    n_per_group = n,
    skew = skew,
    shift_sd = shift,
    mean_reject_percent = 100 * mean(mean_reject),
    rank_reject_percent = 100 * mean(rank_reject),
    agreement_percent = 100 * mean(mean_reject == rank_reject),
    both_significant_percent = 100 * mean(mean_reject & rank_reject),
    both_not_significant_percent = 100 * mean(!mean_reject & !rank_reject),
    mean_only_percent = 100 * mean(mean_reject & !rank_reject),
    rank_only_percent = 100 * mean(!mean_reject & rank_reject)
  )
}

cat("Running mild-skew route simulation with NREP =", NREP, "\n")
route_rows <- list()
i <- 1
for (k in groups) {
  for (n in ns) {
    for (skew in skews) {
      route_rows[[i]] <- route_cell(skew, n, k)
      cat(sprintf("  route: groups=%d n=%d skew=%.1f\n", k, n, skew))
      i <- i + 1
    }
  }
}
route_table <- do.call(rbind, route_rows)

cat("Running mild-skew agreement simulation with NREP =", NREP, "\n")
agreement_rows <- list()
i <- 1
for (k in groups) {
  for (n in ns) {
    for (shift in shifts) {
      for (skew in skews) {
        agreement_rows[[i]] <- agreement_cell(skew, n, k, shift)
        cat(sprintf("  agreement: groups=%d n=%d shift=%.1f skew=%.1f\n",
                    k, n, shift, skew))
        i <- i + 1
      }
    }
  }
}
agreement_table <- do.call(rbind, agreement_rows)

write.csv(route_table, file.path(OUTDIR, "figure_x_route_to_rank_mild_skew.csv"),
          row.names = FALSE)
write.csv(agreement_table, file.path(OUTDIR, "figure_y_mean_rank_agreement_mild_skew.csv"),
          row.names = FALSE)
saveRDS(
  list(
    route_table = route_table,
    agreement_table = agreement_table,
    nrep = NREP,
    alpha = ALPHA,
    skews = skews,
    ns = ns,
    groups = groups,
    shifts = shifts,
    generator = "standardised Gamma(shape = (2 / skew)^2, scale = 1)"
  ),
  file.path(OUTDIR, "mild_skew_route_agreement_simulation_results.rds")
)

readme <- c(
  "Mild-skew Route 1 stress test",
  paste("NREP per cell:", NREP),
  paste("alpha:", ALPHA),
  "",
  "Data-generating distribution:",
  "  Identical standardised Gamma groups unless shift_sd > 0.",
  "  Gamma shape = (2 / skew)^2, scale = 1; then standardised to mean 0 and SD 1.",
  "  Target skewness values: 0.1, 0.2, 0.3, 0.4, 0.5.",
  "  Caveat: along this Gamma path, skewness and kurtosis vary together.",
  "",
  "Figure X:",
  "  figure_x_route_to_rank_mild_skew.png",
  "  Percentage of simulations routed to Wilcoxon/Kruskal-Wallis by Shapiro-Wilk",
  "  on internally studentised residuals.",
  "  DGP: all groups are identical; no group shift; equal means and equal ordering.",
  "",
  "Figure Y:",
  "  figure_y_mean_rank_agreement_mild_skew.png",
  "  Percentage of simulations where the mean-based test and rank-based test",
  "  give the same significant/not-significant decision.",
  "  DGP: same standardised Gamma shape in all groups; last group shifted by",
  "  0, 0.3, or 0.5 SD.",
  "  Mean tests: Student t-test for 2 groups; Fisher ANOVA for 4 groups.",
  "  Rank tests: Wilcoxon for 2 groups; Kruskal-Wallis for 4 groups."
)
writeLines(readme, file.path(OUTDIR, "README.txt"))

ggplot2 <- asNamespace("ggplot2")

p_route <- ggplot2$ggplot(
  route_table,
  ggplot2$aes(x = skew, y = route_rank_percent,
              colour = factor(n_per_group), group = factor(n_per_group))
) +
  ggplot2$geom_line(linewidth = 0.8) +
  ggplot2$geom_point(size = 2) +
  ggplot2$facet_wrap(stats::as.formula("~ groups"),
                     labeller = ggplot2$label_both) +
  ggplot2$scale_x_continuous(breaks = skews) +
  ggplot2$scale_y_continuous(limits = c(0, 100)) +
  ggplot2$labs(
    title = "Figure X. Percentage routed to ranks under identical mild-skew Gamma groups",
    subtitle = "No group shift: equal means and equal ordering are true; routing uses Shapiro-Wilk on studentised residuals",
    x = "target population skewness of standardised Gamma distribution",
    y = "percentage routed to Wilcoxon/Kruskal-Wallis",
    colour = "n per group"
  ) +
  ggplot2$theme_minimal(base_size = 10)

p_agree <- ggplot2$ggplot(
  agreement_table,
  ggplot2$aes(x = skew, y = agreement_percent,
              colour = factor(n_per_group), group = factor(n_per_group))
) +
  ggplot2$geom_hline(yintercept = 90, linetype = 2, colour = "grey45") +
  ggplot2$geom_line(linewidth = 0.8) +
  ggplot2$geom_point(size = 2) +
  ggplot2$facet_grid(groups ~ shift_sd, labeller = ggplot2$label_both) +
  ggplot2$scale_x_continuous(breaks = skews) +
  ggplot2$scale_y_continuous(limits = c(80, 100)) +
  ggplot2$labs(
    title = "Figure Y. Agreement of mean-based and rank-based decisions under mild skew",
    subtitle = "Same standardised Gamma shape in all groups; last group shifted by 0, 0.3, or 0.5 SD",
    x = "target population skewness of standardised Gamma distribution",
    y = "percentage with same significant/not-significant decision",
    colour = "n per group"
  ) +
  ggplot2$theme_minimal(base_size = 10)

ggplot2$ggsave(file.path(OUTDIR, "figure_x_route_to_rank_mild_skew.png"),
               p_route, width = 9, height = 5.5, dpi = 180)
ggplot2$ggsave(file.path(OUTDIR, "figure_y_mean_rank_agreement_mild_skew.png"),
               p_agree, width = 10, height = 6, dpi = 180)

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
    plot.caption = ggplot2$element_text(hjust = 0, size = 9)
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
    plot.caption = ggplot2$element_text(hjust = 0, size = 8.5)
  )

ggplot2$ggsave(file.path(OUTDIR, "figure_x_route_to_rank_mild_skew_heatmap.png"),
               p_route_heatmap, width = 10, height = 6.2, dpi = 180)
ggplot2$ggsave(file.path(OUTDIR, "figure_y_mean_rank_agreement_mild_skew_heatmap.png"),
               p_agree_heatmap, width = 11.5, height = 7, dpi = 180)

cat("\nWrote outputs to:", OUTDIR, "\n")
cat("Done.\n")
