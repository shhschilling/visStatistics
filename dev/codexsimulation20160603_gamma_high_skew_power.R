## ---------------------------------------------------------------------------
## Gamma power sweep for Route 1: mean vs rank vs Shapiro-gated route.
##
## Question tested:
##   power to detect a 0.5 SD shift in the last group.
##
## Distributions:
##   standardised Gamma distributions with target skewness 1, 2, 3, and 6.
##   Gamma shape = (2 / skew)^2; excess kurtosis = 1.5 * skew^2.
##
## Caveat:
##   Along this Gamma path, skewness and excess kurtosis vary together.
## ---------------------------------------------------------------------------

set.seed(20260603)

args <- commandArgs(trailingOnly = TRUE)
NREP <- if (length(args) >= 1) as.integer(args[1]) else 2000
ALPHA <- 0.05
SHIFT_SD <- 0.5
OUTDIR <- file.path("dev", "codexsimulation20160603_gamma_high_skew_power_outputs")
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

stopifnot(NREP > 0)
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required.")
}

levene_p <- function(y, g) {
  g <- factor(g)
  z <- abs(y - ave(y, g, FUN = median))
  stats::anova(stats::lm(z ~ g))[["Pr(>F)"]][1]
}

normality_p <- function(y, g) {
  model <- stats::lm(y ~ factor(g))
  raw <- stats::residuals(model)
  rs <- suppressWarnings(stats::rstandard(model))
  if (any(!is.finite(rs))) {
    rs <- raw / max(stats::sigma(model), 1e-8)
  }
  rs <- rs[is.finite(rs)]
  if (length(rs) < 3) return(NA_real_)
  stats::shapiro.test(rs)$p.value
}

route_once <- function(y, g, alpha = ALPHA) {
  g <- factor(g)
  k <- nlevels(g)
  p_norm <- normality_p(y, g)
  normality_met <- is.na(p_norm) || p_norm >= alpha
  p_lev <- levene_p(y, g)
  equal_var <- is.na(p_lev) || p_lev >= alpha

  if (k == 2) {
    lev <- levels(g)
    a <- y[g == lev[1]]
    b <- y[g == lev[2]]
    p_mean <- stats::t.test(a, b, var.equal = equal_var)$p.value
    p_rank <- suppressWarnings(stats::wilcox.test(a, b)$p.value)
  } else {
    p_fisher <- stats::anova(stats::lm(y ~ g))[["Pr(>F)"]][1]
    p_welch <- stats::oneway.test(y ~ g, var.equal = FALSE)$p.value
    p_mean <- if (equal_var) p_fisher else p_welch
    p_rank <- stats::kruskal.test(y, g)$p.value
  }

  p_gate <- if (normality_met) p_mean else p_rank

  c(mean_reject = p_mean < alpha,
    rank_reject = p_rank < alpha,
    gate_reject = p_gate < alpha,
    route_rank = !normality_met)
}

shape_from_skew <- function(skew) (2 / skew)^2

draw_gamma <- function(n, skew) {
  shape <- shape_from_skew(skew)
  (stats::rgamma(n, shape = shape, scale = 1) - shape) / sqrt(shape)
}

make_shift_data <- function(skew, n, k, shift = SHIFT_SD) {
  g <- factor(rep(seq_len(k), each = n))
  y <- unlist(lapply(seq_len(k), function(i) draw_gamma(n, skew)))
  y[g == levels(g)[k]] <- y[g == levels(g)[k]] + shift
  list(y = y, g = g)
}

summarise_binary <- function(values) {
  p <- mean(values)
  se <- sqrt(p * (1 - p) / length(values))
  c(rate = p, mc_se = se)
}

run_cell <- function(skew, n, k) {
  out <- replicate(NREP, {
    dat <- make_shift_data(skew, n, k)
    route_once(dat$y, dat$g)
  }, simplify = FALSE)

  mean_reject <- vapply(out, `[[`, logical(1), "mean_reject")
  rank_reject <- vapply(out, `[[`, logical(1), "rank_reject")
  gate_reject <- vapply(out, `[[`, logical(1), "gate_reject")
  route_rank <- vapply(out, `[[`, logical(1), "route_rank")

  c(mean = summarise_binary(mean_reject),
    rank = summarise_binary(rank_reject),
    gate = summarise_binary(gate_reject),
    route_rank = summarise_binary(route_rank))
}

skews <- c(1, 2, 3, 6)
ns <- c(10, 20, 50, 100, 200)
ks <- c(2, 4)

rows <- list()
idx <- 1
for (k in ks) {
  for (n in ns) {
    for (skew in skews) {
      res <- run_cell(skew, n, k)
      rows[[idx]] <- data.frame(
        scenario = "one group shifted by 0.5 SD",
        distribution = "standardised Gamma",
        skew = skew,
        gamma_shape = shape_from_skew(skew),
        excess_kurtosis = 1.5 * skew^2,
        n_per_group = n,
        groups = k,
        mean_power = res["mean.rate"],
        mean_mc_se = res["mean.mc_se"],
        rank_power = res["rank.rate"],
        rank_mc_se = res["rank.mc_se"],
        gate_power = res["gate.rate"],
        gate_mc_se = res["gate.mc_se"],
        route_rank_probability = res["route_rank.rate"],
        route_rank_mc_se = res["route_rank.mc_se"],
        row.names = NULL
      )
      cat(sprintf("done: k=%d n=%d skew=%s\n", k, n, skew))
      idx <- idx + 1
    }
  }
}

power <- do.call(rbind, rows)
power$skew_label <- sprintf(
  "Gamma skew = %.0f\nshape = %.2f\nexcess kurtosis = %.1f",
  power$skew,
  power$gamma_shape,
  power$excess_kurtosis
)
power$n_label <- paste0("n = ", power$n_per_group)
power$groups_label <- paste0(power$groups, " groups")

write.csv(power, file.path(OUTDIR, "gamma_high_skew_power.csv"),
          row.names = FALSE)
saveRDS(power, file.path(OUTDIR, "gamma_high_skew_power.rds"))

readme <- c(
  "Gamma high-skew power simulation",
  paste("NREP per cell:", NREP),
  paste("alpha:", ALPHA),
  paste("effect:", SHIFT_SD, "SD shift added to the last group"),
  "",
  "Power means: probability of p < alpha for detecting the 0.5 SD shift.",
  "Mean test: Student/Welch for 2 groups; Fisher/Welch ANOVA for 4 groups,",
  "  with Levene selecting equal-variance versus Welch.",
  "Rank test: Wilcoxon for 2 groups; Kruskal-Wallis for 4 groups.",
  "Gate: Shapiro-Wilk on standardised residuals; if rejected, use rank test;",
  "  otherwise use the mean test.",
  "",
  "Gamma path caveat: skewness and excess kurtosis vary together.",
  "",
  "Files:",
  "  gamma_high_skew_power.csv",
  "  gamma_high_skew_power.rds",
  "  gamma_high_skew_power.png",
  "  gamma_high_skew_route_probability.png",
  "  gamma_high_skew_gate_rank_overlay.png",
  "  gamma_high_skew_rank_power_gain.png"
)
writeLines(readme, file.path(OUTDIR, "README.txt"))

ggplot2 <- asNamespace("ggplot2")

to_long <- function(dat) {
  out <- rbind(
    transform(dat, strategy = "mean test", power = mean_power),
    transform(dat, strategy = "rank test", power = rank_power),
    transform(dat, strategy = "Shapiro-gated route", power = gate_power)
  )
  out$strategy <- factor(
    out$strategy,
    levels = c("mean test", "rank test", "Shapiro-gated route")
  )
  out
}

power_long <- to_long(power)

p_power <- ggplot2$ggplot(
  power_long,
  ggplot2$aes(x = n_label, y = power, colour = strategy, linetype = strategy,
              shape = strategy, group = strategy)
) +
  ggplot2$geom_line(linewidth = 0.75) +
  ggplot2$geom_point(size = 2.1) +
  ggplot2$facet_grid(groups_label ~ skew_label) +
  ggplot2$scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  ggplot2$scale_linetype_manual(
    values = c("mean test" = "solid",
               "rank test" = "dashed",
               "Shapiro-gated route" = "dotted")
  ) +
  ggplot2$scale_shape_manual(
    values = c("mean test" = 16,
               "rank test" = 17,
               "Shapiro-gated route" = 15)
  ) +
  ggplot2$labs(
    title = "Power to detect a 0.5 SD group shift under Gamma skew",
    subtitle = "Mean test vs rank test vs Route 1 Shapiro-gated route",
    x = "n per group",
    y = "power: percentage with p < 0.05",
    colour = "test strategy",
    linetype = "test strategy",
    shape = "test strategy",
    caption = paste(
      "Data: standardised Gamma groups; last group shifted by 0.5 SD.",
      "Gamma caveat: skewness and excess kurtosis vary together."
    )
  ) +
  ggplot2$theme_minimal(base_size = 11) +
  ggplot2$theme(
    legend.position = "right",
    plot.caption = ggplot2$element_text(hjust = 0, size = 9)
  )

p_route <- ggplot2$ggplot(
  power,
  ggplot2$aes(x = skew_label, y = n_label, fill = route_rank_probability)
) +
  ggplot2$geom_tile(colour = "white", linewidth = 0.7) +
  ggplot2$geom_text(
    ggplot2$aes(label = sprintf("%.0f%%", 100 * route_rank_probability)),
    size = 3.2
  ) +
  ggplot2$facet_wrap(stats::as.formula("~ groups_label"), nrow = 1) +
  ggplot2$scale_fill_gradient(
    limits = c(0, 1),
    low = "#edf8fb",
    high = "#006d2c",
    labels = scales::percent,
    name = "% routed\nto rank test"
  ) +
  ggplot2$labs(
    title = "Route 1 probability of using the rank test",
    subtitle = "Cells show how often Shapiro-Wilk rejects residual normality",
    x = "Gamma skewness",
    y = "n per group",
    caption = paste(
      "Data: standardised Gamma groups; last group shifted by 0.5 SD.",
      "Gamma caveat: skewness and excess kurtosis vary together."
    )
  ) +
  ggplot2$theme_minimal(base_size = 11) +
  ggplot2$theme(
    axis.text.x = ggplot2$element_text(size = 8),
    legend.position = "right",
    plot.caption = ggplot2$element_text(hjust = 0, size = 9)
  )

ggplot2$ggsave(file.path(OUTDIR, "gamma_high_skew_power.png"),
               p_power, width = 12, height = 6.2, dpi = 180)
ggplot2$ggsave(file.path(OUTDIR, "gamma_high_skew_route_probability.png"),
               p_route, width = 10, height = 5.4, dpi = 180)

cat("Wrote outputs to:", OUTDIR, "\n")
