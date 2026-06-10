## ---------------------------------------------------------------------------
## Reviewer-facing simulation for the Route 1 residual-shape gate.
##
## The script mirrors the current visstat() decision logic without drawing the
## visstat() panels:
##   residual shape gate:
##     Shapiro-Wilk on rstandard(lm(y ~ g)) for N <= 5000 residuals,
##     Anderson-Darling for N > 5000 residuals;
##   if residual shape passes:
##     Levene gate -> Student/Fisher or Welch;
##   if residual shape fails:
##     Wilcoxon/Kruskal-Wallis.
##
## It writes CSV tables and PNG figures to dev/sw_gate_reviewer_outputs/.
## ---------------------------------------------------------------------------

set.seed(20260602)

args <- commandArgs(trailingOnly = TRUE)
NREP <- if (length(args) >= 1) as.integer(args[1]) else 2000
ALPHA <- 0.05
OUTDIR <- file.path("dev", "codexsimulation20160602_route1_residual_shape_gate_outputs")
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

stopifnot(NREP > 0)
if (!requireNamespace("nortest", quietly = TRUE)) {
  stop("Package 'nortest' is required.")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required.")
}

## ---- route logic ----------------------------------------------------------

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
    s <- max(stats::sigma(model), 1e-8)
    rs <- raw / s
  }
  rs <- rs[is.finite(rs)]
  if (length(rs) < 3) return(NA_real_)
  if (length(rs) > 5000) {
    nortest::ad.test(rs)$p.value
  } else {
    stats::shapiro.test(rs)$p.value
  }
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
    p_student <- stats::t.test(a, b, var.equal = TRUE)$p.value
    p_welch <- stats::t.test(a, b, var.equal = FALSE)$p.value
    p_mean <- if (equal_var) p_student else p_welch
    p_rank <- suppressWarnings(stats::wilcox.test(a, b)$p.value)
    mean_method <- if (equal_var) "Student" else "Welch"
    rank_method <- "Wilcoxon"
  } else {
    p_fisher <- stats::anova(stats::lm(y ~ g))[["Pr(>F)"]][1]
    p_welch <- stats::oneway.test(y ~ g, var.equal = FALSE)$p.value
    p_mean <- if (equal_var) p_fisher else p_welch
    p_rank <- stats::kruskal.test(y, g)$p.value
    mean_method <- if (equal_var) "Fisher" else "Welch"
    rank_method <- "Kruskal-Wallis"
  }

  p_gate <- if (normality_met) p_mean else p_rank
  gate_method <- if (normality_met) mean_method else rank_method

  c(mean_reject = p_mean < alpha,
    rank_reject = p_rank < alpha,
    gate_reject = p_gate < alpha,
    route_rank = !normality_met,
    route_welch = normality_met && !equal_var,
    norm_p = p_norm,
    levene_p = p_lev,
    gate_method = gate_method)
}

## ---- data generators ------------------------------------------------------

standardise <- function(x) (x - mean(x)) / stats::sd(x)

draw_shape <- function(shape, n) {
  if (shape == "normal") return(stats::rnorm(n))
  if (shape == "t3") return(stats::rt(n, df = 3) / sqrt(3))
  if (shape == "gamma_skew1") return((stats::rgamma(n, shape = 4) - 4) / 2)
  if (shape == "exponential_skew2") return(stats::rexp(n) - 1)
  if (shape == "lognormal_skew6") {
    sigma <- 1.0
    mu <- -sigma^2 / 2
    x <- stats::rlnorm(n, meanlog = mu, sdlog = sigma)
    return((x - 1) / sqrt((exp(sigma^2) - 1) * exp(2 * mu + sigma^2)))
  }
  stop("Unknown shape: ", shape)
}

make_common_null <- function(shape, n, k) {
  g <- factor(rep(seq_len(k), each = n))
  y <- unlist(lapply(seq_len(k), function(i) draw_shape(shape, n)))
  list(y = y, g = g)
}

make_location_shift <- function(shape, n, k, shift = 0.5) {
  dat <- make_common_null(shape, n, k)
  dat$y[dat$g == levels(dat$g)[k]] <- dat$y[dat$g == levels(dat$g)[k]] + shift
  dat
}

make_equal_mean_lognormal <- function(n_vec) {
  sdlog <- seq(0.3, 1.1, length.out = length(n_vec))
  meanlog <- 1 - sdlog^2 / 2
  names(sdlog) <- names(n_vec)
  names(meanlog) <- names(n_vec)
  g <- factor(rep(names(n_vec), times = n_vec), levels = names(n_vec))
  y <- unlist(lapply(names(n_vec), function(i) {
    stats::rlnorm(n_vec[i], meanlog = meanlog[i], sdlog = sdlog[i])
  }))
  list(y = y, g = g)
}

summarise_binary <- function(values) {
  p <- mean(values)
  se <- sqrt(p * (1 - p) / length(values))
  c(rate = p, mc_se = se)
}

run_cell <- function(make_data, nrep = NREP) {
  out <- replicate(nrep, {
    dat <- make_data()
    route_once(dat$y, dat$g)
  }, simplify = FALSE)

  mean_reject <- as.logical(vapply(out, `[[`, character(1), "mean_reject"))
  rank_reject <- as.logical(vapply(out, `[[`, character(1), "rank_reject"))
  gate_reject <- as.logical(vapply(out, `[[`, character(1), "gate_reject"))
  route_rank <- as.logical(vapply(out, `[[`, character(1), "route_rank"))
  route_welch <- as.logical(vapply(out, `[[`, character(1), "route_welch"))

  c(mean = summarise_binary(mean_reject),
    rank = summarise_binary(rank_reject),
    gate = summarise_binary(gate_reject),
    route_rank = summarise_binary(route_rank),
    route_welch = summarise_binary(route_welch))
}

as_row <- function(res, scenario, shape, n, k, estimand) {
  data.frame(
    scenario = scenario,
    shape = shape,
    n_per_group = n,
    groups = k,
    estimand = estimand,
    mean_rate = res["mean.rate"],
    mean_mc_se = res["mean.mc_se"],
    rank_rate = res["rank.rate"],
    rank_mc_se = res["rank.mc_se"],
    gate_rate = res["gate.rate"],
    gate_mc_se = res["gate.mc_se"],
    route_rank_rate = res["route_rank.rate"],
    route_rank_mc_se = res["route_rank.mc_se"],
    route_welch_rate = res["route_welch.rate"],
    route_welch_mc_se = res["route_welch.mc_se"],
    row.names = NULL
  )
}

## ---- simulation sweeps ----------------------------------------------------

shapes <- c("normal", "t3", "gamma_skew1", "exponential_skew2",
            "lognormal_skew6")
shape_labels <- c(
  normal = "Normal N(0,1)",
  t3 = "t(df=3), scaled to unit variance",
  gamma_skew1 = "standardised Gamma(shape=4), skew=1",
  exponential_skew2 = "Exponential(1)-1, skew=2",
  lognormal_skew6 = "standardised Lognormal(sdlog=1), skew approx 6.2"
)
ns <- c(20, 50, 100, 200)
ks <- c(2, 4)

cat("Running common-null and power sweeps with NREP =", NREP, "\n")

common_rows <- list()
power_rows <- list()
idx <- 1
for (k in ks) {
  for (n in ns) {
    for (shape in shapes) {
      common_res <- run_cell(function() make_common_null(shape, n, k))
      common_rows[[idx]] <- as_row(
        common_res, "common null: identical distributions",
        shape, n, k, "equal means and equal ordering"
      )
      power_res <- run_cell(function() make_location_shift(shape, n, k, shift = 0.5))
      power_rows[[idx]] <- as_row(
        power_res, "location shift: one group shifted by 0.5 SD",
        shape, n, k, "mean and ordering differ"
      )
      cat(sprintf("  done: k=%d n=%d shape=%s\n", k, n, shape))
      idx <- idx + 1
    }
  }
}

common <- do.call(rbind, common_rows)
power <- do.call(rbind, power_rows)
common$distribution <- shape_labels[common$shape]
power$distribution <- shape_labels[power$shape]

conflict_designs <- list(
  "n_50_to_100" = c(A = 55, B = 65, C = 75, D = 85, E = 95),
  "n_gt_100" = c(A = 110, B = 130, C = 150, D = 170, E = 190),
  "n_gt_500" = c(A = 510, B = 530, C = 550, D = 570, E = 590)
)

cat("Running equal-mean lognormal conflict cells with NREP =", NREP, "\n")
conflict_rows <- list()
for (nm in names(conflict_designs)) {
  res <- run_cell(function() make_equal_mean_lognormal(conflict_designs[[nm]]))
  conflict_rows[[nm]] <- as_row(
    res, "equal means, unequal lognormal shape",
    nm, paste(conflict_designs[[nm]], collapse = "/"), 5,
    "equal means; ordering differs"
  )
  cat(sprintf("  done: %s\n", nm))
}
conflict <- do.call(rbind, conflict_rows)
conflict$distribution <- paste(
  "Lognormal groups with sdlog from 0.3 to 1.1;",
  "meanlog adjusted so all population means equal exp(1)"
)

## ---- write tables ---------------------------------------------------------

write.csv(common, file.path(OUTDIR, "common_null_rates.csv"), row.names = FALSE)
write.csv(power, file.path(OUTDIR, "location_shift_power.csv"), row.names = FALSE)
write.csv(conflict, file.path(OUTDIR, "equal_mean_lognormal_conflict.csv"),
          row.names = FALSE)

summary_lines <- c(
  "Reviewer-facing Route 1 gate simulation",
  paste("NREP per cell:", NREP),
  paste("alpha:", ALPHA),
  "",
  "Testing rule:",
  "  Shapiro-Wilk on internally studentised residuals routes N <= 5000;",
  "  Anderson-Darling is used above 5000 residuals. If residual shape",
  "  passes, Levene selects Student/Fisher versus Welch. If residual shape",
  "  fails, Wilcoxon or Kruskal-Wallis is used.",
  "",
  "Distribution labels:",
  paste("  normal:", shape_labels["normal"]),
  paste("  t3:", shape_labels["t3"]),
  paste("  gamma_skew1:", shape_labels["gamma_skew1"]),
  paste("  exponential_skew2:", shape_labels["exponential_skew2"]),
  paste("  lognormal_skew6:", shape_labels["lognormal_skew6"]),
  "",
  "Files:",
  "  common_null_rates.csv",
  "    DGP: all groups sampled from the same distribution; no shift;",
  "    equal means and equal ordering are both true.",
  "  location_shift_power.csv",
  "    DGP: all groups have the same shape; the last group is shifted by",
  "    0.5 SD; mean and ordering alternatives are both true.",
  "  equal_mean_lognormal_conflict.csv",
  "    DGP: five lognormal groups have equal population means but unequal",
  "    spread/skew; mean null is true, rank/ordering null is false.",
  "  common_null_typeI.png",
  "  location_shift_power.png",
  "  route_to_rank_probability.png",
  "    Values are percentages of simulations routed to the rank branch under",
  "    identical group distributions with no shift.",
  "  equal_mean_lognormal_conflict.png",
  "",
  "Interpretation:",
  "  common_null_rates.csv: all three tests have a true null.",
  "  location_shift_power.csv: all three tests have a false null.",
  "  equal_mean_lognormal_conflict.csv: the mean null is true, but the",
  "    ordering/rank null is false; gate rejections are route-dependent."
)
writeLines(summary_lines, file.path(OUTDIR, "README.txt"))

## ---- plots ----------------------------------------------------------------

ggplot2 <- asNamespace("ggplot2")

to_long <- function(dat, rates = c("mean_rate", "rank_rate", "gate_rate")) {
  out <- lapply(rates, function(col) {
    data.frame(
      scenario = dat$scenario,
      shape = dat$shape,
      n_per_group = dat$n_per_group,
      groups = dat$groups,
      strategy = sub("_rate$", "", col),
      rate = dat[[col]],
      row.names = NULL
    )
  })
  do.call(rbind, out)
}

common_long <- to_long(common)
power_long <- to_long(power)
common_long$distribution <- shape_labels[common_long$shape]
power_long$distribution <- shape_labels[power_long$shape]

p_typeI <- ggplot2$ggplot(common_long,
                          ggplot2$aes(x = n_per_group, y = rate,
                                      colour = strategy, group = strategy)) +
  ggplot2$geom_hline(yintercept = ALPHA, linetype = 1, colour = "grey30") +
  ggplot2$geom_hline(yintercept = c(0.025, 0.075), linetype = 2,
                     colour = "grey55") +
  ggplot2$geom_line(linewidth = 0.6) +
  ggplot2$geom_point(size = 1.8) +
  ggplot2$facet_grid(groups ~ distribution, labeller = ggplot2$label_both) +
  ggplot2$scale_x_continuous(breaks = ns) +
  ggplot2$scale_y_continuous(limits = c(0, 0.14)) +
  ggplot2$labs(x = "n per group", y = "rejection rate",
               title = "Type I check under identical group distributions",
               subtitle = paste("No group shift: equal means and equal ordering are true.",
                                "Solid line = alpha .05; dashed = Bradley [.025, .075]")) +
  ggplot2$theme_minimal(base_size = 10)

p_power <- ggplot2$ggplot(power_long,
                          ggplot2$aes(x = n_per_group, y = rate,
                                      colour = strategy, group = strategy)) +
  ggplot2$geom_line(linewidth = 0.6) +
  ggplot2$geom_point(size = 1.8) +
  ggplot2$facet_grid(groups ~ distribution, labeller = ggplot2$label_both) +
  ggplot2$scale_x_continuous(breaks = ns) +
  ggplot2$scale_y_continuous(limits = c(0, 1)) +
  ggplot2$labs(x = "n per group", y = "power",
               title = "Power under same-shape groups with one shifted group",
               subtitle = "Last group shifted by 0.5 SD: mean and ordering alternatives are both true") +
  ggplot2$theme_minimal(base_size = 10)

p_route <- ggplot2$ggplot(common,
                          ggplot2$aes(x = n_per_group, y = route_rank_rate,
                                      colour = distribution,
                                      group = distribution)) +
  ggplot2$geom_line(linewidth = 0.7) +
  ggplot2$geom_point(size = 1.8) +
  ggplot2$facet_wrap(stats::as.formula("~ groups"), labeller = ggplot2$label_both) +
  ggplot2$scale_x_continuous(breaks = ns) +
  ggplot2$scale_y_continuous(limits = c(0, 1)) +
  ggplot2$labs(x = "n per group", y = "percentage routed to rank branch",
               title = "Route-to-rank percentage under identical group distributions",
               subtitle = "No group shift: equal means and equal ordering are true") +
  ggplot2$theme_minimal(base_size = 10)

conflict_long <- to_long(conflict)
conflict_long$n_per_group <- factor(conflict_long$n_per_group,
                                    levels = conflict$n_per_group)
p_conflict <- ggplot2$ggplot(conflict_long,
                             ggplot2$aes(x = n_per_group, y = rate,
                                         fill = strategy)) +
  ggplot2$geom_col(position = ggplot2$position_dodge(width = 0.75),
                   width = 0.7) +
  ggplot2$geom_hline(yintercept = ALPHA, linetype = 1, colour = "grey30") +
  ggplot2$scale_y_continuous(limits = c(0, 1)) +
  ggplot2$labs(x = "group-size regime", y = "rejection rate",
               title = "Equal population means, unequal lognormal spread/skew",
               subtitle = "Mean null is true; rank/ordering null is false") +
  ggplot2$theme_minimal(base_size = 10) +
  ggplot2$theme(axis.text.x = ggplot2$element_text(angle = 20, hjust = 1))

ggplot2$ggsave(file.path(OUTDIR, "common_null_typeI.png"), p_typeI,
               width = 12, height = 6, dpi = 180)
ggplot2$ggsave(file.path(OUTDIR, "location_shift_power.png"), p_power,
               width = 12, height = 6, dpi = 180)
ggplot2$ggsave(file.path(OUTDIR, "route_to_rank_probability.png"), p_route,
               width = 8, height = 5, dpi = 180)
ggplot2$ggsave(file.path(OUTDIR, "equal_mean_lognormal_conflict.png"),
               p_conflict, width = 8, height = 5, dpi = 180)

cat("\nWrote outputs to:", OUTDIR, "\n")
cat("Done.\n")
