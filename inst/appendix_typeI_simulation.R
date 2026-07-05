## ---------------------------------------------------------------------------
## Appendix: Why visstat() bypasses the residual-normality test above 50
##           observations per group -- a Type I error simulation.
##
## Companion code for the visStatistics paper / vignette. Self-contained and
## reproducible: run top to bottom with
##
##     source(system.file("appendix_typeI_simulation.R", package = "visStatistics"))
##
## or open it and step through interactively. Figures are drawn to the active
## graphics device (your plot pane); no files are written.
##
## ---------------------------------------------------------------------------
## What is being shown
## ---------------------------------------------------------------------------
## The central-tendency router in visstat() stops using the Shapiro-Wilk /
## Anderson-Darling residual-normality test to choose between a mean-based test
## (Welch's t / Welch's one-way ANOVA) and a rank-based test (Wilcoxon /
## Kruskal-Wallis) once every group has more than 50 observations. Above that
## size the choice rests on the central limit theorem, not on the residuals
## being normal -- which, for skewed data, they are not.
##
## This script asks the sharpest possible question of that rule: in the
## least-favourable configuration -- many groups, unequal variances, and
## pronounced skewness -- does bypassing the normality test let the mean-based
## test exceed its nominal error rate?
##
## Construction (k = 5 groups):
##   * EQUAL population means. Hence every rejection by a mean-based test of
##     "all means equal" is a FALSE POSITIVE, so the empirical rejection rate
##     IS the Type I error rate -- it cannot be contaminated by a real effect.
##   * UNEQUAL standard deviations (heteroscedastic).
##   * A common, TUNABLE skewness s, identical in every group.
##
## Skewness is invariant under positive scaling and shifting, so a single
## standardised skewed base z (mean 0, sd 1, skewness s) yields all three
## properties at once:
##
##       y_i = mu + sigma_i * z ,
##
## giving mean(y_i) = mu (equal means), sd(y_i) = sigma_i (heteroscedastic),
## and skewness(y_i) = s (common skew). The base z is built from a lognormal
## whose log-standard-deviation is solved to hit the target skewness.
##
## Benchmark: Bradley's (1978) liberal robustness criterion. For a nominal
## alpha = .05 a test is "robust" if its empirical Type I error lies in
## [.025, .075]. We report the DIRECTION of the result (robust vs not),
## which is stable to Monte Carlo noise, rather than a knife-edge estimate.
## ---------------------------------------------------------------------------

library(visStatistics)

## ---- 1. Data-generating machinery -----------------------------------------

## Per-group standard deviations and common mean.
group_sd <- c(A = 0.3, B = 0.5, C = 0.7, D = 0.9, E = 1.1)
group_mean <- 10

## Target skewness s -> log-standard-deviation of the lognormal base.
## Lognormal skewness is (w + 2) sqrt(w - 1) with w = exp(sdlog^2); this is
## strictly increasing in sdlog, so the root is unique.
skew_to_sdlog <- function(s) {
  stopifnot(s > 0)
  uniroot(function(sdlog) {
    w <- exp(sdlog^2)
    (w + 2) * sqrt(w - 1) - s
  }, c(1e-4, 5))$root
}

## Standardised skewed base: mean 0, sd 1, skewness s.
make_base <- function(s) {
  sdlog <- skew_to_sdlog(s)
  m <- exp(sdlog^2 / 2)
  sdz <- sqrt((exp(sdlog^2) - 1) * exp(sdlog^2))
  function(n) (rlnorm(n, 0, sdlog) - m) / sdz
}

## One realisation of the k-group sample as a (value, group) pair.
make_sample <- function(n_per_group, base) {
  groups <- names(n_per_group)
  value <- unlist(lapply(
    groups,
    function(g) group_mean + group_sd[g] * base(n_per_group[g])
  ))
  list(
    value = value,
    group = factor(rep(groups, times = n_per_group))
  )
}

## ---- 2. Empirical Type I error of Welch's one-way ANOVA --------------------
## Welch's test (oneway.test with var.equal = FALSE) is the mean-based test
## visstat() uses when variances are unequal, which they are here.

welch_typeI <- function(n_per_group, s, nrep = 2000, alpha = 0.05) {
  mean(replicate(nrep, {
    oneway.test(
      (unlist(lapply(
        names(n_per_group),
        function(g) group_mean + group_sd[g] * make_base(s)(n_per_group[g])
      )) ~ factor(rep(names(n_per_group), times = n_per_group))),
      var.equal = FALSE
    )$p.value < alpha
  }))
}

## ---- 3. The sweep: skewness x per-group-size regime ------------------------

run_sweep <- function(skews = 2:8, nrep = 2000, seed = 1) {
  regimes <- list(
    "n in (50,100)" = c(A = 55, B = 65, C = 75, D = 85, E = 95),
    "n > 100"       = c(A = 110, B = 130, C = 150, D = 170, E = 190),
    "n > 500"       = c(A = 510, B = 530, C = 550, D = 570, E = 590)
  )
  tab <- matrix(NA_real_, length(skews), length(regimes),
    dimnames = list(skewness = skews, regime = names(regimes))
  )
  for (i in seq_along(skews)) {
    for (j in seq_along(regimes)) {
      set.seed(seed)
      tab[i, j] <- welch_typeI(regimes[[j]], skews[i], nrep = nrep)
    }
  }
  tab
}

cat(
  "Welch one-way ANOVA, empirical Type I error",
  "(k = 5, equal means, heteroscedastic).\n"
)
cat("Nominal alpha = .05; Bradley (1978) liberal bound [.025, .075].\n\n")

typeI <- run_sweep(skews = 2:8, nrep = 2000, seed = 1)
print(round(typeI, 4))

flagged <- ifelse(typeI > 0.075,
  paste0(format(round(typeI, 3), nsmall = 3), "*"),
  format(round(typeI, 3), nsmall = 3)
)
cat("\n('*' = above Bradley's upper bound .075)\n")
print(noquote(flagged))

## ---- 4. Figure: Type I error vs skewness, with Bradley's band --------------
## Drawn to the active device so it adopts your plot-pane size.

matplot_typeI <- function(tab) {
  skews <- as.numeric(rownames(tab))
  cols <- c("#1b9e77", "#d95f02", "#7570b3")
  matplot(skews, tab,
    type = "b", pch = 16, lty = 1, lwd = 2, col = cols,
    ylim = c(0.04, max(0.095, max(tab))),
    xlab = "common group skewness",
    ylab = "empirical Type I error",
    main = "Welch ANOVA Type I error vs skewness (k = 5, equal means)"
  )
  rect(par("usr")[1], 0.025, par("usr")[2], 0.075,
    col = adjustcolor("grey80", alpha.f = 0.35), border = NA
  )
  matlines(skews, tab, type = "b", pch = 16, lty = 1, lwd = 2, col = cols)
  abline(h = 0.05, lty = 3)
  legend("topleft", colnames(tab), col = cols, pch = 16, lwd = 2, bty = "n")
  legend("bottomright", "Bradley band [.025, .075]",
    fill = adjustcolor("grey80", alpha.f = 0.6), border = NA, bty = "n"
  )
}
matplot_typeI(typeI)

## ---- 5. How realistic is the skewness that breaks the bound? ---------------
## The bound is exceeded only at the smallest sizes once skewness reaches ~6.
## Two facts put that in context.

cat("\n--- Interpreting the skewness scale ---\n")
cat("Reference skewness:  exponential / chi-sq(2) = 2.00 ;  chi-sq(1) = 2.83\n")
for (s in c(2, 4, 6, 8)) {
  sdlog <- skew_to_sdlog(s)
  cv <- sqrt(exp(sdlog^2) - 1)
  cat(sprintf("  skewness %d  <->  lognormal CV = %.2f\n", s, cv))
}
cat("\nMax POSSIBLE sample skewness for n points is (n-2)/sqrt(n-1):\n")
for (n in c(55, 75, 95)) {
  cat(sprintf("  n = %d  ->  ceiling = %.2f\n", n, (n - 2) / sqrt(n - 1)))
}
cat(paste(strwrap(paste(
  "At n = 55 the sample skewness cannot exceed 7.2, so an observed value of 6",
  "sits near that ceiling and is typically driven by one or two extreme",
  "points rather than the bulk shape. A stable population skewness of 6",
  "(lognormal CV > 1.2) is heavy-tailed data for which the mean is itself a",
  "questionable summary."
), width = 78), collapse = "\n"), "\n")

## ---- 6. Reproducible visstat() demonstration -------------------------------
## A single dataset from the same construction, passed to visstat(). With every
## group larger than 50, the residual-normality test is bypassed and the
## heteroscedasticity test routes to Welch's one-way ANOVA -- even though the
## residuals are visibly non-normal (see the Q-Q panel of the assumptions
## plot). visstat() draws the assumptions plot and the result plot to the
## active device.

set.seed(1)
demo <- make_sample(c(A = 55, B = 65, C = 75, D = 85, E = 95),
  base = make_base(4)
)

cat("\n--- visstat() demonstration (skewness 4, n in (50,100)) ---\n")
cat(
  "group means  :",
  paste(round(tapply(demo$value, demo$group, mean), 2), collapse = " / "),
  "\n"
)
cat(
  "group medians:",
  paste(round(tapply(demo$value, demo$group, median), 2), collapse = " / "),
  "\n"
)
cat(
  "group sds    :",
  paste(round(tapply(demo$value, demo$group, sd), 2), collapse = " / "),
  "\n"
)

result <- visstat(demo$group, demo$value)

## Report the routed test from the returned object. The one-way ANOVA slot
## holds an "htest" (Welch's test) when variances are unequal, otherwise a
## summary.aov (Fisher's ANOVA).
anova_obj <- result[["summary statistics of ANOVA"]]
if (inherits(anova_obj, "htest")) {
  cat(sprintf(
    "visstat() selected: Welch's one-way ANOVA (p = %.3f)\n",
    anova_obj$p.value
  ))
} else {
  p_fisher <- anova_obj[[1]][["Pr(>F)"]][1]
  cat(sprintf(
    "visstat() selected: Fisher's one-way ANOVA (p = %.3f)\n",
    p_fisher
  ))
}
cat(
  "Note: group means coincide while group medians do not, and the residuals",
  "are\nnon-normal -- yet with every group above 50 the mean-based Welch test",
  "is used,\njustified by the central limit theorem rather than by residual",
  "normality.\n"
)

## ---- 7. Session information for reproducibility ----------------------------
cat("\n--- sessionInfo() ---\n")
print(sessionInfo())
