## ---------------------------------------------------------------------------
## CLT demonstration for the central-tendency gate.
##
## Claim being shown: the residual-normality test inspects the WRONG object.
## The general linear model's inference on means (the estimated coefficients)
## relies on the sampling distribution of the MEAN being normal -- not on the
## residuals being normal. The central limit theorem normalises the former,
## never the latter, when the data-generating distribution is skewed/heavy.
##
## Two distributions are run, to separate the two shape axes:
##   A. Johnson SU: skewness = 2 with VERY HIGH excess kurtosis (~22). Heavy
##      tails, modest skew. The mean normalises by n = 50-100.
##   B. Lognormal "income": EXTREME right skew (~6), long right tail. The mean
##      does NOT normalise by n = 50-100; it needs thousands per group.
##
## Why the two differ is the Edgeworth moment law. For ANY finite-variance
## distribution with skewness g1 and excess kurtosis g2, the standardised
## sample mean has
##       skewness        ~ g1 / sqrt(n)      (order 1/sqrt(n))  <- binding term
##       excess kurtosis ~ g2 / n            (order 1/n)
## so skewness dominates and kurtosis decays a full order faster. Huge g2
## (case A) is tamed quickly; large g1 (case B) is not. MC here ILLUSTRATES
## two distributions; the moment law above IS the general statement (a proof
## would need every finite-moment distribution).
##
## Run:  source("dev/clt_mean_demo.R")   # draws two figures to the active device
## ---------------------------------------------------------------------------

suppressMessages(library(SuppDists))

skw    <- function(x) { m <- mean(x); mean((x - m)^3) / sd(x)^3 }
exkurt <- function(x) { m <- mean(x); mean((x - m)^4) / sd(x)^4 - 3 }

NREP <- 4e4                                 # groups drawn per n (MC resolution)

## ---- Reusable exercise -----------------------------------------------------
## rgen(n) draws n iid values from the base distribution. ns_fig are the two
## group sizes plotted; ns_more are extra sizes shown only in the table (to
## locate where the mean finally normalises). xlim sets the histogram window.
run_clt_demo <- function(rgen, label, ns_fig = c(50, 100), ns_more = c(500, 1000),
                         xlim = c(-4, 4), big_n = 2e6) {
  big <- rgen(big_n); mu <- mean(big); sg <- sd(big)
  g1  <- skw(big);   g2 <- exkurt(big)

  ## Draw NREP groups of size n; standardise each group mean. CLT: z -> N(0,1).
  smean <- function(n, seed = 7) {
    set.seed(seed)
    replicate(NREP, (mean(rgen(n)) - mu) / (sg / sqrt(n)))
  }
  ns_all <- c(ns_fig, ns_more)
  Z <- lapply(ns_all, smean); names(Z) <- as.character(ns_all)

  ## Statistical companion: empirical vs Edgeworth-predicted moments.
  cat(sprintf("\n=== %s ===\n", label))
  cat(sprintf("Base distribution: skewness = %.2f, excess kurtosis = %.1f\n",
              g1, g2))
  cat(sprintf("Sampling distribution of the standardised mean (%g groups per n)\n",
              NREP))
  cat("    n |  emp.skew  g1/sqrt(n) |  emp.exkurt    g2/n\n")
  for (n in ns_all) {
    z <- Z[[as.character(n)]]
    cat(sprintf(" %4d |   %6.3f    %6.3f   |   %7.3f   %6.3f\n",
                n, skw(z), g1 / sqrt(n), exkurt(z), g2 / n))
  }

  ## Figure: raw variate (residual scale) | mean of n[1] | mean of n[2].
  op <- par(no.readonly = TRUE); on.exit(par(op))
  par(mfrow = c(2, 3), mar = c(4.2, 4.2, 3, 1), oma = c(0, 0, 2, 0))
  set.seed(2); raw <- as.numeric(scale(rgen(2000)))
  panels <- list(
    list(v = raw, main = sprintf("raw variate\nskew %.1f, ex.kurt %.0f", g1, g2)),
    list(v = Z[[as.character(ns_fig[1])]], main = sprintf("mean of n = %d", ns_fig[1])),
    list(v = Z[[as.character(ns_fig[2])]], main = sprintf("mean of n = %d", ns_fig[2])))

  for (p in panels) {                       # row 1: QQ plots
    qqnorm(p$v, pch = 16, cex = 0.4, col = "#444444",
           main = p$main, xlab = "normal quantiles", ylab = "sample quantiles")
    qqline(p$v, col = "#d95f02", lwd = 2)
  }
  for (p in panels) {                       # row 2: histograms + densities
    hist(p$v, breaks = 80, freq = FALSE, col = "grey90", border = "grey70",
         xlim = xlim, main = "", xlab = "standardised value")
    lines(density(p$v), col = "#444444", lwd = 2)
    curve(dnorm(x), add = TRUE, col = "#d95f02", lwd = 2, lty = 2)
  }
  legend("topright", c("kernel density", "N(0,1)"),
         col = c("#444444", "#d95f02"), lwd = 2, lty = c(1, 2), bty = "n", cex = 0.9)
  mtext(label, outer = TRUE, cex = 0.95, font = 2)
  invisible(Z)
}

## ---- Scenarios -------------------------------------------------------------
## A. Heavy tails, modest skew: the mean normalises by n = 50-100.
## B. Income (lognormal 0,1): skewness ~6.2, the textbook income shape; skew of
##    the mean is g1/sqrt(n) = 6.2/sqrt(50) ~ 0.9 -- still visibly skewed, and
##    ~n>3800 is needed to push it below 0.1.
johnson <- JohnsonFit(c(0, 1, 2, 25), moment = "use")   # skew 2, ex.kurt ~22
SCENARIOS <- list(
  johnson = list(rgen  = function(n) rJohnson(n, johnson),
                 label = "Johnson SU: skew 2, heavy tails  ->  mean ~normal by n = 50-100",
                 xlim  = c(-4, 4)),
  income  = list(rgen  = function(n) rlnorm(n, 0, 1),
                 label = "Lognormal (income): extreme skew  ->  mean still skewed at n = 50-100",
                 xlim  = c(-3, 6)))

## Auto-run both when sourced interactively; skipped when the Rmd reference
## note sources this file only for its function definitions (sets the option).
if (!isTRUE(getOption("clt_demo_defs_only"))) {
  for (s in SCENARIOS) run_clt_demo(s$rgen, s$label, xlim = s$xlim)
}
