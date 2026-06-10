## Trial: self-calibrating QQ plot for vis_lm_assumptions().
##
## Oldford (2016, doi:10.1080/00031305.2015.1090338) shows in Fig. 6 that
## ordinary QQ plots from normal samples can vary substantially. This trial adds
## a simulated 95% reference envelope for rstandard(aov(...)).

set.seed(20260605)

outdir <- file.path("dev", "codextrial20260605_self_calibrating_qq_outputs")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

standardised_residuals <- function(y, g) {
  fit <- stats::aov(y ~ factor(g))
  rs <- suppressWarnings(stats::rstandard(fit))
  if (any(!is.finite(rs))) {
    raw <- stats::residuals(fit)
    rs <- raw / max(stats::sigma(fit), 1e-8)
  }
  list(fit = fit, rs = rs)
}

simulate_reference_residuals <- function(fit, g, nsim = 1000) {
  g <- factor(g)
  fitted_values <- stats::fitted(fit)
  sigma_hat <- stats::sigma(fit)
  replicate(nsim, {
    y_sim <- fitted_values + stats::rnorm(length(g), sd = sigma_hat)
    sort(standardised_residuals(y_sim, g)$rs)
  })
}

qq_data <- function(y, g, nsim = 1000) {
  observed <- standardised_residuals(y, g)
  rs <- sort(observed$rs)
  n <- length(rs)
  p <- stats::ppoints(n)
  x <- stats::qnorm(p)
  sim <- simulate_reference_residuals(observed$fit, g, nsim = nsim)
  list(
    x = x,
    y = rs,
    fit = observed$fit,
    sim = sim,
    q95 = apply(sim, 1, stats::quantile, probs = c(0.025, 0.975))
  )
}

plot_standard_qq <- function(obj, title) {
  plot(
    obj$x, obj$y,
    pch = 1,
    col = "grey25",
    xlab = "Theoretical normal quantiles",
    ylab = "Ordered standardised residuals",
    main = title,
    las = 1
  )
  qqline(obj$y, col = "#d73027", lwd = 1.4)
  abline(h = 0, col = "grey85")
}

plot_self_calibrating_qq <- function(obj, title) {
  x <- obj$x
  y <- obj$y
  y_lim <- range(y, obj$q95)
  plot(
    x, y,
    type = "n",
    ylim = y_lim,
    xlab = "Theoretical normal quantiles",
    ylab = "Ordered standardised residuals",
    main = title,
    las = 1
  )
  polygon(
    c(x, rev(x)),
    c(obj$q95[1, ], rev(obj$q95[2, ])),
    border = NA,
    col = grDevices::adjustcolor("grey75", alpha.f = 0.55)
  )
  abline(0, 1, col = "#d73027", lwd = 1.4)
  points(x, y, pch = 1, col = "grey25", lwd = 1.1)
  text(
    x = grDevices::xy.coords(par("usr")[1:2], par("usr")[3:4])$x[1],
    y = grDevices::xy.coords(par("usr")[1:2], par("usr")[3:4])$y[2],
    labels = "95% envelope",
    adj = c(-0.08, 1.15),
    col = "grey35",
    cex = 0.78
  )
}

make_data <- function(kind = c("normal", "gamma_skew2"), n = 25, groups = 4) {
  kind <- match.arg(kind)
  g <- factor(rep(LETTERS[seq_len(groups)], each = n))
  if (kind == "normal") {
    y <- stats::rnorm(length(g))
  } else {
    shape <- 1
    y <- (stats::rgamma(length(g), shape = shape, scale = 1) - shape) / sqrt(shape)
  }
  list(y = y, g = g)
}

normal_dat <- make_data("normal", n = 25)
skew_dat <- make_data("gamma_skew2", n = 25)
normal_qq <- qq_data(normal_dat$y, normal_dat$g)
skew_qq <- qq_data(skew_dat$y, skew_dat$g)

outfile <- file.path(outdir, "self_calibrating_qq_residual_trial.png")
grDevices::png(outfile, width = 2200, height = 1400, res = 180)
op <- par(mfrow = c(2, 2), mar = c(4.4, 4.5, 3.2, 1.0), oma = c(0, 0, 2.0, 0))
plot_standard_qq(normal_qq, "Normal data: ordinary QQ")
plot_self_calibrating_qq(normal_qq, "Normal data: calibrated QQ")
plot_standard_qq(skew_qq, "Gamma skew = 2: ordinary QQ")
plot_self_calibrating_qq(skew_qq, "Gamma skew = 2: calibrated QQ")
mtext("Trial for vis_lm_assumptions(): QQ plot of rstandard(aov(...))", outer = TRUE, cex = 1.1)
par(op)
grDevices::dev.off()

message("Wrote: ", outfile)
