## Trial: scale-location regression diagnostic with 95% smooth band.
##
## The panel follows the visual logic described for Fig. 6: plot
## sqrt(abs(rstandard(lm(...)))) against scaled fitted values. Constant
## variance should appear as a roughly flat smooth.

set.seed(20260605)

outdir <- file.path("dev", "codextrial20260605_scale_location_outputs")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
pkgload::load_all(".", quiet = TRUE)

make_data <- function(kind = c("constant variance", "curved variance"), n = 100) {
  kind <- match.arg(kind)
  x <- sort(stats::runif(n, -2, 2))
  if (kind == "constant variance") {
    sigma <- rep(0.8, n)
  } else {
    sigma <- 0.35 + 1.0 * abs(x)
  }
  y <- 1.0 + 1.5 * x + stats::rnorm(n, sd = sigma)
  data.frame(x = x, y = y)
}

scale_location_data <- function(dat) {
  fit <- stats::lm(y ~ x, data = dat)
  data.frame(
    fitted_scaled = as.numeric(scale(stats::fitted(fit))),
    sqrt_abs_rstandard = sqrt(abs(stats::rstandard(fit))),
    bp_p = bp.test(fit)$p.value
  )
}

plot_scale_location <- function(dat, main) {
  one <- scale_location_data(dat)
  smooth <- stats::loess(
    sqrt_abs_rstandard ~ fitted_scaled,
    data = one,
    span = 0.8,
    degree = 2
  )
  grid_x <- seq(min(one$fitted_scaled), max(one$fitted_scaled), length.out = 200)
  pred <- stats::predict(smooth, newdata = data.frame(fitted_scaled = grid_x), se = TRUE)
  upper <- pred$fit + stats::qt(0.975, pred$df) * pred$se.fit
  lower <- pred$fit - stats::qt(0.975, pred$df) * pred$se.fit
  y_lim <- range(0, one$sqrt_abs_rstandard, lower, upper, na.rm = TRUE)

  plot(
    one$fitted_scaled,
    one$sqrt_abs_rstandard,
    pch = 1,
    col = "grey25",
    xlab = "Scaled fitted values",
    ylab = "sqrt(abs(standardised residuals))",
    main = sprintf("%s\nBreusch-Pagan p = %.3f", main, one$bp_p[1]),
    ylim = y_lim,
    las = 1
  )
  polygon(
    c(grid_x, rev(grid_x)),
    c(upper, rev(lower)),
    border = NA,
    col = grDevices::adjustcolor("grey75", alpha.f = 0.55)
  )
  points(one$fitted_scaled, one$sqrt_abs_rstandard, pch = 1, col = "grey25")
  lines(grid_x, pred$fit, col = "#d73027", lwd = 1.5)
  text(
    x = par("usr")[1],
    y = par("usr")[4],
    labels = "95% CI",
    adj = c(-0.08, 1.15),
    col = "grey35",
    cex = 0.78
  )
}

outfile <- file.path(outdir, "scale_location_regression_trial.png")
grDevices::png(outfile, width = 1800, height = 900, res = 180)
op <- par(mfrow = c(1, 2), mar = c(4.5, 4.8, 4.0, 1.0), oma = c(0, 0, 2.0, 0))
plot_scale_location(make_data("constant variance"), "Constant variance")
plot_scale_location(make_data("curved variance"), "Curved variance")
mtext("Trial for regression diagnostics: scale-location plot with smooth and 95% CI", outer = TRUE, cex = 1.0)
par(op)
grDevices::dev.off()

message("Wrote: ", outfile)
