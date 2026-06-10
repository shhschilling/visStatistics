## Trial: residual structure in signed residuals vs scale-location.
##
## Compares the visible residual structure in signed residuals with the
## sqrt(abs(rstandard())) scale-location transform.

set.seed(20260605)

outdir <- file.path("dev", "codextrial20260605_quadratic_residual_outputs")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

make_quadratic_data <- function(n = 150) {
  x <- seq(-2, 2, length.out = n)
  y <- x^2 + stats::rnorm(n, sd = 0.35)
  list(x = x, y = y, fit = stats::lm(y ~ x))
}

make_gamma_skew6_data <- function(n = 150) {
  x <- seq(-2, 2, length.out = n)
  shape <- (2 / 6)^2
  gamma_error <- stats::rgamma(n, shape = shape, scale = 1)
  gamma_error <- (gamma_error - shape) / sqrt(shape)
  y <- 1 + 1.5 * x + gamma_error
  list(x = x, y = y, fit = stats::lm(y ~ x))
}

add_smooth <- function(x, y) {
  smooth <- stats::loess(y ~ x, span = 0.8, degree = 2)
  grid_x <- seq(min(x), max(x), length.out = 200)
  pred <- stats::predict(smooth, newdata = data.frame(x = grid_x), se = TRUE)
  upper <- pred$fit + stats::qt(0.975, pred$df) * pred$se.fit
  lower <- pred$fit - stats::qt(0.975, pred$df) * pred$se.fit
  polygon(
    c(grid_x, rev(grid_x)),
    c(upper, rev(lower)),
    border = NA,
    col = grDevices::adjustcolor("grey75", alpha.f = 0.55)
  )
  lines(grid_x, pred$fit, col = "#d73027", lwd = 1.5)
}

plot_panel <- function(x, y, main, ylab, h0 = TRUE) {
  y_lim <- range(y, na.rm = TRUE)
  y_pad <- diff(y_lim) * 0.12
  plot(
    x, y,
    pch = 1,
    col = "grey25",
    xlab = "Scaled fitted values",
    ylab = ylab,
    main = main,
    ylim = y_lim + c(-y_pad, y_pad),
    las = 1
  )
  if (h0) abline(h = 0, col = "grey85", lwd = 1)
  add_smooth(x, y)
  points(x, y, pch = 1, col = "grey25")
}

plot_original <- function(dat, main) {
  x_grid <- seq(min(dat$x), max(dat$x), length.out = 200)
  plot(
    dat$x,
    dat$y,
    pch = 1,
    col = "grey25",
    xlab = "x",
    ylab = "y",
    main = main,
    las = 1
  )
  lines(x_grid, stats::predict(dat$fit, newdata = data.frame(x = x_grid)),
        col = "#d73027", lwd = 1.5)
}

plot_comparison <- function(dat, title, outfile) {
  fitted_scaled <- as.numeric(scale(stats::fitted(dat$fit)))
  raw_residuals <- stats::residuals(dat$fit)
  z_residuals <- raw_residuals / stats::sigma(dat$fit)
  scale_location <- sqrt(abs(stats::rstandard(dat$fit)))

  grDevices::png(outfile, width = 2800, height = 900, res = 180)
  op <- par(mfrow = c(1, 4), mar = c(4.6, 4.8, 4.0, 1.0), oma = c(0, 0, 2.0, 0))
  plot_original(dat, "Original y vs x")
  plot_panel(fitted_scaled, raw_residuals, "Raw residuals", "Residuals")
  plot_panel(fitted_scaled, z_residuals, "z residuals", "z residuals")
  plot_panel(
    fitted_scaled,
    scale_location,
    "Scale-location transform",
    "sqrt(abs(rstandard))",
    h0 = FALSE
  )
  mtext(title, outer = TRUE, cex = 1.05)
  par(op)
  grDevices::dev.off()
  message("Wrote: ", outfile)
}

plot_comparison(
  make_quadratic_data(),
  "Missed quadratic: y = x^2 + error, fitted as lm(y ~ x)",
  file.path(outdir, "quadratic_residual_vs_scale_location.png")
)

plot_comparison(
  make_gamma_skew6_data(),
  "Extreme skew: y = 1 + 1.5x + Gamma error, skew = 6, fitted as lm(y ~ x)",
  file.path(outdir, "gamma_skew6_residual_vs_scale_location.png")
)
