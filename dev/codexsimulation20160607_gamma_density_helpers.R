gamma_group_cols <- c(A = "#B79F00", B = "#56B4E9", C = "#009E73", D = "#D55E00")

shape_from_skew <- function(skew) {
  if (skew == 0) Inf else (2 / skew)^2
}

standardised_gamma_density <- function(y, alpha, shift = 0) {
  x <- (y - shift) * sqrt(alpha) + alpha
  out <- stats::dgamma(x, shape = alpha, scale = 1) * sqrt(alpha)
  out[x <= 0] <- NA_real_
  out
}

standardised_gamma_curve <- function(alpha, shift = 0, sd = 1,
                                     xlim = c(-2.5, 5), n = 700,
                                     y_cap = Inf) {
  support <- shift - sd * sqrt(alpha)
  t_max <- alpha + sqrt(alpha) * (xlim[2] - shift) / sd
  if (!is.finite(t_max) || t_max <= 0) {
    return(data.frame(x = numeric(), density = numeric(),
                      piece = character()))
  }

  density_t <- function(t) {
    stats::dgamma(t, shape = alpha, scale = 1) * sqrt(alpha) / sd
  }

  t_start <- max(0, alpha + sqrt(alpha) * (xlim[1] - shift) / sd)
  if (xlim[1] <= support && support <= xlim[2]) {
    if (alpha < 1 && is.finite(y_cap)) {
      if (density_t(t_max) > y_cap) {
        t_start <- t_max
      } else {
        t_start <- stats::uniroot(
          function(t) density_t(t) - y_cap,
          c(.Machine$double.eps, t_max)
        )$root
      }
    } else {
      t_start <- 0
    }
  }

  t_grid <- seq(t_start, t_max, length.out = n)
  density <- density_t(t_grid)
  if (is.finite(y_cap)) density <- pmin(density, y_cap)
  right_curve <- data.frame(
    x = shift + sd * (t_grid - alpha) / sqrt(alpha),
    density = density,
    piece = "density"
  )

  if (xlim[1] < support && support <= xlim[2]) {
    zero_end <- min(support, xlim[2])
    zero_x <- seq(xlim[1], zero_end, length.out = max(2, floor(n / 10)))
    zero_curve <- data.frame(x = zero_x, density = 0, piece = "support")
    return(rbind(zero_curve, right_curve))
  }

  right_curve
}
