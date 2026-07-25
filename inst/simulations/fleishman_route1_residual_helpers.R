fleishman_cases <- data.frame(
  panel = 1:5,
  distribution = c(
    "normal",
    rep("F.P.", 4)
  ),
  skew = c(0, 0, 0.5, 1, 2),
  excess_kurtosis = c(0, 6, 1, 1.6, 6.6),
  a = c(
    0,
    0,
    -0.07311803,
    -0.15937609,
    -0.27606127
  ),
  b = c(
    1,
    0.66268160,
    0.92409763,
    0.94242977,
    0.78553817
  ),
  c = c(
    0,
    0,
    0.07311803,
    0.15937609,
    0.27606127
  ),
  d = c(
    0,
    0.10189081,
    0.02298245,
    0.01049963,
    0.04301770
  ),
  stringsAsFactors = FALSE
)

fleishman_panel_label <- function(panel) {
  one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
  if (nrow(one) != 1) stop("Unknown Fleishman panel: ", panel)
  if (one$panel == 1) {
    return("Normal distribution\nN(0, 1)\nskew = 0; excess kurtosis = 0")
  }
  sprintf(
    "F.P.\na = %.3f, b = %.3f, c = -a, d = %.3f\nskew = %s; excess kurtosis = %s",
    one$a,
    one$b,
    one$d,
    format(one$skew, trim = TRUE, scientific = FALSE),
    format(one$excess_kurtosis, trim = TRUE, scientific = FALSE)
  )
}

fleishman_panel_short_label <- function(panel) {
  one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
  if (nrow(one) != 1) stop("Unknown Fleishman panel: ", panel)
  if (one$panel == 1) {
    return("Normal distribution\nskew = 0; excess kurtosis = 0")
  }
  sprintf(
    "F.P.\nskew = %s\nexcess kurtosis = %s",
    format(one$skew, trim = TRUE, scientific = FALSE),
    format(one$excess_kurtosis, trim = TRUE, scientific = FALSE)
  )
}

draw_fleishman_panel <- function(n, panel) {
  one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
  if (nrow(one) != 1) stop("Unknown Fleishman panel: ", panel)
  z <- stats::rnorm(n)
  one$a + one$b * z + one$c * z^2 + one$d * z^3
}

fleishman_density_panel <- function(x, panel) {
  one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
  if (nrow(one) != 1) stop("Unknown Fleishman panel: ", panel)
  if (one$panel == 1) {
    return(stats::dnorm(x))
  }
  vapply(x, function(xx) {
    roots <- polyroot(c(one$a - xx, one$b, one$c, one$d))
    real_roots <- Re(roots)[abs(Im(roots)) < 1e-8]
    if (!length(real_roots)) {
      return(0)
    }
    deriv <- one$b + 2 * one$c * real_roots + 3 * one$d * real_roots^2
    density <- stats::dnorm(real_roots) / abs(deriv)
    density <- density[is.finite(density)]
    if (!length(density)) 0 else sum(density)
  }, numeric(1))
}

fleishman_scaled_density <- function(x, panel, sd = 1, shift = 0) {
  fleishman_density_panel((x - shift) / sd, panel) / sd
}

fleishman_group_cols <- c(
  A = "#B79F00",
  B = "#56B4E9",
  C = "#009E73",
  D = "#D55E00"
)
