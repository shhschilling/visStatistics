## Fleishman distributions used for the Route 1 simulations.
##
## The non-normal cases follow Blanca et al. (2017), Table 2.  The
## polynomial is
##
##   Y = -c + b Z + c Z^2 + d Z^3,  Z ~ N(0, 1),
##
## with coefficients solving E(Y)=0, Var(Y)=1, skewness=gamma1, and
## excess kurtosis=gamma2.

fleishman_cases <- data.frame(
  case = 1:4,
  distribution = c("normal", rep("Fleishman", 3)),
  skew = c(0, 0.4, 1, 2),
  excess_kurtosis = c(0, 0.4, 1, 6),
  b = c(NA, 0.97364644778958, 1.01748518639308, 0.82632385707279),
  c = c(NA, 0.06398516961880, 0.19099508385631, 0.31374908535786),
  d = c(NA, 0.00736257635466, -0.01857699796906, 0.02270660515949)
)

fleishman_case <- function(skew, excess_kurtosis) {
  hit <- which(
    abs(fleishman_cases$skew - skew) < 1e-12 &
      abs(fleishman_cases$excess_kurtosis - excess_kurtosis) < 1e-12
  )
  if (length(hit) != 1) {
    stop("Unknown Fleishman case: skew=", skew,
         ", excess kurtosis=", excess_kurtosis)
  }
  fleishman_cases[hit, , drop = FALSE]
}

draw_fleishman_or_normal <- function(n, skew, excess_kurtosis) {
  case <- fleishman_case(skew, excess_kurtosis)
  z <- stats::rnorm(n)
  if (case$distribution == "normal") return(z)
  -case$c + case$b * z + case$c * z^2 + case$d * z^3
}

fleishman_density <- function(x, skew, excess_kurtosis) {
  case <- fleishman_case(skew, excess_kurtosis)
  if (case$distribution == "normal") return(stats::dnorm(x))
  b <- case$b
  cc <- case$c
  d <- case$d
  vapply(x, function(y) {
    roots <- polyroot(c(-cc - y, b, cc, d))
    real_roots <- Re(roots)[abs(Im(roots)) < 1e-7]
    if (!length(real_roots)) return(NA_real_)
    deriv <- b + 2 * cc * real_roots + 3 * d * real_roots^2
    dens <- stats::dnorm(real_roots) / abs(deriv)
    dens <- dens[is.finite(dens)]
    if (!length(dens)) NA_real_ else sum(dens)
  }, numeric(1))
}

fleishman_label <- function(skew, excess_kurtosis) {
  if (skew == 0 && excess_kurtosis == 0) {
    return("normal\nskew = 0\nexcess kurtosis = 0")
  }
  sprintf(
    "Fleishman\nskew = %s\nexcess kurtosis = %s",
    format(skew, trim = TRUE, scientific = FALSE),
    format(excess_kurtosis, trim = TRUE, scientific = FALSE)
  )
}

fleishman_title_line <- function(skew, excess_kurtosis) {
  if (skew == 0 && excess_kurtosis == 0) return("N(0, 1)")
  "Fleishman polynomial"
}
