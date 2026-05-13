# Validate the Shapiro-Wilk W statistic definition used in the manuscript.
# This is a standalone numerical check, not a package test.

normal_order_moments <- function(n) {
  order_density <- function(i, z) {
    exp(
      lgamma(n + 1) - lgamma(i) - lgamma(n - i + 1) +
        (i - 1) * pnorm(z, log.p = TRUE) +
        (n - i) * pnorm(z, lower.tail = FALSE, log.p = TRUE) +
        dnorm(z, log = TRUE)
    )
  }

  m <- numeric(n)
  ez2 <- numeric(n)

  for (i in seq_len(n)) {
    m[i] <- integrate(
      function(z) z * order_density(i, z),
      lower = -Inf, upper = Inf, rel.tol = 1e-9
    )$value

    ez2[i] <- integrate(
      function(z) z^2 * order_density(i, z),
      lower = -Inf, upper = Inf, rel.tol = 1e-9
    )$value
  }

  ezz <- matrix(NA_real_, n, n)
  diag(ezz) <- ez2

  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      const <- exp(
        lgamma(n + 1) - lgamma(i) - lgamma(j - i) -
          lgamma(n - j + 1)
      )

      moment_for_y <- function(y) {
        fy <- dnorm(y)
        fy_cdf <- pnorm(y)

        integrate(
          function(x) {
            fx_cdf <- pnorm(x)
            const * x * y *
              fx_cdf^(i - 1) *
              (fy_cdf - fx_cdf)^(j - i - 1) *
              (1 - fy_cdf)^(n - j) *
              dnorm(x) * fy
          },
          lower = -Inf, upper = y, rel.tol = 1e-7
        )$value
      }

      ezz[i, j] <- integrate(
        Vectorize(moment_for_y),
        lower = -Inf, upper = Inf, rel.tol = 1e-6
      )$value
      ezz[j, i] <- ezz[i, j]
    }
  }

  list(m = m, V = ezz - tcrossprod(m))
}

shapiro_w_from_definition <- function(x) {
  x <- sort(x[is.finite(x)])
  n <- length(x)

  if (n < 3) {
    stop("Shapiro-Wilk requires at least three finite observations.")
  }
  if (sum((x - mean(x))^2) == 0) {
    stop("All observations are identical.")
  }

  moments <- normal_order_moments(n)
  v_inv_m <- solve(moments$V, moments$m)
  a <- as.vector(v_inv_m / sqrt(sum(v_inv_m^2)))

  sum(a * x)^2 / sum((x - mean(x))^2)
}

validate_shapiro_w_definition <- function(tolerance = 5e-4) {
  samples <- list(
    c(-1.2, 0.4, 2.1),
    c(-1.0, 0.2, 0.9, 1.7),
    c(-2.0, -0.5, 0.1, 0.8, 2.4),
    c(-1.8, -0.7, -0.1, 0.3, 1.1, 2.0),
    c(-2.2, -1.4, -0.2, 0.1, 0.5, 1.6, 2.1),
    c(-2.5, -1.7, -0.8, -0.1, 0.4, 0.9, 1.4, 2.3)
  )

  results <- data.frame(
    n = integer(),
    w_definition = numeric(),
    w_shapiro_test = numeric(),
    absolute_difference = numeric()
  )

  for (x in samples) {
    w_definition <- shapiro_w_from_definition(x)
    w_shapiro_test <- unname(stats::shapiro.test(x)$statistic)

    results <- rbind(
      results,
      data.frame(
        n = length(x),
        w_definition = w_definition,
        w_shapiro_test = w_shapiro_test,
        absolute_difference = abs(w_definition - w_shapiro_test)
      )
    )
  }

  if (any(results$absolute_difference > tolerance)) {
    print(results, digits = 12)
    stop("Definition-based W does not match shapiro.test() within tolerance.")
  }

  results
}

if (identical(environment(), globalenv())) {
  print(validate_shapiro_w_definition(), digits = 12)
}
