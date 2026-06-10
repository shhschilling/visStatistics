#' Simulated Q-Q envelopes for linear model residuals
#'
#' Computes pointwise and simultaneous Q-Q envelopes for internally
#' studentised residuals from an unweighted \code{lm} or \code{aov} object by
#' repeatedly simulating responses from the fitted normal-error model and
#' refitting the model. The simultaneous envelope follows the Monte Carlo
#' tolerance-band idea of Schuetzenmeister et al. (2012,
#' doi:10.1080/03610918.2011.582560).
#'
#' @param model An unweighted \code{lm} or \code{aov} object.
#' @param conf.level Numeric confidence level for the envelopes.
#' @param nsim Integer number of simulated refits.
#' @param q.type Integer quantile type passed to \code{stats::quantile()}.
#' @param tol Numeric tolerance for the simultaneous-band coverage search.
#' @param max.iter Integer maximum number of bisection iterations.
#'
#' @return A list of class \code{qq_lm_envelope} with the observed sorted
#'   residuals, theoretical quantiles, pointwise bounds, simultaneous bounds,
#'   the simulated sorted residual matrix, and the achieved simultaneous
#'   coverage.
#'
#' @examples
#' fit <- lm(mpg ~ wt, data = mtcars)
#' env <- qq_lm_envelope(fit, nsim = 100)
#' str(env)
#'
#' @export
qq_lm_envelope <- function(model, conf.level = 0.95,
                           nsim = getOption("visStatistics.qq_nsim", 5000L),
                           q.type = 2L, tol = 1e-4, max.iter = 100L) {
  if (!inherits(model, "lm")) {
    stop("`model` must inherit from \"lm\".", call. = FALSE)
  }
  if (!is.null(stats::weights(model))) {
    stop("`qq_lm_envelope()` currently supports unweighted fits only.",
         call. = FALSE)
  }
  if (!is.numeric(conf.level) || length(conf.level) != 1L ||
      !is.finite(conf.level) || conf.level <= 0 || conf.level >= 1) {
    stop("`conf.level` must be one number between 0 and 1.", call. = FALSE)
  }

  nsim <- as.integer(nsim)
  if (is.na(nsim) || nsim < 20L) {
    stop("`nsim` must be at least 20.", call. = FALSE)
  }
  q.type <- as.integer(q.type)
  if (is.na(q.type) || !(q.type %in% 1:9)) {
    stop("`q.type` must be an integer from 1 to 9.", call. = FALSE)
  }
  max.iter <- as.integer(max.iter)
  if (is.na(max.iter) || max.iter < 1L) {
    stop("`max.iter` must be at least 1.", call. = FALSE)
  }
  if (!is.numeric(tol) || length(tol) != 1L || !is.finite(tol) || tol <= 0) {
    stop("`tol` must be one positive number.", call. = FALSE)
  }

  observed <- sort(stats::rstandard(model))
  if (any(!is.finite(observed))) {
    stop("The model has non-finite standardised residuals.", call. = FALSE)
  }

  sim_orders <- .qq_lm_simulated_rstandard_orders(model, nsim = nsim)
  bands <- .qq_lm_bands(
    sim_orders = sim_orders,
    conf.level = conf.level,
    tol = tol,
    max.iter = max.iter,
    q.type = q.type
  )

  result <- c(
    list(
      expected = stats::qnorm(stats::ppoints(length(observed))),
      observed = observed,
      sim_orders = sim_orders,
      conf.level = conf.level,
      nsim = nsim,
      q.type = q.type
    ),
    bands
  )
  class(result) <- "qq_lm_envelope"
  result
}

.qq_lm_simulated_rstandard_orders <- function(model, nsim) {
  x <- stats::model.matrix(model)
  fitted_values <- stats::fitted(model)
  residual_scale <- stats::sigma(model)
  hat_values <- stats::hatvalues(model)
  df_residual <- stats::df.residual(model)
  n <- length(fitted_values)

  if (!is.finite(residual_scale) || residual_scale <= 0 ||
      is.na(df_residual) || df_residual <= 0) {
    stop("The model has no positive residual degrees of freedom.",
         call. = FALSE)
  }
  if (any(!is.finite(hat_values)) || any(hat_values >= 1)) {
    stop("The model has hat values greater than or equal to 1.", call. = FALSE)
  }

  denom_factor <- sqrt(1 - hat_values)
  sim_orders <- matrix(NA_real_, nrow = nsim, ncol = n)

  for (i in seq_len(nsim)) {
    y_sim <- fitted_values + stats::rnorm(n, sd = residual_scale)
    fit_sim <- stats::lm.fit(x = x, y = y_sim)
    sigma_sim <- sqrt(sum(fit_sim$residuals^2) / df_residual)
    if (!is.finite(sigma_sim) || sigma_sim <= 0) {
      stop("A simulated refit has no positive residual scale.", call. = FALSE)
    }
    sim_orders[i, ] <- sort(fit_sim$residuals / (sigma_sim * denom_factor))
  }

  sim_orders
}

.qq_lm_coverage <- function(sim_orders, bounds) {
  inside <- sweep(sim_orders, 2, bounds[1, ], `>=`) &
    sweep(sim_orders, 2, bounds[2, ], `<=`)
  mean(rowSums(inside) == ncol(sim_orders))
}

.qq_lm_order_quantiles <- function(sim_orders, local_alpha, q.type = 2L) {
  apply(
    sim_orders, 2, stats::quantile,
    probs = c(local_alpha / 2, 1 - local_alpha / 2),
    type = q.type,
    names = FALSE
  )
}

.qq_lm_bands <- function(sim_orders, conf.level, tol, max.iter, q.type) {
  alpha <- 1 - conf.level
  pointwise <- .qq_lm_order_quantiles(sim_orders, alpha, q.type = q.type)

  lower_alpha <- 0
  upper_alpha <- alpha
  best_alpha <- lower_alpha
  best_bounds <- .qq_lm_order_quantiles(sim_orders, best_alpha, q.type = q.type)
  best_coverage <- .qq_lm_coverage(sim_orders, best_bounds)

  for (i in seq_len(max.iter)) {
    local_alpha <- (lower_alpha + upper_alpha) / 2
    bounds <- .qq_lm_order_quantiles(sim_orders, local_alpha, q.type = q.type)
    coverage <- .qq_lm_coverage(sim_orders, bounds)

    if (coverage >= conf.level) {
      best_alpha <- local_alpha
      best_bounds <- bounds
      best_coverage <- coverage
      lower_alpha <- local_alpha
    } else {
      upper_alpha <- local_alpha
    }

    if (abs(coverage - conf.level) <= tol) break
  }

  list(
    pointwise = pointwise,
    global = best_bounds,
    global_coverage = best_coverage,
    global_local_alpha = best_alpha
  )
}
