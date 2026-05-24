# Tests for bp.test():
# verifies that the package implementation matches the formula stated in
# the vignette appendix (Section sec:bp, Equation eq:breusch-pagan-bp):
#
#   BP = N * R^2_aux,
#
# where R^2_aux is the coefficient of determination from regressing the
# squared model residuals e_i^2 on the fitted values yhat_i; the statistic
# is compared to chi^2(p - 1) where p is the number of fitted parameters
# of the original model (intercept + slope = 2 in simple linear
# regression, giving df = 1).

library(testthat)

# -----------------------------------------------------------------------------
# Structure
# -----------------------------------------------------------------------------

test_that("bp.test returns an htest object with the expected components", {
  set.seed(1)
  x <- runif(50, 0, 10)
  y <- 2 + 3 * x + rnorm(50, sd = 1)
  fit <- lm(y ~ x)
  res <- bp.test(fit)

  expect_s3_class(res, "htest")
  expect_true(all(c("statistic", "parameter", "p.value",
                    "method", "data.name") %in% names(res)))
  expect_named(res$statistic, "BP")
  expect_named(res$parameter, "df")
  expect_equal(unname(res$parameter), 1L)
})

test_that("bp.test rejects non-lm input", {
  expect_error(bp.test("not a model"), "must be an object of class 'lm'")
})

# -----------------------------------------------------------------------------
# Mathematical consistency with the vignette formula
# -----------------------------------------------------------------------------

test_that("BP statistic equals N * R^2 from the auxiliary regression", {
  set.seed(42)
  n <- 100
  x <- runif(n, 0, 10)
  y <- 2 + 3 * x + rnorm(n, sd = 1)
  fit <- lm(y ~ x)

  res <- bp.test(fit)

  # Manual computation per Equation eq:breusch-pagan-bp.
  e2     <- residuals(fit)^2
  yhat   <- fitted(fit)
  aux    <- lm(e2 ~ yhat)
  r2_aux <- summary(aux)$r.squared

  bp_manual <- n * r2_aux
  df_manual <- length(coef(fit)) - 1L
  p_manual  <- pchisq(bp_manual, df = df_manual, lower.tail = FALSE)

  expect_equal(unname(res$statistic), bp_manual)
  expect_equal(unname(res$parameter), df_manual)
  expect_equal(res$p.value, p_manual)
})

# -----------------------------------------------------------------------------
# Behaviour: rejection pattern
# -----------------------------------------------------------------------------

test_that("homoscedastic case gives larger p-value than heteroscedastic case", {
  # Robust relative check: avoids spurious Type I errors that any single seed
  # can produce when testing absolute thresholds on a stochastic null.
  set.seed(123)
  n <- 200
  x <- runif(n, 0, 10)
  y_homo <- 2 + 3 * x + rnorm(n, sd = 1)
  y_het  <- 2 + 3 * x + rnorm(n, sd = 0.5 + 0.5 * x)

  p_homo <- bp.test(lm(y_homo ~ x))$p.value
  p_het  <- bp.test(lm(y_het ~ x))$p.value

  expect_gt(p_homo, p_het)
})

test_that("heteroscedastic data (sd grows with x) rejects H0", {
  set.seed(456)
  n <- 200
  x <- runif(n, 0, 10)
  y <- 2 + 3 * x + rnorm(n, sd = 0.5 + 0.5 * x)
  fit <- lm(y ~ x)
  res <- bp.test(fit)
  expect_lt(res$p.value, 0.05)
})
