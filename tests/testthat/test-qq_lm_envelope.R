test_that("qq_lm_envelope returns inspectable simulated bands", {
  set.seed(20260605)
  fit <- lm(mpg ~ wt, data = mtcars)
  env <- qq_lm_envelope(fit, nsim = 99)
  n <- length(stats::rstandard(fit))

  expect_s3_class(env, "qq_lm_envelope")
  expect_equal(length(env$expected), n)
  expect_equal(length(env$observed), n)
  expect_equal(dim(env$sim_orders), c(99L, n))
  expect_equal(dim(env$pointwise), c(2L, n))
  expect_equal(dim(env$global), c(2L, n))
  expect_true(all(env$global[1, ] <= env$pointwise[1, ]))
  expect_true(all(env$global[2, ] >= env$pointwise[2, ]))
  expect_true(env$global_coverage >= env$conf.level)
})

test_that("qq_lm_envelope simultaneous band agrees with STB", {
  skip_if_not_installed("STB")

  set.seed(20260605)
  fit <- lm(mpg ~ wt, data = mtcars)
  env <- qq_lm_envelope(fit, nsim = 999, tol = 1e-4)
  stb <- STB::getSTB(
    env$sim_orders,
    alpha = 1 - env$conf.level,
    tol = 1e-4,
    q.type = env$q.type,
    output = FALSE,
    timer = FALSE,
    Ncpu = 1
  )

  expect_equal(env$global_coverage, stb$coverage, tolerance = 1e-12)
  expect_equal(as.numeric(env$global), as.numeric(stb$Q), tolerance = 1e-12)
})

test_that("qq_lm_envelope bands widen with higher confidence level", {
  fit <- lm(mpg ~ wt, data = mtcars)

  set.seed(20260605)
  env90 <- qq_lm_envelope(fit, conf.level = 0.90, nsim = 499)
  set.seed(20260605)
  env95 <- qq_lm_envelope(fit, conf.level = 0.95, nsim = 499)

  width90 <- env90$global[2, ] - env90$global[1, ]
  width95 <- env95$global[2, ] - env95$global[1, ]
  point90 <- env90$pointwise[2, ] - env90$pointwise[1, ]
  point95 <- env95$pointwise[2, ] - env95$pointwise[1, ]

  expect_true(all(width95 >= width90))
  expect_true(all(point95 >= point90))
})
