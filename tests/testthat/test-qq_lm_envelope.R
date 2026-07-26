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

## The comparison against the reference STB implementation lives in
## inst/validation/qq_lm_envelope_vs_STB.R, because STB is not a declared
## dependency. See that file for how to run it.

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
