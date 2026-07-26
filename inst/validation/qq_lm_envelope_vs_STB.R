# Validation of qq_lm_envelope() against the reference implementation in the
# STB package (Schuetzenmeister et al. 2012).
#
# This lives outside tests/ on purpose. It depends on STB, which is not a
# declared dependency of visStatistics: the comparison is a development-time
# check of the package's own implementation, not something CRAN needs to run
# on every machine.
#
# Run manually with STB installed:
#
#   testthat::test_file(
#     system.file("validation", "qq_lm_envelope_vs_STB.R",
#                 package = "visStatistics")
#   )
#
# or, from a source checkout:
#
#   devtools::load_all(".")
#   testthat::test_file("inst/validation/qq_lm_envelope_vs_STB.R")

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
