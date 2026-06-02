library(testthat)

# Defends the conservative, group-count-aware bypass threshold used by the
# central-tendency router (50 per group for k <= 4, 100 per group for k > 4),
# following Delacre et al. (2019): with more than four groups under pronounced
# skewness, the Welch test needs ~100 per group to control the Type I error.
#
# Construction (from dev/welch_corner_example.R): k = 5 lognormal groups with
# EQUAL population means but rising variance and skew. For a lognormal,
# mean = exp(mu + sigma^2/2), so fixing mu_i = M - sigma_i^2/2 holds every group
# mean at exp(M) while spread and skew climb with sigma_i. Because all means are
# equal, EVERY rejection by a mean-based test (Welch's ANOVA) is a false
# positive, so the rejection rate IS the empirical Type I error -- it cannot be
# inflated by a built-in mean difference.
#
# Bradley's (1978) liberal robustness bound for nominal alpha = .05 is
# [.025, .075]. The test asserts the DIRECTION of the effect (robust to Monte
# Carlo noise), not a knife-edge point estimate.

test_that("k=5 skewed heteroscedastic groups: Welch Type I needs n > 100 (defends the gate)", {
  sdlog   <- c(A = 0.3, B = 0.5, C = 0.7, D = 0.9, E = 1.1)  # rising spread/skew
  M       <- 1                                               # every group mean = exp(M)
  meanlog <- M - sdlog^2 / 2                                 # equalise means

  welch_typeI <- function(n, nrep = 2000, alpha = 0.05) {
    grp <- factor(rep(names(n), times = n))
    mean(replicate(nrep, {
      val <- unlist(lapply(names(n),
        function(k) rlnorm(n[k], meanlog[k], sdlog[k])))
      oneway.test(val ~ grp, var.equal = FALSE)$p.value < alpha
    }))
  }

  set.seed(1)
  ti_50_100 <- welch_typeI(c(A = 55,  B = 65,  C = 75,  D = 85,  E = 95))
  ti_100    <- welch_typeI(c(A = 110, B = 130, C = 150, D = 170, E = 190))
  ti_500    <- welch_typeI(c(A = 510, B = 530, C = 550, D = 570, E = 590))

  bradley_upper <- 0.075

  # 1. Bypassing at >50 (the uniform rule) routes to a Welch ANOVA whose
  #    Type I error is OUTSIDE Bradley's bound for this corner case.
  expect_gt(ti_50_100, bradley_upper)

  # 2. Requiring >100 per group (the k > 4 rule) reduces the Type I error.
  expect_lt(ti_100, ti_50_100)

  # 3. The error keeps falling as the per-group size grows; by n > 500 it is
  #    back inside Bradley's bound.
  expect_lt(ti_500, ti_100)
  expect_lte(ti_500, bradley_upper)
})
