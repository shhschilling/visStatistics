library(testthat)

# Helper function to suppress graphics during tests to avoid opening windows
suppress_graphics <- function(code) {
  pdf(NULL)
  on.exit(if (dev.cur() > 1) dev.off())
  force(code)
}

# =============================================================================
# 1. TEST NUMERIC VS 2-LEVEL FACTOR (The main decision pipeline)
# =============================================================================

test_that("visstat_core - Numeric vs 2-level factor: Path and Diagnostics", {
  
  # 1.1: Large samples - parametric path
  set.seed(123)
  n_large <- 110
  large_df <- data.frame(
    val = c(rnorm(n_large, 10, 2), rnorm(n_large, 12, 2)),
    grp = factor(rep(c("G1", "G2"), each = n_large))
  )
  
  res_large <- suppress_graphics(visstat_core(large_df, "val", "grp"))
  
  # Verify it chose the parametric path
  expect_true("t-test-statistics" %in% names(res_large))
  
  # 1.2: Small samples with skewed residuals trigger Wilcoxon
  set.seed(123)
  skew_df <- data.frame(
    val = c(rexp(25, 0.1), rexp(25, 0.5)),
    grp = factor(rep(c("A", "B"), each = 25))
  )
  
  res_skew <- suppress_graphics(visstat_core(skew_df, "val", "grp"))
  
  # Verify it chose non-parametric path
  expect_true("statsWilcoxon" %in% names(res_skew))
})

# =============================================================================
# 2. TEST MULTI-LEVEL FACTOR (ANOVA/Kruskal)
# =============================================================================



# =============================================================================
# 3. TEST ERROR HANDLING (Original Logic Verification)
# =============================================================================

test_that("visstat_core - Error Handling for insufficient data", {
  
  # 3.1: Total samples < 3
  tiny_df <- data.frame(val = c(1, 2), grp = factor(c("A", "B")))
  res_err <- suppress_graphics(visstat_core(tiny_df, "val", "grp"))
  
  expect_equal(res_err$error, "Insufficient data")
  
  # 3.2: Empty factor levels
  empty_df <- data.frame(
    val = rnorm(10),
    grp = factor(rep("A", 10), levels = c("A", "B"))
  )
  res_empty <- suppress_graphics(visstat_core(empty_df, "val", "grp"))
  
  expect_equal(res_empty$error, "Insufficient data")
})

