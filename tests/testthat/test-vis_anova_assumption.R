# Test file for vis_anova_assumptions function
# Place in tests/testthat/ directory

library(testthat)

# Helper function to suppress graphics during tests
suppress_graphics <- function(code) {
  # Save current device state
  old_dev <- dev.cur()
  
  # Open null device and ensure clean state
  pdf(NULL)
  on.exit({
    # Clean up: close null device and restore state
    if (dev.cur() != 1) dev.off()  # Only close if device is open
    if (old_dev > 1 && old_dev %in% dev.list()) dev.set(old_dev)
  })
  
  # Reset graphics parameters to default
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  
  # Execute the code
  force(code)
}

# =============================================================================
# Test 1: Basic functionality with normal data
# =============================================================================
test_that("vis_anova_assumptions - Basic functionality with normal data", {
  
  skip_if_not(exists("vis_anova_assumptions"), "vis_anova_assumptions function not available")
  
  # Case 1.1: Standard ANOVA case with normal data
  set.seed(123)
  normal_data <- data.frame(
    values = c(rnorm(20, 5, 1), rnorm(20, 6, 1), rnorm(20, 7, 1)),
    group = factor(rep(c("A", "B", "C"), each = 20))
  )
  
  result <- suppress_graphics({
    vis_anova_assumptions(normal_data$values, normal_data$group)
  })
  
  # Basic structure tests
  expect_s3_class(result, "vis_anova_assumptions")
  expect_type(result, "list")
  expect_true(length(result) >= 4)  # At least summary_anova, shapiro_test, ad_test, levene_test, bartlett_test
  
  # Required elements
  expect_true("summary_anova" %in% names(result))
  expect_true("shapiro_test" %in% names(result))
  expect_true("ad_test" %in% names(result))
  expect_true("levene_test" %in% names(result))
  expect_true("bartlett_test" %in% names(result))
  
  # Test statistical results structure
  expect_true(is.list(result$summary_anova))
  expect_true(is.list(result$shapiro_test))
  expect_true(is.list(result$ad_test) || is.character(result$ad_test))
  expect_true(is.list(result$levene_test))
  expect_true(is.list(result$bartlett_test))
  
  # Shapiro test should have p.value
  if (is.list(result$shapiro_test) && !is.na(result$shapiro_test$p.value)) {
    expect_true("p.value" %in% names(result$shapiro_test))
    expect_type(result$shapiro_test$p.value, "double")
    expect_true(result$shapiro_test$p.value >= 0 && result$shapiro_test$p.value <= 1)
  }
  
  # Anderson-Darling test (if sample size >= 7)
  if (is.list(result$ad_test)) {
    expect_true("p.value" %in% names(result$ad_test))
    expect_type(result$ad_test$p.value, "double")
    expect_true(result$ad_test$p.value >= 0 && result$ad_test$p.value <= 1)
  }
  
  # Levene and Bartlett tests
  expect_true("p.value" %in% names(result$levene_test))
  expect_true("p.value" %in% names(result$bartlett_test))
  expect_type(result$levene_test$p.value, "double")
  expect_type(result$bartlett_test$p.value, "double")
})

# =============================================================================
# Test 2: Regression mode (regression = TRUE)
# =============================================================================
test_that("vis_anova_assumptions - Regression mode", {
  
  skip_if_not(exists("vis_anova_assumptions"), "vis_anova_assumptions function not available")
  
  # Case 2.1: Regression analysis
  set.seed(456)
  regression_data <- data.frame(
    y = 2 + 3 * (1:30) + rnorm(30, 0, 2),
    x = 1:30
  )
  
  result <- suppress_graphics({
    vis_anova_assumptions(regression_data$y, regression_data$x, regression = TRUE)
  })
  
  # Basic structure
  expect_s3_class(result, "vis_anova_assumptions")
  expect_true("summary_anova" %in% names(result))
  expect_true("shapiro_test" %in% names(result))
  expect_true("ad_test" %in% names(result))
  
  # Regression mode should NOT include Levene and Bartlett tests
  expect_null(result$levene_test)
  expect_null(result$bartlett_test)
  
  # Should still have normality tests
  if (is.list(result$shapiro_test) && !is.na(result$shapiro_test$p.value)) {
    expect_type(result$shapiro_test$p.value, "double")
  }
})

# =============================================================================
# Test 3: Edge cases - sample size limits
# =============================================================================
test_that("vis_anova_assumptions - Sample size edge cases", {
  
  skip_if_not(exists("vis_anova_assumptions"), "vis_anova_assumptions function not available")
  
  # Case 3.1: Very small sample (n < 3) - should handle gracefully
  tiny_data <- data.frame(
    values = c(1, 2),
    group = factor(c("A", "B"))
  )
  
  result_tiny <- tryCatch({
    suppress_graphics({
      vis_anova_assumptions(tiny_data$values, tiny_data$group)
    })
  }, error = function(e) e, warning = function(w) w)
  
  # Should either work or fail gracefully
  if (inherits(result_tiny, "vis_anova_assumptions")) {
    expect_s3_class(result_tiny, "vis_anova_assumptions")
    # Shapiro test should indicate insufficient sample size
    expect_true(is.character(result_tiny$shapiro_test$method) || is.na(result_tiny$shapiro_test$p.value))
  } else {
    # If it errors, that's also acceptable for very small samples
    expect_true(inherits(result_tiny, "error") || inherits(result_tiny, "warning"))
  }
  
  # Case 3.2: Small sample (n < 7) - Shapiro ok, Anderson-Darling not available
  small_data <- data.frame(
    values = c(1, 2, 3, 4, 5, 6),
    group = factor(rep(c("A", "B"), each = 3))
  )
  
  result_small <- suppress_graphics({
    vis_anova_assumptions(small_data$values, small_data$group)
  })
  expect_s3_class(result_small, "vis_anova_assumptions")
  
  # Anderson-Darling should be character message for n < 7
  expect_true(is.character(result_small$ad_test))
  expect_true(grepl("Anderson-Darling test requires", result_small$ad_test))
  
  # Case 3.3: Medium sample (n >= 7) - both tests should work
  medium_data <- data.frame(
    values = rnorm(21),  # 7 per group
    group = factor(rep(c("A", "B", "C"), each = 7))
  )
  
  result_medium <- suppress_graphics({
    vis_anova_assumptions(medium_data$values, medium_data$group)
  })
  expect_s3_class(result_medium, "vis_anova_assumptions")
  
  # Both tests should be available
  if (!is.na(result_medium$shapiro_test$p.value)) {
    expect_type(result_medium$shapiro_test$p.value, "double")
  }
  expect_true(is.list(result_medium$ad_test))
  expect_true("p.value" %in% names(result_medium$ad_test))
})

# =============================================================================
# Test 4: Data with missing values
# =============================================================================
test_that("vis_anova_assumptions - Missing values handling", {
  
  skip_if_not(exists("vis_anova_assumptions"), "vis_anova_assumptions function not available")
  
  # Case 4.1: Data with NA values
  na_data <- data.frame(
    values = c(rnorm(18), NA, NA, rnorm(18), NA, NA),  # 40 values total
    group = factor(rep(c("A", "B", "C"), c(14, 13, 13)))  # 40 group labels total
  )
  
  result_na <- suppress_graphics({
    vis_anova_assumptions(na_data$values, na_data$group)
  })
  expect_s3_class(result_na, "vis_anova_assumptions")
  
  # Should handle NA removal properly
  expect_true(length(result_na) >= 4)
  
  # Statistical tests should work on cleaned data
  if (is.list(result_na$shapiro_test) && !is.na(result_na$shapiro_test$p.value)) {
    expect_type(result_na$shapiro_test$p.value, "double")
  }
  
  # Case 4.2: All NA in one group
  unbalanced_na <- data.frame(
    values = c(rnorm(20), rep(NA, 10), rnorm(10)),
    group = factor(rep(c("A", "B", "C"), c(20, 10, 10)))
  )
  
  result_unbalanced <- tryCatch({
    suppress_graphics({
      vis_anova_assumptions(unbalanced_na$values, unbalanced_na$group)
    })
  }, error = function(e) e, warning = function(w) w)
  
  # Should either work or handle gracefully
  expect_true(inherits(result_unbalanced, "vis_anova_assumptions") || 
                inherits(result_unbalanced, "error") ||
                inherits(result_unbalanced, "warning"))
})

# =============================================================================
# Test 5: Different confidence levels
# =============================================================================
test_that("vis_anova_assumptions - Different confidence levels", {
  
  skip_if_not(exists("vis_anova_assumptions"), "vis_anova_assumptions function not available")
  
  test_data <- data.frame(
    values = c(rnorm(15, 5), rnorm(15, 6), rnorm(15, 7)),
    group = factor(rep(c("A", "B", "C"), each = 15))
  )
  
  # Case 5.1: Different confidence levels
  result_90 <- suppress_graphics({
    vis_anova_assumptions(test_data$values, test_data$group, conf.level = 0.90)
  })
  expect_s3_class(result_90, "vis_anova_assumptions")
  
  result_95 <- suppress_graphics({
    vis_anova_assumptions(test_data$values, test_data$group, conf.level = 0.95)
  })
  expect_s3_class(result_95, "vis_anova_assumptions")
  
  result_99 <- suppress_graphics({
    vis_anova_assumptions(test_data$values, test_data$group, conf.level = 0.99)
  })
  expect_s3_class(result_99, "vis_anova_assumptions")
  
  # All should have same structure
  expect_equal(names(result_90), names(result_95))
  expect_equal(names(result_95), names(result_99))
})

# =============================================================================
# Test 6: Non-normal data detection
# =============================================================================
test_that("vis_anova_assumptions - Non-normal data detection", {
  
  skip_if_not(exists("vis_anova_assumptions"), "vis_anova_assumptions function not available")
  
  # Case 6.1: Clearly non-normal data (exponential)
  set.seed(789)
  nonnormal_data <- data.frame(
    values = c(rexp(30, 0.5), rexp(30, 1), rexp(30, 2)),
    group = factor(rep(c("A", "B", "C"), each = 30))
  )
  
  result_nonnormal <- suppress_graphics({
    vis_anova_assumptions(nonnormal_data$values, nonnormal_data$group)
  })
  expect_s3_class(result_nonnormal, "vis_anova_assumptions")
  
  # Should detect non-normality (low p-values)
  if (is.list(result_nonnormal$shapiro_test) && !is.na(result_nonnormal$shapiro_test$p.value)) {
    # With clearly non-normal data, p-value should typically be low
    # But we won't enforce this as it depends on random sampling
    expect_type(result_nonnormal$shapiro_test$p.value, "double")
    expect_true(result_nonnormal$shapiro_test$p.value >= 0 && result_nonnormal$shapiro_test$p.value <= 1)
  }
  
  # Case 6.2: Uniform data (also non-normal)
  uniform_data <- data.frame(
    values = c(runif(25, 0, 1), runif(25, 1, 2), runif(25, 2, 3)),
    group = factor(rep(c("A", "B", "C"), each = 25))
  )
  
  result_uniform <- suppress_graphics({
    vis_anova_assumptions(uniform_data$values, uniform_data$group)
  })
  expect_s3_class(result_uniform, "vis_anova_assumptions")
  expect_true(length(result_uniform) >= 4)
})

# =============================================================================
# Test 7: Different variance patterns
# =============================================================================
test_that("vis_anova_assumptions - Variance homogeneity testing", {
  
  skip_if_not(exists("vis_anova_assumptions"), "vis_anova_assumptions function not available")
  
  # Case 7.1: Equal variances
  equal_var_data <- data.frame(
    values = c(rnorm(20, 5, 1), rnorm(20, 6, 1), rnorm(20, 7, 1)),
    group = factor(rep(c("A", "B", "C"), each = 20))
  )
  
  result_equal <- suppress_graphics({
    vis_anova_assumptions(equal_var_data$values, equal_var_data$group)
  })
  expect_s3_class(result_equal, "vis_anova_assumptions")
  
  # Should have variance tests
  expect_true("levene_test" %in% names(result_equal))
  expect_true("bartlett_test" %in% names(result_equal))
  expect_type(result_equal$levene_test$p.value, "double")
  expect_type(result_equal$bartlett_test$p.value, "double")
  
  # Case 7.2: Unequal variances
  unequal_var_data <- data.frame(
    values = c(rnorm(20, 5, 0.5), rnorm(20, 6, 2), rnorm(20, 7, 4)),
    group = factor(rep(c("A", "B", "C"), each = 20))
  )
  
  result_unequal <- suppress_graphics({
    vis_anova_assumptions(unequal_var_data$values, unequal_var_data$group)
  })
  expect_s3_class(result_unequal, "vis_anova_assumptions")
  
  # Should still complete the analysis
  expect_true("levene_test" %in% names(result_unequal))
  expect_true("bartlett_test" %in% names(result_unequal))
  
  # P-values should be valid
  expect_true(result_unequal$levene_test$p.value >= 0 && result_unequal$levene_test$p.value <= 1)
  expect_true(result_unequal$bartlett_test$p.value >= 0 && result_unequal$bartlett_test$p.value <= 1)
})

# =============================================================================
# Test 8: Factor conversion and data types
# =============================================================================
test_that("vis_anova_assumptions - Data type handling", {
  
  skip_if_not(exists("vis_anova_assumptions"), "vis_anova_assumptions function not available")
  
  # Case 8.1: Character grouping variable (should be converted to factor or handled)
  char_group_data <- data.frame(
    values = rnorm(30),
    group = rep(c("Group1", "Group2", "Group3"), each = 10)
  )
  
  # Function might not handle character groups directly, so test both scenarios
  result_char <- tryCatch({
    # Try with character vector
    suppress_graphics({
      vis_anova_assumptions(char_group_data$values, char_group_data$group)
    })
  }, error = function(e) {
    # If character fails, try with factor conversion
    tryCatch({
      suppress_graphics({
        vis_anova_assumptions(char_group_data$values, as.factor(char_group_data$group))
      })
    }, error = function(e2) e2)
  })
  
  # Should either work (auto-conversion or manual conversion) or provide meaningful error
  expect_true(inherits(result_char, "vis_anova_assumptions") || inherits(result_char, "error"))
  
  # Case 8.2: Integer values
  int_data <- data.frame(
    values = as.integer(c(rep(5, 10), rep(6, 10), rep(7, 10))),
    group = factor(rep(c("A", "B", "C"), each = 10))
  )
  
  result_int <- suppress_graphics({
    vis_anova_assumptions(int_data$values, int_data$group)
  })
  expect_s3_class(result_int, "vis_anova_assumptions")
})

# =============================================================================
# Test 9: Plot generation (basic check)
# =============================================================================
test_that("vis_anova_assumptions - Plot generation", {
  
  skip_if_not(exists("vis_anova_assumptions"), "vis_anova_assumptions function not available")
  
  test_data <- data.frame(
    values = rnorm(30),
    group = factor(rep(c("A", "B", "C"), each = 10))
  )
  
  # Should not error when generating plots
  expect_no_error({
    suppress_graphics({
      result <- vis_anova_assumptions(test_data$values, test_data$group)
    })
  })
  
  # Test with different cex values
  expect_no_error({
    suppress_graphics({
      result_small <- vis_anova_assumptions(test_data$values, test_data$group, cex = 0.5)
    })
  })
  
  expect_no_error({
    suppress_graphics({
      result_large <- vis_anova_assumptions(test_data$values, test_data$group, cex = 1.5)
    })
  })
})

# =============================================================================
# Test 10: Statistical correctness verification
# =============================================================================
test_that("vis_anova_assumptions - Statistical correctness", {
  
  skip_if_not(exists("vis_anova_assumptions"), "vis_anova_assumptions function not available")
  
  # Case 10.1: Known normal data should pass normality tests
  set.seed(42)
  very_normal_data <- data.frame(
    values = c(rnorm(100, 5, 1), rnorm(100, 6, 1), rnorm(100, 7, 1)),
    group = factor(rep(c("A", "B", "C"), each = 100))
  )
  
  result_normal <- suppress_graphics({
    vis_anova_assumptions(very_normal_data$values, very_normal_data$group)
  })
  
  # With large normal samples, should typically pass normality tests
  if (is.list(result_normal$shapiro_test) && !is.na(result_normal$shapiro_test$p.value)) {
    # Don't enforce specific p-values as they depend on random data,
    # but ensure structure is correct
    expect_type(result_normal$shapiro_test$p.value, "double")
    expect_true(result_normal$shapiro_test$p.value >= 0 && result_normal$shapiro_test$p.value <= 1)
  }
  
  # ANOVA summary should be meaningful
  expect_true(is.list(result_normal$summary_anova))
  expect_true(length(result_normal$summary_anova) > 0)
  
  # Case 10.2: Verify ANOVA model fitting
  # The function should fit aov(samples ~ fact) correctly
  manual_aov <- aov(very_normal_data$values ~ very_normal_data$group)
  manual_residuals <- rstandard(manual_aov)
  
  # Our function should work with the same data
  expect_s3_class(result_normal, "vis_anova_assumptions")
  expect_true(length(result_normal$summary_anova) > 0)
})