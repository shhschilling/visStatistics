# Test cases for vis_anova_assumptions function
# Save this as: tests/testthat/test-vis_anova_assumptions.R

library(testthat)

test_that("vis_anova_assumptions returns correct structure", {
  # Basic test with ToothGrowth data
  ToothGrowth$dose <- as.factor(ToothGrowth$dose)
  
  # Open a temporary graphics device for testing
  pdf(NULL)
  on.exit(dev.off())
  
  result <- vis_anova_assumptions(ToothGrowth$len, ToothGrowth$dose)
  
  # Check return structure
  expect_type(result, "list")
  expect_named(result, c("summary_anova", "shapiro_test", "ad_test", "levene_test", "bartlett_test"))
  
  # Check that statistical tests are included for ANOVA (regression = FALSE)
  expect_false(is.null(result$levene_test))
  expect_false(is.null(result$bartlett_test))
  
  # Check that summary_anova is correct type
  expect_s3_class(result$summary_anova, "summary.aov")
})

test_that("vis_anova_assumptions works with regression = TRUE", {
  ToothGrowth$dose <- as.factor(ToothGrowth$dose)
  
  pdf(NULL)
  on.exit(dev.off())
  
  result <- vis_anova_assumptions(ToothGrowth$len, ToothGrowth$dose, regression = TRUE)
  
  # Check that variance tests are skipped for regression
  expect_null(result$levene_test)
  expect_null(result$bartlett_test)
  
  # But other tests should still be present
  expect_false(is.null(result$shapiro_test))
  expect_false(is.null(result$ad_test))
})

test_that("vis_anova_assumptions handles small sample sizes", {
  # Create small dataset with adequate group sizes (n = 3 per group, total n = 9)
  small_data <- data.frame(
    y = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )
  
  pdf(NULL)
  on.exit(dev.off())
  
  result <- vis_anova_assumptions(small_data$y, small_data$group)
  
  # Shapiro test should work with n = 9
  expect_s3_class(result$shapiro_test, "htest")
  
  # Anderson-Darling should work for n >= 7
  expect_s3_class(result$ad_test, "htest")
})

test_that("vis_anova_assumptions handles very small sample sizes for Anderson-Darling", {
  # Create dataset with n = 6 (too small for Anderson-Darling but OK for others)
  small_data <- data.frame(
    y = c(1, 2, 3, 4, 5, 6),
    group = factor(rep(c("A", "B"), each = 3))
  )
  
  pdf(NULL)
  on.exit(dev.off())
  
  result <- vis_anova_assumptions(small_data$y, small_data$group)
  
  # Shapiro test should work
  expect_s3_class(result$shapiro_test, "htest")
  
  # Anderson-Darling should return character message
  expect_type(result$ad_test, "character")
  expect_match(result$ad_test, "Anderson-Darling test requires sample size of at least 7")
})

test_that("vis_anova_assumptions works with different confidence levels", {
  ToothGrowth$dose <- as.factor(ToothGrowth$dose)
  
  pdf(NULL)
  on.exit(dev.off())
  
  # Test different confidence levels
  result_90 <- vis_anova_assumptions(ToothGrowth$len, ToothGrowth$dose, conf.level = 0.90)
  result_99 <- vis_anova_assumptions(ToothGrowth$len, ToothGrowth$dose, conf.level = 0.99)
  
  # Results should have same structure regardless of confidence level
  expect_named(result_90, c("summary_anova", "shapiro_test", "ad_test", "levene_test", "bartlett_test"))
  expect_named(result_99, c("summary_anova", "shapiro_test", "ad_test", "levene_test", "bartlett_test"))
})

test_that("vis_anova_assumptions validates inputs", {
  ToothGrowth$dose <- as.factor(ToothGrowth$dose)
  
  pdf(NULL)
  on.exit(dev.off())
  
  # Test with automatic factor conversion
  result <- vis_anova_assumptions(ToothGrowth$len, as.character(ToothGrowth$dose))
  expect_type(result, "list")
})

test_that("vis_anova_assumptions works with perfectly normal data", {
  # Create perfectly normal data with equal variances
  set.seed(42)
  n_per_group <- 20
  normal_data <- data.frame(
    y = c(rnorm(n_per_group, mean = 0, sd = 1),
          rnorm(n_per_group, mean = 1, sd = 1),
          rnorm(n_per_group, mean = 2, sd = 1)),
    group = factor(rep(c("A", "B", "C"), each = n_per_group))
  )
  
  pdf(NULL)
  on.exit(dev.off())
  
  result <- vis_anova_assumptions(normal_data$y, normal_data$group)
  
  # All tests should complete successfully
  expect_s3_class(result$shapiro_test, "htest")
  expect_s3_class(result$ad_test, "htest")
  expect_s3_class(result$levene_test, "htest")
  expect_s3_class(result$bartlett_test, "htest")
})

test_that("vis_anova_assumptions works with unequal variances", {
  # Create data with clearly unequal variances
  set.seed(123)
  n_per_group <- 25
  unequal_var_data <- data.frame(
    y = c(rnorm(n_per_group, mean = 0, sd = 0.5),   # small variance
          rnorm(n_per_group, mean = 0, sd = 2),     # medium variance  
          rnorm(n_per_group, mean = 0, sd = 5)),    # large variance
    group = factor(rep(c("A", "B", "C"), each = n_per_group))
  )
  
  pdf(NULL)
  on.exit(dev.off())
  
  result <- vis_anova_assumptions(unequal_var_data$y, unequal_var_data$group)
  
  # Tests should complete successfully
  expect_s3_class(result$levene_test, "htest")
  expect_s3_class(result$bartlett_test, "htest")
  
  # p-values should be numeric
  expect_type(result$levene_test$p.value, "double")
  expect_type(result$bartlett_test$p.value, "double")
})

test_that("vis_anova_assumptions works with different cex values", {
  ToothGrowth$dose <- as.factor(ToothGrowth$dose)
  
  pdf(NULL)
  on.exit(dev.off())
  
  # Test different cex values (should not affect statistical results)
  result_small <- vis_anova_assumptions(ToothGrowth$len, ToothGrowth$dose, cex = 0.5)
  result_large <- vis_anova_assumptions(ToothGrowth$len, ToothGrowth$dose, cex = 1.5)
  
  # Statistical results should be identical regardless of cex
  expect_equal(result_small$shapiro_test$p.value, result_large$shapiro_test$p.value)
  expect_equal(result_small$levene_test$p.value, result_large$levene_test$p.value)
})
