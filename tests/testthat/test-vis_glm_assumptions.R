test_that("vis_glm_assumptions returns correct structure for ANOVA", {
  # Setup test data
  ToothGrowth$dose <- as.factor(ToothGrowth$dose)
  
  # Run function
  result <- vis_glm_assumptions(ToothGrowth$len, ToothGrowth$dose)
  
  # Test structure - should have 6 elements
  expect_named(result, c("summary_anova", "shapiro_test", "ad_test", 
                         "levene_test", "bartlett_test", "bp_test"))
  
  # For ANOVA (regression=FALSE): levene and bartlett exist, bp_test is NULL
  expect_s3_class(result$levene_test, "htest")
  expect_s3_class(result$bartlett_test, "htest")
  expect_null(result$bp_test)
  
  # Check class of summary_anova
  expect_s3_class(result$summary_anova, "summary.aov")
  
  # Check normality tests
  expect_s3_class(result$shapiro_test, "htest")
  expect_true(is.list(result$ad_test) || is.character(result$ad_test))
})

test_that("vis_glm_assumptions returns correct structure for regression", {
  # Run function with regression=TRUE
  result <- vis_glm_assumptions(mtcars$mpg, as.factor(mtcars$cyl), regression = TRUE)
  
  # Test structure - should have 6 elements
  expect_named(result, c("summary_anova", "shapiro_test", "ad_test", 
                         "levene_test", "bartlett_test", "bp_test"))
  
  # For regression (regression=TRUE): bp_test exists, levene and bartlett are NULL
  expect_null(result$levene_test)
  expect_null(result$bartlett_test)
  expect_s3_class(result$bp_test, "htest")
  
  # Check that bp_test has correct structure
  expect_named(result$bp_test, c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(result$bp_test$method, "Breusch-Pagan test for heteroscedasticity")
  
  # Check class of summary_anova (aov is used even for regression in this function)
  expect_s3_class(result$summary_anova, "summary.aov")
  
  # Check normality tests
  expect_s3_class(result$shapiro_test, "htest")
  expect_true(is.list(result$ad_test) || is.character(result$ad_test))
})

test_that("vis_glm_assumptions handles small samples correctly", {
  # Create small sample (n < 7)
  small_sample <- c(1, 2, 3, 4, 5)
  small_groups <- factor(c(1, 1, 2, 2, 2))
  
  result <- vis_glm_assumptions(small_sample, small_groups)
  
  # Should still return structure but with warnings for small n
  expect_named(result, c("summary_anova", "shapiro_test", "ad_test", 
                         "levene_test", "bartlett_test", "bp_test"))
  
  # Anderson-Darling should be character message for n < 7
  expect_type(result$ad_test, "character")
})

test_that("vis_glm_assumptions produces plots", {
  ToothGrowth$dose <- as.factor(ToothGrowth$dose)
  
  # Should not error when plotting
  expect_no_error({
    result <- vis_glm_assumptions(ToothGrowth$len, ToothGrowth$dose)
  })
})

test_that("vis_glm_assumptions handles NA values", {
  # Create data with NAs
  data_with_na <- c(1, 2, 3, NA, 5, 6, 7, 8, 9, 10)
  groups_with_na <- factor(c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2))
  
  # Should handle NAs gracefully
  expect_no_error({
    result <- vis_glm_assumptions(data_with_na, groups_with_na)
  })
})

test_that("vis_glm_assumptions cex parameter works", {
  ToothGrowth$dose <- as.factor(ToothGrowth$dose)
  
  # Should accept different cex values
  expect_no_error({
    result <- vis_glm_assumptions(ToothGrowth$len, ToothGrowth$dose, cex = 0.8)
  })
  
  expect_no_error({
    result <- vis_glm_assumptions(ToothGrowth$len, ToothGrowth$dose, cex = 1.5)
  })
})

test_that("bp_test gives reasonable results", {
  # Test with homoscedastic data
  set.seed(123)
  x <- runif(100)
  y <- 2 + 3*x + rnorm(100, sd = 1)
  model1 <- lm(y ~ x)
  
  result1 <- bp.test(model1)
  
  expect_s3_class(result1, "htest")
  expect_named(result1, c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(result1$method, "Breusch-Pagan test for heteroscedasticity")
  expect_true(result1$p.value > 0.01)  # Should not reject for homoscedastic data
  
  # Test with heteroscedastic data
  set.seed(456)
  x2 <- runif(100)
  y2 <- 2 + 3*x2 + rnorm(100, sd = 0.5 + 2*x2)
  model2 <- lm(y2 ~ x2)
  
  result2 <- bp.test(model2)
  
  expect_s3_class(result2, "htest")
  expect_equal(result2$method, "Breusch-Pagan test for heteroscedasticity")
  expect_true(result2$p.value < 0.05)  # Should reject for heteroscedastic data
})

test_that("vis_glm_assumptions uses plot.lm correctly", {
  # Test that plots are generated without errors
  ToothGrowth$dose <- as.factor(ToothGrowth$dose)
  
  # Capture plot output to check it runs
  expect_no_error({
    pdf(NULL)  # Use null device to suppress actual plotting
    result <- vis_glm_assumptions(ToothGrowth$len, ToothGrowth$dose)
    dev.off()
  })
  
  # For regression
  expect_no_error({
    pdf(NULL)
    result <- vis_glm_assumptions(mtcars$mpg, as.factor(mtcars$cyl), regression = TRUE)
    dev.off()
  })
})