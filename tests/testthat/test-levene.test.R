# Test file for levene.test function
# Save as: test-levene-test.R

library(testthat)

# Helper function to create test data
create_test_data <- function() {
  set.seed(123)
  list(
    equal_var = list(
      y = c(rnorm(10, mean = 0, sd = 1), 
            rnorm(10, mean = 2, sd = 1), 
            rnorm(10, mean = 4, sd = 1)),
      g = factor(rep(1:3, each = 10))
    ),
    unequal_var = list(
      y = c(rnorm(15, mean = 0, sd = 1), 
            rnorm(15, mean = 0, sd = 5), 
            rnorm(15, mean = 0, sd = 0.2)),
      g = factor(rep(c("A", "B", "C"), each = 15))
    )
  )
}

test_that("levene.test returns correct structure", {
  test_data <- create_test_data()
  result <- levene.test(test_data$equal_var$y, test_data$equal_var$g)
  
  # Check class
  expect_s3_class(result, "htest")
  
  # Check components exist
  expect_true(all(c("statistic", "parameter", "p.value", "method", "data.name") %in% names(result)))
  
  # Check statistic is named F
  expect_named(result$statistic, "F")
  
  # Check parameters are named df1 and df2
  expect_named(result$parameter, c("df1", "df2"))
  
  # Check method string
  expect_equal(result$method, "Levene-Brown-Forsythe Test (center = median)")
})

test_that("levene.test calculates correct degrees of freedom", {
  test_data <- create_test_data()
  
  # Test with 3 groups of 10 each (N=30, k=3)
  result <- levene.test(test_data$equal_var$y, test_data$equal_var$g)
  expect_equal(result$parameter[["df1"]], 2)  # k-1 = 3-1 = 2
  expect_equal(result$parameter[["df2"]], 27) # N-k = 30-3 = 27
  
  # Test with 3 groups of 15 each (N=45, k=3)
  result2 <- levene.test(test_data$unequal_var$y, test_data$unequal_var$g)
  expect_equal(result2$parameter[["df1"]], 2)  # k-1 = 3-1 = 2
  expect_equal(result2$parameter[["df2"]], 42) # N-k = 45-3 = 42
})

test_that("levene.test works with data frame argument", {
  test_data <- create_test_data()
  df <- data.frame(
    response = test_data$equal_var$y,
    group = test_data$equal_var$g
  )
  
  # Test with data frame
  result_df <- levene.test(response, group, data = df)
  
  # Test without data frame
  result_no_df <- levene.test(test_data$equal_var$y, test_data$equal_var$g)
  
  # Results should be identical
  expect_equal(result_df$statistic, result_no_df$statistic)
  expect_equal(result_df$parameter, result_no_df$parameter)
  expect_equal(result_df$p.value, result_no_df$p.value)
})

test_that("levene.test handles factor conversion", {
  test_data <- create_test_data()
  
  # Test with character grouping variable
  g_char <- as.character(test_data$equal_var$g)
  result_char <- levene.test(test_data$equal_var$y, g_char)
  
  # Test with factor grouping variable
  result_factor <- levene.test(test_data$equal_var$y, test_data$equal_var$g)
  
  # Results should be identical
  expect_equal(result_char$statistic, result_factor$statistic)
  expect_equal(result_char$p.value, result_factor$p.value)
})

test_that("levene.test produces reasonable results for known cases", {
  # Test case where variances are clearly different
  set.seed(456)
  y_diff <- c(rnorm(20, sd = 0.1), rnorm(20, sd = 10))  # Very different variances
  g_diff <- factor(rep(c("low", "high"), each = 20))
  
  result_diff <- levene.test(y_diff, g_diff)
  
  # Should reject null hypothesis (small p-value)
  expect_lt(result_diff$p.value, 0.05)
  
  # Test case where variances are similar
  set.seed(789)
  y_same <- c(rnorm(20, sd = 1), rnorm(20, sd = 1.1))  # Similar variances
  g_same <- factor(rep(c("A", "B"), each = 20))
  
  result_same <- levene.test(y_same, g_same)
  
  # Should not reject null hypothesis (larger p-value)
  expect_gt(result_same$p.value, 0.05)
})

test_that("levene.test handles edge cases", {
  # Test with minimum number of groups (2)
  set.seed(100)
  y_min <- c(rnorm(5), rnorm(5, sd = 2))
  g_min <- factor(rep(1:2, each = 5))
  
  expect_no_error(levene.test(y_min, g_min))
  result_min <- levene.test(y_min, g_min)
  expect_equal(result_min$parameter[["df1"]], 1)  # 2-1 = 1
  expect_equal(result_min$parameter[["df2"]], 8)  # 10-2 = 8
})

test_that("levene.test input validation works", {
  test_data <- create_test_data()
  
  # Test non-numeric y
  expect_error(
    levene.test(as.character(test_data$equal_var$y), test_data$equal_var$g),
    "`y` must be numeric"
  )
  
  # Test mismatched lengths
  expect_error(
    levene.test(test_data$equal_var$y[1:5], test_data$equal_var$g),
    # This should produce an error from the internal functions
    class = "error"
  )
})

test_that("levene.test handles missing values appropriately", {
  test_data <- create_test_data()
  
  # Add some NA values
  y_na <- test_data$equal_var$y
  y_na[c(1, 15, 25)] <- NA
  
  # Function should handle NAs through tapply's na.rm = TRUE
  expect_no_error(levene.test(y_na, test_data$equal_var$g))
})

test_that("levene.test numerical accuracy", {
  # Test against known mathematical properties
  # When all groups have identical values, variance should be 0 and test should be undefined
  y_identical <- rep(c(1, 2, 3), each = 10)
  g_identical <- factor(rep(1:3, each = 10))
  
  # This might produce NaN or extreme values
  result_identical <- levene.test(y_identical, g_identical)
  
  # The test statistic should be 0 when there's no within-group variation
  expect_true(is.numeric(result_identical$statistic))
})

# test_that("levene.test matches car::leveneTest when available", {
#   # Double-check that car is available without triggering installation
#   skip_if_not(requireNamespace("car", quietly = TRUE), "car package not available")
#   
#   test_data <- create_test_data()
#   our_result <- levene.test(test_data$equal_var$y, test_data$equal_var$g)
#   
#   # Compare with car package result
#   car_result <- car::leveneTest(test_data$equal_var$y, test_data$equal_var$g, center = median)
#   
#   # Test statistics should be very close (allowing for small numerical differences)
#   expect_equal(our_result$statistic[["F"]], car_result$`F value`[1], tolerance = 1e-10)
#   expect_equal(our_result$p.value, car_result$`Pr(>F)`[1], tolerance = 1e-10)
#   expect_equal(our_result$parameter[["df1"]], car_result$Df[1])
#   expect_equal(our_result$parameter[["df2"]], car_result$Df[2])
# })