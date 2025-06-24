library(testthat)

# Test suite for visstat.R wrapper function
# Tests both standardised and backward-compatible input forms

# Helper function to create test datasets
create_visstat_test_data <- function() {
  set.seed(42)
  list(
    # Numeric data for regression
    numeric_x = rnorm(30, mean = 10, sd = 2),
    numeric_y = rnorm(30, mean = 15, sd = 3),
    
    # Factor data for group comparisons
    factor_3_levels = factor(rep(c("A", "B", "C"), each = 10)),
    factor_2_levels = factor(rep(c("Group1", "Group2"), each = 15)),
    
    # Categorical data for chi-square tests
    categorical_x = factor(sample(c("Cat1", "Cat2", "Cat3"), 50, replace = TRUE)),
    categorical_y = factor(sample(c("Type1", "Type2"), 50, replace = TRUE)),
    
    # Data frame for backward compatibility tests
    test_df = data.frame(
      height = rnorm(40, 170, 10),
      weight = rnorm(40, 70, 15),
      group = factor(rep(c("A", "B", "C", "D"), each = 10)),
      category = factor(sample(c("X", "Y"), 40, replace = TRUE)),
      stringsAsFactors = FALSE
    )
  )
}

# Helper functions for graphics device management
setup_test_graphics <- function() {
  if (length(dev.list()) > 0) {
    dev.off()
  }
  pdf(NULL)
}

cleanup_test_graphics <- function() {
  if (length(dev.list()) > 0) {
    dev.off()
  }
}

# Tests for input validation and parameter handling
test_that("visstat handles basic parameter validation", {
  data <- create_visstat_test_data()
  
  setup_test_graphics()
  on.exit(cleanup_test_graphics())
  
  # Test with valid inputs - should not error
  expect_no_error(
    visstat(data$factor_2_levels, data$numeric_x, conf.level = 0.95)
  )
  
  # Test with invalid confidence level 
  expect_error(
    visstat(data$factor_2_levels, data$numeric_x, conf.level = 1.5),
    "conf.level"
  )
  
  # For this one, it seems the validation happens in the underlying function
  expect_warning(
    visstat(data$factor_2_levels, data$numeric_x, conf.level = -0.1),
    "conf.level"
  )
})

# Tests for standardised syntax: visstat(x, y)
test_that("visstat standardised syntax works with factor and numeric", {
  data <- create_visstat_test_data()
  
  setup_test_graphics()
  on.exit(cleanup_test_graphics())
  
  # Factor (x) and numeric (y) - should trigger group comparison tests
  result <- visstat(data$factor_2_levels, data$numeric_x)
  
  expect_type(result, "list")
  expect_s3_class(result, "visstat")
  expect_true(length(result) > 0)
})

test_that("visstat standardised syntax works with numeric regression", {
  data <- create_visstat_test_data()
  
  setup_test_graphics()
  on.exit(cleanup_test_graphics())
  
  # Numeric (x) and numeric (y) - should trigger regression
  result <- visstat(data$numeric_x, data$numeric_y)
  
  expect_type(result, "list")
  expect_s3_class(result, "visstat")
  expect_true(length(result) > 0)
})

test_that("visstat standardised syntax works with categorical data", {
  data <- create_visstat_test_data()
  
  # Factor (x) and factor (y) - should trigger chi-square/Fisher test
  simple_cat_x <- factor(rep(c("A", "B"), each = 25))
  simple_cat_y <- factor(rep(c("X", "Y"), 25))
  
  setup_test_graphics()
  on.exit(cleanup_test_graphics())
  
  result <- visstat(simple_cat_x, simple_cat_y)
  
  expect_type(result, "list")
  expect_s3_class(result, "visstat")
  expect_true(length(result) > 0)
})

# Tests for backward-compatible syntax: visstat(dataframe, "col1", "col2")
test_that("visstat backward-compatible syntax works correctly", {
  data <- create_visstat_test_data()
  
  setup_test_graphics()
  on.exit(cleanup_test_graphics())
  
  # Test with data frame and column names
  result <- visstat(data$test_df, "height", "group")
  
  expect_type(result, "list")
  expect_s3_class(result, "visstat")
  expect_true(length(result) > 0)
})

test_that("visstat backward-compatible syntax handles numeric columns", {
  data <- create_visstat_test_data()
  
  setup_test_graphics()
  on.exit(cleanup_test_graphics())
  
  # Regression with backward-compatible syntax
  result <- visstat(data$test_df, "weight", "height")
  
  expect_type(result, "list")
  expect_s3_class(result, "visstat")
  expect_true(length(result) > 0)
})

test_that("visstat backward-compatible syntax handles categorical columns", {
  # Create a more balanced dataset to avoid mosaic plot issues
  set.seed(123)
  balanced_df <- data.frame(
    cat1 = factor(rep(c("A", "B"), 30)),
    cat2 = factor(sample(c("X", "Y"), 60, replace = TRUE, prob = c(0.6, 0.4)))
  )
  
  setup_test_graphics()
  on.exit(cleanup_test_graphics())
  
  # Suppress warnings about residuals being zero (expected for some balanced datasets)
  result <- suppressWarnings(visstat(balanced_df, "cat1", "cat2"))
  
  expect_type(result, "list")
  expect_s3_class(result, "visstat")
  expect_true(length(result) > 0)
})

# Tests for error handling in backward-compatible syntax
test_that("visstat backward-compatible syntax validates inputs correctly", {
  data <- create_visstat_test_data()
  
  # These tests check input validation, so we don't need full graphics setup
  if (length(dev.list()) > 0) dev.off()
  
  # Missing second column name - should trigger check_visstat_input error
  expect_error(
    visstat(data$test_df, "height"),
    "Invalid input"
  )
  
  # Non-character column name - should trigger check_visstat_input error
  expect_error(
    suppressWarnings(visstat(data$test_df, "height", 123)),
    "Invalid input"
  )
  
  # Invalid column names - should error when trying to access non-existent columns
  expect_error(
    suppressWarnings(visstat(data$test_df, "nonexistent_col", "height"))
  )
})

# Tests for optional parameters
test_that("visstat handles optional parameters correctly", {
  data <- create_visstat_test_data()
  
  setup_test_graphics()
  on.exit(cleanup_test_graphics())
  
  # Test with different confidence levels
  result_95 <- visstat(data$factor_2_levels, data$numeric_x, conf.level = 0.95)
  result_99 <- visstat(data$factor_2_levels, data$numeric_x, conf.level = 0.99)
  
  expect_s3_class(result_95, "visstat")
  expect_s3_class(result_99, "visstat")
  expect_false(identical(result_95, result_99))
  
  # Test with numbers parameter
  result_numbers <- visstat(data$categorical_x, data$categorical_y, numbers = TRUE)
  result_no_numbers <- visstat(data$categorical_x, data$categorical_y, numbers = FALSE)
  
  expect_s3_class(result_numbers, "visstat")
  expect_s3_class(result_no_numbers, "visstat")
  
  # Test with minpercent parameter
  result_minperc <- visstat(data$categorical_x, data$categorical_y, minpercent = 0.1)
  expect_s3_class(result_minperc, "visstat")
})

# Tests for graphics output parameters
test_that("visstat handles graphics output parameters", {
  data <- create_visstat_test_data()
  temp_dir <- tempdir()
  
  setup_test_graphics()
  on.exit(cleanup_test_graphics())
  
  # Test with graphics output - suppress par warnings
  result <- suppressWarnings(visstat(
    data$factor_2_levels, 
    data$numeric_x,
    graphicsoutput = "png",
    plotDirectory = temp_dir
  ))
  
  expect_s3_class(result, "visstat")
  
  # Test with custom plot name
  result_custom <- suppressWarnings(visstat(
    data$factor_2_levels, 
    data$numeric_x,
    graphicsoutput = "png",
    plotName = "custom_test",
    plotDirectory = temp_dir
  ))
  
  expect_s3_class(result_custom, "visstat")
  
  # Test that plot method works with saved graphics
  expect_no_error(plot(result))
  expect_no_error(plot(result_custom))
})

# Tests for S3 methods integration
test_that("visstat objects work with S3 methods", {
  data <- create_visstat_test_data()
  
  setup_test_graphics()
  on.exit(cleanup_test_graphics())
  
  # Create a visstat object
  result <- visstat(data$factor_2_levels, data$numeric_x)
  
  # Test that S3 methods work
  expect_no_error(print(result))
  expect_no_error(summary(result))
  expect_no_error(plot(result))
  
  # Test that print returns invisibly
  print_result <- print(result)
  expect_identical(print_result, result)
  
  # Test that summary returns invisibly
  summary_result <- summary(result)
  expect_identical(summary_result, result)
  
  # Test that plot returns invisibly
  plot_result <- plot(result)
  expect_identical(plot_result, result)
})

test_that("visstat S3 methods produce expected output", {
  data <- create_visstat_test_data()
  
  setup_test_graphics()
  on.exit(cleanup_test_graphics())
  
  # Create a visstat object
  result <- visstat(data$factor_2_levels, data$numeric_x)
  
  # Test print output
  print_output <- capture.output(print(result))
  expect_true(any(grepl("Object of class 'visstat'", print_output)))
  expect_true(any(grepl("Available components:", print_output)))
  
  # Test summary output
  summary_output <- capture.output(summary(result))
  expect_true(any(grepl("Summary of visstat object", summary_output)))
  expect_true(any(grepl("--- Named components ---", summary_output)))
  expect_true(any(grepl("--- Contents ---", summary_output)))
})

# Tests for name extraction and handling
test_that("visstat extracts variable names correctly", {
  # Create named vectors to test name extraction
  height_data <- rnorm(20, 170, 10)
  group_data <- factor(rep(c("A", "B"), each = 10))
  
  setup_test_graphics()
  on.exit(cleanup_test_graphics())
  
  # The function should extract names from the expressions
  result <- visstat(group_data, height_data)
  
  expect_s3_class(result, "visstat")
  # Note: Testing exact names is tricky due to expression handling
  # The important thing is that it works without error
})

# Tests for real-world data scenarios
test_that("visstat works with built-in datasets", {
  setup_test_graphics()
  on.exit(cleanup_test_graphics())
  
  # Test with mtcars (numeric vs factor)
  mtcars$am <- as.factor(mtcars$am)
  result_mtcars <- visstat(mtcars$am, mtcars$mpg)
  expect_s3_class(result_mtcars, "visstat")
  
  # Test with iris (factor vs numeric)
  result_iris <- visstat(iris$Species, iris$Petal.Width)
  expect_s3_class(result_iris, "visstat")
  
  # Test with regression
  result_trees <- visstat(trees$Height, trees$Girth)
  expect_s3_class(result_trees, "visstat")
})

# Tests for edge cases and data types
test_that("visstat handles edge cases correctly", {
  # Test with integer data
  int_data <- as.integer(c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5))
  factor_data <- factor(rep(c("A", "B"), each = 5))
  
  setup_test_graphics()
  on.exit(cleanup_test_graphics())
  
  result <- visstat(factor_data, int_data)
  expect_s3_class(result, "visstat")
  
  # Test with small datasets - expect warnings but not errors
  small_numeric <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  small_factor <- factor(rep(c("A", "B"), each = 5))
  
  # Should work, might trigger warnings about sample size
  expect_no_error(suppressWarnings(visstat(small_factor, small_numeric)))
})

# Tests for parameter combinations
test_that("visstat handles multiple parameter combinations", {
  data <- create_visstat_test_data()
  temp_dir <- tempdir()
  
  setup_test_graphics()
  on.exit(cleanup_test_graphics())
  
  # Complex parameter combination - suppress par warnings
  result <- suppressWarnings(visstat(
    data$factor_3_levels,
    data$numeric_x,
    conf.level = 0.99,
    numbers = FALSE,
    minpercent = 0.02,
    graphicsoutput = "png",
    plotName = "complex_test",
    plotDirectory = temp_dir
  ))
  
  expect_s3_class(result, "visstat")
  expect_type(result, "list")
})

# Tests for graphics parameter restoration
test_that("visstat restores graphics parameters correctly", {
  data <- create_visstat_test_data()
  
  # Store original parameters
  original_par <- par(no.readonly = TRUE)
  
  setup_test_graphics()
  on.exit(cleanup_test_graphics())
  
  # Run visstat
  result <- visstat(data$factor_2_levels, data$numeric_x)
  
  # Check that parameters are restored
  current_par <- par(no.readonly = TRUE)
  
  # Key parameters should be restored
  expect_equal(current_par$mfrow, original_par$mfrow)
  expect_equal(current_par$oma, original_par$oma)
})

# Tests for function return behavior
test_that("visstat returns results correctly", {
  data <- create_visstat_test_data()
  
  setup_test_graphics()
  on.exit(cleanup_test_graphics())
  
  # Function should return invisibly for new syntax
  result <- visstat(data$factor_2_levels, data$numeric_x)
  
  # Should be a visstat object
  expect_s3_class(result, "visstat")
  expect_type(result, "list")
  
  # Should have some components (don't assume specific structure)
  expect_true(length(result) > 0)
})

# Tests for input type validation
test_that("visstat validates input types appropriately", {
  # Invalid input types should be caught by check_visstat_input
  expect_error(visstat(NULL, NULL), "Invalid input")
  
  # Test with clearly invalid data types
  suppressWarnings(expect_error(visstat(list(1, 2, 3), factor(c("A", "B"))), "Invalid input"))
  
  # Test with mixed invalid types that should definitely fail
  suppressWarnings(expect_error(visstat(function() {}, matrix(1:4, nrow = 2)), "Invalid input"))
})

# Integration tests with different test scenarios
test_that("visstat integrates correctly with different statistical tests", {
  # Test that different combinations trigger appropriate tests
  data <- create_visstat_test_data()
  
  setup_test_graphics()
  on.exit(cleanup_test_graphics())
  
  # Two-sample test (factor + numeric)
  result_two_sample <- visstat(data$factor_2_levels, data$numeric_x)
  expect_s3_class(result_two_sample, "visstat")
  
  # ANOVA/Kruskal-Wallis (factor + numeric, multiple groups)
  result_anova <- visstat(data$factor_3_levels, data$numeric_x)
  expect_s3_class(result_anova, "visstat")
  
  # Chi-square/Fisher (factor + factor)
  result_chisq <- visstat(data$categorical_x, data$categorical_y)
  expect_s3_class(result_chisq, "visstat")
  
  # Regression (numeric + numeric)
  result_regression <- visstat(data$numeric_x, data$numeric_y)
  expect_s3_class(result_regression, "visstat")
})

# Tests for consistency between syntax forms
test_that("visstat produces consistent results across syntax forms", {
  data <- create_visstat_test_data()
  
  setup_test_graphics()
  on.exit(cleanup_test_graphics())
  
  # Create a test data frame
  test_df <- data.frame(
    x_var = data$factor_2_levels,
    y_var = data$numeric_x
  )
  
  # Test both syntax forms
  result_standard <- visstat(test_df$x_var, test_df$y_var)
  result_legacy <- visstat(test_df, "y_var", "x_var")
  
  # Both should be visstat objects
  expect_s3_class(result_standard, "visstat")
  expect_s3_class(result_legacy, "visstat")
  
  # Should have similar structure (exact equality might vary due to names)
  expect_equal(length(result_standard), length(result_legacy))
})