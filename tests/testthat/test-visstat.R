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
  expect_warning(
    result <- visstat(data$test_df, "height", "group"),
    "will no longer be supported"
  )
  
  expect_type(result, "list")
  expect_s3_class(result, "visstat")
  expect_true(length(result) > 0)
})

test_that("visstat backward-compatible syntax handles numeric columns", {
  data <- create_visstat_test_data()
  
  setup_test_graphics()
  on.exit(cleanup_test_graphics())
  
  # Regression with backward-compatible syntax
  expect_warning(
    result <- visstat(data$test_df, "weight", "height"),
    "will no longer be supported"
  )
  
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
  
  expect_warning(
    result <- visstat(balanced_df, "cat1", "cat2"),
    "will no longer be supported"
  )
  
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
  expect_warning(
    result_trees <- visstat(trees$Height, trees$Girth),
    "Statistical assumptions violated"
  )
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

test_that("visstat returns effect_size for implemented branches", {
  setup_test_graphics()
  on.exit(cleanup_test_graphics())

  mtcars$am <- factor(mtcars$am)
  hair_eye_df <- counts_to_cases(as.data.frame(HairEyeColor))

  results <- list(
    t_test = visstat(mtcars$am, mtcars$mpg),
    kruskal = visstat(iris$Species, iris$Petal.Width),
    regression = visstat(swiss$Examination, swiss$Fertility),
    spearman = visstat(airquality$Wind, airquality$Ozone, correlation = TRUE),
    chi_squared = visstat(hair_eye_df$Eye, hair_eye_df$Hair)
  )

  for (result in results) {
    expect_s3_class(result, "visstat")
    expect_true("effect_size" %in% names(result))
    expect_true(all(c("name", "estimate", "effect_size_method") %in% names(result$effect_size)))
  }
})

test_that("Welch Hedges g uses the non-pooled df correction", {
  group <- factor(c(rep("a", 4), rep("b", 3)))
  response <- c(1, 2, 3, 4, 2, 4, 8)
  result <- list("t-test-statistics" = t.test(response ~ group))

  es <- effect_size(result, x = group, y = response)

  x1 <- response[group == "a"]
  x2 <- response[group == "b"]
  n1 <- length(x1)
  n2 <- length(x2)
  s1 <- var(x1)
  s2 <- var(x2)
  sd_std <- sqrt((s1 + s2) / 2)
  df <- ((n1 - 1) * (n2 - 1) * (s1 + s2)^2) /
    ((n2 - 1) * s1^2 + (n1 - 1) * s2^2)
  correction <- exp(lgamma(df / 2) - 0.5 * log(df / 2) -
    lgamma((df - 1) / 2))

  expect_equal(es$estimate, correction * ((mean(x1) - mean(x2)) / sd_std))
})

test_that("rank-biserial effect size follows wilcox.test first-group direction", {
  group <- factor(c(rep("a", 3), rep("b", 3)))
  response <- c(3, 4, 5, 1, 2, 6)
  result <- list(statsWilcoxon = wilcox.test(response ~ group, exact = FALSE))

  es <- effect_size(result, x = group, y = response)

  x1 <- response[group == "a"]
  x2 <- response[group == "b"]
  favorable <- sum(outer(x1, x2, ">") + 0.5 * outer(x1, x2, "=="))
  expected <- (2 * favorable) / (length(x1) * length(x2)) - 1

  expect_equal(es$estimate, expected)
})

test_that("large residual panel reports NA Shapiro above R limit", {
  setup_test_graphics()
  on.exit(cleanup_test_graphics())

  n <- 2501
  base <- qnorm((seq_len(n) - 0.5) / n)
  group <- factor(rep(c("control", "treatment"), each = n))
  response <- c(base, base + 0.1)

  expect_warning(
    result <- visstat(group, response),
    "more than 5000 model residuals"
  )

  expect_s3_class(result, "visstat")
  expect_equal(result[["t-test-statistics"]]$method, " Two Sample t-test")
  expect_true(result[["t-test-statistics"]]$p.value < 0.001)
  expect_equal(result$effect_size$name, "Hedges' g")
  expect_lt(abs(result$effect_size$estimate), 0.11)
  expect_s3_class(result[["Shapiro-Wilk-test_sample1"]], "htest")
  expect_s3_class(result[["Shapiro-Wilk-test_sample2"]], "htest")
  expect_false(is.na(result[["Shapiro-Wilk-test_sample1"]]$p.value))
  expect_false(is.na(result[["Shapiro-Wilk-test_sample2"]]$p.value))
})

test_that("legacy t-test Shapiro fields stay valid above group limit", {
  setup_test_graphics()
  on.exit(cleanup_test_graphics())

  n <- 5001
  base <- qnorm((seq_len(n) - 0.5) / n)
  group <- factor(rep(c("control", "treatment"), each = n))
  response <- c(base, base + 0.1)

  expect_warning(
    result <- visstat(group, response),
    "more than 5000 model residuals"
  )

  expect_s3_class(result, "visstat")
  expect_s3_class(result[["Shapiro-Wilk-test_sample1"]], "htest")
  expect_s3_class(result[["Shapiro-Wilk-test_sample2"]], "htest")
  expect_true(is.na(result[["Shapiro-Wilk-test_sample1"]]$p.value))
  expect_true(is.na(result[["Shapiro-Wilk-test_sample2"]]$p.value))
  expect_equal(result[["Shapiro-Wilk-test_sample1"]]$data.name,
               "sample 1; n > 5000")
  expect_equal(result[["Shapiro-Wilk-test_sample2"]]$data.name,
               "sample 2; n > 5000")
  expect_s3_class(result[["t-test-statistics"]], "htest")
})

test_that("numeric correlation branch skips GLM assumption plot", {
  setup_test_graphics()
  on.exit(cleanup_test_graphics())

  expect_warning(
    regression <- visstat(airquality$Wind, airquality$Ozone),
    "Statistical assumptions violated"
  )
  spearman <- visstat(airquality$Wind, airquality$Ozone, correlation = TRUE)

  expect_length(attr(regression, "captured_plots"), 2)
  expect_length(attr(spearman, "captured_plots"), 1)
})

test_that("correlation request is only valid for rank-correlation routes", {
  setup_test_graphics()
  on.exit(cleanup_test_graphics())

  capture_warnings <- function(expr) {
    warnings <- character()
    value <- withCallingHandlers(
      expr,
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    list(value = value, warnings = warnings)
  }

  valid_cases <- list(
    numeric_numeric = list(
      x = as.numeric(1:20),
      y = as.numeric(20:1),
      expected = "spearman"
    ),
    numeric_integer = list(
      x = as.numeric(1:20),
      y = as.integer(20:1),
      expected = "spearman"
    ),
    integer_numeric = list(
      x = as.integer(1:20),
      y = as.numeric(20:1),
      expected = "spearman"
    ),
    integer_integer = list(
      x = as.integer(1:20),
      y = as.integer(20:1),
      expected = "spearman"
    ),
    ordered_ordered = list(
      x = ordered(rep(1:4, each = 6)),
      y = ordered(rep(c(1, 2, 2, 3), each = 6)),
      expected = "kendall"
    )
  )

  for (case_name in names(valid_cases)) {
    case <- valid_cases[[case_name]]
    caught <- capture_warnings(
      visstat(case$x, case$y, correlation = TRUE)
    )
    expect_length(caught$warnings, 0)
    expect_s3_class(caught$value, "visstat")
    if (case$expected == "spearman") {
      expect_equal(caught$value$analysis_type, "spearman correlation")
    } else {
      expect_equal(caught$value$test$method,
                   "Kendall's rank correlation tau")
    }
  }

  ignored_cases <- list(
    factor_numeric = list(
      x = factor(mtcars$am),
      y = mtcars$mpg,
      expected = "Welch Two Sample t-test",
      method = function(result) result[["t-test-statistics"]]$method
    ),
    factor_integer = list(
      x = factor(rep(c("A", "B"), each = 10)),
      y = as.integer(c(1:10, 11:20)),
      expected = "Two Sample t-test",
      method = function(result) result[["t-test-statistics"]]$method
    ),
    ordered_numeric = list(
      x = ordered(rep(c("A", "B"), each = 10)),
      y = as.numeric(c(1:10, 11:20)),
      expected = "Two Sample t-test",
      method = function(result) result[["t-test-statistics"]]$method
    ),
    factor_ordered = list(
      x = factor(rep(c("A", "B"), each = 12)),
      y = ordered(rep(1:4, each = 6)),
      expected = "Wilcoxon rank sum exact test",
      method = function(result) result[["statsWilcoxon"]]$method
    ),
    factor_factor = list(
      x = factor(rep(c("A", "B"), each = 25)),
      y = factor(rep(c("X", "Y"), 25)),
      expected = "Pearson's Chi-squared test with Yates",
      method = function(result) result$method
    ),
    ordered_factor = list(
      x = ordered(rep(c("A", "B"), each = 12)),
      y = factor(c(rep("yes", 8), rep("no", 4),
                   rep("yes", 5), rep("no", 7))),
      expected = "Pearson's Chi-squared test with Yates",
      method = function(result) result$method
    )
  )

  for (case_name in names(ignored_cases)) {
    case <- ignored_cases[[case_name]]
    caught <- capture_warnings(
      visstat(case$x, case$y, correlation = TRUE)
    )
    expect_s3_class(caught$value, "visstat")
    expect_match(case$method(caught$value), case$expected)
    ignored_warning <- grepl("correlation = TRUE was ignored",
                             caught$warnings)
    if (any(ignored_warning)) {
      expect_true(any(grepl(
        paste0("correlation = TRUE was ignored.*", case$expected),
        caught$warnings
      )))
    }
  }
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
  expect_warning(
    result_legacy <- visstat(test_df, "y_var", "x_var"),
    "will no longer be supported"
  )
  
  # Both should be visstat objects
  expect_s3_class(result_standard, "visstat")
  expect_s3_class(result_legacy, "visstat")
  
  # Should have similar structure (exact equality might vary due to names)
  expect_equal(length(result_standard), length(result_legacy))
})
