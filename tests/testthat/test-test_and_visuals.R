library(testthat)
library(nortest)  # for ad.test
library(multcompView)  # for multcompLetters
library(vcd) # for mosaic plots

# Test suite for test_and_visuals.R functions
# Note: Many functions create plots, so we focus on testing return values and error handling

# Helper function to create test data
create_test_data <- function() {
  set.seed(123)
  list(
    numeric_data = rnorm(30, mean = 10, sd = 2),
    factor_data = factor(rep(c("A", "B", "C"), each = 10)),
    binary_factor = factor(rep(c("Group1", "Group2"), each = 15)),
    count_data = factor(sample(c("Cat1", "Cat2", "Cat3"), 50, replace = TRUE)),
    x_regression = seq(1, 20, length.out = 30),
    y_regression = 2 * seq(1, 20, length.out = 30) + rnorm(30, 0, 1)
  )
}

# Tests for helper functions
test_that("colorscheme function works correctly", {
  # Test listing all schemes
  all_schemes <- colorscheme(NULL)
  expect_type(all_schemes, "list")
  expect_named(all_schemes, c("colortuple", "colortuple2", "ColorPalette"))
  
  # Test individual schemes
  scheme1 <- colorscheme(1)
  expect_type(scheme1, "character")
  expect_length(scheme1, 2)
  expect_match(scheme1[1], "^#[A-Fa-f0-9]{6}$")  # hex color format
  
  scheme2 <- colorscheme(2)
  expect_type(scheme2, "character")
  expect_length(scheme2, 2)
  
  scheme3 <- colorscheme(3)
  expect_type(scheme3, "character")
  expect_length(scheme3, 12)
  
  # Test invalid input
  expect_message(colorscheme(4), "Choose valid parameter")
})

test_that("create_two_samples_vector works correctly", {
  data <- create_test_data()
  
  result <- create_two_samples_vector(data$numeric_data, data$binary_factor)
  
  expect_type(result, "list")
  expect_named(result, c("sample1", "sample2", "sample1and2"))
  expect_length(result$sample1, 15)
  expect_length(result$sample2, 15)
  expect_length(result$sample1and2, 30)
  
  # Test with more than 2 levels
  expect_warning(
    create_two_samples_vector(data$numeric_data, data$factor_data),
    "only two level input allowed"
  )
})

test_that("calc_min_max_of_y_axis works correctly", {
  test_data <- c(1, 5, 10, 15, 20)
  
  result <- calc_min_max_of_y_axis(test_data, 0.1, 0.2)
  
  expect_type(result, "list")
  expect_length(result, 2)
  
  min_val <- result[[1]]
  max_val <- result[[2]]
  
  expect_true(min_val < min(test_data))
  expect_true(max_val > max(test_data))
  expect_type(min_val, "double")
  expect_type(max_val, "double")
})

test_that("check_assumptions_shapiro works correctly", {
  # Valid data
  valid_data <- rnorm(100)
  expect_true(check_assumptions_shapiro(valid_data))
  
  # Too small sample
  small_data <- c(1, 2)
  expect_warning(check_assumptions_shapiro(small_data), "sample size must be between 3 and 5000")
  expect_false(suppressWarnings(check_assumptions_shapiro(small_data)))
  
  # All identical values
  identical_data <- rep(5, 10)
  expect_warning(check_assumptions_shapiro(identical_data), "all 'x' values are identical")
  expect_false(suppressWarnings(check_assumptions_shapiro(identical_data)))
})

test_that("check_assumptions_count_data works correctly", {
  # Valid count data
  samples <- factor(c("A", "A", "B", "B", "C", "C"))
  fact <- factor(c("X", "Y", "X", "Y", "X", "Y"))
  expect_true(check_assumptions_count_data(samples, fact))
  
  # Invalid data - only one row
  samples_bad <- factor(c("A", "A", "A", "A"))
  fact_bad <- factor(c("X", "Y", "X", "Y"))
  expect_warning(
    check_assumptions_count_data(samples_bad, fact_bad),
    "need 2 or more non-zero row marginals"
  )
  expect_false(suppressWarnings(check_assumptions_count_data(samples_bad, fact_bad)))
})

test_that("makeTable works correctly", {
  samples <- factor(c("A", "A", "B", "B", "C"))
  fact <- factor(c("X", "Y", "X", "Y", "X"))
  
  result <- makeTable(samples, fact, "Sample", "Factor")
  
  expect_s3_class(result, "table")
  expect_equal(length(dimnames(result)), 2)  # Should have row and column names
  expect_true(all(colSums(result) > 0))  # no zero columns
  
  # Check that result is properly structured (don't assume specific order)
  expect_true(nrow(result) >= 2)  # Should have at least 2 rows
  expect_true(ncol(result) >= 2)  # Should have at least 2 columns
  
  # Check that the table contains our data
  expect_true(sum(result) == 5)  # Total count should be 5
})

test_that("test_norm works correctly", {
  # Normal data
  normal_data <- rnorm(100)
  result <- test_norm(normal_data)
  
  expect_s3_class(result, "htest")
  expect_equal(result$method, "Shapiro-Wilk normality test")
  expect_true("p.value" %in% names(result))
  
  # Data with NAs
  data_with_na <- c(rnorm(50), NA, NA)
  result_na <- test_norm(data_with_na)
  expect_s3_class(result_na, "htest")
})

test_that("check_assumption_sample_size_t_test works correctly", {
  x1 <- rnorm(10)
  x2 <- rnorm(15)
  
  # Sufficient sample sizes
  expect_true(check_assumption_sample_size_t_test(x1, x2, 5))
  
  # Insufficient sample sizes
  expect_false(check_assumption_sample_size_t_test(x1, x2, 12))
})

test_that("side_of_nh works correctly", {
  expect_equal(side_of_nh("less"), ">=")
  expect_equal(side_of_nh("greater"), "<=")
  expect_equal(side_of_nh("two.sided"), "equals")
})

test_that("calculate_comparepvalue works correctly", {
  expect_equal(calculate_comparepvalue(0.01, 0.95), "<")
  expect_equal(calculate_comparepvalue(0.1, 0.95), ">")
})

test_that("type_sample_fact works correctly", {
  samples <- rnorm(10)
  fact <- factor(rep(c("A", "B"), 5))
  
  result <- type_sample_fact(samples, fact)
  
  expect_type(result, "list")
  expect_named(result, c("typesample", "typefactor"))
  expect_equal(result$typesample, "numeric")
  expect_equal(result$typefactor, "factor")
})

# Tests for main statistical functions (focusing on return values and error handling)
test_that("test_norm_vis returns correct structure", {
  skip_if_not_installed("nortest")
  
  data <- rnorm(100, mean = 10, sd = 2)
  
  # Capture plot output and test return value
  result <- test_norm_vis(data)
  
  expect_type(result, "list")
  expect_named(result, c("Anderson-Darling", "Shapiro"))
  expect_s3_class(result$`Anderson-Darling`, "htest")
  expect_s3_class(result$Shapiro, "htest")
})

test_that("fisher_chi works correctly", {
  # Create a contingency table
  counts <- matrix(c(10, 5, 8, 12), nrow = 2)
  dimnames(counts) <- list(c("Row1", "Row2"), c("Col1", "Col2"))
  
  result <- fisher_chi(counts)
  
  expect_s3_class(result, "htest")
  expect_true("p.value" %in% names(result))
  
  # Check that method is one of the expected chi-square or Fisher test variants
  # The exact method name might vary depending on the implementation
  expect_true(grepl("Chi-squared|Fisher|chi", result$method, ignore.case = TRUE))
  
  # Check that we get a valid p-value
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
})

test_that("odds_ratio function works correctly", {
  # Test basic odds ratio calculation
  result <- odds_ratio(a = 10, b = 5, c = 8, d = 12, alpha = 0.05)
  
  expect_type(result, "double")
  expect_true(nrow(result) == 4)  # OR, lowconf, upconf, SE
  expect_true(all(is.finite(result[!is.nan(result)])))
})

# Tests for visualization functions (checking they run without error)
test_that("vis_chi_squared_test runs without error for valid data", {
  skip_if_not_installed("vcd")
  
  # Create categorical data
  samples <- factor(sample(c("Cat1", "Cat2"), 50, replace = TRUE))
  fact <- factor(sample(c("Group1", "Group2"), 50, replace = TRUE))
  
  expect_no_error({
    result <- vis_chi_squared_test(samples, fact, "Sample", "Factor")
  })
})

test_that("two_sample_t_test runs without error for valid data", {
  data <- create_test_data()
  
  expect_no_error({
    result <- two_sample_t_test(
      samples = data$numeric_data,
      fact = data$binary_factor,
      samplename = "Values",
      factorname = "Groups"
    )
  })
})

test_that("two_sample_wilcoxon_test runs without error for valid data", {
  data <- create_test_data()
  
  expect_no_error({
    result <- two_sample_wilcoxon_test(
      samples = data$numeric_data,
      fact = data$binary_factor,
      samplename = "Values",
      factorname = "Groups"
    )
  })
})

test_that("vis_anova runs without error for valid data", {
  skip_if_not_installed("multcompView")
  
  data <- create_test_data()
  
  expect_no_error({
    result <- vis_anova(
      samples = data$numeric_data,
      fact = data$factor_data,
      samplename = "Values",
      factorname = "Groups"
    )
  })
})

test_that("vis_Kruskal_Wallis_clusters runs without error for valid data", {
  skip_if_not_installed("multcompView")
  
  data <- create_test_data()
  
  expect_no_error({
    result <- vis_Kruskal_Wallis_clusters(
      samples = data$numeric_data,
      fact = data$factor_data,
      samplename = "Values",
      factorname = "Groups"
    )
  })
})

test_that("vis_regression runs without error for valid data", {
  data <- create_test_data()
  
  expect_no_error({
    result <- vis_regression(
      y = data$y_regression,
      x = data$x_regression,
      name_of_factor = "X",
      name_of_sample = "Y"
    )
  })
})

test_that("vis_normality_assumptions runs without error for valid data", {
  skip_if_not_installed("nortest")
  
  data <- create_test_data()
  
  expect_no_error({
    result <- vis_normality_assumptions(
      y = data$y_regression,
      x = data$x_regression
    )
  })
})

# Test error handling for main functions
test_that("two_sample_wilcoxon_test handles invalid inputs", {
  expect_warning(
    two_sample_wilcoxon_test(
      samples = c("not", "numeric"),
      fact = factor(c("A", "B")),
      conf.level = 0.95
    ),
    "'samples' must be numeric"
  )
  
  expect_warning(
    two_sample_wilcoxon_test(
      samples = c(1, 2),
      fact = c("A", "B"),  # not a factor
      conf.level = 0.95
    ),
    "'fact' must be factorial"
  )
  
  expect_warning(
    two_sample_wilcoxon_test(
      samples = c(1, 2),
      fact = factor(c("A", "B")),
      conf.level = 1.5  # invalid confidence level
    ),
    "'conf.level' must be a single number between 0 and 1"
  )
})

test_that("two_sample_t_test handles invalid confidence levels", {
  data <- create_test_data()
  
  expect_error(
    two_sample_t_test(
      samples = data$numeric_data,
      fact = data$binary_factor,
      conf.level = 1.5
    ),
    "'conf.level' must be a single number between 0 and 1"
  )
})

# Test confidence band functions
test_that("conf_band and progn_band work correctly", {
  x <- 1:10
  y <- 2 * x + rnorm(10, 0, 0.5)
  reg <- lm(y ~ x)
  
  conf_result <- conf_band(x, reg, conf.level = 0.95, up = 1)
  expect_type(conf_result, "double")
  expect_length(conf_result, length(x))
  
  progn_result <- progn_band(x, reg, conf.level = 0.95, up = 1)
  expect_type(progn_result, "double")
  expect_length(progn_result, length(x))
  
  # Prediction bands should be wider than confidence bands
  expect_true(all(progn_result >= conf_result))
})

# Test that functions handle missing parameters correctly
test_that("functions handle missing optional parameters", {
  data <- create_test_data()
  
  # Test functions with missing conf.level
  expect_no_error({
    two_sample_wilcoxon_test(
      samples = data$numeric_data,
      fact = data$binary_factor
    )
  })
  
  expect_no_error({
    vis_anova(
      samples = data$numeric_data,
      fact = data$factor_data
    )
  })
})

# Test graphics parameter restoration
test_that("functions restore graphics parameters", {
  # Save original parameters
  original_par <- par(no.readonly = TRUE)
  
  # Run a function that changes parameters
  data <- create_test_data()
  test_norm_vis(data$numeric_data)
  
  # Check that most important parameters are restored
  current_par <- par(no.readonly = TRUE)
  
  # Key parameters should be restored (allowing for minor differences)
  expect_equal(current_par$mfrow, original_par$mfrow)
  expect_equal(current_par$oma, original_par$oma)
})

# Performance test for large datasets
test_that("functions handle reasonably large datasets", {
  skip_on_cran()  # Skip on CRAN to avoid long test times
  
  large_data <- rnorm(1000)
  large_factor <- factor(rep(c("A", "B"), 500))
  
  expect_no_error({
    result <- test_norm(large_data)
  })
  
  expect_no_error({
    result <- create_two_samples_vector(large_data, large_factor)
  })
})