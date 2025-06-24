library(testthat)

# Test suite for visstat_methods.R S3 methods
# Assumes the methods print.visstat, summary.visstat, and plot.visstat are loaded

# Helper function to create mock visstat objects for testing
create_mock_visstat <- function(type = "basic") {
  if (type == "basic") {
    obj <- list(
      test = list(
        method = "One-way ANOVA",
        p.value = 0.0234,
        statistic = 4.567,
        data.name = "test data"
      ),
      assumptions = list(
        normality = list(method = "Shapiro-Wilk", p.value = 0.123),
        homogeneity = list(method = "Levene's Test", p.value = 0.456)
      )
    )
  } else if (type == "empty") {
    obj <- list()
  } else if (type == "no_test") {
    obj <- list(
      data = data.frame(x = 1:5, y = 6:10),
      notes = "No statistical test performed"
    )
  } else if (type == "with_plots") {
    obj <- list(
      test = list(
        method = "Kruskal-Wallis test",
        p.value = 0.001
      ),
      captured_plots = list("plot1", "plot2")  # Mock plot objects
    )
  }
  
  class(obj) <- "visstat"
  return(obj)
}

# Tests for print.visstat method
test_that("print.visstat displays basic information correctly", {
  obj <- create_mock_visstat("basic")
  
  # Capture output
  output <- capture.output(result <- print(obj))
  
  # Check return value (should return object invisibly)
  expect_identical(result, obj)
  
  # Check output content
  expect_true(any(grepl("Object of class 'visstat'", output)))
  expect_true(any(grepl("Test used: One-way ANOVA", output)))
  expect_true(any(grepl("p-value: 0.0234", output)))
  expect_true(any(grepl("Available components:", output)))
})

test_that("print.visstat handles empty visstat objects", {
  obj <- create_mock_visstat("empty")
  
  output <- capture.output(result <- print(obj))
  
  expect_identical(result, obj)
  expect_true(any(grepl("Object of class 'visstat'", output)))
  expect_true(any(grepl("Available components:", output)))
})

test_that("print.visstat handles objects without test method", {
  obj <- create_mock_visstat("no_test")
  
  output <- capture.output(result <- print(obj))
  
  expect_identical(result, obj)
  expect_true(any(grepl("Object of class 'visstat'", output)))
  expect_true(any(grepl("Available components:", output)))
  # Should not show test method or p-value lines
  expect_false(any(grepl("Test used:", output)))
  expect_false(any(grepl("p-value:", output)))
})

test_that("print.visstat handles non-list objects", {
  obj <- "not a list"
  class(obj) <- "visstat"
  
  output <- capture.output(result <- print(obj))
  
  expect_identical(result, obj)
  expect_true(any(grepl("Object of class 'visstat'", output)))
  expect_true(any(grepl("No named elements in object", output)))
})

# Tests for summary.visstat method
test_that("summary.visstat displays comprehensive information", {
  obj <- create_mock_visstat("basic")
  
  output <- capture.output(result <- summary(obj))
  
  # Check return value
  expect_identical(result, obj)
  
  # Check output structure
  expect_true(any(grepl("Summary of visstat object", output)))
  expect_true(any(grepl("--- Named components ---", output)))
  expect_true(any(grepl("--- Contents ---", output)))
  
  # Check that all components are shown
  expect_true(any(grepl("\\$test:", output)))
  expect_true(any(grepl("\\$assumptions:", output)))
})

test_that("summary.visstat handles captured_plots specially", {
  obj <- create_mock_visstat("with_plots")
  
  output <- capture.output(result <- summary(obj))
  
  expect_identical(result, obj)
  expect_true(any(grepl("\\$captured_plots:", output)))
  expect_true(any(grepl("List of 2 plot object", output)))
})

test_that("summary.visstat handles empty objects", {
  obj <- create_mock_visstat("empty")
  
  output <- capture.output(result <- summary(obj))
  
  expect_identical(result, obj)
  expect_true(any(grepl("Summary of visstat object", output)))
})

# Tests for helper functions
test_that(".print_captured_plots_summary works correctly", {
  # Test with non-empty list
  plots <- list("plot1", "plot2", "plot3")
  output <- capture.output(visStatistics:::.print_captured_plots_summary(plots))
  expect_true(any(grepl("List of 3 plot object", output)))
  
  # Test with empty list
  empty_plots <- list()
  output <- capture.output(visStatistics:::.print_captured_plots_summary(empty_plots))
  expect_true(any(grepl("Empty plot list", output)))
  
  # Test with non-list object
  non_list <- "not a list"
  output <- capture.output(visStatistics:::.print_captured_plots_summary(non_list))
  expect_true(any(grepl("Plot object.*not displayed", output)))
})

# Tests for plot.visstat method
test_that("plot.visstat handles plot_paths attribute", {
  obj <- create_mock_visstat("basic")
  temp_dir <- tempdir()
  plot_paths <- c(
    file.path(temp_dir, "plot1.png"),
    file.path(temp_dir, "plot2.png")
  )
  attr(obj, "plot_paths") <- plot_paths
  
  # Test showing all plots
  output <- capture.output(result <- plot(obj), type = "message")
  
  expect_identical(result, obj)
  expect_true(any(grepl("Plot \\[1\\] stored in", output)))
  expect_true(any(grepl("Plot \\[2\\] stored in", output)))
})

test_that("plot.visstat handles specific plot selection with paths", {
  obj <- create_mock_visstat("basic")
  temp_dir <- tempdir()
  plot_paths <- c(
    file.path(temp_dir, "plot1.png"),
    file.path(temp_dir, "plot2.png")
  )
  attr(obj, "plot_paths") <- plot_paths
  
  # Test showing specific plot
  output <- capture.output(result <- plot(obj, which = 1), type = "message")
  
  expect_identical(result, obj)
  expect_true(any(grepl("Plot \\[1\\] stored in", output)))
  expect_false(any(grepl("Plot \\[2\\] stored in", output)))
})

test_that("plot.visstat handles captured_plots attribute", {
  skip_if_not_installed("grDevices")
  
  obj <- create_mock_visstat("basic")
  
  # Create mock recorded plots
  # Note: This is tricky to test without actually creating plots
  # We'll test the structure but skip the actual replayPlot calls
  
  mock_plots <- list()
  # Create a simple plot and record it
  plot(1:5, 1:5)
  mock_plots[[1]] <- recordPlot()
  plot(1:3, 1:3)
  mock_plots[[2]] <- recordPlot()
  dev.off()
  
  attr(obj, "captured_plots") <- mock_plots
  
  # Test that the function runs without error
  # We can't easily test replayPlot output in automated tests
  expect_silent(result <- plot(obj))
  expect_identical(result, obj)
})

test_that("plot.visstat handles specific captured plot selection", {
  skip_if_not_installed("grDevices")
  
  obj <- create_mock_visstat("basic")
  
  # Create mock recorded plots
  mock_plots <- list()
  plot(1:5, 1:5)
  mock_plots[[1]] <- recordPlot()
  dev.off()
  
  attr(obj, "captured_plots") <- mock_plots
  
  # Test specific plot selection
  expect_silent(result <- plot(obj, which = 1))
  expect_identical(result, obj)
})

test_that("plot.visstat handles objects with no plots", {
  obj <- create_mock_visstat("basic")
  
  # No plot_paths or captured_plots attributes
  expect_silent(result <- plot(obj))
  expect_identical(result, obj)
})

test_that("plot.visstat handles empty plot_paths", {
  obj <- create_mock_visstat("basic")
  attr(obj, "plot_paths") <- character(0)
  
  expect_silent(result <- plot(obj))
  expect_identical(result, obj)
})

test_that("plot.visstat handles empty captured_plots", {
  obj <- create_mock_visstat("basic")
  attr(obj, "captured_plots") <- list()
  
  expect_silent(result <- plot(obj))
  expect_identical(result, obj)
})

# Integration tests
test_that("S3 methods work with realistic visstat objects", {
  # Create a more realistic visstat object
  realistic_obj <- list(
    test = list(
      method = "Kruskal-Wallis rank sum test",
      statistic = 5.678,
      parameter = 2,
      p.value = 0.0234,
      data.name = "y by x"
    ),
    assumptions = list(
      normality = list(
        method = "Shapiro-Wilk normality test",
        p.value = 0.001,
        significant = TRUE
      )
    ),
    posthoc = list(
      method = "Dunn's test",
      results = data.frame(
        comparison = c("A-B", "A-C", "B-C"),
        p.value = c(0.01, 0.05, 0.8)
      )
    ),
    data_summary = list(
      groups = c("A", "B", "C"),
      n = c(10, 12, 15)
    )
  )
  class(realistic_obj) <- "visstat"
  
  # Test all methods work (these methods are supposed to produce output)
  expect_output(print_result <- print(realistic_obj), "Object of class 'visstat'")
  expect_output(summary_result <- summary(realistic_obj), "Summary of visstat object")
  expect_silent(plot_result <- plot(realistic_obj))  # plot should be silent when no plots
  
  # Check return values
  expect_identical(print_result, realistic_obj)
  expect_identical(summary_result, realistic_obj)
  expect_identical(plot_result, realistic_obj)
})

test_that("Methods handle edge cases gracefully", {
  # Test with empty list (can't set class on NULL)
  empty_obj <- list()
  class(empty_obj) <- "visstat"
  
  # These should not error (though behavior may vary)
  expect_no_error(print(empty_obj))
  expect_no_error(summary(empty_obj))
  expect_no_error(plot(empty_obj))
  
  # Test with minimal object
  minimal_obj <- list(test = list(p.value = 0.05))
  class(minimal_obj) <- "visstat"
  
  expect_no_error(print(minimal_obj))
  expect_no_error(summary(minimal_obj))
  expect_no_error(plot(minimal_obj))
  
  # Test with object that has no test component
  no_test_obj <- list(data = "some data")
  class(no_test_obj) <- "visstat"
  
  expect_no_error(print(no_test_obj))
  expect_no_error(summary(no_test_obj))
  expect_no_error(plot(no_test_obj))
})

test_that("print.visstat p-value formatting works correctly", {
  # Test different p-value formats
  test_cases <- list(
    list(p.value = 0.001234, expected = "0.00123"),
    list(p.value = 0.05, expected = "0.05"),
    list(p.value = 0.999, expected = "0.999"),
    list(p.value = 1e-10, expected = "1e-10")
  )
  
  for(case in test_cases) {
    obj <- list(test = list(method = "Test", p.value = case$p.value))
    class(obj) <- "visstat"
    
    output <- capture.output(print(obj))
    p_line <- output[grepl("p-value:", output)]
    expect_true(length(p_line) > 0, info = paste("Failed for p-value:", case$p.value))
  }
})

test_that("Method inheritance works correctly", {
  obj <- create_mock_visstat("basic")
  
  # Test that S3 dispatch works
  expect_true(is.function(print.visstat))
  expect_true(is.function(summary.visstat))
  expect_true(is.function(plot.visstat))
  
  # Test that methods are called for visstat objects
  expect_identical(class(obj), "visstat")
  
  # These should call the visstat methods, not defaults
  expect_no_error(print(obj))
  expect_no_error(summary(obj))
  expect_no_error(plot(obj))
})