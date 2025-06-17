test_that("visstat_core - Wilcoxon or t-test path", {
  # Case 1: Large samples (>30) should trigger t-test
  set.seed(123)
  large_data <- data.frame(
    values = c(rnorm(35, mean = 5, sd = 1), rnorm(35, mean = 6, sd = 1)),
    group = factor(rep(c("A", "B"), each = 35))
  )
  
  result_large <- visstat_core(large_data, "values", "group")
  expect_s3_class(result_large, "visstat")
  expect_true("t-test-statistics" %in% names(result_large))
  expect_true("Shapiro-Wilk-test_sample1" %in% names(result_large))
  expect_true("Shapiro-Wilk-test_sample2" %in% names(result_large))
  
  # Check t-test structure
  t_stats <- result_large[["t-test-statistics"]]
  expect_s3_class(t_stats, "htest")
  expect_equal(t_stats$method, "Welch Two Sample t-test")
  
  # Case 2: Small normal samples should trigger t-test
  set.seed(123)
  normal_data <- data.frame(
    values = c(rnorm(15, mean = 5, sd = 1), rnorm(15, mean = 6, sd = 1)),
    group = factor(rep(c("A", "B"), each = 15))
  )
  
  result_normal <- visstat_core(normal_data, "values", "group")
  expect_s3_class(result_normal, "visstat")
  expect_true("t-test-statistics" %in% names(result_normal))
  
  # Case 3: Small non-normal samples should trigger Wilcoxon
  set.seed(123)
  skewed_data <- data.frame(
    values = c(rexp(10, rate = 0.2), rexp(10, rate = 0.8)),
    group = factor(rep(c("A", "B"), each = 10))
  )
  
  result_wilcox <- visstat_core(skewed_data, "values", "group")
  expect_s3_class(result_wilcox, "visstat")
  expect_true("statsWilcoxon" %in% names(result_wilcox))
  expect_true("statsBoxplot" %in% names(result_wilcox))
  
  # Check Wilcoxon structure
  wilcox_stats <- result_wilcox$statsWilcoxon
  expect_s3_class(wilcox_stats, "htest")
  expect_true(grepl("Wilcoxon", wilcox_stats$method))
  
  # Case 4: Data with missing values should still work
  na_data <- data.frame(
    values = c(rnorm(20, 5), NA, rnorm(20, 6), NA),
    group = factor(rep(c("A", "B"), each = 21))
  )
  
  result_na <- visstat_core(na_data, "values", "group")
  expect_s3_class(result_na, "visstat")
  expect_true("t-test-statistics" %in% names(result_na) || 
                "statsWilcoxon" %in% names(result_na))
  
  # Case 5: Check S3 class and attributes are set
  expect_true(!is.null(attr(result_large, "plot_paths")) || 
                !is.null(attr(result_large, "captured_plots")))
  
  # Error cases that should NOT go to this path
  # More than 2 factor levels should go to ANOVA path
  multi_group <- data.frame(
    values = rnorm(30),
    group = factor(rep(c("A", "B", "C"), each = 10))
  )
  
  result_multi <- visstat_core(multi_group, "values", "group")
  expect_false("t-test-statistics" %in% names(result_multi))
  expect_false("statsWilcoxon" %in% names(result_multi))
  
  # Both factor should go to Chi-square path
  factor_data <- data.frame(
    cat1 = factor(rep(c("X", "Y"), 25)),
    cat2 = factor(rep(c("A", "B"), each = 25))
  )
  
  result_factors <- visstat_core(factor_data, "cat1", "cat2")
  expect_false("t-test-statistics" %in% names(result_factors))
  expect_false("statsWilcoxon" %in% names(result_factors))
})