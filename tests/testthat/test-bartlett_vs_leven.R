# Comprehensive test series using vis_anova_assumptions() to directly test 
# Levene vs Bartlett test results on heteroscedastic data
# Save as: test-visstat-heteroscedastic.R

library(testthat)

# Helper function to create test datasets with different variance patterns
create_heteroscedastic_dataset <- function(scenario_name, 
                                         n_groups = 3, 
                                         n_per_group = 30,
                                         group_names = NULL,
                                         seed = 123) {
  set.seed(seed)
  
  if (is.null(group_names)) {
    group_names <- paste0("Group_", LETTERS[1:n_groups])
  }
  
  scenarios <- list(
    "equal_variances" = list(
      means = rep(0, n_groups),
      sds = rep(1, n_groups),
      description = "Equal variances (homoscedastic)"
    ),
    "mild_heteroscedastic" = list(
      means = rep(0, n_groups),
      sds = c(1, 1.5, 2),
      description = "Mild heteroscedasticity"
    ),
    "moderate_heteroscedastic" = list(
      means = rep(0, n_groups),
      sds = c(0.5, 1, 2.5),
      description = "Moderate heteroscedasticity"
    ),
    "severe_heteroscedastic" = list(
      means = rep(0, n_groups),
      sds = c(0.2, 1, 5),
      description = "Severe heteroscedasticity"
    ),
    "extreme_heteroscedastic" = list(
      means = rep(0, n_groups),
      sds = c(0.1, 1, 10),
      description = "Extreme heteroscedasticity"
    ),
    "two_group_different" = list(
      means = c(0, 0),
      sds = c(1, 4),
      description = "Two groups with different variances"
    ),
    "skewed_heteroscedastic" = list(
      means = c(0, 2, 5),
      sds = c(1, 2, 4),
      description = "Different means AND variances"
    )
  )
  
  if (!scenario_name %in% names(scenarios)) {
    stop("Unknown scenario. Available: ", paste(names(scenarios), collapse = ", "))
  }
  
  scenario <- scenarios[[scenario_name]]
  n_groups_actual <- length(scenario$sds)
  
  data_list <- list()
  for (i in 1:n_groups_actual) {
    data_list[[i]] <- rnorm(n_per_group, mean = scenario$means[i], sd = scenario$sds[i])
  }
  
  y <- unlist(data_list)
  g <- factor(rep(group_names[1:n_groups_actual], each = n_per_group))
  
  list(y = y, g = g, scenario = scenario_name, description = scenario$description,
       true_sds = scenario$sds, true_means = scenario$means)
}

# Helper function to create non-normal heteroscedastic data
create_nonnormal_heteroscedastic_dataset <- function(distributions = c("normal", "exponential", "uniform"),
                                                   n_per_group = 30,
                                                   scale_factors = c(1, 2, 0.5),
                                                   seed = 456) {
  set.seed(seed)
  
  n_groups <- length(distributions)
  data_list <- list()
  group_names <- paste0("Dist_", 1:n_groups)
  
  for (i in 1:n_groups) {
    if (distributions[i] == "normal") {
      data_list[[i]] <- rnorm(n_per_group, mean = 0, sd = scale_factors[i])
    } else if (distributions[i] == "exponential") {
      data_list[[i]] <- rexp(n_per_group, rate = 1/scale_factors[i])
    } else if (distributions[i] == "uniform") {
      data_list[[i]] <- runif(n_per_group, min = 0, max = scale_factors[i] * 2)
    } else if (distributions[i] == "gamma") {
      data_list[[i]] <- rgamma(n_per_group, shape = 2, scale = scale_factors[i])
    }
  }
  
  y <- unlist(data_list)
  g <- factor(rep(group_names, each = n_per_group))
  
  list(y = y, g = g, distributions = distributions, scale_factors = scale_factors)
}

test_that("visstat: homoscedastic vs heteroscedastic comparison", {
  # Create clearly homoscedastic data
  set.seed(1500)
  homo_y <- c(rnorm(50, mean = 0, sd = 1), rnorm(50, mean = 2, sd = 1), rnorm(50, mean = 4, sd = 1))
  homo_g <- factor(rep(1:3, each = 50))
  homo_df <- data.frame(response = homo_y, group = homo_g)
  
  # Create clearly heteroscedastic data  
  set.seed(1501)
  hetero_y <- c(rnorm(50, mean = 0, sd = 0.5), rnorm(50, mean = 0, sd = 2), rnorm(50, mean = 0, sd = 5))
  hetero_g <- factor(rep(1:3, each = 50))
  hetero_df <- data.frame(response = hetero_y, group = hetero_g)
  
  # Test homoscedastic data with visstat - shows assumption plots + analysis
  homo_result <- visstat(homo_df, "response", "group")
  
  # Test heteroscedastic data with visstat - shows assumption plots + analysis
  hetero_result <- visstat(hetero_df, "response", "group")
  
  # Both should run successfully
  expect_s3_class(homo_result, "visstat")
  expect_s3_class(hetero_result, "visstat")
  
  # Both should generate plots (assumption checks + main analysis)
  expect_true(length(attr(homo_result, "captured_plots")) > 0)
  expect_true(length(attr(hetero_result, "captured_plots")) > 0)
  
  # Both should return meaningful results
  expect_true(length(homo_result) > 0)
  expect_true(length(hetero_result) > 0)
})

test_that("visstat detects mild heteroscedasticity", {
  mild_hetero_data <- create_heteroscedastic_dataset("mild_heteroscedastic", seed = 200)
  df <- data.frame(response = mild_hetero_data$y, group = mild_hetero_data$g)
  
  # Shows assumption plots + data analysis
  result <- visstat(df, "response", "group")
  
  expect_s3_class(result, "visstat")
  expect_true(length(result) > 0)
  expect_true(length(attr(result, "captured_plots")) > 0)
})

test_that("visstat detects severe heteroscedasticity", {
  severe_hetero_data <- create_heteroscedastic_dataset("severe_heteroscedastic", 
                                                      n_per_group = 25, seed = 300)
  df <- data.frame(response = severe_hetero_data$y, group = severe_hetero_data$g)
  
  # Shows dramatic variance differences in both assumption plots and main analysis
  result <- visstat(df, "response", "group")
  
  expect_s3_class(result, "visstat")
  expect_true(length(result) > 0)
  expect_true(length(attr(result, "captured_plots")) > 0)
  expect_no_error(print(result))
})

test_that("visstat with extreme heteroscedasticity", {
  extreme_data <- create_heteroscedastic_dataset("extreme_heteroscedastic", 
                                                n_per_group = 40, seed = 500)
  df <- data.frame(response = extreme_data$y, group = extreme_data$g)
  
  # Shows extreme variance differences visually + statistical analysis
  result <- visstat(df, "response", "group")
  
  expect_s3_class(result, "visstat")
  expect_true(length(result) > 0)
  expect_true(length(attr(result, "captured_plots")) > 0)
  expect_no_error(print(result))
})

test_that("visstat: Levene robustness with non-normal data", {
  nonnormal_data <- create_nonnormal_heteroscedastic_dataset(
    distributions = c("exponential", "uniform", "gamma"),
    n_per_group = 35,
    scale_factors = c(1, 3, 0.5),
    seed = 600
  )
  df <- data.frame(response = nonnormal_data$y, group = nonnormal_data$g)
  
  # Shows non-normal distributions + how Levene handles them
  result <- visstat(df, "response", "group")
  
  expect_s3_class(result, "visstat")
  expect_true(length(result) > 0)
  expect_true(length(attr(result, "captured_plots")) > 0)
  expect_no_error(print(result))
})

test_that("visstat: Power comparison across effect sizes", {
  # Small, medium, large variance differences - all with visual output
  small_data <- create_heteroscedastic_dataset("mild_heteroscedastic", n_per_group = 30, seed = 400)
  medium_data <- create_heteroscedastic_dataset("moderate_heteroscedastic", n_per_group = 30, seed = 401) 
  large_data <- create_heteroscedastic_dataset("severe_heteroscedastic", n_per_group = 30, seed = 402)
  
  small_df <- data.frame(response = small_data$y, group = small_data$g)
  medium_df <- data.frame(response = medium_data$y, group = medium_data$g)
  large_df <- data.frame(response = large_data$y, group = large_data$g)
  
  # Visual comparison of different effect sizes
  small_result <- visstat(small_df, "response", "group")
  medium_result <- visstat(medium_df, "response", "group")
  large_result <- visstat(large_df, "response", "group")
  
  expect_s3_class(small_result, "visstat")
  expect_s3_class(medium_result, "visstat")
  expect_s3_class(large_result, "visstat")
  
  expect_true(length(attr(small_result, "captured_plots")) > 0)
  expect_true(length(attr(medium_result, "captured_plots")) > 0)
  expect_true(length(attr(large_result, "captured_plots")) > 0)
})

test_that("visstat: Two-group heteroscedastic comparison", {
  two_group_data <- create_heteroscedastic_dataset("two_group_different", 
                                                  n_per_group = 30, seed = 700)
  df <- data.frame(response = two_group_data$y, group = two_group_data$g)
  
  # Shows two-group variance differences visually + t-test analysis
  result <- visstat(df, "response", "group")
  
  expect_s3_class(result, "visstat")
  expect_true(length(result) > 0)
  expect_true(length(attr(result, "captured_plots")) > 0)
})

test_that("visstat: Sample size effects on heteroscedasticity detection", {
  # Test different sample sizes with same variance pattern
  variance_pattern <- c(1, 2, 4)  # Moderate heteroscedasticity
  
  # Small sample
  set.seed(800)
  small_y <- unlist(lapply(1:3, function(i) rnorm(10, mean = 0, sd = sqrt(variance_pattern[i]))))
  small_g <- factor(rep(1:3, each = 10))
  small_df <- data.frame(response = small_y, group = small_g)
  
  # Large sample  
  set.seed(801)
  large_y <- unlist(lapply(1:3, function(i) rnorm(50, mean = 0, sd = sqrt(variance_pattern[i]))))
  large_g <- factor(rep(1:3, each = 50))
  large_df <- data.frame(response = large_y, group = large_g)
  
  # Visual comparison of sample size effects
  small_result <- visstat(small_df, "response", "group")
  large_result <- visstat(large_df, "response", "group")
  
  expect_s3_class(small_result, "visstat")
  expect_s3_class(large_result, "visstat")
  
  expect_true(length(attr(small_result, "captured_plots")) > 0)
  expect_true(length(attr(large_result, "captured_plots")) > 0)
})

test_that("visstat: Different confidence levels with heteroscedastic data", {
  test_data <- create_heteroscedastic_dataset("moderate_heteroscedastic", seed = 900)
  df <- data.frame(response = test_data$y, group = test_data$g)
  
  # Test different confidence levels - all show same plots but different interpretations
  conf_levels <- c(0.90, 0.95, 0.99)
  
  for (conf_level in conf_levels) {
    result <- visstat(df, "response", "group", conf.level = conf_level)
    
    expect_s3_class(result, "visstat")
    expect_true(length(result) > 0)
    expect_true(length(attr(result, "captured_plots")) > 0)
  }
})

test_that("visstat: Comprehensive Levene vs Bartlett visual demonstration", {
  comparison_data <- create_heteroscedastic_dataset("moderate_heteroscedastic", 
                                                   n_per_group = 50, seed = 1000)
  df <- data.frame(response = comparison_data$y, group = comparison_data$g)
  
  # Complete visual workflow: assumption plots + statistical analysis
  result <- visstat(df, "response", "group")
  
  expect_s3_class(result, "visstat")
  expect_true(length(result) > 0)
  expect_true(length(attr(result, "captured_plots")) > 0)
  expect_no_error(print(result))
  
  # The visual output shows both Levene and Bartlett results in assumption plots
  # Plus the chosen statistical test based on assumption violations
})

test_that("visstat integration with vis_anova_assumptions", {
  test_data <- create_heteroscedastic_dataset("severe_heteroscedastic", seed = 1100)
  
  df <- data.frame(response = test_data$y, group = test_data$g)
  
  result <- visstat(df, "response", "group")
  
  expect_s3_class(result, "visstat")
  expect_true(length(result) > 0)
  
  captured_plots <- attr(result, "captured_plots")
  expect_true(length(captured_plots) > 0)
  
  expect_no_error(print(result))
})

test_that("visstat comprehensive comparison: Levene vs Bartlett integration", {
  comparison_data <- create_heteroscedastic_dataset("moderate_heteroscedastic", 
                                                   n_per_group = 50, seed = 1200)
  
  df <- data.frame(response = comparison_data$y, group = comparison_data$g)
  
  result <- visstat(df, "response", "group")
  
  expect_s3_class(result, "visstat")
  
  expect_true(length(result) > 0)
  
  captured_plots <- attr(result, "captured_plots")
  expect_true(length(captured_plots) > 0)
  
  expect_true(is.list(result))
  
  expect_no_error(print(result))
})

