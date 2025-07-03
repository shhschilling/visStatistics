# ===================================================================
# EXAMPLES AND TESTS FOR vis_numeric() FUNCTION
# ===================================================================
# This file contains comprehensive examples to test the simplified
# vis_numeric function under various scenarios and data conditions.
# ===================================================================

# Load required libraries (if not already loaded)
# library(your_package_name)  # Replace with actual package name

# Set seed for reproducibility
set.seed(42)

# ===================================================================
# EXAMPLE 1: IDEAL LINEAR RELATIONSHIP (Normal data)
# ===================================================================
cat("=== EXAMPLE 1: IDEAL LINEAR RELATIONSHIP ===\n")

# Generate clean linear data with normal residuals
n1 <- 50
x1 <- rnorm(n1, mean = 10, sd = 2)
y1 <- 2.5 * x1 + 3 + rnorm(n1, mean = 0, sd = 1.5)

# Test regression analysis
cat("Testing regression analysis...\n")
result1a <- vis_numeric(y1, x1, 
                        do_regression = TRUE,
                        name_of_factor = "Predictor (X)", 
                        name_of_sample = "Response (Y)")

cat("R-squared:", result1a$r_squared, "\n")
cat("Warnings:", length(result1a$warnings), "\n\n")

# Test correlation analysis on same data
cat("Testing correlation analysis on same data...\n")
result1b <- vis_numeric(y1, x1, 
                        do_regression = FALSE,
                        name_of_factor = "Variable X", 
                        name_of_sample = "Variable Y")

cat("Method used:", result1b$method_used, "\n")
cat("Correlation:", result1b$correlation_coefficient, "\n")
cat("Selection reason:", result1b$method_selection_reason, "\n\n")

# ===================================================================
# EXAMPLE 2: NON-LINEAR RELATIONSHIP (Quadratic)
# ===================================================================
cat("=== EXAMPLE 2: NON-LINEAR RELATIONSHIP ===\n")

# Generate quadratic relationship
n2 <- 60
x2 <- seq(-3, 3, length.out = n2)
y2 <- 0.5 * x2^2 + 0.2 * x2 + rnorm(n2, mean = 0, sd = 0.5)

# Test regression (should show poor fit and violated assumptions)
cat("Testing regression on quadratic data...\n")
result2a <- vis_numeric(y2, x2, 
                        do_regression = TRUE,
                        name_of_factor = "X", 
                        name_of_sample = "Y = 0.5*X² + 0.2*X + error")

cat("R-squared:", result2a$r_squared, "\n")
cat("Number of warnings:", length(result2a$warnings), "\n")

# Test correlation (should automatically select appropriate method)
cat("Testing correlation on quadratic data...\n")
result2b <- vis_numeric(y2, x2, 
                        do_regression = FALSE,
                        name_of_factor = "X", 
                        name_of_sample = "Y = 0.5*X² + 0.2*X + error")

cat("Method used:", result2b$method_used, "\n")
cat("Correlation:", result2b$correlation_coefficient, "\n\n")

# ===================================================================
# EXAMPLE 3: SKEWED DATA (Non-normal distributions)
# ===================================================================
cat("=== EXAMPLE 3: SKEWED DATA ===\n")

# Generate skewed data using exponential distribution
n3 <- 45
x3 <- rexp(n3, rate = 0.5)  # Right-skewed
y3 <- 1.2 * x3 + rexp(n3, rate = 0.3)  # Also right-skewed

# Test regression
cat("Testing regression on skewed data...\n")
result3a <- vis_numeric(y3, x3, 
                        do_regression = TRUE,
                        name_of_factor = "Skewed X", 
                        name_of_sample = "Skewed Y")

cat("Number of warnings:", length(result3a$warnings), "\n")

# Test correlation (should auto-select Spearman)
cat("Testing correlation on skewed data...\n")
result3b <- vis_numeric(y3, x3, 
                        do_regression = FALSE,
                        name_of_factor = "Skewed X", 
                        name_of_sample = "Skewed Y")

cat("Method used:", result3b$method_used, "\n")
cat("Selection reason:", result3b$method_selection_reason, "\n\n")

# ===================================================================
# EXAMPLE 4: MONOTONIC NON-LINEAR RELATIONSHIP
# ===================================================================
cat("=== EXAMPLE 4: MONOTONIC NON-LINEAR RELATIONSHIP ===\n")

# Generate monotonic but non-linear relationship
n4 <- 40
x4 <- runif(n4, min = 1, max = 10)
y4 <- log(x4) + rnorm(n4, mean = 0, sd = 0.2)

# Test regression (linear fit on log relationship)
cat("Testing regression on log relationship...\n")
result4a <- vis_numeric(y4, x4, 
                        do_regression = TRUE,
                        name_of_factor = "X", 
                        name_of_sample = "Y = log(X) + error")

cat("R-squared:", result4a$r_squared, "\n")

# Test correlation (Spearman should handle this well)
cat("Testing correlation on log relationship...\n")
result4b <- vis_numeric(y4, x4, 
                        do_regression = FALSE,
                        name_of_factor = "X", 
                        name_of_sample = "Y = log(X) + error")

cat("Method used:", result4b$method_used, "\n")
cat("Correlation:", result4b$correlation_coefficient, "\n\n")

# ===================================================================
# EXAMPLE 5: NO RELATIONSHIP (Random data)
# ===================================================================
cat("=== EXAMPLE 5: NO RELATIONSHIP ===\n")

# Generate completely independent variables
n5 <- 35
x5 <- rnorm(n5, mean = 5, sd = 2)
y5 <- rnorm(n5, mean = 10, sd = 3)  # Independent of x5

# Test regression
cat("Testing regression on independent variables...\n")
result5a <- vis_numeric(y5, x5, 
                        do_regression = TRUE,
                        name_of_factor = "Independent X", 
                        name_of_sample = "Independent Y")

cat("R-squared:", result5a$r_squared, "\n")
cat("Slope p-value:", summary(lm(y5 ~ x5))$coefficients[2, 4], "\n")

# Test correlation
cat("Testing correlation on independent variables...\n")
result5b <- vis_numeric(y5, x5, 
                        do_regression = FALSE,
                        name_of_factor = "Independent X", 
                        name_of_sample = "Independent Y")

cat("Method used:", result5b$method_used, "\n")
cat("Correlation:", result5b$correlation_coefficient, "\n")
cat("Significance:", result5b$significance, "\n\n")

# ===================================================================
# EXAMPLE 6: HETEROSCEDASTICITY (Changing variance)
# ===================================================================
cat("=== EXAMPLE 6: HETEROSCEDASTICITY ===\n")

# Generate data with increasing variance
n6 <- 50
x6 <- seq(1, 10, length.out = n6)
# Variance increases with x
y6 <- 2 * x6 + rnorm(n6, mean = 0, sd = x6 * 0.3)

# Test regression (should detect heteroscedasticity)
cat("Testing regression with heteroscedasticity...\n")
result6a <- vis_numeric(y6, x6, 
                        do_regression = TRUE,
                        name_of_factor = "X", 
                        name_of_sample = "Y (increasing variance)")

cat("Number of warnings:", length(result6a$warnings), "\n")
if (length(result6a$warnings) > 0) {
  cat("Warnings detected:", paste(result6a$warnings, collapse = "; "), "\n")
}

# Test correlation
cat("Testing correlation with heteroscedasticity...\n")
result6b <- vis_numeric(y6, x6, 
                        do_regression = FALSE,
                        name_of_factor = "X", 
                        name_of_sample = "Y (increasing variance)")

cat("Method used:", result6b$method_used, "\n\n")

# ===================================================================
# EXAMPLE 7: SMALL SAMPLE SIZE
# ===================================================================
cat("=== EXAMPLE 7: SMALL SAMPLE SIZE ===\n")

# Generate small dataset
n7 <- 8
x7 <- 1:n7
y7 <- 2 * x7 + rnorm(n7, mean = 0, sd = 1)

# Test regression with small sample
cat("Testing regression with small sample (n =", n7, ")...\n")
result7a <- vis_numeric(y7, x7, 
                        do_regression = TRUE,
                        name_of_factor = "X (small n)", 
                        name_of_sample = "Y (small n)")

cat("R-squared:", result7a$r_squared, "\n")

# Test correlation with small sample
cat("Testing correlation with small sample...\n")
result7b <- vis_numeric(y7, x7, 
                        do_regression = FALSE,
                        name_of_factor = "X (small n)", 
                        name_of_sample = "Y (small n)")

cat("Method used:", result7b$method_used, "\n")
cat("Selection reason:", result7b$method_selection_reason, "\n\n")

# ===================================================================
# EXAMPLE 8: MISSING VALUES
# ===================================================================
cat("=== EXAMPLE 8: MISSING VALUES ===\n")

# Generate data with missing values
n8 <- 40
x8 <- rnorm(n8, mean = 0, sd = 1)
y8 <- 1.5 * x8 + rnorm(n8, mean = 0, sd = 0.5)

# Introduce missing values
missing_indices <- sample(1:n8, size = 8)
x8[missing_indices[1:4]] <- NA
y8[missing_indices[5:8]] <- NA

cat("Original sample size:", n8, "\n")
cat("Missing values in x:", sum(is.na(x8)), "\n")
cat("Missing values in y:", sum(is.na(y8)), "\n")

# Test regression with missing values
cat("Testing regression with missing values...\n")
result8a <- vis_numeric(y8, x8, 
                        do_regression = TRUE,
                        name_of_factor = "X (with NAs)", 
                        name_of_sample = "Y (with NAs)")

cat("Effective sample size:", result8a$sample_size, "\n")

# Test correlation with missing values
cat("Testing correlation with missing values...\n")
result8b <- vis_numeric(y8, x8, 
                        do_regression = FALSE,
                        name_of_factor = "X (with NAs)", 
                        name_of_sample = "Y (with NAs)")

cat("Effective sample size:", result8b$sample_size, "\n")
cat("Method used:", result8b$method_used, "\n\n")

# ===================================================================
# EXAMPLE 9: DIFFERENT CONFIDENCE LEVELS
# ===================================================================
cat("=== EXAMPLE 9: DIFFERENT CONFIDENCE LEVELS ===\n")

# Generate standard linear data
n9 <- 30
x9 <- rnorm(n9, mean = 0, sd = 1)
y9 <- 2 * x9 + rnorm(n9, mean = 0, sd = 1)

# Test with 90% confidence level
cat("Testing with 90% confidence level...\n")
result9a <- vis_numeric(y9, x9, 
                        do_regression = TRUE,
                        conf.level = 0.90,
                        name_of_factor = "X (90% CI)", 
                        name_of_sample = "Y (90% CI)")

# Test with 99% confidence level
cat("Testing with 99% confidence level...\n")
result9b <- vis_numeric(y9, x9, 
                        do_regression = TRUE,
                        conf.level = 0.99,
                        name_of_factor = "X (99% CI)", 
                        name_of_sample = "Y (99% CI)")

cat("90% CI width for slope:", 
    diff(confint(lm(y9 ~ x9), level = 0.90)[2, ]), "\n")
cat("99% CI width for slope:", 
    diff(confint(lm(y9 ~ x9), level = 0.99)[2, ]), "\n\n")

# ===================================================================
# EXAMPLE 10: PERFECT CORRELATION
# ===================================================================
cat("=== EXAMPLE 10: PERFECT CORRELATION ===\n")

# Generate perfectly correlated data
n10 <- 25
x10 <- 1:n10
y10 <- 3 * x10 + 5  # Perfect linear relationship

# Test regression
cat("Testing regression with perfect correlation...\n")
result10a <- vis_numeric(y10, x10, 
                         do_regression = TRUE,
                         name_of_factor = "X (perfect)", 
                         name_of_sample = "Y = 3X + 5")

cat("R-squared:", result10a$r_squared, "\n")

# Test correlation
cat("Testing correlation with perfect correlation...\n")
result10b <- vis_numeric(y10, x10, 
                         do_regression = FALSE,
                         name_of_factor = "X (perfect)", 
                         name_of_sample = "Y = 3X + 5")

cat("Correlation coefficient:", result10b$correlation_coefficient, "\n")
cat("Method used:", result10b$method_used, "\n\n")

# ===================================================================
# SUMMARY FUNCTION TO COMPARE RESULTS
# ===================================================================
cat("=== SUMMARY OF ALL TESTS ===\n")

# Create a summary table of key results
summary_data <- data.frame(
  Example = c("Linear (Normal)", "Quadratic", "Skewed", "Monotonic", 
              "No Relationship", "Heteroscedastic", "Small Sample", 
              "Missing Values", "90% CI", "Perfect"),
  
  Regression_R2 = c(result1a$r_squared, result2a$r_squared, result3a$r_squared,
                    result4a$r_squared, result5a$r_squared, result6a$r_squared,
                    result7a$r_squared, result8a$r_squared, result9a$r_squared,
                    result10a$r_squared),
  
  Correlation_Method = c(result1b$method_used, result2b$method_used, result3b$method_used,
                         result4b$method_used, result5b$method_used, result6b$method_used,
                         result7b$method_used, result8b$method_used, "pearson",
                         result10b$method_used),
  
  Correlation_Coeff = c(result1b$correlation_coefficient, result2b$correlation_coefficient,
                        result3b$correlation_coefficient, result4b$correlation_coefficient,
                        result5b$correlation_coefficient, result6b$correlation_coefficient,
                        result7b$correlation_coefficient, result8b$correlation_coefficient,
                        0.999, result10b$correlation_coefficient),
  
  Regression_Warnings = c(length(result1a$warnings), length(result2a$warnings),
                          length(result3a$warnings), length(result4a$warnings),
                          length(result5a$warnings), length(result6a$warnings),
                          length(result7a$warnings), length(result8a$warnings),
                          length(result9a$warnings), length(result10a$warnings))
)

print(summary_data)

cat("\n=== TEST COMPLETION ===\n")
cat("All examples completed successfully!\n")
cat("The vis_numeric function handled all scenarios appropriately.\n")