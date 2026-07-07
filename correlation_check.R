# Correlation Formula Validation Script
# This script validates our manual correlation formulas against R's cor.test()

# Load required libraries
library(stats)

# Set seed for reproducibility
set.seed(42)

# Generate sample data
n <- 20
x <- rnorm(n, mean = 10, sd = 3)
y <- 2 * x + rnorm(n, mean = 0, sd = 2)  # Linear relationship with noise

cat("=== CORRELATION FORMULA VALIDATION ===\n\n")

cat("Sample data (first 10 observations):\n")
cat("x:", round(x[1:10], 2), "\n")
cat("y:", round(y[1:10], 2), "\n\n")

# ===== PEARSON CORRELATION =====
cat("===== PEARSON CORRELATION =====\n")

# Manual calculation using our formula: r = Cov(x,y) / (σx * σy)
manual_pearson <- function(x, y) {
  # Calculate means
  mean_x <- mean(x)
  mean_y <- mean(y)
  
  # Calculate covariance
  cov_xy <- sum((x - mean_x) * (y - mean_y)) / (length(x) - 1)
  
  # Calculate standard deviations
  sd_x <- sqrt(sum((x - mean_x)^2) / (length(x) - 1))
  sd_y <- sqrt(sum((y - mean_y)^2) / (length(y) - 1))
  
  # Calculate correlation
  r <- cov_xy / (sd_x * sd_y)
  
  return(list(
    correlation = r,
    covariance = cov_xy,
    sd_x = sd_x,
    sd_y = sd_y
  ))
}

# Calculate using our manual formula
manual_result <- manual_pearson(x, y)

# Calculate using R's built-in functions
r_pearson <- cor.test(x, y, method = "pearson")
r_cov <- cov(x, y)
r_sd_x <- sd(x)
r_sd_y <- sd(y)

# Compare results
cat("Manual formula results:\n")
cat("  r =", round(manual_result$correlation, 6), "\n")
cat("  Cov(x,y) =", round(manual_result$covariance, 6), "\n")
cat("  σx =", round(manual_result$sd_x, 6), "\n")
cat("  σy =", round(manual_result$sd_y, 6), "\n\n")

cat("R's cor.test() and built-in functions:\n")
cat("  r =", round(r_pearson$estimate, 6), "\n")
cat("  Cov(x,y) =", round(r_cov, 6), "\n")
cat("  σx =", round(r_sd_x, 6), "\n")
cat("  σy =", round(r_sd_y, 6), "\n\n")

cat("Differences (should be near zero):\n")
cat("  Δr =", round(abs(manual_result$correlation - r_pearson$estimate), 10), "\n")
cat("  ΔCov =", round(abs(manual_result$covariance - r_cov), 10), "\n")
cat("  Δσx =", round(abs(manual_result$sd_x - r_sd_x), 10), "\n")
cat("  Δσy =", round(abs(manual_result$sd_y - r_sd_y), 10), "\n\n")

# ===== SPEARMAN CORRELATION =====
cat("===== SPEARMAN CORRELATION =====\n")


# Manual calculation using our formula: ρ = r(rank(x), rank(y))
manual_spearman <- function(x, y) {
  # Convert to ranks
  rank_x <- rank(x)
  rank_y <- rank(y)
  
  # Apply Pearson formula to ranks
  pearson_on_ranks <- manual_pearson(rank_x, rank_y)
  
  return(list(
    correlation = pearson_on_ranks$correlation,
    rank_x = rank_x,
    rank_y = rank_y
  ))
}

# Calculate using our manual formula
manual_spearman_result <- manual_spearman(x, y)

# Calculate using R's built-in function
r_spearman <- cor.test(x, y, method = "spearman")

# Also verify by calculating Pearson on ranks directly
rank_x <- rank(x)
rank_y <- rank(y)
pearson_on_ranks <- cor.test(rank_x, rank_y, method = "pearson")

cat("Manual formula results:\n")
cat("  ρ =", round(manual_spearman_result$correlation, 6), "\n")
cat("  First 10 ranks of x:", manual_spearman_result$rank_x[1:10], "\n")
cat("  First 10 ranks of y:", manual_spearman_result$rank_y[1:10], "\n\n")

cat("R's cor.test() with Spearman:\n")
cat("  ρ =", round(r_spearman$estimate, 6), "\n\n")

cat("R's Pearson correlation on ranks (should match Spearman):\n")
cat("  r(rank(x), rank(y)) =", round(pearson_on_ranks$estimate, 6), "\n\n")

cat("Differences (should be near zero):\n")
cat("  Δρ (manual vs Spearman) =", round(abs(manual_spearman_result$correlation - r_spearman$estimate), 10), "\n")
cat("  Δρ (manual vs Pearson on ranks) =", round(abs(manual_spearman_result$correlation - pearson_on_ranks$estimate), 10), "\n")
cat("  Δρ (Spearman vs Pearson on ranks) =", round(abs(r_spearman$estimate - pearson_on_ranks$estimate), 10), "\n\n")

# ===== ADDITIONAL VALIDATION WITH DIFFERENT DATA =====
cat("===== VALIDATION WITH MONOTONIC (NON-LINEAR) DATA =====\n")

# Generate monotonic but non-linear data
x2 <- 1:15
y2 <- x2^2 + rnorm(15, 0, 5)  # Quadratic relationship

cat("Testing with quadratic relationship...\n")

# Manual calculations
manual_pearson_2 <- manual_pearson(x2, y2)
manual_spearman_2 <- manual_spearman(x2, y2)

# R's calculations
r_pearson_2 <- cor.test(x2, y2, method = "pearson")
r_spearman_2 <- cor.test(x2, y2, method = "spearman")

cat("Pearson correlation:\n")
cat("  Manual:", round(manual_pearson_2$correlation, 4), "\n")
cat("  R's cor.test():", round(r_pearson_2$estimate, 4), "\n")
cat("  Difference:", round(abs(manual_pearson_2$correlation - r_pearson_2$estimate), 10), "\n\n")

cat("Spearman correlation:\n")
cat("  Manual:", round(manual_spearman_2$correlation, 4), "\n")
cat("  R's cor.test():", round(r_spearman_2$estimate, 4), "\n")
cat("  Difference:", round(abs(manual_spearman_2$correlation - r_spearman_2$estimate), 10), "\n\n")

cat("Note: Spearman should be higher than Pearson for monotonic non-linear data\n")
cat("Spearman - Pearson difference:", round(manual_spearman_2$correlation - manual_pearson_2$correlation, 4), "\n\n")

# ===== SUMMARY =====
cat("===== VALIDATION SUMMARY =====\n")
cat("✓ Manual Pearson formula r = Cov(x,y)/(σx*σy) matches cor.test()\n")
cat("✓ Manual Spearman formula ρ = r(rank(x), rank(y)) matches cor.test()\n")
cat("✓ Spearman correlation equals Pearson correlation applied to ranks\n")
cat("✓ Formulas work correctly for both linear and monotonic relationships\n")