# Examples demonstrating visstat() decision logic:
# When it switches between t-test, wilcox.test, aov, oneway.test, and kruskal.test
# Run each section to see the visual output and understand the decision process

library(visStatistics)

# Helper function to create data with specific properties
create_test_data <- function(scenario, n_per_group = 30, seed = 123) {
  set.seed(seed)
  
  if (scenario == "normal_equal_var_2groups") {
    # Normal data, equal variances, 2 groups -> t.test()
    group1 <- rnorm(n_per_group, mean = 0, sd = 1)
    group2 <- rnorm(n_per_group, mean = 2, sd = 1)  # Different mean, same variance
    
    df <- data.frame(
      response = c(group1, group2),
      group = factor(rep(c("A", "B"), each = n_per_group))
    )
    
  } else if (scenario == "normal_unequal_var_2groups") {
    # Normal data, unequal variances, 2 groups -> t.test() with var.equal=FALSE (Welch)
    group1 <- rnorm(n_per_group, mean = 0, sd = 0.5)
    group2 <- rnorm(n_per_group, mean = 2, sd = 2.5)  # Different mean AND variance
    
    df <- data.frame(
      response = c(group1, group2),
      group = factor(rep(c("A", "B"), each = n_per_group))
    )
    
  } else if (scenario == "nonnormal_2groups") {
    # Non-normal data, 2 groups -> wilcox.test()
    group1 <- rexp(n_per_group, rate = 1)  # Exponential distribution
    group2 <- rexp(n_per_group, rate = 0.5)  # Different rate = different location
    
    df <- data.frame(
      response = c(group1, group2),
      group = factor(rep(c("A", "B"), each = n_per_group))
    )
    
  } else if (scenario == "normal_equal_var_3groups") {
    # Normal data, equal variances, 3+ groups -> aov()
    group1 <- rnorm(n_per_group, mean = 0, sd = 1)
    group2 <- rnorm(n_per_group, mean = 2, sd = 1)
    group3 <- rnorm(n_per_group, mean = 4, sd = 1)
    
    df <- data.frame(
      response = c(group1, group2, group3),
      group = factor(rep(c("A", "B", "C"), each = n_per_group))
    )
    
  } else if (scenario == "normal_unequal_var_3groups") {
    # Normal data, unequal variances, 3+ groups -> oneway.test() (Welch ANOVA)
    group1 <- rnorm(n_per_group, mean = 0, sd = 0.5)
    group2 <- rnorm(n_per_group, mean = 2, sd = 1.5)
    group3 <- rnorm(n_per_group, mean = 4, sd = 3.0)
    
    df <- data.frame(
      response = c(group1, group2, group3),
      group = factor(rep(c("A", "B", "C"), each = n_per_group))
    )
    
  } else if (scenario == "nonnormal_3groups") {
    # Non-normal data, 3+ groups -> kruskal.test()
    group1 <- rexp(n_per_group, rate = 2)
    group2 <- rgamma(n_per_group, shape = 2, scale = 1)
    group3 <- runif(n_per_group, min = 0, max = 4)
    
    df <- data.frame(
      response = c(group1, group2, group3),
      group = factor(rep(c("A", "B", "C"), each = n_per_group))
    )
    
  } else if (scenario == "small_sample_normal_2groups") {
    # Small sample, normal data, 2 groups -> should test normality first
    set.seed(seed)
    group1 <- rnorm(8, mean = 0, sd = 1)  # Small sample
    group2 <- rnorm(8, mean = 2, sd = 1)
    
    df <- data.frame(
      response = c(group1, group2),
      group = factor(rep(c("A", "B"), each = 8))
    )
    
  } else if (scenario == "small_sample_nonnormal_2groups") {
    # Small sample, non-normal data, 2 groups -> wilcox.test()
    set.seed(seed)
    group1 <- rexp(8, rate = 1)  # Small sample, non-normal
    group2 <- rexp(8, rate = 0.5)
    
    df <- data.frame(
      response = c(group1, group2),
      group = factor(rep(c("A", "B"), each = 8))
    )
  }
  
  return(df)
}

# ============================================================================
# EXAMPLE 1: Two Groups, One Group Normal Data, other group normal with outlier,  Equal Variances -> t.test(), as n>30 in both groups---
# ============================================================================

cat("\n=== EXAMPLE 1: Two Groups, Normal, Equal Variances ===\n")
cat("Expected: t.test() with equal variances\n")
cat("Assumption tests should PASS (Levene & Bartlett p > 0.05)\n\n")

data1 <- create_test_data("normal_equal_var_2groups", n_per_group = 35, seed = 100)
result1 <- visstat(data1, "response", "group")
print(result1)  


# ============================================================================
# EXAMPLE 2: Two Groups, One gorup Normal Data, Unequal Variances -> Welch t.test()-----
# ============================================================================
cat("\n=== EXAMPLE 2: Two Groups, Normal, Unequal Variances ===\n")
cat("Expected: Welch t.test() (var.equal = FALSE)\n") 
cat("Assumption tests should FAIL (Levene & Bartlett p < 0.05)\n\n")

data2 <- create_test_data("normal_unequal_var_2groups", n_per_group = 35, seed = 200)
result2 <- visstat(data2, "response", "group")
print(result2)
#note: add levenep test two test loigc
# ============================================================================
# EXAMPLE 3: Two Groups, Non-normal Data -> wilcox.test()-----
# ============================================================================
cat("\n=== EXAMPLE 3: Two Groups, Non-normal Data ===\n")
cat("Expected: wilcox.test() (Mann-Whitney U)\n")
cat("Normality tests should FAIL (Shapiro p < 0.05)\n\n")

data3 <- create_test_data("nonnormal_2groups", n_per_group = 25, seed = 300)
result3 <- visstat(data3, "response", "group")
print(result3)

# ============================================================================
# EXAMPLE 4: Three Groups, Normal Data, Equal Variances -> aov()----------
# ============================================================================
cat("\n=== EXAMPLE 4: Three Groups, Normal, Equal Variances ===\n")
cat("Expected: aov() with TukeyHSD()\n")
cat("All assumption tests should PASS\n\n")

data4 <- create_test_data("normal_equal_var_3groups", n_per_group = 30, seed = 400)
result4 <- visstat(data4, "response", "group")
print(result4)

# ============================================================================
# EXAMPLE 5: Three Groups, Non Normal Data, Unequal Variances -> kruskal.test()----
# ============================================================================
cat("\n=== EXAMPLE 5: Three Groups, Normal, Unequal Variances ===\n")
cat("Expected: kruskal.test() with pairwise.wilcox.test()\n")
cat(" Variance tests should FAIL\n\n")

data5 <- create_test_data("normal_unequal_var_3groups", n_per_group = 30, seed = 500)
result5 <- visstat(data5, "response", "group")
print(result5)

# ============================================================================
# EXAMPLE 6: Three Groups, Non-normal Data -> kruskal.test()----
# ============================================================================
cat("\n=== EXAMPLE 6: Three Groups, Non-normal Data ===\n")
cat("Expected: kruskal.test() with pairwise.wilcox.test()\n")
cat("Normality tests should FAIL\n\n")

data6 <- create_test_data("nonnormal_3groups", n_per_group = 25, seed = 600)
result6 <- visstat(data6, "response", "group")
print(result6)

# ============================================================================
# EXAMPLE 7: Small Sample, Normal Data -> Welch t.test() -----
# ============================================================================
cat("\n=== EXAMPLE 7: Small Sample, Normal Data ===\n")
cat("Expected: t.test() if Shapiro test passes, wilcox.test() if fails\n")
cat("With small samples, normality testing is crucial\n\n")

data7 <- create_test_data("small_sample_normal_2groups", seed = 700)
result7 <- visstat(data7, "response", "group")
print(result7)


# ============================================================================
# SUMMARY: Decision Logic
# ============================================================================

cat("=== VISSTAT DECISION LOGIC SUMMARY ===\n")
cat("
TWO GROUPS:
1. If both groups n > 30: t.test() (CLT applies)
2. If n ≤ 30: Check normality with Shapiro test
   - If normal (p > α): t.test()  
   - If non-normal (p ≤ α): wilcox.test()

THREE+ GROUPS:
1. Check normality with Shapiro test on residuals
   - If normal: Check variance homogeneity
     * Equal variances (Levene p > α): aov() + TukeyHSD()
     * Unequal variances (Levene p ≤ α): oneway.test() + TukeyHSD()
   - If non-normal: kruskal.test() + pairwise.wilcox.test()

KEY ASSUMPTION TESTS:
- Normality: shapiro.test() (also ad.test() as addtional reference)
- Variance homogeneity: levene.test() (my implementation!)
- Also: bartlett.test() (less robust to non-normality)

VISUAL OUTPUT:
- Assumption plots show: box plots, residual plots, Q-Q plots
- Test results displayed with assumption test p-values
- Main analysis plots show the chosen statistical test results
")

# ============================================================================
# BONUS: Extreme example showing all assumption violations
# ============================================================================
cat("\n=== BONUS EXAMPLE: Multiple Assumption Violations ===\n")
cat("Extreme case: Non-normal + Unequal variances + Outliers\n")
cat("Expected: Non-parametric test (wilcox or kruskal)\n\n")

set.seed(999)
# Create pathological data
group1 <- c(rnorm(20, 0, 0.2), 10)  # Normal + outlier, small variance
group2 <- rexp(20, 0.5)             # Exponential, medium scale  
group3 <- c(runif(15, 0, 2), 15, 20)  # Uniform + outliers, large range

extreme_data <- data.frame(
  response = c(group1, group2, group3),
  group = factor(rep(c("A", "B", "C"), c(21, 20, 17)))
)

cat("Data summary:\n")
print(aggregate(response ~ group, extreme_data, function(x) c(mean=mean(x), sd=sd(x), min=min(x), max=max(x))))

result_extreme <- visstat(extreme_data, "response", "group")
print(result_extreme)