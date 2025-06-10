# ============================================================================
# CORRECTED: REPRODUCING R's EXACT W STATISTIC (Small Samples)
# ============================================================================

rm(list = ls())


# ============================================================================
# EXAMPLE 1: Test with clear data
# ============================================================================

x <- c(1, 2, 4)      # Sample 1: n = 3
y <- c(3, 5, 6, 7)   # Sample 2: m = 4

n <- length(x)
m <- length(y)
N <- n + m           # Total sample size = 7

cat("EXAMPLE DATA:\n")
cat("x =", x, "(n =", n, ")\n") 
cat("y =", y, "(m =", m, ")\n")
cat("Total N =", N, "\n\n")

# ============================================================================
# STEP 1: Get R's actual W statistic
# ============================================================================

result <- wilcox.test(x, y, exact = TRUE)
W_R <- as.numeric(result$statistic)

cat("STEP 1: R's Output\n")
cat("==================\n")
cat("R's wilcox.test(x, y) W =", W_R, "\n\n")

# ============================================================================
# STEP 2: Manual ranking and calculations
# ============================================================================

cat("STEP 2: Manual Ranking\n")

cat("======================\n")

combined <- c(x, y)
ranks <- rank(combined)

cat("Combined data:", combined, "\n")
cat("Sorted data:  ", sort(combined), "\n")
cat("Ranks:        ", ranks, "\n\n")

# Calculate rank sums
ranks_x <- ranks[1:n]
ranks_y <- ranks[(n+1):N]

R_x <- sum(ranks_x)
R_y <- sum(ranks_y)

cat("X sample ranks:", ranks_x, "→ R_x =", R_x, "\n")
cat("Y sample ranks:", ranks_y, "→ R_y =", R_y, "\n")
cat("Total: R_x + R_y =", R_x + R_y, "(should equal", N*(N+1)/2, ")\n\n")

# ============================================================================
# STEP 3: TEST BOTH POSSIBLE FORMULAS
# ============================================================================

cat("STEP 3: Testing Possible Formulas\n")
cat("==================================\n")

# Formula 1: W = R_x - n(n+1)/2 (using first sample size)
formula1 <- R_x - n*(n+1)/2
cat("Formula 1: W = R_x - n(n+1)/2\n")
cat("  W =", R_x, "-", n*(n+1)/2, "=", formula1, "\n")
cat("  Matches R?", formula1 == W_R, "\n\n")



# Let me also check if it could be related to the traditional Wilcoxon differently
# Traditional Wilcoxon W is just R_x, so maybe R transforms it differently?

cat("Additional checks:\n")
cat("  Traditional Wilcoxon W₁ = R_x =", R_x, "\n")
cat("  R's W =", W_R, "\n")
cat("  Difference =", R_x - W_R, "\n")
cat("  n(n+1)/2 =", n*(n+1)/2, "\n")
cat("  Does R_x - W_R = n(n+1)/2?", (R_x - W_R) == n*(n+1)/2, "\n\n")

# ============================================================================
# STEP 4: Mann-Whitney U verification
# ============================================================================

cat("STEP 4: Mann-Whitney U Verification\n")
cat("===================================\n")

# Direct count of U₁ (times x > y)
U1_direct <- 0
comparison_table <- matrix(nrow = n, ncol = m)
for(i in 1:n) {
  for(j in 1:m) {
    if(x[i] > y[j]) {
      U1_direct <- U1_direct + 1
      comparison_table[i,j] <- 1
    } else {
      comparison_table[i,j] <- 0
    }
  }
}

cat("Pairwise comparisons (x[i] > y[j]):\n")
rownames(comparison_table) <- paste("x[", 1:n, "]=", x, sep="")
colnames(comparison_table) <- paste("y[", 1:m, "]=", y, sep="")
print(comparison_table)
cat("U₁ (direct count) =", U1_direct, "\n")

# U₁ from rank formula
U1_from_ranks <- n*m + n*(n+1)/2 - R_x
cat("U₁ (from ranks) = nm + n(n+1)/2 - R_x =", n*m, "+", n*(n+1)/2, "-", R_x, "=", U1_from_ranks, "\n")

cat("Does U₁ = R's W?", U1_direct == W_R, "\n")
cat("Do both U₁ calculations match?", U1_direct == U1_from_ranks, "\n\n")

# ============================================================================
# STEP 5: Test with multiple examples to confirm
# ============================================================================

cat("STEP 5: Multiple Examples\n")
cat("=========================\n")

test_formula <- function(sample1, sample2, name) {
  n1 <- length(sample1)
  m1 <- length(sample2)
  
  # R's W
  W_R <- as.numeric(wilcox.test(sample1, sample2, exact = TRUE)$statistic)
  
  # Manual calculation
  combined <- c(sample1, sample2)
  ranks <- rank(combined)
  R_x <- sum(ranks[1:n1])
  
  # Test the formula
  W_formula <- R_x - n1*(n1+1)/2
  
  # Mann-Whitney U
  U1 <- sum(outer(sample1, sample2, ">"))
  
  cat(sprintf("Example %s: x=%s, y=%s\n", 
              name, 
              paste(sample1, collapse=","), 
              paste(sample2, collapse=",")))
  cat(sprintf("  R's W = %g\n", W_R))
  cat(sprintf("  R_x = %g, n1(n1+1)/2 = %g\n", R_x, n1*(n1+1)/2))
  cat(sprintf("  Formula W = R_x - n1(n1+1)/2 = %g\n", W_formula))
  cat(sprintf("  Mann-Whitney U₁ = %g\n", U1))
  cat(sprintf("  All equal? %s\n\n", W_R == W_formula && W_R == U1))
  
  return(list(W_R = W_R, W_formula = W_formula, U1 = U1, match = W_R == W_formula))
}

# Multiple test cases
examples <- list(
  list(c(1, 3), c(2, 4, 5), "A"),
  list(c(10, 15, 20), c(12, 14), "B"),
  list(c(5), c(1, 2, 3, 4), "C"),
  list(c(1, 2, 4), c(3, 5, 6, 7), "Original")
)

all_results <- list()
for(i in seq_along(examples)) {
  result <- test_formula(examples[[i]][[1]], examples[[i]][[2]], examples[[i]][[3]])
  all_results[[i]] <- result
}

# ============================================================================
# STEP 6: Final conclusion
# ============================================================================

cat("STEP 6: CONCLUSION\n")
cat("==================\n")

all_match <- all(sapply(all_results, function(x) x$match))

if(all_match) {
  cat("✓ CONFIRMED: R's W statistic formula is:\n\n")
  cat("    W = R_x - n(n+1)/2\n\n")
  cat("Where:\n")
  cat("  - R_x = sum of ranks for the FIRST sample\n")
  cat("  - n = size of the FIRST sample (NOT total sample size)\n")
  cat("  - n(n+1)/2 = minimum possible rank sum for first sample\n\n")
  cat("This is equivalent to:\n")
  cat("  - Mann-Whitney U₁ statistic\n")
  cat("  - Number of times first sample exceeds second sample\n\n")
} else {
  cat("✗ Formula does not match in all cases. Need further investigation.\n")
}

cat("The confusion about 'n' vs 'N':\n")
cat("  - Use n (first sample size), NOT N (total sample size)\n")
cat("  - This makes sense because we're adjusting only the first sample's ranks\n")
cat("  - The minimum rank sum for n observations is n(n+1)/2\n")
cat("  - This shifts the range to start at 0 instead of n(n+1)/2\n")