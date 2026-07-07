# ============================================================================
# Example: Pairwise Confidence Intervals from Tukey HSD
# Shows the CLEANER approach: visualize differences, not individual means
# ============================================================================

# Use the npk dataset (same as in the vignette)
data(npk)

# Run ANOVA
anova_result <- aov(yield ~ block, data = npk)

# Apply TukeyHSD
tukey_result <- TukeyHSD(anova_result, conf.level = 0.95)

# ============================================================================
# APPROACH 1: R base plot() method for TukeyHSD
# This shows horizontal confidence intervals for all pairwise differences
# ============================================================================

par(mar = c(4, 8, 4, 2))
plot(tukey_result, las = 1)

# Interpretation:
# - Each horizontal line = confidence interval for the difference between two groups
# - Line crosses 0 → not significant (the two means are equal)
# - Line does NOT cross 0 → significant (the means differ)
# This directly answers: "Are these two groups different?"

cat("\n=== TukeyHSD Output (pairwise differences) ===\n")
print(tukey_result)

# ============================================================================
# APPROACH 2: Manual horizontal plot (more control)
# ============================================================================

tukey_matrix <- tukey_result$block  # Extract the matrix of results

# Create data frame for plotting
pairwise_df <- data.frame(
  comparison = rownames(tukey_matrix),
  estimate = tukey_matrix[, "diff"],
  lower = tukey_matrix[, "lwr"],
  upper = tukey_matrix[, "upr"],
  p_adj = tukey_matrix[, "p adj"]
)

# Create horizontal plot
par(mar = c(5, 12, 4, 2))
n_comparisons <- nrow(pairwise_df)

plot(NULL,
     xlim = c(min(pairwise_df$lower) - 5, max(pairwise_df$upper) + 5),
     ylim = c(0.5, n_comparisons + 0.5),
     xlab = "Difference in Means (yield)",
     ylab = "",
     main = "Pairwise Comparisons: Tukey HSD (conf.level = 0.95)",
     axes = FALSE)

axis(1)
axis(2, at = 1:n_comparisons, labels = pairwise_df$comparison, las = 2)

# Add vertical line at 0 (no difference)
abline(v = 0, lty = 2, col = "red", lwd = 2)

# Plot confidence intervals
for (i in 1:n_comparisons) {
  # Color based on significance
  col <- ifelse(pairwise_df$lower[i] <= 0 & pairwise_df$upper[i] >= 0,
                "gray40", "darkgreen")  # Gray if contains 0, green if significant

  # Draw confidence interval
  lines(c(pairwise_df$lower[i], pairwise_df$upper[i]),
        c(i, i),
        col = col,
        lwd = 3)

  # Draw point estimate
  points(pairwise_df$estimate[i], i,
         col = col,
         pch = 19,
         cex = 1.2)
}

# Add legend
legend("bottomright",
       c("Not significant (CI contains 0)",
         "Significant (CI does not contain 0)"),
       col = c("gray40", "darkgreen"),
       lwd = 3,
       pch = 19)

# ============================================================================
# KEY ADVANTAGES over individual means + letters
# ============================================================================

cat("\n=== ADVANTAGES of pairwise CI approach ===\n")
cat("1. Directly answers: 'Are these two groups different?'\n")
cat("2. No ambiguity about interval interpretation\n")
cat("3. CI crossing zero = not significant (crystal clear)\n")
cat("4. CI NOT crossing zero = significant (crystal clear)\n")
cat("5. Visualization and testing are ALIGNED\n")
cat("6. No confusing mix of Šidák intervals + Tukey letters\n")
cat("7. User doesn't need to learn what green letters mean\n")

cat("\n=== Significant differences (p adj < 0.05) ===\n")
significant <- pairwise_df[pairwise_df$p_adj < 0.05, ]
if (nrow(significant) > 0) {
  print(significant)
} else {
  cat("None\n")
}
