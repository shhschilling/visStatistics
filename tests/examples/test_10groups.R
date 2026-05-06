# Test case: 10 groups with space-saving N= labels
# This demonstrates the optimized labeling for many groups

library(visStatistics)

set.seed(123)

# Create data with 10 groups
groups_10 <- data.frame(
  response = c(
    rnorm(15, mean = 10, sd = 2),  # Group 1
    rnorm(18, mean = 11, sd = 2),  # Group 2
    rnorm(22, mean = 12, sd = 2),  # Group 3
    rnorm(19, mean = 13, sd = 2),  # Group 4
    rnorm(21, mean = 14, sd = 2),  # Group 5
    rnorm(20, mean = 12, sd = 2),  # Group 6
    rnorm(19, mean = 11, sd = 2),  # Group 7
    rnorm(18, mean = 13, sd = 2),  # Group 8
    rnorm(22, mean = 12, sd = 2),  # Group 9
    rnorm(21, mean = 14, sd = 2)   # Group 10
  ),
  group = factor(rep(paste0("G", 1:10), times = c(15, 18, 22, 19, 21, 20, 19, 18, 22, 21)))
)

cat("\n=== TEST: 10 Groups with Optimized N= Labels ===\n")
cat("Expected behavior:\n")
cat("- Since n_groups (10) >= 6, should show: 'N = 15' for first group\n")
cat("- Then just numbers: '18', '22', '19', '21', '20', '19', '18', '22', '21'\n\n")

result_10 <- visstat(groups_10, "response", "group")
print(result_10)

cat("\n✓ Test complete. Check the plot for optimized N= labels.\n")
