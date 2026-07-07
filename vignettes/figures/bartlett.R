# Bartlett test: manual computation vs built-in
set.seed(44)

# Example data: 3 groups with different variances
g1 <- rnorm(10, mean = 5, sd = 1)
g2 <- rnorm(12, mean = 5, sd = 1.5)
g3 <- rnorm(8,  mean = 5, sd = 0.8)

# Combine data
values <- c(g1, g2, g3)
groups <- factor(rep(c("A", "B", "C"), times = c(10, 12, 8)))

# Built-in Bartlett test
bt_builtin <- bartlett.test(values ~ groups)

# Manual calculation
group_list <- split(values, groups)
n_i <- sapply(group_list, length)
s_i2 <- sapply(group_list, var)
k <- length(n_i)
N <- sum(n_i)

# Pooled variance
sp2_num <- sum((n_i - 1) * s_i2)
sp2 <- sp2_num / (N - k)

# Numerator
num <- (N - k) * log(sp2) - sum((n_i - 1) * log(s_i2))

# Denominator correction
c_term <- 1 + (1 / (3 * (k - 1))) * (sum(1 / (n_i - 1)) - 1 / (N - k))

# Bartlett test statistic
K2_manual <- num / c_term

# p-value
p_manual <- pchisq(K2_manual, df = k - 1, lower.tail = FALSE)

# Output comparison
cat("Manual computation:\n")
cat(sprintf("  K^2 = %.4f\n", K2_manual))
cat(sprintf("  df  = %d\n", k - 1))
cat(sprintf("  p   = %.4g\n\n", p_manual))

cat("Built-in bartlett.test():\n")
print(bt_builtin)

library(visStatistics)
# leven.test
levene <- levene.test(values,groups)
levene
