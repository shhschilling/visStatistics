# Welch's One-Way ANOVA — manual computation matching oneway.test()

set.seed(42)

# Simulate data with unequal variances
x1 <- rnorm(10, mean = 5, sd = 1)
x2 <- rnorm(12, mean = 7, sd = 2)
x3 <- rnorm(8,  mean = 9, sd = 3)

x <- c(x1, x2, x3)
group <- factor(rep(c("A", "B", "C"), times = c(10, 12, 8)))

# Built-in Welch's ANOVA
result <- oneway.test(x ~ group, var.equal = FALSE)
print(result)

# Manual computation
groups <- split(x, group)
n_i     <- sapply(groups, length)
xbar_i  <- sapply(groups, mean)
s2_i    <- sapply(groups, var)

# Weights and weighted mean
w_i <- n_i / s2_i
w   <- sum(w_i)
xbar_w <- sum(w_i * xbar_i) / w

# Numerator of F-statistic
SSB <- sum(w_i * (xbar_i - xbar_w)^2)
df1 <- length(groups) - 1
F_val <- SSB / df1

# Refined Welch–Satterthwaite denominator degrees of freedom
numer <- SSB^2
denom <- sum((w_i^2 * (xbar_i - xbar_w)^4) / (n_i - 1))
df2 <- numer / denom

# Manual p-value
p_val <- pf(F_val, df1 = df1, df2 = df2, lower.tail = FALSE)

# Output manual results
cat("\nManual Welch's F =", round(F_val, 4), "\n")
cat("Degrees of freedom:", round(df1, 2), "and", round(df2, 3), "\n")
cat("Manual p-value =", round(p_val, 5), "\n")
result