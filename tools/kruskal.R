# Simulate example data
set.seed(42)
x1 <- rnorm(10, mean = 5)
x2 <- rnorm(12, mean = 7)
x3 <- rnorm(8,  mean = 9)

x <- c(x1, x2, x3)
group <- factor(rep(c("A", "B", "C"), times = c(10, 12, 8)))

# Built-in Kruskal-Wallis test
res <- kruskal.test(x ~ group)
print(res)

# Manual computation of H statistic
N <- length(x)
ranks <- rank(x)

# Split ranks by group
r_group <- split(ranks, group)
n_i <- sapply(r_group, length)
Rbar_i <- sapply(r_group, mean)

Rbar <- (N + 1) / 2  # Mean of all ranks

# Kruskal–Wallis H formula
H_manual <- (12 / (N * (N + 1))) * sum(n_i * (Rbar_i - Rbar)^2)

# Output manual result
cat("\nManual H =", round(H_manual, 4), "\n")
cat("Built-in H =", round(res$statistic, 4), "\n")
cat("p-value (chi-squared, df =", res$parameter, ") =",
    round(pchisq(H_manual, df = res$parameter, lower.tail = FALSE), 5), "\n")

res