# Example: Manual calculation of Fisher's exact test p-value

# Suppose we have the following 2x2 contingency table:
#
#           | Group 1 | Group 2 |   <- Columns (e.g., success/failure)
# --------- | ------- | ------- |
# Row 1     |    a    |    b    |
# Row 2     |    c    |    d    |
#
# With margins:
#   row1 = a + b
#   row2 = c + d
#   col1 = a + c
#   col2 = b + d
#   total = a + b + c + d

# Manual calculation of the probability of the observed table
# This uses the hypergeometric formula:
#   P(observed table) = choose(a + b, a) * choose(c + d, c) / choose(n, a + c)

a <- 8
b <- 2
c <- 1
d <- 5
n <- a + b + c + d  # total number of observations

p_obs <- choose(a + b, a) * choose(c + d, c) / choose(n, a + c)
p_obs  # Probability of exactly this table under the null hypothesis

# Matrix version of the same table
mat <- matrix(c(a, b, c, d), nrow = 2, byrow = TRUE)

# Compare with the built-in Fisher test (two-sided)
fisher.test(mat, alternative = "two.sided")$p.value


# Manual enumeration of all possible tables with fixed margins

# Fixed margins (e.g. from known row and column totals)
row1 <- a+b  # a + b
row2 <- c+d   # c + d
col1 <- a+c  # a + c
col2 <- b+d   # b + d

# The total number of observations is still:
n <- row1 + row2

# Observed value in cell [1,1] = a
obs_a <- a

# Compute the probability of seeing exactly 8 in cell [1,1]
# under the hypergeometric distribution
obs_prob <- dhyper(obs_a, col1, col2, row1)
# Compute the range of valid values for cell [1,1] = a
# We must ensure that all other cells (b, c, d) are ≥ 0

# Constraints:
# a ≤ row1       (because b = row1 - a ≥ 0)
# a ≤ col1       (because c = col1 - a ≥ 0)
# → Upper limit:
max_a <- min(row1, col1)

# a ≥ row1 - col2 (because b ≤ col2 → a ≥ row1 - col2)
# a ≥ col1 - row2 (because c ≤ row2 → a ≥ col1 - row2)
# and a ≥ 0 of course
# → Lower limit:
min_a <- max(0, row1 - col2)

# Sequence of valid a values that respect fixed margins
a_vals <- min_a:max_a

# Compute the probability of each possible table (value of a)
# given the fixed margins
probs <- dhyper(a_vals, col1, col2, row1)

# Compute the two-sided p-value:
# Sum of probabilities of all tables with probability ≤ that of the observed
p_val <- sum(probs[probs <= obs_prob])
p_val

# Compare again with fisher.test on the same matrix
mat <- matrix(c(8, 2, 1, 5), nrow = 2, byrow = TRUE)
fisher.test(mat, alternative = "two.sided")$p.value

