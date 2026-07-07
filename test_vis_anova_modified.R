# Test script for modified vis_anova function
# This tests the cleaned-up version (no Šidák intervals, just raw data + means + letters)

# Source the modified function and its dependencies
source("R/levene.test.R")
source("R/games.howell.R")
source("R/vis_anova.R")

# Install multcompView if not present
if (!require("multcompView", quietly = TRUE)) {
  install.packages("multcompView", quiet = TRUE)
  library(multcompView)
}

cat("\n=== TEST 1: Equal variances (uses TukeyHSD) ===\n")
cat("Data: npk dataset - yield vs block\n")
data(npk)
result1 <- vis_anova(npk$yield, npk$block,
                     samplename = "Yield (pounds)",
                     factorname = "Block")

cat("\nPost-hoc results:\n")
print(result1$`post-hoc analysis `)

cat("\n=== TEST 2: Unequal variances (uses Games-Howell) ===\n")
cat("Creating synthetic data with unequal variances...\n")
set.seed(123)
group_a <- rnorm(30, mean = 100, sd = 5)    # Small variance
group_b <- rnorm(30, mean = 110, sd = 15)   # Large variance
group_c <- rnorm(30, mean = 105, sd = 8)    # Medium variance

values <- c(group_a, group_b, group_c)
groups <- factor(rep(c("A", "B", "C"), each = 30))

result2 <- vis_anova(values, groups,
                     samplename = "Response",
                     factorname = "Group")

cat("\nPost-hoc results:\n")
print(result2$`post-hoc analysis `)

cat("\n=== COMPARISON ===\n")
cat("Old design: Šidák intervals (thick lines) + standard intervals (arrows) + letters = CONFUSING\n")
cat("New design: Raw data (points) + group means (red lines) + letters = CLEAN\n")
cat("\nGroup means shown as DARK RED HORIZONTAL LINES\n")
cat("Significance determined by POST-HOC LETTERS only (not by visual interval overlap)\n")
