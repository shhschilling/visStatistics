library(visStatistics)

# Examples------
while (!is.null(dev.list())) {
  dev.off()
}


options(warn = 0) # for debugging also warnings
filedir <- tempdir()

# Student's t-test (equal variances, two groups)----
ttest = visstat(sleep$group, sleep$extra)
tvalue <- unname(ttest[[3]]$statistic)
# Show equivalance of student's t-test to aov approach-----
aov_sleep <- aov(extra ~ group, data = sleep)
summary_aov <- summary(aov_sleep)
F_anova <- (summary_aov[[1]][["F value"]][1])
stopifnot(all.equal(tvalue^2, F_anova))
# Show equivalance of student's t-test to lm approach---
# Extract t - statistics
lm_sleep <- lm(extra ~ group, data = sleep)
summary_lm <- summary(lm_sleep)
abst = (summary_lm$coefficients[2, 3])
stopifnot(all.equal(abst, abs(tvalue)))
beta1 = summary_lm$coefficients[2, 1]
# Check group means
mean1 = mean(sleep$extra[sleep$group == 1])
mean2 = mean(sleep$extra[sleep$group == 2])
stopifnot(all.equal(beta1, abs(mean2 - mean1)))



# Welch's t-test (unequal variances, two groups)----
visstat(mtcars$am, mtcars$mpg)


# Wilcoxon-----
grades_gender <- data.frame(
  Sex = as.factor(c(rep("Girl", 20), rep("Boy", 20))),
  Grade = c(
    19.25,
    18.1,
    15.2,
    18.34,
    7.99,
    6.23,
    19.44,
    20.33,
    9.33,
    11.3,
    18.2,
    17.5,
    10.22,
    20.33,
    13.3,
    17.2,
    15.1,
    16.2,
    17.3,
    16.5,
    5.1,
    15.25,
    17.41,
    14.5,
    15,
    14.3,
    7.53,
    15.23,
    6,
    17.33,
    7.25,
    14,
    13.5,
    8,
    19.5,
    13.4,
    17.5,
    17.4,
    16.5,
    15.6
  )
)
visstat(grades_gender$Sex, grades_gender$Grade)

# Fisher's ANOVA (equal variances, >2 groups)
visstat(PlantGrowth$group, PlantGrowth$weight)


# Welch ANOVA 1 -----
set.seed(1)
welch_anova_data <- data.frame(group = factor(rep(c("A", "B", "C"), each = 20)),
                               value = c(
                                 rnorm(20, mean = 50, sd = 5),
                                 rnorm(20, mean = 55, sd = 10),
                                 rnorm(20, mean = 60, sd = 15)
                               ))
welch_annova = visstat(welch_anova_data$group, welch_anova_data$value)

set.seed(123)
# Welch ANOVA  with 10 groups  -----
# 
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



result_10 <- visstat(groups_10, "response", "group")


# Kruskal-Wallis test: iris----
iris_data = visstat(iris$Species, iris$Petal.Width)

iris_data_stored = visstat(
  iris$Species,
  iris$Petal.Width,
  graphicsoutput = "pdf",
  plotName = "iris_kruskal",
  plotDirectory = filedir
)

# Linear regression: trees data set  ----
linear_regression_trees <- visstat(trees$Girth, trees$Volume,conf.level=0.99)
dev.off()
plot(linear_regression_trees, which = 1) # replays assumption plot


# Chi squared, mosaic plots with HairEyeColor----
# HairEyeColor data set: Pearsons Chi squared, mosaic plot with Pearson's residuals
HairEyeColorMale <- counts_to_cases(as.data.frame(HairEyeColor[, , 1]))
visstat(HairEyeColorMale$Eye, HairEyeColorMale$Hair)
HairEyeColorMaleFisher <- HairEyeColor[, , 1]
# replace cells to smaller values to enforce Cochran's rule
HairEyeColorMaleFisher[HairEyeColorMaleFisher < 10] <- 4
HairEyeColorMaleFisher <- counts_to_cases(as.data.frame(HairEyeColorMaleFisher))

res_chi <- visstat(
  HairEyeColorMaleFisher$Eye,
  HairEyeColorMaleFisher$Hair,
  graphicsoutput = "png",
  plotDirectory = filedir
) # stores two graphics outputs

# 2x2 contingency tables----
HairEyeColorMaleFisher <- HairEyeColor[, , 1]
# slicing out a 2 x2 contingency table
blackBrownHazelGreen <- HairEyeColorMaleFisher[1:2, 3:4]
fishertest <- blackBrownHazelGreen

blackBrownHazelGreen <- counts_to_cases(as.data.frame(blackBrownHazelGreen))
fisher_stats <- visstat(blackBrownHazelGreen, "Hair", "Eye")


# The visstat-methods -------
summary(iris_data_stored)
print(iris_data_stored)
plot(iris_data_stored) #file paths to plots
plot(iris_data, which = 1) #replay plot 1


# Replay plots  or file paths of stored graphics------
plot(linear_regression_trees)
plot(res_chi)# paths two column plot and mosaic plot
plot(fisher_stats, which = 2) #mosaic plot only


# Saving the graphical output to a user specified plotDirectory in user specified graphicsouput format ----
linear_regression_trees_paths <- visstat(
  trees$Height,
  trees$Girth,
  graphicsoutput = "svg",
  plotName = "trees",
  plotDirectory = filedir
)


# Show all graphics  of type ".png", ".pdf", ".svg", ".ps" in filedir=tempdir()-----
graphicaltypes <- c(".png", ".pdf", ".svg", ".ps")
for (i in graphicaltypes) {
  plotname <- dir(filedir, pattern = i)
  print(file.path(filedir, plotname))
  # file.remove(file.path(filedir, plotname)) # removes all files of type ".png", ".pdf", ".svg", ".ps" in filedir=tempdir()-
}
