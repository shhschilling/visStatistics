## Examples------
# clean the workspace -----
while (!is.null(dev.list())) {
  dev.off()
} # closes eventual open graphical devices and restore default values of par()
# debugging: stop with warnings
options(warn = 2)
# load libraries -----
library(visStatistics)
# only while developing, comment on when installed from CRAN
library(nortest)
library(vcd)
library(multcompView)
library(Cairo)


# specify directory where plots will be stored----
# Without definition of plotDirectory: current working directory
filedir <- tempdir()



# Welch two sample t.test: mtcars data set ----

welch_cars <- visstat(mtcars, "mpg", "am")
welch_cars <- visstat(mtcars,
  "mpg",
  "am",
  graphicsoutput = "png",
  plotDirectory = filedir
)
welch_cars

# Kruskal-Wallis test: iris----
iris_kruskal <- visstat(iris, "Petal.Width", "Species")
iris_kruskal <- visstat(
  iris,
  "Petal.Width",
  "Species",
  graphicsoutput = "png",
  plotDirectory = filedir
)
iris_kruskal <- visstat(iris, "Petal.Width", "Species") # error: overlaying plots
iris_kruskal

# Welch two sample t.test: InsectSprays ----
# select sprays A and B
# not functioning
InsectSpraysAB <- InsectSprays[which(InsectSprays$spray == "A" |
  InsectSprays$spray == "B"), ]
# resets the number of levels to 2, attention: as.factor does not do that
InsectSpraysAB$spray <- factor(InsectSpraysAB$spray)
# Welcht-t-Test
insect_t_test <- visstat(InsectSpraysAB, "count", "spray")
insect_t_test

# Wilcoxon rank sum test: ToothGrowth ----
# t-test weight
women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
men_weight <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4)
# Create a data frame
weight_gender <- data.frame(
  gender = as.factor(rep(c("Woman", "Man"), each = 9)),
  gender2 = c(rep(c("Woman", "Man"), each = 9)),
  gender6 = c(rep(c("Frau", "Mann"), each = 9)),
  gender3 = c(rep(c("Woman", 1), each = 9)),
  gender4 = c(rep(c(0, 1), each = 9)),
  gender5 = c(rep(c(0.2, 1.2), each = 9)),
  weight = c(women_weight, men_weight)
)

visstat(weight_gender, "weight", "gender") # sorts independent variable in alphabetial order
vis_weight <- visstat(weight_gender, "weight", "gender2")
visstat(weight_gender, "weight", "gender3")
visstat(weight_gender, "weight", "gender4")
visstat(weight_gender, "weight", "gender5")
visstat(weight_gender, "weight", "gender6")


HairEyeColorMaleFisher <- HairEyeColor[, , 1]
# replace cells to smaller values to enforce Cochran's rule
HairEyeColorMaleFisher[HairEyeColorMaleFisher < 10] <- 4
HairEyeColorMaleFisher <- counts_to_cases(as.data.frame(HairEyeColorMaleFisher))
hair_chi <- visstat(HairEyeColorMaleFisher, "Hair", "Eye") # test statistics stored in res_chi
# stores two graphics outputs

# 2x2 contingency tables----
HairEyeColorMaleFisher <- HairEyeColor[, , 1]
# slicing out a 2 x2 contingency table
blackBrownHazelGreen <- HairEyeColorMaleFisher[1:2, 3:4]
fishertest <- blackBrownHazelGreen
blackBrownHazelGreen <- counts_to_cases(as.data.frame(blackBrownHazelGreen))
fisher_stats <- visstat(blackBrownHazelGreen, "Hair", "Eye")
fisher_stats



# linear regression: trees data set:----
linear_regression_trees <- visstat(trees, "Girth", "Volume")
linear_regression_trees <- visstat(trees, "Girth", "Height")
linear_regression_trees <- visstat(trees,
  "Girth",
  "Height",
  graphicsoutput = "png",
  plotDirectory = filedir
)
linear_regression_trees <- visstat(trees, "Girth", "Height")
# display stats of linear regression
linear_regression_trees


# remove output  plots
# graphicaltypes=c(".png")
# for (i in graphicaltypes) {
#   plotname=dir(filedir,pattern=i)
#   print(file.path(filedir,plotname))
#   file.remove(file.path(filedir,plotname))
# }
