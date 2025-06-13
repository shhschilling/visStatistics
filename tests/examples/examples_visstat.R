## Examples------
while (!is.null(dev.list())) {
  dev.off()
}
library(visStatistics)
options(warn = 0) # for debugging also warnings
# only while developing, comment  when installed from CRAN
# library(nortest)
# library(vcd)
# library(multcompView)
# library(Cairo)

# specify directory where plots will be stored. Without definition of plotDirectory: current working directory
filedir <- tempdir()

# If graphicsoutput parameter is set, all plots are stored in the directory specified in the parameter plotDirectory.
# Default directory of plotDirectory is the working directory.
# Graphical output is named following the naming convention
# "statisticalTestName_varsample_varfactor.graphicsoutput"
#
linear_regression_trees <- visstat(trees, "Girth", "Height")
linear_regression_trees <- visstat(trees,
  "Girth",
  "Height",
  graphicsoutput = "png",
  plotDirectory = filedir
)
linear_regression_trees <- visstat(
  trees,
  "Girth",
  "Height",
  graphicsoutput = "pdf",
  plotName = "hugo",
  plotDirectory = filedir
)
linear_regression_trees <- visstat(
  trees,
  "Girth",
  "Height",
  graphicsoutput = "svg", ,
  plotName = "dante",
  plotDirectory = filedir
)
# display stats of linear regression
linear_regression_trees

# Welch two sample t.test: mtcars data set ----
mtcars$am <- as.factor(mtcars$am)
welch_cars <- visstat(mtcars, "mpg", "am")
# store graphical output in different formats in directory defined in argument plotDirectory
welch_cars <- visstat(
  mtcars,
  "mpg",
  "am",
  graphicsoutput = "png",
  plotName = "hans",
  plotDirectory = filedir
)
# standard naming convention
welch_cars <- visstat(mtcars,
  "mpg",
  "am",
  graphicsoutput = "pdf",
  plotDirectory = filedir
)

# ANOVA and oneway.test -----
anova_npk <- visstat(npk, "yield", "block")
anova_npk # print out results


# Kruskal-Wallis test: iris----
visstat(iris, "Petal.Width", "Species")
visstat(
  iris,
  "Petal.Width",
  "Species",
  graphicsoutput = "pdf",
  plotDirectory = filedir
)
visstat(
  iris,
  "Petal.Width",
  "Species",
  graphicsoutput = "pdf",
  plotName = "iris_kruskal",
  plotDirectory = filedir
)


# Welch two sample t.test: InsectSprays ----
# select sprays A and B
InsectSpraysAB <- InsectSprays[which(InsectSprays$spray == "A" |
  InsectSprays$spray == "B"), ]
InsectSpraysAB$spray <- factor(InsectSpraysAB$spray)
# Welcht-t-Test
visstat(InsectSpraysAB, "count", "spray") # plots not saved
visstat(
  InsectSpraysAB,
  "count",
  "spray",
  graphicsoutput = "png",
  plotName = "insect_count_spray",
  plotDirectory = filedir
)

# Wilcoxon
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
visstat(grades_gender, "Grade", "Sex")


# Chi squared, mosaic plots with HairEyeColor----
# HairEyeColor data set: Pearsons Chi squared, mosaic plot with Pearson's residuals
HairEyeColorMale <- counts_to_cases(as.data.frame(HairEyeColor[, , 1]))
visstat(HairEyeColorMale, "Hair", "Eye")
HairEyeColorMaleFisher <- HairEyeColor[, , 1]
# replace cells to smaller values to enforce Cochran's rule
HairEyeColorMaleFisher[HairEyeColorMaleFisher < 10] <- 4
HairEyeColorMaleFisher <- counts_to_cases(as.data.frame(HairEyeColorMaleFisher))
res_chi <- visstat(HairEyeColorMaleFisher, "Hair", "Eye") # test statistics stored in res_chi
res_chi <- visstat(
  HairEyeColorMaleFisher,
  "Hair",
  "Eye",
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
fisher_stats


# remove output plots from directory filedir----
# graphicaltypes=c(".png", ".pdf", ".svg", ".ps")
# for (i in graphicaltypes) {
#   plotname=dir(filedir,pattern=i)
#  print(file.path(filedir,plotname))
#   file.remove(file.path(filedir,plotname))
# }
