library(visStatistics)

# Examples------
while (!is.null(dev.list())) {
  dev.off()
}


options(warn = 0) # for debugging also warnings
filedir <- tempdir()


# Welch two sample t.test: InsectSprays ----
# select sprays A and B
InsectSpraysAB <- InsectSprays[which(InsectSprays$spray == "A" |
                                       InsectSprays$spray == "B"), ]
InsectSpraysAB$spray <- factor(InsectSpraysAB$spray)


insect_spray=visstat(
  InsectSpraysAB,
  "count",
  "spray",
  graphicsoutput = "png",
  plotName = "insect_count_spray",
  plotDirectory = filedir
)

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
visstat(grades_gender$Sex,grades_gender$Grade)

# ANOVA -----
anova_npk <- visstat(npk$block, npk$yield) 


# Kruskal-Wallis test: iris----
iris_data=visstat(iris$Species, iris$Petal.Width)

iris_data_stored=visstat(
  iris$Species, iris$Petal.Width,
  graphicsoutput = "pdf",
  plotName = "iris_kruskal",
  plotDirectory = filedir
)

# Linear regression: trees data set  ----
linear_regression_trees <- visstat(trees$Height, trees$Girth)
plot(linear_regression_trees, which=1) # replays assumption plot


# Chi squared, mosaic plots with HairEyeColor----
# HairEyeColor data set: Pearsons Chi squared, mosaic plot with Pearson's residuals
HairEyeColorMale <- counts_to_cases(as.data.frame(HairEyeColor[, , 1]))
visstat(HairEyeColorMale$Eye, HairEyeColorMale$Hair)
HairEyeColorMaleFisher <- HairEyeColor[, , 1]
# replace cells to smaller values to enforce Cochran's rule
HairEyeColorMaleFisher[HairEyeColorMaleFisher < 10] <- 4
HairEyeColorMaleFisher <- counts_to_cases(as.data.frame(HairEyeColorMaleFisher))

res_chi <- visstat(
  HairEyeColorMaleFisher$Eye, HairEyeColorMaleFisher$Hair,
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
summary.visstat(iris_data_stored)
print.visstat(iris_data_stored)
plot(iris_data_stored) #file paths
plot(iris_data,which = 1) #replay plot 1


# Replay plots  or file paths of stored graphics------
plot(insect_spray,which = 1) # path of assumption plot
plot(anova_npk)
plot(linear_regression_trees)
plot(res_chi)# paths two column plot and mosaic plot
plot(fisher_stats,which = 2) #mosaic plot only


# Saving the graphical output to a user specified plotDirectory in user specified graphicsouput format ----
linear_regression_trees_paths <- visstat(
  trees$Height, trees$Girth,
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

