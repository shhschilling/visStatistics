## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5,
  out.width = "100%"
)

## ----setup--------------------------------------------------------------------
library(visStatistics)

## -----------------------------------------------------------------------------
mtcars$am <- as.factor(mtcars$am)
ttestStatistics <- visstat(mtcars, "mpg", "am")
# Print the test statistics
ttestStatistics

## -----------------------------------------------------------------------------
# Set the confidence level explicitly resulting in wider confidence levels
mtcars$am <- as.factor(mtcars$am)
ttestStatistics <- visstat(mtcars, "mpg", "am", conf.level = 0.99)

## -----------------------------------------------------------------------------
grades_gender <- data.frame(
  Sex = as.factor(c(rep("Girl", 21), rep("Boy", 23))),
  Grade = c(
    19.3, 18.1, 15.2, 18.3, 7.9, 6.2, 19.4,
    20.3, 9.3, 11.3, 18.2, 17.5, 10.2, 20.1, 13.3, 17.2, 15.1, 16.2, 17.0,
    16.5, 5.1, 15.3, 17.1, 14.8, 15.4, 14.4, 7.5, 15.5, 6.0, 17.4,
    7.3, 14.3, 13.5, 8.0, 19.5, 13.4, 17.9, 17.7, 16.4, 15.6, 17.3, 19.9, 4.4, 2.1
  )
)

wilcoxonStatistics <- visstat(grades_gender, "Grade", "Sex")

## -----------------------------------------------------------------------------
oneway_npk <- visstat(npk, "yield", "block")

## -----------------------------------------------------------------------------
InsectSprays_tr <- InsectSprays
InsectSprays_tr$count <- sqrt(InsectSprays$count)
visstat(InsectSprays_tr, "count", "spray")

## -----------------------------------------------------------------------------
visstat(iris, "Petal.Width", "Species")

## -----------------------------------------------------------------------------
linreg_cars <- visstat(cars, "dist", "speed")

## -----------------------------------------------------------------------------
linreg_cars <- visstat(cars, "dist", "speed", conf.level = 0.99)
# Extract the test statistics
linreg_cars$anderson_darling_test_residuals
linreg_cars$shapiro_test_residuals

## -----------------------------------------------------------------------------
linreg_cars <- visstat(trees, "Height", "Girth", conf.level = 0.9)

## -----------------------------------------------------------------------------
HairEyeColorDataFrame <- counts_to_cases(as.data.frame(HairEyeColor))

## -----------------------------------------------------------------------------
HairEyeColorDataFrame <- counts_to_cases(as.data.frame(HairEyeColor))
visstat(HairEyeColorDataFrame, "Hair", "Eye")

## -----------------------------------------------------------------------------
HairEyeColorMaleFisher <- HairEyeColor[, , 1]
# slicing out a 2 x2 contingency table
blackBrownHazelGreen <- HairEyeColorMaleFisher[1:2, 3:4]
blackBrownHazelGreen <- counts_to_cases(as.data.frame(blackBrownHazelGreen))
fisher_stats <- visstat(blackBrownHazelGreen, "Hair", "Eye")
# fisher_stats  #uncommenting prints out summary statistics

## -----------------------------------------------------------------------------
visstat(blackBrownHazelGreen, "Hair", "Eye",
  graphicsoutput = "png", plotDirectory =

    tempdir()
)

## -----------------------------------------------------------------------------
file.remove(file.path(tempdir(), "chi_squared_or_fisher_Hair_Eye.png"))
file.remove(file.path(tempdir(), "mosaic_complete_Hair_Eye.png"))

