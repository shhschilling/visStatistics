## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5,
  out.width = "100%"
)

## ----setup--------------------------------------------------------------------
library(visStatistics)

## ----fig-decision-tree, fig.cap="Decision tree used to select the appropriate statistical test for a categorical predictor and numerical response, based on the number of factor levels, normality and homoscedasticity.", out.width="100%,fig.height =100% "----
knitr::include_graphics("../man/figures/decision_tree.png")

## -----------------------------------------------------------------------------
mtcars$am <- as.factor(mtcars$am)
t_test_statistics <- visstat(mtcars, "mpg", "am")

## -----------------------------------------------------------------------------
mtcars$am <- as.factor(mtcars$am)
t_test_statistics_99 <- visstat(mtcars, "mpg", "am", conf.level = 0.99)

## -----------------------------------------------------------------------------
grades_gender <- data.frame(
  sex = as.factor(c(rep("girl", 21), rep("boy", 23))),
  grade = c(
    19.3, 18.1, 15.2, 18.3, 7.9, 6.2, 19.4,
    20.3, 9.3, 11.3, 18.2, 17.5, 10.2, 20.1, 13.3, 17.2, 15.1, 16.2, 17.0,
    16.5, 5.1, 15.3, 17.1, 14.8, 15.4, 14.4, 7.5, 15.5, 6.0, 17.4,
    7.3, 14.3, 13.5, 8.0, 19.5, 13.4, 17.9, 17.7, 16.4, 15.6, 17.3, 19.9, 4.4, 2.1
  )
)

wilcoxon_statistics <- visstat(grades_gender, "grade", "sex")

## -----------------------------------------------------------------------------
oneway_npk <- visstat(npk, "yield", "block")

## -----------------------------------------------------------------------------
insect_sprays_tr <- InsectSprays
insect_sprays_tr$count_sqrt <- sqrt(InsectSprays$count)
visstat(insect_sprays_tr, "count_sqrt", "spray")

## -----------------------------------------------------------------------------
visstat(iris, "Petal.Width", "Species")

## -----------------------------------------------------------------------------
linreg_cars <- visstat(cars, "dist", "speed")

## -----------------------------------------------------------------------------
linreg_cars <- visstat(cars, "dist", "speed", conf.level = 0.99)

## -----------------------------------------------------------------------------
linreg_trees <- visstat(trees, "Volume", "Girth", conf.level = 0.9)

## -----------------------------------------------------------------------------
linreg_cars <- visstat(trees, "Volume", "Girth", conf.level = 0.9)

## -----------------------------------------------------------------------------
HairEyeColorDataFrame <- counts_to_cases(as.data.frame(HairEyeColor))

## -----------------------------------------------------------------------------
hair_eye_color_df <- counts_to_cases(as.data.frame(HairEyeColor))
visstat(hair_eye_color_df, "Hair", "Eye")

## -----------------------------------------------------------------------------
hair_black_brown_eyes_brown_blue <- HairEyeColor[1:2, 1:2, ]
# Transform to data frame
hair_black_brown_eyes_brown_blue_df <- counts_to_cases(as.data.frame(hair_black_brown_eyes_brown_blue))
# Chi-squared test
visstat(hair_black_brown_eyes_brown_blue_df, "Hair", "Eye")

## -----------------------------------------------------------------------------
hair_eye_color_male <- HairEyeColor[, , 1]
# Slice out a 2 by 2 contingency table
black_brown_hazel_green_male <- hair_eye_color_male[1:2, 3:4]
# Transform to data frame
black_brown_hazel_green_male <- counts_to_cases(as.data.frame(black_brown_hazel_green_male))
# Fisher test
fisher_stats <- visstat(black_brown_hazel_green_male, "Hair", "Eye")

## -----------------------------------------------------------------------------
visstat(black_brown_hazel_green_male, "Hair", "Eye",
  graphicsoutput = "png", plotDirectory =

    tempdir()
)

## -----------------------------------------------------------------------------
file.remove(file.path(tempdir(), "chi_squared_or_fisher_Hair_Eye.png"))
file.remove(file.path(tempdir(), "mosaic_complete_Hair_Eye.png"))

