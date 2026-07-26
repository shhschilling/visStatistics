## ---------------------------------------------------------------------------
## Replication script for
##
##   "visStatistics: Automated test selection, visualised"
##   Sabine Schilling
##
## Every figure and every numerical result shown in the manuscript is
## reproduced below, in the order in which it appears. The comments give the
## label of the corresponding figure, so that each block can be matched to the
## manuscript.
##
## Requirements
##   R (>= 4.1), the package visStatistics, and for Section 3 additionally
##   ggplot2, patchwork, scales, colorspace, ggtext and nortest.
##
## Run time
##   Section 2 (all examples) takes a few minutes. Section 3 re-runs the Monte
##   Carlo grid at a reduced number of replications, see the note there, and
##   takes about a quarter of an hour on eight cores. The published grid uses
##   B = 50000 replications per cell and runs for several hours; the exact
##   command is given as a comment.
##
## Output
##   All example figures are written to code_figures.pdf in the working
##   directory. The simulation figures are written as .png files into the
##   working directory of Section 3.
## ---------------------------------------------------------------------------


## ---------------------------------------------------------------------------
## 1 Preliminaries
## ---------------------------------------------------------------------------

library("visStatistics")

## The manuscript is rendered with these two settings. The seed fixes the
## normal deviates drawn for the simulated Q-Q envelopes of the residual
## diagnostics and the two synthetic data sets used below; qq_nsim fixes the
## number of those draws.
set.seed(20260615)
options(visStatistics.qq_nsim = 999L)

pdf("code_figures.pdf", width = 7, height = 4.5, onefile = TRUE)


## ---------------------------------------------------------------------------
## 2 Examples
## ---------------------------------------------------------------------------

## Motivating example: numeric response, factor predictor with more than two
## levels (Section "Introduction").
visstat(npk$block, npk$yield)

## Student's t-test, ToothGrowth (fig:student-ttest-example).
student_ttest <- visstat(ToothGrowth$supp, ToothGrowth$len)

## Fisher's one-way ANOVA with Tukey HSD, PlantGrowth
## (fig:anova-plantgrowth-panels).
anova_plantgrowth <- visstat(PlantGrowth$group, PlantGrowth$weight)
plot(anova_plantgrowth, which = 1)
plot(anova_plantgrowth, which = 2)

## Saving the graphics to files, and the printed output of the same object.
anova_plantgrowth_stored <- visstat(
  PlantGrowth$group,
  PlantGrowth$weight,
  graphicsoutput = "png",
  plotName = "anova_plantgrowth",
  plotDirectory = tempdir()
)
paths <- attr(anova_plantgrowth_stored, "plot_paths")
print(basename(paths))
print(anova_plantgrowth)
summary(anova_plantgrowth)
file.remove(paths)

## Welch's t-test, mtcars (fig:ttest-example).
mtcars$am <- as.factor(mtcars$am)
t_test_stats <- visstat(mtcars$am, mtcars$mpg)

## Welch's ANOVA with Games-Howell, iris (fig:welch-anova-example).
welch_anova_iris <- visstat(iris$Species, iris$Sepal.Length)

## Wilcoxon rank-sum test, warpbreaks (fig:wilcoxon-example).
wilcoxon_stats <- visstat(warpbreaks$wool, warpbreaks$breaks)

## Kruskal-Wallis test, iris (fig:kruskal-example).
kruskal_iris <- visstat(iris$Species, iris$Petal.Width)

## Ordered response, two predictor levels: Titanic
## (fig:ordinal-wilcoxon-kruskal-example, left).
titanic_df <- counts_to_cases(as.data.frame(Titanic))
titanic_df$Class <- ordered(titanic_df$Class,
  levels = c("1st", "2nd", "3rd", "Crew")
)
wilcox_ordered <- visstat(titanic_df$Sex, titanic_df$Class)

## Ordered response, three predictor levels: synthetic survey
## (fig:ordinal-wilcoxon-kruskal-example, right). The seed is the one used in
## the manuscript.
set.seed(123)
market <- factor(rep(c("Europe", "North America", "Asia"), each = 50))
comfort_numeric <- c(
  sample(1:5, 50, replace = TRUE, prob = c(0.30, 0.30, 0.20, 0.15, 0.05)),
  sample(1:5, 50, replace = TRUE, prob = c(0.10, 0.20, 0.40, 0.20, 0.10)),
  sample(1:5, 50, replace = TRUE, prob = c(0.05, 0.10, 0.20, 0.35, 0.30))
)
survey_data_3 <- data.frame(
  market = market,
  comfort = ordered(comfort_numeric)
)
kruskal_ordered <- visstat(comfort ~ market, data = survey_data_3)

## Simple linear regression, swiss, at conf.level = 0.99
## (fig:regression-example).
linreg_swiss <- visstat(swiss$Examination, swiss$Fertility, conf.level = 0.99)

## Default regression route for Ozone by Wind (fig:ozone-lm-triage).
ozone_lm <- visstat(airquality$Wind, airquality$Ozone)

## Gamma GLM with log link as the alternative route, and its AIC compared with
## the Gaussian fit (fig:gamma-glm-plot).
model_gamma <- glm(Ozone ~ Wind, data = airquality, family = Gamma(link = "log"))
model_gamma$aic

model_lm <- glm(Ozone ~ Wind, data = airquality)
model_lm$aic

plot(airquality$Wind, airquality$Ozone,
  log = "y",
  pch = 1, col = "black", xlab = "Wind (mph)", ylab = "Ozone (ppb) [log scale]",
  main = "Gamma GLM Fit (Log Link)"
)
wind_seq <- seq(min(airquality$Wind), max(airquality$Wind), length.out = 100)
preds <- predict(model_gamma, newdata = data.frame(Wind = wind_seq), type = "response")
lines(wind_seq, preds, col = "red", lwd = 2)

legend("topright",
  legend = c("Data", "Gamma GLM (log link)"),
  col = c("black", "red"), pch = c(1, NA), lty = c(NA, 1), lwd = c(NA, 2)
)

## Normality of the standardised deviance residuals of the Gamma GLM.
std_dev_res <- rstandard(model_gamma, type = "deviance")
shapiro.test(std_dev_res)
nortest::ad.test(std_dev_res)

## Pearson's chi-squared test, HairEyeColor (fig:chisq-example).
hair_eye_df <- counts_to_cases(as.data.frame(HairEyeColor))
visstat(hair_eye_df$Eye, hair_eye_df$Hair)

## Yates-corrected chi-squared and Fisher's exact test on two 2 x 2 tables
## (fig:yates-fisher-example).
hair_bb_eyes_bb <- HairEyeColor[1:2, 1:2, ]
hair_bb_eyes_bb_df <- counts_to_cases(
  as.data.frame(hair_bb_eyes_bb)
)
yates_stats <- visstat(
  hair_bb_eyes_bb_df$Eye,
  hair_bb_eyes_bb_df$Hair
)
yates_stats$effect_size

hair_eye_male <- HairEyeColor[, , 1]
black_brown_hazel_green <- hair_eye_male[1:2, 3:4]
black_brown_hazel_green_df <- counts_to_cases(
  as.data.frame(black_brown_hazel_green)
)
fisher_stats <- visstat(
  black_brown_hazel_green_df$Eye,
  black_brown_hazel_green_df$Hair
)
plot(yates_stats, which = 1)
plot(fisher_stats, which = 1)

## Kendall's tau_b on a synthetic survey and Spearman's rho on airquality
## (fig:kendall-spearman-example). The seed is the one used in the manuscript.
set.seed(42)
n <- 150
xs <- sample(1:5, n, replace = TRUE)
ys <- pmin(5, pmax(1, (6 - xs) + sample(-1:1, n, replace = TRUE)))
likert_alc <- c("never", "rarely", "sometimes", "often", "always")
likert_perf <- c("poor", "fair", "ok", "good", "great")
alcohol <- ordered(likert_alc[xs], levels = likert_alc)
performance <- ordered(likert_perf[ys], levels = likert_perf)
kendall_result <- visstat(performance, alcohol, correlation = TRUE)
spearman_air <- visstat(airquality$Wind, airquality$Ozone, correlation = TRUE)

dev.off()


## ---------------------------------------------------------------------------
## 3 Route 1 Monte Carlo simulations
##
## Reproduces fig:route1-identical-typeI, fig:route1-unequal-typeI and
## fig:route1-power.
##
## The published figures use B = 50000 replications per cell, which takes
## several hours on eight cores. B_REPLICATIONS below is set to 2000, which
## reproduces the same picture within a Monte Carlo standard error of about
## 0.5 percentage points at a rejection rate of 5 percent, and runs within the
## time budget of this script. To reproduce the published figures exactly, set
##
##   B_REPLICATIONS <- 50000
##
## Every replication draws from its own L'Ecuyer-CMRG stream, fixed in the
## parent process by the seed at the top of route1_simulations.R, so the output
## does not depend on the number of cores. The parallel layer is
## parallel::mclapply(), which forks and therefore runs in parallel on
## Unix-alikes only; on Windows it falls back to a single core.
## ---------------------------------------------------------------------------

B_REPLICATIONS <- 2000
N_CORES <- max(1L, min(8L, parallel::detectCores(logical = FALSE) - 1L))

## The scripts read and write in the current working directory, so they are run
## on a copy in a scratch directory.
sim_source <- system.file("simulations", package = "visStatistics")
sim_dir <- file.path(getwd(), "route1_replication")
dir.create(sim_dir, showWarnings = FALSE, recursive = TRUE)
## Only the scripts are copied. The results and figures shipped with the
## package are left where they are, so that they cannot be confused with the
## ones produced by this run.
file.copy(
  list.files(sim_source, pattern = "\\.R$", full.names = TRUE), sim_dir,
  overwrite = TRUE
)

old_wd <- setwd(sim_dir)

## Run the grid. Results are written to fleishman_route1_power_B<B>_outputs/.
system2(
  file.path(R.home("bin"), "Rscript"),
  c("--vanilla", "route1_simulations.R", B_REPLICATIONS, N_CORES)
)

## The two figure scripts read the saved results from the working directory.
out_dir <- sprintf("fleishman_route1_power_B%d_outputs", B_REPLICATIONS)
file.copy(
  list.files(out_dir, pattern = "\\.(rds|csv)$", full.names = TRUE),
  sim_dir,
  overwrite = TRUE
)

## NREP enters the file names of the two Type I figures, so that a run with a
## reduced number of replications is not mistaken for the published one.
NREP <- B_REPLICATIONS
source("route1_typeI_figures.R")
source("route1_power_figure.R")

setwd(old_wd)


## ---------------------------------------------------------------------------
## 4 Session information
## ---------------------------------------------------------------------------

sessionInfo()
