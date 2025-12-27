#' Visualize normality assumption using within-group standardized residuals
#'
#' Performs within-group standardization and applies a single normality test
#' to avoid multiple testing issues when deciding between parametric and 
#' non-parametric methods. Observations within each group are standardized 
#' using that group's own mean and standard deviation. The standardized 
#' values are pooled and tested for normality. Produces a Q-Q plot, 
#' histogram with normal overlay, and residuals vs fitted plot.
#'
#' @param y Numeric vector; the response variable.
#' @param g Factor or vector; the grouping variable.
#' @param conf.level Numeric; confidence level for the test (default: 0.95).
#' @param cex Numeric; scaling factor for plot text and symbols (default: 1).
#' @param test Character; either "shapiro" for Shapiro-Wilk test or "ad" 
#'   for Anderson-Darling test (default: "shapiro").
#'
#' @return A list with elements:
#' \describe{
#'   \item{shapiro_within_group_std}{Result from \code{shapiro.test()} on within-group standardized residuals.}
#'   \item{ad_within_group_std}{Result from \code{nortest::ad.test()} on within-group standardized residuals, or list with NA if n < 7.}
#'   \item{within_group_std_residuals}{Numeric vector of within-group standardized residuals pooled across groups.}
#'   \item{n_groups}{Number of groups.}
#' }
#'
#' @examples
#' ToothGrowth$dose <- as.factor(ToothGrowth$dose)
#' vis_normality_assumption(ToothGrowth$len, ToothGrowth$dose)
#'
#' @export

vis_normality_assumption <- function(y, g, conf.level = 0.95, 
                                     cex = 1, test = "shapiro") {
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  g <- as.factor(g)
  alpha <- 1 - conf.level
  
  # Compute within-group standardized residuals
  within_group_std_residuals <- unlist(
    tapply(y, g, function(x) {
      n <- sum(!is.na(x))
      if (n < 3) return(rep(NA_real_, length(x)))
      (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    })
  )
  within_group_std_residuals <- within_group_std_residuals[is.finite(within_group_std_residuals)]
  
  n_residuals <- length(within_group_std_residuals)
  
  # Perform Shapiro-Wilk test
  if (n_residuals >= 3 && n_residuals <= 5000) {
    shapiro_test <- shapiro.test(within_group_std_residuals)
  } else if (n_residuals < 3) {
    shapiro_test <- list(statistic = NA, p.value = NA, 
                         method = paste("Shapiro-Wilk test requires sample size of at least 3. (n =", n_residuals, ")"))
  } else {
    shapiro_test <- list(statistic = NA, p.value = NA, 
                         method = paste("Shapiro-Wilk test allows maximal sample size of 5000 (n =", n_residuals, ")"))
  }
  
  # Perform Anderson-Darling test (if n >= 7)
  if (n_residuals >= 7) {
    ad_test <- nortest::ad.test(within_group_std_residuals)
  } else {
    ad_test <- list(p.value = NA, 
                    method = "Anderson-Darling test requires sample size of at least 7.")
  }
  
  # Create plots - exact layout as vis_glm_assumptions
  par(mfrow = c(1, 3), cex = 0.7 * cex, oma = c(0, 0, 3, 0))
  
  # Determine plot limits
  x_min <- min(min(within_group_std_residuals), -3.2)
  x_max <- max(max(within_group_std_residuals), 3.2)
  temp_hist <- hist(within_group_std_residuals, plot = FALSE)
  y_max <- max(max(temp_hist$density), 0.45)
  
  # 1. Histogram with normal overlay
  hist(within_group_std_residuals, freq = FALSE, 
       main = "Hist. of Within-Group Std. Res.", 
       xlab = "Within-Group Std. Residuals", 
       col = "lightblue", border = "black",
       xlim = c(x_min, x_max), ylim = c(0, y_max))
  x_seq <- seq(x_min, x_max, length.out = 200)
  lines(x_seq, dnorm(x_seq), col = "red", lwd = 2)
  
  # 2. Residuals vs Fitted (group means are the fitted values)
  group_means <- tapply(y, g, mean, na.rm = TRUE)[g]
  plot(group_means, within_group_std_residuals, 
       main = "Within-Group Std. Res. vs. Fitted", 
       ylab = "Within-Group Std. Residuals", 
       xlab = "Fitted values")
  abline(h = 0, col = "black", lwd = 2)
  
  # 3. Q-Q plot
  qqnorm(within_group_std_residuals, main = "Normal Q-Q Plot")
  qqline(within_group_std_residuals, col = "red", lwd = 2)
  
  # Title with test results - matching vis_glm_assumptions format
  p_shapiro <- if (!is.na(shapiro_test$p.value)) {
    signif(shapiro_test$p.value, 3)
  } else {
    "N/A"
  }
  
  p_AD <- if (!is.na(ad_test$p.value)) {
    signif(ad_test$p.value, 3)
  } else {
    "N/A"
  }
  
  mtext(
    text = paste(
      "Shapiro p =", p_shapiro,
      "| Anderson-Darling p =", p_AD
    ),
    side = 3,
    outer = TRUE,
    cex = 0.8
  )
  # Return object with clear naming to distinguish from GLM tests
  result <- list(
    shapiro_within_group_std = shapiro_test,
    ad_within_group_std = ad_test,
    within_group_std_residuals = within_group_std_residuals,
    n_groups = nlevels(g)
  )
  class(result) <- "vis_normality_assumption"
  return(result)
}