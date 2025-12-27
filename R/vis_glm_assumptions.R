#' Visualisation of the normality distribution of the standardised residuals
#'
#' Checks for normality of the standardised residuals in the generalised linear model
#' Student's t-test 
#' (t.test,var=EQUAL) 
#' Fisher oneway ANOVA (aov) or simple linear regression.
#' Performs the Shapiro-Wilk test and Anderson-Darling test for normality and,
#' if not a regression, also the Levene-Brown-Forsythe and the Bartlett's test for homogeneity of variances.
#' It produces a histogram with normal overlay, a residuals vs fitted plot,
#' and a normal Q-Q plot.
#'
#' @param samples Numeric vector; the dependent variable.
#' @param fact Factor; the independent variable.
#' @param conf.level Numeric; confidence level for the tests (default: 0.95).
#' @param cex Numeric; scaling factor for plot text and symbols (default: 1).
#' @param regression Logical; if TRUE, skips Bartlett's test (for regression diagnostics). Default is FALSE.
#'
#' @return A list with elements:
#' \describe{
#'   \item{summary_anova}{Summary of the ANOVA model.}
#'   \item{shapiro_test}{Result from \code{shapiro.test()}.}
#'   \item{ad_test}{Result from \code{nortest::ad.test()} or a character message if n < 7.}
#'   \item{levene_test}{Result from \code{levene.test()} (only if \code{regression = FALSE}).}
#'   \item{bartlett_test}{Result from \code{bartlett.test()} (only if \code{regression = FALSE}).}
#'   \item{bp_test}{Result from \code{bp_test()} Breusch-Pagan test (only if \code{regression = TRUE}).}
#' }
#'
#' @examples
#' # ANOVA example
#' ToothGrowth$dose <- as.factor(ToothGrowth$dose)
#' vis_glm_assumptions(ToothGrowth$len, ToothGrowth$dose)
#' 
#' # Linear regression example
#' vis_glm_assumptions(mtcars$mpg, mtcars$wt, regression = TRUE)
#'
#' @export

vis_glm_assumptions <- function(samples, fact, conf.level = 0.95, 
                                cex = 1, regression = FALSE) {
  # old_par <- par(no.readonly = TRUE)
  # old_par$pin <- NULL
  # old_par$new <- FALSE
  # old_par$fig <- NULL
  # old_par$mfg <- NULL
  # on.exit(par(old_par))
  
  oldparanovassum <- par(no.readonly = TRUE)
  on.exit(par(oldparanovassum))
  
  #cleaning the input
  samples3 <- na.omit(samples)
  fact <- subset(fact, !is.na(samples))
  samples <- samples3
  
  
  
  # Fit model based on type
  if (regression) {
    # Use lm() for regression (required for Breusch-Pagan test)
    anova_model <- lm(samples ~ fact)
  } else {
    # Use aov() for ANOVA (maintains backward compatibility)
    anova_model <- aov(samples ~ fact)
  }
  
  std_residuals <- rstandard(anova_model)
  n_residuals <- length(std_residuals)
  
  # Statistical tests
  if (n_residuals >= 3 && n_residuals <= 5000) {
    shapiro_test <- shapiro.test(std_residuals)
  } else if (n_residuals < 3) {
    shapiro_test <- list(statistic = NA, p.value = NA, method = paste("Shapiro-Wilk test requires sample size of at least 3. (n =", n_residuals, ")"))
  } else {
    shapiro_test <- list(statistic = NA, p.value = NA, method = paste("Shapiro-Wilk test allows maximal sample size of 5000 (n =", n_residuals, ")"))
  }
  
  if (n_residuals >= 7) {
    ad_test <- nortest::ad.test(std_residuals)
  } else {
    ad_test <- "Anderson-Darling test requires sample size of at least 7."
  }
  
  if (!regression) {
    # ANOVA: Test homogeneity of variance across groups
    bartlett_test <- bartlett.test(samples ~ fact)
    levene_test <- levene.test(samples, fact)
    bp_test <- NULL
  } else {
    # Regression: Test for heteroscedasticity using Breusch-Pagan
    bp_test <- bp_test(anova_model)
    bartlett_test <- NULL
    levene_test <- NULL
  }
  
  # Create plots
  par(mfrow = c(2, 2), cex = 0.7*cex, oma = c(0, 0, 3, 0))
  
  x_min <- min(min(std_residuals), -3.2)
  x_max <- max(max(std_residuals), 3.2)
  temp_hist <- hist(std_residuals, plot = FALSE)
  y_max <- max(max(temp_hist$density), 0.45)
  
  # 1. Histogram of standardized residuals
  hist(std_residuals, freq = FALSE, main = "Hist. of Std. Res.", 
       xlab = "Std. Residuals", col = "lightblue", border = "black",
       xlim = c(x_min, x_max), ylim = c(0, y_max))
  x_seq <- seq(x_min, x_max, length.out = 200)
  lines(x_seq, dnorm(x_seq), col = "red", lwd = 2)
  
  # 2. Residuals vs Fitted
  plot(fitted(anova_model), std_residuals, main = "Std. Res. vs. Fitted", 
       ylab = "Std. Residuals", xlab = "Fitted values")
  abline(h = 0, col = "black", lwd = 2)
  
  # 3. Normal Q-Q plot
  qqnorm(std_residuals, ylab = "Std. Residuals", main = "Normal Q-Q Plot")
  qqline(std_residuals, col = "red", lwd = 2)
  
  # 4. Fourth plot depends on analysis type
  if (regression) {
    # Residuals vs Leverage plot with Cook's distance contours
    leverage <- hatvalues(anova_model)
    cooks_d <- cooks.distance(anova_model)
    
    # Calculate plot limits to accommodate Cook's distance contours
    p <- length(coef(anova_model))  # number of parameters
    n <- length(std_residuals)
    
    # Extend leverage range for contour visibility
    lev_max <- max(c(leverage, 0.5))
    lev_seq <- seq(0.001, lev_max, length.out = 100)
    
    plot(leverage, std_residuals,
         main = "Residuals vs Leverage",
         xlab = "Leverage",
         ylab = "Std. Residuals",
         xlim = c(0, lev_max * 1.05))
    abline(h = 0, lty = 2, col = "gray")
    abline(v = 0, lty = 2, col = "gray")
    
    # Add Cook's distance contour lines
    # Cook's D contours are curves in the leverage-residual space
    
    # Draw Cook's D = 0.5 and 1.0 contour lines
    cook_levels <- c(0.5, 1.0)
    
    for (i in seq_along(cook_levels)) {
      cook_level <- cook_levels[i]
      # Cook's D formula: D = (r²/(p)) * (h/(1-h))
      # Solving for r (standardized residual): r = ±sqrt(D * p * (1-h)/h)
      r_upper <- sqrt(cook_level * p * (1 - lev_seq) / lev_seq)
      r_lower <- -r_upper
      
      # Only plot where leverage is reasonable
      valid <- lev_seq > 0.001 & lev_seq < 0.99 & is.finite(r_upper)
      
      lty_style <- if (cook_level == 0.5) 2 else 1
      lines(lev_seq[valid], r_upper[valid], col = "red", lty = lty_style, lwd = 1.5)
      lines(lev_seq[valid], r_lower[valid], col = "red", lty = lty_style, lwd = 1.5)
    }
    
    # Add legend for Cook's distance contours
    legend("topleft", 
           legend = c("Cook's distance = 0.5", "Cook's distance = 1.0"),
           col = c("red", "red"), 
           lty = c(2, 1), 
           lwd = 1.5,
           cex = 0.7,
           bty = "n")  # no box around legend
    
    # Label points with high Cook's distance
    high_cooks <- which(cooks_d > 4/(n-p))
    if (length(high_cooks) > 0) {
      points(leverage[high_cooks], std_residuals[high_cooks], 
             pch = 16, col = "red", cex = 1.2)
      text(leverage[high_cooks], std_residuals[high_cooks],
           labels = high_cooks, pos = 4, cex = 0.7, col = "red")
    }
    
  } else {
    # Scale-Location plot for ANOVA
    sqrt_abs_std_res <- sqrt(abs(std_residuals))
    plot(fitted(anova_model), sqrt_abs_std_res,
         main = "Scale-Location",
         xlab = "Fitted values",
         ylab = expression(sqrt("|Std. Residuals|")))
    # Add LOESS smoothing line only if enough unique fitted values
    n_unique_fitted <- length(unique(fitted(anova_model)))
    if (n_unique_fitted > 5 && length(sqrt_abs_std_res) > 10) {
      tryCatch({
        loess_fit <- loess(sqrt_abs_std_res ~ fitted(anova_model), 
                           span = min(0.75, max(0.3, 10/length(sqrt_abs_std_res))))
        fitted_sorted <- sort(unique(fitted(anova_model)))
        lines(fitted_sorted, predict(loess_fit, fitted_sorted), 
              col = "red", lwd = 2)
      }, warning = function(w) {
        # Silently skip LOESS if singular
      }, error = function(e) {
        # Silently skip LOESS if error
      })
    } else {
      # Just add horizontal reference line if not enough points for LOESS
      abline(h = median(sqrt_abs_std_res), col = "red", lwd = 2, lty = 2)
    }
  }
  
  # Title
  p_shapiro <- if (!is.na(shapiro_test$p.value)) signif(shapiro_test$p.value, 3) else "N/A"
  
  p_AD <- if (is.list(ad_test) && !is.null(ad_test$p.value)) signif(ad_test$p.value, 3) else "N/A"
  
  
  if (regression) {
    # Regression title with Breusch-Pagan test
    p_bp <- signif(bp_test$p.value, 3)
    mtext(text = paste("Regression assumptions: Shapiro p =", p_shapiro, 
                       "| Anderson-Darling p =", p_AD, 
                       "| Breusch-Pagan p =", p_bp), 
          side = 3, outer = TRUE, cex = 0.8)
  } else {
    # ANOVA title with Levene and Bartlett
    mtext(
      text = paste(
        "GLM assumptions: Shapiro p =", signif(p_shapiro, 3),
        "| Anderson-Darling p =", if(is.numeric(p_AD)) signif(p_AD, 3) else p_AD, "\n",
        "Levene-Brown-Forsythe p =", signif(levene_test$p.value, 3),
        "| Bartlett p =", signif(bartlett_test$p.value, 3)
      ),
      side = 3,
      outer = TRUE,
      cex = 0.8
    )
  }
  
  result <- list(
    summary_anova = summary(anova_model),
    shapiro_test = shapiro_test,
    ad_test = ad_test,
    levene_test = if (!regression) levene_test else NULL,
    bartlett_test = if (!regression) bartlett_test else NULL,
    bp_test = if (regression) bp_test else NULL
  )
  class(result) <- "vis_glm_assumptions"
  return(invisible(result))
  
}