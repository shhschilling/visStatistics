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
#' }
#'
#' @examples
#' ToothGrowth$dose <- as.factor(ToothGrowth$dose)
#' vis_glm_assumptions(ToothGrowth$len, ToothGrowth$dose)
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
  
  
  
  anova_model <- aov(samples ~ fact)
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
    bartlett_test <- bartlett.test(samples ~ fact)
    levene_test <- levene.test(samples, fact)
  } 
  
  # Create plots
  par(mfrow = c(1, 3), cex = 0.7*cex, oma = c(0, 0, 3, 0))
  
  x_min <- min(min(std_residuals), -3.2)
  x_max <- max(max(std_residuals), 3.2)
  temp_hist <- hist(std_residuals, plot = FALSE)
  y_max <- max(max(temp_hist$density), 0.45)
  
  hist(std_residuals, freq = FALSE, main = "Hist. of Std. Res.", 
       xlab = "Std. Residuals", col = "lightblue", border = "black",
       xlim = c(x_min, x_max), ylim = c(0, y_max))
  x_seq <- seq(x_min, x_max, length.out = 200)
  lines(x_seq, dnorm(x_seq), col = "red", lwd = 2)
  
  plot(fitted(anova_model), std_residuals, main = "Std. Res. vs. Fitted", ylab="Std. Residuals", xlab="Fitted values")
  abline(h = 0, col = "black", lwd = 2)
  
  qqnorm(std_residuals)
  qqline(std_residuals, col = "red", lwd = 2)
  
  # Title
  p_shapiro <- if (!is.na(shapiro_test$p.value)) signif(shapiro_test$p.value, 3) else "N/A"
  
  p_AD <- if (is.list(ad_test) && !is.null(ad_test$p.value)) signif(ad_test$p.value, 3) else "N/A"
  
  
  if (regression) {
    mtext(text = paste("Shapiro p =", p_shapiro, "| Anderson-Darling p =", p_AD), 
          side = 3, outer = TRUE, cex = 0.8)
  } else {
    mtext(
      text = paste(
        "Shapiro p =", signif(p_shapiro, 3),
    #    "| Anderson-Darling p =", signif(p_AD, 3), "\n",
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
    bartlett_test = if (!regression) bartlett_test else NULL
  )
  class(result) <- "vis_anova_assumptions"
  return(result)
  
}