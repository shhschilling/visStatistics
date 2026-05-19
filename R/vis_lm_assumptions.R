#' Visualisation of the normality distribution of the standardised residuals
#'
#' Checks for normality of the standardised residuals in the general linear model
#' Student's t-test 
#' (t.test,var=EQUAL) 
#' Fisher oneway ANOVA (aov) or simple linear regression.
#' Performs the Shapiro-Wilk test and Anderson-Darling test for normality and,
#' if not a regression, also the Brown-Forsythe Levene-type and Bartlett's tests for homogeneity of variances.
#' It produces a histogram with normal overlay, a residuals vs fitted plot,
#' and a normal Q-Q plot.
#'
#' @param samples Numeric vector; the dependent variable.
#' @param fact Factor; the independent variable.
#' @param cex Numeric; scaling factor for plot text and symbols (default: 1).
#' @param correlation Logical. If \code{FALSE} and \code{fact} is numeric,
#'   regression diagnostics are shown. If \code{TRUE}, no regression
#'   diagnostics are shown. Default is \code{FALSE}.
#'
#' @return A list with elements:
#' \describe{
#'   \item{summary_anova}{Summary of the ANOVA model.}
#'   \item{shapiro_test}{Result from \code{shapiro.test()}.}
#'   \item{ad_test}{Result from \code{nortest::ad.test()} or a character message if n < 7.}
#'   \item{levene_test}{Result from \code{levene.test()} (grouped diagnostics only).}
#'   \item{bartlett_test}{Result from \code{bartlett.test()} (grouped diagnostics only).}
#'   \item{bp_test}{Result from \code{bp.test()} (regression diagnostics only).}
#' }
#'
#' @examples
#' ToothGrowth$dose <- as.factor(ToothGrowth$dose)
#' vis_lm_assumptions(ToothGrowth$len, ToothGrowth$dose)
#'
#' @export

vis_lm_assumptions <- function(samples, fact, cex = 1, correlation = FALSE) {
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  # Clean the input
  samples3 <- na.omit(samples)
  fact <- subset(fact, !is.na(samples))
  samples <- samples3
  
  # Fit model
  anova_model <- aov(samples ~ fact) # needed for correct output structure, do not use lm(samples~fact)
  std_residuals <- rstandard(anova_model)
  
  # Run assumption tests
  # Shapiro-Wilk test (for n >= 3 and n <= 5000)
  if (length(std_residuals) >= 3 && length(std_residuals) <= 5000) {
    shapiro_test <- shapiro.test(std_residuals)
  } else {
    shapiro_test <- list(
      method = "Shapiro-Wilk normality test",
      statistic = NA,
      p.value = NA,
      data.name = "standardized residuals"
    )
  }
  
  # Anderson-Darling test (for n >= 7)
  if (length(std_residuals) >= 7) {
    ad_test <- nortest::ad.test(std_residuals)
  } else {
    ad_test <- "Sample size too small (n < 7) for Anderson-Darling test"
  }
  
  regression_mode <- !isTRUE(correlation) && (is.numeric(fact) || is.integer(fact))
  
  # Variance tests (only for grouped diagnostics)
  if (!regression_mode) {
    levene_test <- levene.test(samples, fact)
    bartlett_test <- bartlett.test(samples ~ fact)
     bp_test <- NULL
  } else {
    # For regression: use Breusch-Pagan test for heteroscedasticity
    # (tests if error variance depends on independent variables)
    levene_test <- NULL
    bartlett_test <- NULL
     bp_test <-   bp.test(anova_model)
  }
  
  # Set up plotting area with outer margin for title
  par(mfrow = c(2, 2), oma = c(0, 0, 3, 0), cex = 0.7 * cex)
  
  # Plot 1: Histogram with normal overlay
  x_min <- min(min(std_residuals), -3.2)
  x_max <- max(max(std_residuals), 3.2)
  temp_hist <- hist(std_residuals, plot = FALSE)
  y_max <- max(max(temp_hist$density), 0.45)
  
  hist(std_residuals, freq = FALSE, main = "Histogram and Normal Density", 
       xlab = "Std. Residuals", col = "lightblue", border = "black",
       xlim = c(x_min, x_max), ylim = c(0, y_max))
  x_seq <- seq(x_min, x_max, length.out = 200)
  lines(x_seq, dnorm(x_seq), col = "red", lwd = 1)
  
  # Plot 2: Standardized Residuals vs Fitted (manual - plot.lm doesn't use standardized)
  ylim_res <- extendrange(std_residuals, f = 0.08)
  plot(fitted(anova_model), std_residuals, 
       main = "Std. Res. vs. Fitted",
       xlab = "Fitted values", 
       ylab = "Std. Residuals",
       ylim = ylim_res)
  abline(h = 0, col = "red", lwd = 1)
  
  
  
  # Plot 3: Normal Q-Q (using internal plot.lm method)
  plot(anova_model, which = 2, cex = cex, caption = "", sub.caption = "", main = "Normal Q-Q Plot")
  
  # Plot 4: Depends on regression vs ANOVA
  if (regression_mode) {
    # Residuals vs Leverage with Cook's distance (using internal plot.lm method)
    plot(anova_model, which = 5, cex = cex, caption = "", sub.caption = "", main = "Std. Res. vs. Leverage")
  } else {
    # Scale-Location plot (using internal plot.lm method)
    plot(anova_model, which = 3, cex = cex, caption = "", sub.caption = "", main = "Scale-Location")
  }
  
  # Add overall title with test results
  p_shapiro <- if (!is.na(shapiro_test$p.value)) signif(shapiro_test$p.value, 2) else "N/A"
  p_AD <- if (is.list(ad_test) && !is.null(ad_test$p.value)) signif(ad_test$p.value, 2) else "N/A"
  
  if (regression_mode) {
    # Regression title with Breusch-Pagan test - split into two rows
    p_bp <- signif( bp_test$p.value, 2)
    title_line1 <- paste("Linear model assumptions: Shapiro-Wilk p =", p_shapiro, 
                         "| Anderson-Darling p =", p_AD)
    title_line2 <- paste("Breusch-Pagan p =", p_bp)
    
    mtext(title_line1, side = 3, outer = TRUE, line = 1, cex = 0.7)
    mtext(title_line2, side = 3, outer = TRUE, line = 0, cex = 0.7)
  } else {
    # ANOVA title with Levene and Bartlett - split into two rows
    title_line1 <- paste("Linear model assumptions: Shapiro-Wilk p =", p_shapiro,
                         "| Anderson-Darling p =", if(is.numeric(p_AD)) signif(p_AD, 2) else p_AD)
    title_line2 <- paste("Brown-Forsythe Levene-type p =", signif(levene_test$p.value, 2),
                         "| Bartlett p =", signif(bartlett_test$p.value, 2))
    
    mtext(title_line1, side = 3, outer = TRUE, line = 1, cex = 0.7)
    mtext(title_line2, side = 3, outer = TRUE, line = 0, cex = 0.7)
  }
  
  # Return results
  result <- list(
    summary_anova = summary(anova_model),
    shapiro_test = shapiro_test,
    ad_test = ad_test,
    levene_test = if (!regression_mode) levene_test else NULL,
    bartlett_test = if (!regression_mode) bartlett_test else NULL,
    bp_test = if (regression_mode)  bp_test else NULL
  )
  
  class(result) <- "vis_lm_assumptions"
  return(invisible(result))
}

#' @noRd
vis_anova_assumptions <- function(...) {
  .Deprecated("vis_lm_assumptions")
  vis_lm_assumptions(...)
}
