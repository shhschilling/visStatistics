#' Visualize pooled standardized residuals normality assessment
#'
#' Creates diagnostic plots for the pooled normality test
#'
#' @param y Numeric vector of response values
#' @param g Factor or vector defining groups
#' @param test_result Result from pooled_normality_test()
#' @param conf.level Confidence level for the test
#' @keywords internal
#' @noRd
plot_pooled_normality <- function(y, g, test_result, conf.level = 0.95) {
  
  g <- as.factor(g)
  alpha <- 1 - conf.level
  
  # Compute standardized residuals (same as in test)
  z <- unlist(
    tapply(y, g, function(x) {
      n <- sum(!is.na(x))
      if (n < 3) return(rep(NA_real_, length(x)))
      (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    })
  )
  z <- z[is.finite(z)]
  
  # Set up 2x2 plot layout
  par(mfrow = c(2, 2))
  
  # 1. Q-Q plot
  qqnorm(z, main = "Normal Q-Q Plot\nPooled Standardized Residuals",
         pch = 19, col = "darkblue")
  qqline(z, col = "red", lwd = 2)
  
  # 2. Histogram with normal overlay
  hist(z, probability = TRUE, 
       main = "Histogram\nPooled Standardized Residuals",
       xlab = "Standardized Residual", 
       col = "lightblue", border = "white")
  curve(dnorm(x, mean = mean(z), sd = sd(z)), 
        add = TRUE, col = "red", lwd = 2)
  
  # 3. Boxplot by group (original, not standardized)
  boxplot(y ~ g, 
          main = "Original Data by Group",
          ylab = "Response", xlab = "Group",
          col = "lightgreen")
  
  # 4. Test results summary
  plot.new()
  title(main = "Normality Test Results")
  
  # Extract test info
  test_name <- test_result$method
  test_stat <- round(test_result$statistic, 4)
  p_value <- round(test_result$p.value, 4)
  
  # Decision text
  decision <- if (p_value < alpha) {
    "Reject normality assumption"
  } else {
    "Fail to reject normality"
  }
  
  # Display text
  text(0.5, 0.8, test_name, cex = 1.2, font = 2)
  text(0.5, 0.6, sprintf("Test statistic: %s", test_stat), cex = 1.1)
  text(0.5, 0.5, sprintf("p-value: %s", p_value), cex = 1.1)
  text(0.5, 0.4, sprintf("α = %.2f", alpha), cex = 1.1)
  text(0.5, 0.25, decision, 
       cex = 1.3, font = 2, 
       col = if (p_value < alpha) "red" else "darkgreen")
  
  par(mfrow = c(1, 1))  # Reset layout
}