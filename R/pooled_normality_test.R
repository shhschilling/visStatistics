#' Test normality using pooled standardized residuals
#'
#' Standardizes values within each group and applies a single normality 
#' test to the pooled standardized values. This avoids multiple testing 
#' issues when deciding between parametric and non-parametric methods.
#'
#' @param y Numeric vector of response values
#' @param g Factor or vector defining groups (must have at least 2 levels)
#' @param test Character, either "shapiro" or "ad" for Anderson-Darling
#' @param min_n Minimum sample size per group required (default 3)
#' @return A list with test results (statistic, p.value, method, data.name)
#' @keywords internal
#' @noRd
pooled_normality_test <- function(y, g, 
                                  test = c("shapiro", "ad"),
                                  min_n = 3L) {
  test <- match.arg(test)
  g <- as.factor(g)
  
  # Check minimum number of groups
  k <- nlevels(g)
  if (k < 2L) {
    stop("At least 2 groups required for normality test")
  }
  
  # Within-group standardization: z_ij = (y_ij - mean_i) / sd_i
  z <- unlist(
    tapply(y, g, function(x) {
      n <- sum(!is.na(x))
      if (n < min_n) {
        return(rep(NA_real_, length(x)))
      }
      (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    })
  )
  
  # Remove non-finite values
  z <- z[is.finite(z)]
  
  # Check if sufficient data remains
  if (length(z) < min_n) {
    warning(
      sprintf(
        "Insufficient data for normality test: only %d finite standardized values",
        length(z)
      )
    )
    return(list(
      statistic = NA_real_, 
      p.value = NA_real_, 
      method = "Insufficient data for normality test",
      data.name = "pooled standardized residuals"
    ))
  }
  
  # Handle Shapiro-Wilk sample size limits
  if (test == "shapiro") {
    if (length(z) > 5000L) {
      warning("Sample size exceeds Shapiro-Wilk limit (5000); using Anderson-Darling")
      test <- "ad"
    }
  }
  
  # Perform selected test
  if (test == "shapiro") {
    result <- shapiro.test(z)
    result$data.name <- "pooled standardized residuals"
    return(result)
  }
  
  if (test == "ad") {
    if (!requireNamespace("nortest", quietly = TRUE)) {
      stop("Package 'nortest' required for Anderson-Darling test")
    }
    result <- nortest::ad.test(z)
    result$data.name <- "pooled standardized residuals"
    return(result)
  }
}