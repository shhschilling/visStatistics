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

pooled_normality_test <- function(y, g, 
                                  test = c("shapiro", "ad"),
                                  min_n = 3L) {
  test <- match.arg(test)
  g <- as.factor(g)
  
  # Ensure y and g are the same length and handle NAs upfront
  complete_cases <- !is.na(y) & !is.na(g)
  y <- y[complete_cases]
  g <- g[complete_cases]
  
  if (nlevels(g) < 2L) {
    stop("At least 2 groups required for normality test")
  }
  
  # Within-group standardization: z_ij = (y_ij - mean_i) / sd_i
  # ave() keeps the original order and is more concise
  z <- ave(y, g, FUN = function(x) {
    n <- length(x)
    if (n < min_n) return(rep(NA_real_, n))
    # Scale center = TRUE, scale = TRUE performs (x - mean)/sd
    as.vector(scale(x))
  })
  
  # Remove NAs from groups that didn't meet min_n
  z <- z[!is.na(z)]
  
  # Sample size checks
  n_total <- length(z)
  if (n_total < min_n) {
    warning(sprintf("Insufficient data: only %d values available", n_total))
    return(list(statistic = NA, p.value = NA, method = "Insufficient data", data.name = "z"))
  }
  
  # Automated fallback for Shapiro-Wilk limit
  if (test == "shapiro" && n_total > 5000L) {
    warning("Sample size > 5000; falling back to Anderson-Darling")
    test <- "ad"
  }
  
  # Execute tests
  res <- switch(test,
                shapiro = shapiro.test(z),
                ad = {
                  if (!requireNamespace("nortest", quietly = TRUE)) {
                    stop("Package 'nortest' required for Anderson-Darling test")
                  }
                  nortest::ad.test(z)
                }
  )
  
  res$data.name <- "pooled standardized residuals"
  return(res)
}