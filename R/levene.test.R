#' Levene-Brown-Forsythe Test for Homogeneity of Variance (center = median)
#'
#' @description Performs Levene's test using the
#'  Brown-Forsyth modification (median-centred). It reproduces th
#'  default behaviour of the leveneTest() of the car-package. 
#'
#' @param y A numeric response vector.
#' @param g A grouping factor.
#' @param data Optional data frame containing `y` and `g`.
#'
#' @return An object of class \code{"htest"} with components:
#' \item{statistic}{the value of the F-statistic.}
#' \item{parameter}{degrees of freedom of the numerator and denominator.}
#' \item{p.value}{the p-value of the test.}
#' \item{method}{a character string indicating the test performed.}
#' \item{data.name}{a character string giving the name(s) of the data.}
#'
#' @references
#' Brown, M. B., and Forsythe, A. B. (1974). Robust tests for the equality of
#' variances. Journal of the American Statistical Association, 69(346), 364–367.
#' DOI: 10.1080/01621459.1974.10482955
#' 
#' @examples
#' set.seed(123)
#' y <- c(rnorm(10), rnorm(10, sd = 2), rnorm(10, sd = 0.5))
#' g <- factor(rep(1:3, each = 10))
#' levene.test(y, g)
#' 
#' # Usage with data frame
#' df <- data.frame(response = y, group = g)
#' levene.test(response, group, data = df)
#' 
#' # Example with unequal variances (should reject null hypothesis)
#' set.seed(456)
#' y_unequal <- c(rnorm(15, sd = 1), rnorm(15, sd = 5), rnorm(15, sd = 0.2))
#' g_unequal <- factor(rep(c("A", "B", "C"), each = 15))
#' levene.test(y_unequal, g_unequal)
#'
#' 
#'
#' @export
levene.test <- function(y, g, data = NULL) {
  
  
   # Error handling ----
  
  
 
  
  if (!is.null(data)) {
    y <- eval(substitute(y), data, parent.frame())
    g <- eval(substitute(g), data, parent.frame())
  }
  
  if (!is.numeric(y)) stop("`y` must be numeric")
  if (!is.factor(g)) g <- factor(g)
  
  # Leven test ------
  
  medians <- tapply(y, g, median, na.rm = TRUE)
  z <- abs(y - medians[g])
  
  fit <- stats::aov(z ~ g)
  aov_summary <- summary(fit)[[1]]
  
  f_value <- aov_summary[["F value"]][1]
  p_value <- aov_summary[["Pr(>F)"]][1]
  df1 <- aov_summary[["Df"]][1]
  df2 <- aov_summary[["Df"]][2]
  
  structure(
    list(
      statistic = c(F = f_value),
      parameter = c(df1 = df1, df2 = df2),
      p.value = p_value,
      method = "Levene-Brown-Forsythe Test (center = median)",
      data.name = "absolute deviations from group medians (for ANOVA on spread differences)"
    ),
    class = "htest"
  )
}