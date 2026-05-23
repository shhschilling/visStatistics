#' Mean-centred Levene Test for Homogeneity of Variance
#'
#' @description Performs Levene's original mean-centred test. 
#'  It tests the null hypothesis that all groups 
#' have equal variances by testing whether the absolute deviations
#'  from group means are equal across groups.
#'  The function reproduces the behaviour of
#'  leveneTest(y, g, center = mean, ...) of the car package.
#'
#' @param y A numeric response vector.
#' @param g A grouping factor.
#' @param data Optional data frame containing `y` and `g`.
#'
#' @return An object of class \code{"htest"} with components:
#' \item{statistic}{the value of the F-statistic.}
#' \item{parameter}{degrees of freedom: df1=k-1, 
#' df3=N-k, where k is the number of groups and N the total sample size }
#' \item{p.value}{the p-value of the test.}
#' \item{method}{a character string indicating the test performed.}
#' \item{data.name}{a character string giving the name(s) of the data.}
#'
#' @details 
#' For each observation \eqn{y_{ij}} in group \eqn{i}, compute the absolute 
#' deviation from the group mean:
#' 
#' \deqn{z_{ij} = |y_{ij} - \bar{y}_i|}
#' 
#' where \eqn{\bar{y}_i} is the mean of group \eqn{i}.
#' 
#' The test statistic is the F-statistic from a one-way ANOVA on the \eqn{z_{ij}} values:
#' 
#' \deqn{F = \frac{(N-k) \sum_{i=1}^{k} n_i (\bar{z}_i - \bar{z})^2}{(k-1) \sum_{i=1}^{k} \sum_{j=1}^{n_i} (z_{ij} - \bar{z}_i)^2}}
#' 
#' where:
#' \itemize{
#'   \item \eqn{k} = number of groups
#'   \item \eqn{N} = total sample size
#'   \item \eqn{n_i} = sample size of group \eqn{i}
#'   \item \eqn{\bar{z}_i} = mean of absolute deviations in group \eqn{i}
#'   \item \eqn{\bar{z}} = overall mean of all absolute deviations
#' }
#' 
#' Under the null hypothesis of equal variances, the test statistic follows 
#' an F-distribution: \eqn{F \sim F(k-1, N-k)}.
#' 
#' @references
#' Levene, H. (1960). Robust tests for equality of variances. In I. Olkin
#' (Ed.), Contributions to Probability and Statistics (pp. 278-292).
#' Stanford University Press.
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
  
  # Levene test ------
  
  means <- tapply(y, g, mean, na.rm = TRUE)
  z <- abs(y - means[g])
  
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
      method = "Mean-centred Levene Test",
      data.name = "absolute deviations from group means"
    ),
    class = "htest"
  )
}
