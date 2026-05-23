#' Breusch-Pagan Test for Heteroscedasticity
#'
#' @description Performs the Breusch-Pagan test for heteroscedasticity in 
#' linear regression models. Tests the null hypothesis that the error variance 
#' is constant (homoscedasticity) against the alternative that the error 
#' variance depends on the fitted values.
#'
#' @param model A fitted linear model object (from \code{lm()}).
#'
#' @return An object of class \code{"htest"} with components:
#' \item{statistic}{the value of the chi-squared test statistic.}
#' \item{parameter}{degrees of freedom.}
#' \item{p.value}{the p-value of the test.}
#' \item{method}{a character string indicating the test performed.}
#' \item{data.name}{a character string giving the name of the model.}
#'
#' @details 
#' The Breusch-Pagan test regresses the squared raw residuals on the fitted
#' values. The test statistic is:
#' 
#' \deqn{BP = n \cdot R^2}
#' 
#' where:
#' \itemize{
#'   \item \eqn{n} = sample size
#'   \item \eqn{R^2} = coefficient of determination from auxiliary regression of 
#'         \eqn{e_i^2} on \eqn{\hat{y}_i}
#' }
#' 
#' Under the null hypothesis of homoscedasticity, the test statistic follows 
#' a chi-squared distribution: \eqn{BP \sim \chi^2(p-1)} where \eqn{p} is the 
#' number of parameters in the model (including intercept).
#' 
#' Large values of the test statistic (small p-values) provide evidence against 
#' homoscedasticity.
#' 
#' @references
#' Breusch, T. S., and Pagan, A. R. (1979). A simple test for heteroscedasticity 
#' and random coefficient variation. Econometrica, 47(5), 1287-1294.
#' DOI: 10.2307/1911963
#' 
#' @examples
#' # Example with homoscedastic errors
#' set.seed(123)
#' x <- runif(100)
#' y <- 2 + 3*x + rnorm(100, sd = 1)
#' model1 <- lm(y ~ x)
#' bp.test(model1)  # Should not reject (p > 0.05)
#' 
#' # Example with heteroscedastic errors (variance increases with x)
#' set.seed(456)
#' x <- runif(100)
#' y <- 2 + 3 *x + rnorm(100, sd = 0.5 + 2*x)
#' model2 <- lm(y ~ x)
#' bp.test(model2)  # Should reject (p < 0.05)
#'
#' @export

bp.test <- function(model) {
  
  # Error handling
  if (!inherits(model, "lm")) {
    stop("model must be an object of class 'lm'")
  }
  
  # Extract residuals and fitted values
  raw_residuals <- residuals(model)
  fitted_values <- fitted(model)
  n <- length(raw_residuals)
  
  # Number of parameters (including intercept)
  p <- length(coef(model))
  
  # Square the raw residuals
  sq_raw_residuals <- raw_residuals^2
  
  # Auxiliary regression: sq_raw_residuals ~ fitted_values
  aux_model <- lm(sq_raw_residuals ~ fitted_values)
  
  # Get R-squared from auxiliary regression
  r_squared <- summary(aux_model)$r.squared
  
  # Calculate test statistic: n * R^2
  bp_statistic <- n * r_squared
  
  # Degrees of freedom (number of predictors in auxiliary regression)
  # For simple linear regression with 1 predictor, df = 1
  df <- p - 1
  
  # Calculate p-value from chi-squared distribution
  p_value <- pchisq(bp_statistic, df = df, lower.tail = FALSE)
  
  # Return htest object
  structure(
    list(
      statistic = c(BP = bp_statistic),
      parameter = c(df = df),
      p.value = p_value,
      method = "Breusch-Pagan test for heteroscedasticity",
      data.name = deparse(substitute(model))
    ),
    class = "htest"
  )
}
