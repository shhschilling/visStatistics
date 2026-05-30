#' Visualize Numeric Relationships: Regression or Correlation Analysis
#'
#' This function provides unified visualization for numeric relationships between two continuous variables.
#' It can perform either regression analysis (with confidence and prediction bands) or
#' Spearman rank correlation analysis with appropriate visualizations and statistical output.
#' For regression, statistical assumptions are checked and warnings are issued if violated, but analysis proceeds.
#'
#' @param y Numeric vector. The response variable (dependent variable) for regression analysis, 
#'   or the y-axis variable for correlation analysis.
#' @param x Numeric vector. The predictor variable (independent variable) for regression analysis,
#'   or the x-axis variable for correlation analysis. Must have the same length as y.
#' @param correlation Logical. If FALSE (default), performs regression analysis with
#'   confidence and prediction bands. If TRUE, performs Spearman rank correlation analysis.
#' @param conf.level Numeric. Confidence level for statistical tests and intervals. 
#'   Must be between 0 and 1. Default is 0.95 (95 percent confidence level).
#' @param name_of_factor Character string. Label for the x-axis (independent variable). 
#'   If empty, defaults to the variable name.
#' @param name_of_sample Character string. Label for the y-axis (dependent variable). 
#'   If empty, defaults to the variable name.
#'
#' @return A list containing analysis results and assumption checks. Content depends on analysis type.
#'   For regression analysis: analysis_type, summary_regression, assumptions, warnings, r_squared, adj_r_squared.
#'   For correlation analysis: analysis_type, correlation_test, correlation_coefficient, assumptions,
#'   warnings, method_used.
#'
#' @details
#' Statistical Assumptions Checked:
#' Regression: Normality of residuals (Shapiro-Wilk test) and
#' homoscedasticity (Breusch-Pagan test). All regression analyses proceed even if
#' assumptions are violated, but appropriate warnings are issued.
#' Correlation: Spearman rank correlation requires no distributional assumptions.
#'
#' @examples
#' \dontrun{
#' # Generate sample data
#' set.seed(123)
#' x <- rnorm(50, mean = 10, sd = 2)
#' y <- 2 * x + rnorm(50, mean = 0, sd = 1)
#' 
#' # Regression analysis (default)
#' result1 <- vis_numeric(y, x, 
#'                       name_of_factor = "Predictor", 
#'                       name_of_sample = "Response")
#' 
#' # Spearman rank correlation
#' result2 <- vis_numeric(y, x, correlation = TRUE)
#' }
#'
#' @seealso \code{\link{cor.test}}, \code{\link{lm}}, \code{\link{shapiro.test}}
#' 
#' @author Sabine Schilling
#' @noRd
vis_numeric <- function(y,
                        x,
                        correlation = FALSE,
                        conf.level = 0.95,
                        name_of_factor = character(),
                        name_of_sample = character()) {
  
  # Input validation
  if (!is.numeric(y) || !is.numeric(x)) {
    stop("Both 'y' and 'x' must be numeric vectors")
  }
  
  if (length(y) != length(x)) {
    stop("'y' and 'x' must have the same length")
  }
  
  if (!is.logical(correlation) || length(correlation) != 1) {
    stop("'correlation' must be a single logical value (TRUE or FALSE)")
  }
  
  if (!is.numeric(conf.level) || length(conf.level) != 1 || 
      conf.level <= 0 || conf.level >= 1) {
    stop("'conf.level' must be a single number between 0 and 1")
  }
  
  # Define fallback colors if colorscheme() function not available
  if (!exists("colorscheme", mode = "function")) {
    colorscheme <- function(x) {
      if (x == 1) return(c("#1f77b4", "#ff7f0e"))  # blue, orange
      if (x == 2) return(c("#2ca02c", "#d62728"))  # green, red
      return(c("#1f77b4", "#ff7f0e"))  # default
    }
  }
  
  # Store and restore graphical parameters
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  alpha <- 1 - conf.level
  
  # Set default variable names if not provided
  if (length(name_of_factor) == 0 || name_of_factor == "") {
    name_of_factor <- "x"
  }
  if (length(name_of_sample) == 0 || name_of_sample == "") {
    name_of_sample <- "y"
  }
  
  # Remove all NAs from both vectors
  complete_cases <- !is.na(y) & !is.na(x)
  if (sum(complete_cases) < 3) {
    stop("At least 3 complete observations required after removing missing values")
  }
  
  x <- x[complete_cases]
  y <- y[complete_cases]
  n <- length(x)
  
  # Initialize warnings vector
  warnings_list <- character(0)
  
  if (!correlation) {
    # ========== REGRESSION ANALYSIS ==========
    
    # Fit regression model
    reg <- lm(y ~ x)
    reg_summary <- summary(reg)
    raw_residuals <- residuals(reg)
    # Internally studentised residuals for the normality test, matching the
    # residual scale shown by vis_lm_assumptions().
    scaled_residuals <- rstandard(reg)
    if (any(!is.finite(scaled_residuals)))
      scaled_residuals <- raw_residuals / max(sigma(reg), 1e-8)

    # Check regression assumptions
    assumptions <- list()

    # 1. Normality of residuals
    if (n >= 3 && n <= 5000) {
      assumptions$shapiro_residuals <- shapiro.test(scaled_residuals)
      if (assumptions$shapiro_residuals$p.value < alpha) {
        warnings_list <- c(warnings_list, 
                           paste("Normality of residuals violated (Shapiro-Wilk p =", 
                                 signif(assumptions$shapiro_residuals$p.value, 3), ")"))
      }
    } else {
      assumptions$shapiro_residuals <- list(p.value = NA, 
                                            method = "Sample size outside valid range for Shapiro-Wilk test")
    }
    
    # 2. Test for homoscedasticity using Breusch-Pagan test (correct for regression)
    bp_result <- tryCatch({
      bp.test(reg)
    }, error = function(e) {
      list(p.value = NA, method = paste("Breusch-Pagan test failed:", e$message))
    })
    
    assumptions$bp_homoscedasticity <- bp_result
    
    if (!is.na(bp_result$p.value) && bp_result$p.value < alpha) {
      warnings_list <- c(warnings_list, 
                         paste("Homoscedasticity violated (Breusch-Pagan p =", 
                               signif(bp_result$p.value, 3), ")"))
    }
    
    # Order data for plotting
    ord <- order(x)
    x_sorted <- x[ord]
    y_sorted <- y[ord]
    
    # Calculate prediction intervals
    conf_int <- predict(reg, interval = "confidence", level = conf.level)
    pred_int <- suppressWarnings(predict(reg, interval = "prediction", level = conf.level))
    
    y_conf_low <- conf_int[ord, 2]
    y_conf_up <- conf_int[ord, 3]
    y_pred_low <- pred_int[ord, 2]
    y_pred_up <- pred_int[ord, 3]
    fitted_sorted <- reg$fitted[ord]
    
    # Set up plot
    ma <- max(y_sorted, y_pred_up, na.rm = TRUE)
    mi <- min(y_sorted, y_pred_low, na.rm = TRUE)
    spread <- ma - mi
    
    par(mfrow = c(1, 1))
    plot(x_sorted, y_sorted, 
         ylim = c(mi - 0.1 * spread, ma + 0.4 * spread),
         xlab = name_of_factor,
         ylab = name_of_sample,
         main = "")
    
    polygon(c(x_sorted, rev(x_sorted)),
            c(y_conf_low, rev(y_conf_up)),
            col = adjustcolor(colorscheme(1)[1], alpha.f = 0.2),
            border = NA)
    
    # Add regression line and bands
    lines(x_sorted, fitted_sorted, col = colorscheme(2)[1], lwd = 2)
    lines(x_sorted, y_conf_low, col = colorscheme(1)[1], lwd = 2, lty = 2)
    lines(x_sorted, y_conf_up, col = colorscheme(1)[1], lwd = 2, lty = 2)
    lines(x_sorted, y_pred_low, col = colorscheme(1)[2], lwd = 2, lty = 3)
    lines(x_sorted, y_pred_up, col = colorscheme(1)[2], lwd = 2, lty = 3)
    
    legend("topleft", horiz = FALSE,
           c("regression line",
             paste(conf.level*100,"% confidence band"),
             paste(conf.level*100,"% prediction band")),
           lwd = 2,
           col = c(colorscheme(2)[1], colorscheme(1)[1], colorscheme(1)[2]),
           lty = c(1, 2, 3),
           bty = "n",
           cex = 0.7)
    
    # Create title
    conf_int_coeffs <- confint(reg, level = conf.level)
    
    title_text <- paste0("y = b1*x + b0, R-squared = ", sprintf("%.3f", reg_summary$r.squared),
                         ", conf. level = ", sprintf("%.2f", conf.level),
                         "\nslope b1 = ", sprintf("%.2f", reg$coefficients[2]),
                         ", CI [", sprintf("%.2f", conf_int_coeffs[2, 1]),
                         ", ", sprintf("%.2f", conf_int_coeffs[2, 2]), "]",
                         ", p = ", sprintf("%.2e", reg_summary$coefficients[2, 4]),
                         "\nintercept b0 = ", sprintf("%.2f", reg$coefficients[1]),
                         ", CI [", sprintf("%.2f", conf_int_coeffs[1, 1]),
                         ", ", sprintf("%.2f", conf_int_coeffs[1, 2]), "]",
                         ", p = ", sprintf("%.2e", reg_summary$coefficients[1, 4]))
    
    mtext(title_text, cex = 0.7)
    
    # Prepare return values
    result_list <- list(
      analysis_type = "regression",
      independent_variable_x = name_of_factor,
      dependent_variable_y = name_of_sample,
      summary_regression = reg_summary,
      effect_size = effect_size_record(
        "R-squared",
        reg_summary$r.squared,
        "Coefficient of determination for simple linear regression"
      ),
      assumptions = assumptions,
      warnings = warnings_list,
      r_squared = reg_summary$r.squared,
      adj_r_squared = reg_summary$adj.r.squared,
      sample_size = n
    )
    
  } else {
    # ========== CORRELATION ANALYSIS ==========
    
    # Always use Spearman: it subsumes Pearson (nearly identical results
    # for bivariate normal data) while remaining valid for non-normal
    # distributions and non-linear monotonic relationships.
    method_used <- "spearman"
    
    # Perform correlation test
    cor_test <- tryCatch({
      cor.test(x, y, method = method_used, conf.level = conf.level)
    }, warning = function(w) {
      # Handle warnings like "cannot compute exact p-value with ties"
      suppressWarnings(cor.test(x, y, method = method_used, conf.level = conf.level))
    })
    
    cor_coef <- cor_test$estimate
    p_value <- cor_test$p.value
    
    # Spearman correlation: no parametric assumptions required
    assumptions <- list(note = "Spearman correlation: no distributional assumptions required")
    
    # Create visualization
    ord <- order(x)
    x_sorted <- x[ord]
    y_sorted <- y[ord]
    
    # Monotonic trend line for visualization (regression on ranks,
    # mapped back to the original scale)
    trend_reg <- lm(rank(y) ~ rank(x))
    fitted_ranks <- trend_reg$fitted[ord]
    y_range <- max(y_sorted) - min(y_sorted)
    rank_range <- max(fitted_ranks) - min(fitted_ranks)
    if (rank_range > 0) {
      trend_fitted <- min(y_sorted) + (fitted_ranks - min(fitted_ranks)) * y_range / rank_range
    } else {
      trend_fitted <- rep(mean(y_sorted), length(fitted_ranks))
    }
    
    # Set up plot
    ma <- max(y_sorted, na.rm = TRUE)
    mi <- min(y_sorted, na.rm = TRUE)
    spread <- ma - mi
    
    par(mfrow = c(1, 1))

    plot(x_sorted, y_sorted,
         ylim = c(mi - 0.1 * spread, ma + 0.4 * spread),
         xlab = name_of_factor,
         ylab = name_of_sample,
         main = "")

    # Add trend line
    lines(x_sorted, trend_fitted, col = colorscheme(2)[2], lwd = 2, lty = 2)

    legend("topleft", horiz = FALSE,
           paste(tools::toTitleCase(method_used), "correlation trend line"),
           lwd = 2,
           col = colorscheme(2)[2],
           lty = 2,
           bty = "n",
           cex = 0.7)

    # Create title
    significance <- ifelse(p_value < alpha, "significant", "not significant")
    method_name <- tools::toTitleCase(method_used)

    mtext(bquote("Spearman" ~ rho ~ "=" ~ .(signif(cor_coef, 3)) ~
                 ", p =" ~ .(signif(p_value, 3))))
    
    # Prepare return values
    result_list <- list(
      analysis_type = paste(method_used, "correlation"),
      independent_variable_x = name_of_factor,
      dependent_variable_y = name_of_sample,
      correlation_test = cor_test,
      correlation_coefficient = cor_coef,
      effect_size = effect_size_record(
        "Spearman's rho",
        unname(cor_coef),
        "Spearman rank correlation coefficient"
      ),
      p_value = p_value,
      method_used = method_used,
      assumptions = assumptions,
      warnings = warnings_list,
      significance = significance,
      sample_size = n
    )
  }
  
  # Issue warnings to console if any assumptions violated
  if (length(warnings_list) > 0) {
    warning("Statistical assumptions violated:\n",
            paste(warnings_list, collapse = "\n"),
            "\nAnalysis proceeded but interpret results cautiously.",
            call. = FALSE)
    if (!correlation && any(grepl("Normality|Homoscedasticity", warnings_list))) {
      message("RECOMMENDATION: Consider exploring alternatives outside visstat() such as data transformations,\n",
              "generalised linear models, or robust regression. For a non-causal alternative\n",
              "consider rerunning with correlation = TRUE.")
    }
  }
  
  return(result_list)
}
