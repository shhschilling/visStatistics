#' Visualize Numeric Relationships: Regression or Correlation Analysis
#'
#' This function provides unified visualization for numeric relationships between two continuous variables.
#' It can perform either regression analysis (with confidence and prediction bands) or 
#' correlation analysis (automatically selecting the most appropriate method) with 
#' appropriate visualizations and statistical output.
#' All statistical assumptions are checked and warnings are issued if violated, but analysis proceeds.
#'
#' @param y Numeric vector. The response variable (dependent variable) for regression analysis, 
#'   or the y-axis variable for correlation analysis.
#' @param x Numeric vector. The predictor variable (independent variable) for regression analysis,
#'   or the x-axis variable for correlation analysis. Must have the same length as y.
#' @param do_regression Logical. If TRUE (default), performs regression analysis with 
#'   confidence and prediction bands. If FALSE, performs correlation analysis with 
#'   automatic method selection and trend line.
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
#'   warnings, method_used, method_selection_reason.
#'
#' @details
#' Statistical Assumptions Checked:
#' Regression: Normality of residuals (Shapiro-Wilk test) and 
#' homoscedasticity/equal variances (Bartlett test).
#' Correlation: When do_regression = FALSE, the function automatically selects between 
#' Pearson correlation (if both variables are normally distributed) or Spearman correlation 
#' (if normality assumptions are violated or sample size is outside valid range).
#' 
#' All analyses proceed even if assumptions are violated, but appropriate warnings are issued.
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
#' # Correlation analysis with automatic method selection
#' result2 <- vis_numeric(y, x, do_regression = FALSE)
#' }
#'
#' @seealso \code{\link{cor.test}}, \code{\link{lm}}, \code{\link{shapiro.test}}
#' 
#' @author Sabine Schilling
#' @export
vis_numeric <- function(y,
                        x,
                        do_regression = TRUE,
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
  
  if (!is.logical(do_regression) || length(do_regression) != 1) {
    stop("'do_regression' must be a single logical value (TRUE or FALSE)")
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
  
  if (do_regression) {
    # ========== REGRESSION ANALYSIS ==========
    
    # Fit regression model
    reg <- lm(y ~ x)
    reg_summary <- summary(reg)
    
    # Check regression assumptions
    assumptions <- list()
    
    # 1. Normality of residuals
    if (n >= 3 && n <= 5000) {
      assumptions$shapiro_residuals <- shapiro.test(rstandard(reg))
      if (assumptions$shapiro_residuals$p.value < alpha) {
        warnings_list <- c(warnings_list, 
                           paste("Normality of residuals violated (Shapiro-Wilk p =", 
                                 signif(assumptions$shapiro_residuals$p.value, 3), ")"))
      }
    } else {
      assumptions$shapiro_residuals <- list(p.value = NA, 
                                            method = "Sample size outside valid range for Shapiro-Wilk test")
    }
    
    # 2. Test for homoscedasticity using Levene test
    if (n >= 6) {  # Minimum sample size for Levene test
      residuals_std <- rstandard(reg)
      fitted_vals <- reg$fitted.values
      
      # Create groups based on fitted values for Levene test
      fitted_groups <- cut(fitted_vals, breaks = 2, labels = FALSE)
      fitted_groups <- factor(fitted_groups)
      
      # Levene test for homoscedasticity using custom implementation
      levene_result <- tryCatch({
        levene.test(residuals_std, fitted_groups)
      }, error = function(e) {
        list(p.value = NA, method = paste("Levene test failed:", e$message))
      })
      
      assumptions$levene_homoscedasticity <- levene_result
      
      if (!is.na(levene_result$p.value) && levene_result$p.value < alpha) {
        warnings_list <- c(warnings_list, 
                           paste("Homoscedasticity violated (Levene test p =", 
                                 signif(levene_result$p.value, 3), ")"))
      }
    } else {
      assumptions$levene_note <- "Sample size too small for homoscedasticity test (need n >= 6)"
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
    
    par(mfrow = c(1, 1), oma = c(0, 0, 5, 0))
    
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
           cex = 0.8)
    
    # Create title
    conf_int_coeffs <- confint(reg, level = conf.level)
    
    title_text <- paste0("y = b1*x + b0, R-squared = ", signif(reg_summary$r.squared, 3),", conf. level = ",conf.level,
                         "\nslope b1 = ", signif(reg$coefficients[2], 3),
                         ", CI [", signif(conf_int_coeffs[2, 1], 3), 
                         ", ", signif(conf_int_coeffs[2, 2], 3), "]",
                         ", p = ", signif(reg_summary$coefficients[2, 4], 3),
                         "\nintercept b0 = ", signif(reg$coefficients[1], 3),
                         ", CI [", signif(conf_int_coeffs[1, 1], 3),
                         ", ", signif(conf_int_coeffs[1, 2], 3), "]",
                         ", p = ", signif(reg_summary$coefficients[1, 4], 3))
    
    mtext(title_text, outer = TRUE)
    
    # Prepare return values
    result_list <- list(
      analysis_type = "regression",
      independent_variable_x = name_of_factor,
      dependent_variable_y = name_of_sample,
      summary_regression = reg_summary,
      assumptions = assumptions,
      warnings = warnings_list,
      r_squared = reg_summary$r.squared,
      adj_r_squared = reg_summary$adj.r.squared,
      sample_size = n
    )
    
  } else {
    # ========== CORRELATION ANALYSIS ==========
    
    # Automatically determine correlation method
    method_used <- "auto"
    selection_reason <- ""
    
    # Test normality of both variables for automatic selection
    norm_tests <- list()
    
    if (n >= 3 && n <= 5000) {
      norm_tests$shapiro_x <- shapiro.test(x)
      norm_tests$shapiro_y <- shapiro.test(y)
      
      both_normal <- norm_tests$shapiro_x$p.value > alpha && 
        norm_tests$shapiro_y$p.value > alpha
      
      if (both_normal) {
        method_used <- "pearson"
        selection_reason <- "Auto-selected: Both variables normally distributed"
      } else {
        method_used <- "spearman"
        selection_reason <- paste("Auto-selected: Normality violated (", name_of_sample, ": p =", 
                                  signif(norm_tests$shapiro_y$p.value, 3),
                                  ", ", name_of_factor, ": p =", signif(norm_tests$shapiro_x$p.value, 3), ")")
      }
    } else {
      method_used <- "spearman"
      selection_reason <- "Auto-selected: Sample size outside valid range for normality tests"
    }
    
    # Perform correlation test
    cor_test <- tryCatch({
      cor.test(x, y, method = method_used, conf.level = conf.level)
    }, warning = function(w) {
      # Handle warnings like "cannot compute exact p-value with ties"
      suppressWarnings(cor.test(x, y, method = method_used, conf.level = conf.level))
    })
    
    cor_coef <- cor_test$estimate
    p_value <- cor_test$p.value
    
    # Check assumptions for Pearson correlation
    assumptions <- list()
    
    if (method_used == "pearson") {
      # Check bivariate normality (approximated by individual normality tests)
      if (n >= 3 && n <= 5000) {
        assumptions$shapiro_x <- shapiro.test(x)
        assumptions$shapiro_y <- shapiro.test(y)
        
        if (assumptions$shapiro_x$p.value < alpha) {
          warnings_list <- c(warnings_list, 
                             paste("x variable not normally distributed (Shapiro-Wilk p =", 
                                   signif(assumptions$shapiro_x$p.value, 3), ")"))
        }
        
        if (assumptions$shapiro_y$p.value < alpha) {
          warnings_list <- c(warnings_list, 
                             paste("y variable not normally distributed (Shapiro-Wilk p =", 
                                   signif(assumptions$shapiro_y$p.value, 3), ")"))
        }
        
        if (length(warnings_list) > 0) {
          warnings_list <- c(warnings_list, "Consider using do_regression = FALSE for robust analysis")
        }
      } else {
        assumptions$normality_note <- "Sample size outside valid range for normality tests"
      }
    } else {
      assumptions$note <- "Spearman correlation: No parametric assumptions required"
    }
    
    # Create visualization
    ord <- order(x)
    x_sorted <- x[ord]
    y_sorted <- y[ord]
    
    # Fit trend line for visualization only
    if (method_used == "pearson") {
      # Linear trend line
      trend_reg <- lm(y ~ x)
      trend_fitted <- trend_reg$fitted[ord]
    } else {
      # Monotonic trend approximation for Spearman
      trend_reg <- lm(rank(y) ~ rank(x))
      fitted_ranks <- trend_reg$fitted[ord]
      # Map ranks back to approximate original scale
      y_range <- max(y_sorted) - min(y_sorted)
      rank_range <- max(fitted_ranks) - min(fitted_ranks)
      if (rank_range > 0) {
        trend_fitted <- min(y_sorted) + (fitted_ranks - min(fitted_ranks)) * y_range / rank_range
      } else {
        trend_fitted <- rep(mean(y_sorted), length(fitted_ranks))
      }
    }
    
    # Set up plot
    ma <- max(y_sorted, na.rm = TRUE)
    mi <- min(y_sorted, na.rm = TRUE)
    spread <- ma - mi
    
    par(mfrow = c(1, 1), oma = c(0, 0, 5, 0))
    
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
           cex = 0.8)
    
    # Create title
    significance <- ifelse(p_value < alpha, "significant", "not significant")
    method_name <- tools::toTitleCase(method_used)
    
    title_text <- paste0(method_name, " r = ", signif(cor_coef, 3),
                         ", p = ", signif(p_value, 3), 
                         " (", significance, ")")
    
    mtext(title_text, outer = TRUE)
    
    # Prepare return values
    result_list <- list(
      analysis_type = paste(method_used, "correlation"),
      independent_variable_x = name_of_factor,
      dependent_variable_y = name_of_sample,
      correlation_test = cor_test,
      correlation_coefficient = cor_coef,
      p_value = p_value,
      method_used = method_used,
      method_selection_reason = selection_reason,
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
    
    # Suggest correlation analysis if regression assumptions are violated
    if (do_regression && any(grepl("Normality|Homoscedasticity", warnings_list))) {
      message("RECOMMENDATION: Consider using do_regression = FALSE for robust correlation analysis")
    }
  }
  
  return(result_list)
}