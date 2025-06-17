
#' Print method for vis_anova_assumptions
#' @param x An object of class "vis_anova_assumptions"
#' @param ... Additional arguments passed to print
#' @export
print.vis_anova_assumptions <- function(x, ...) {
  cat("ANOVA Assumption Diagnostics\n")
  cat("============================\n\n")
  
  cat("Normality Tests:\n")
  if (!is.null(x$shapiro_test$p.value) && !is.na(x$shapiro_test$p.value)) {
    cat("  Shapiro-Wilk: W =", round(x$shapiro_test$statistic, 4), 
        "p-value =", round(x$shapiro_test$p.value, 4), "\n")
  } else {
    cat("  Shapiro-Wilk:", x$shapiro_test$method, "\n")
  }
  
  if (is.list(x$ad_test) && !is.null(x$ad_test$p.value)) {
    cat("  Anderson-Darling: A =", round(x$ad_test$statistic, 4), 
        "p-value =", round(x$ad_test$p.value, 4), "\n")
  } else {
    if (is.character(x$ad_test)) {
      cat("  Anderson-Darling:", x$ad_test, "\n")
    } else {
      cat("  Anderson-Darling:", x$ad_test$method, "\n")
    }
  }
  
  if (!is.null(x$levene_test) && !is.null(x$bartlett_test)) {
    cat("\nHomogeneity of Variance Tests:\n")
    cat("  Levene-Brown-Forsythe: F =", round(x$levene_test$statistic, 4), 
        "p-value =", round(x$levene_test$p.value, 4), "\n")
    cat("  Bartlett: K-squqred =", round(x$bartlett_test$statistic, 4), 
        "p-value =", round(x$bartlett_test$p.value, 4), "\n")
  }
  
  cat("\n")
  print(x$summary_anova)
  
  invisible(x)
}

#' Summary method for vis_anova_assumptions  
#' @param object An object of class "vis_anova_assumptions"
#' @param ... Additional arguments
#' @export
summary.vis_anova_assumptions <- function(object, ...) {
  print(object)
  invisible(object)
}




