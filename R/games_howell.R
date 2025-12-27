#' Games-Howell Post-Hoc Test
#'
#' Performs pairwise comparisons using the Games-Howell test, which does not
#' assume equal variances or equal sample sizes. This is the appropriate
#' post-hoc test to use after a significant Welch's ANOVA.
#'
#' @param samples Numeric vector; the dependent variable.
#' @param groups Factor or vector; the grouping variable.
#' @param conf.level Numeric; confidence level for confidence intervals (default: 0.95).
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{group1}{First group in comparison}
#'   \item{group2}{Second group in comparison}
#'   \item{mean_diff}{Difference in means (group1 - group2)}
#'   \item{se}{Standard error of the difference}
#'   \item{t}{t-statistic}
#'   \item{df}{Degrees of freedom (Welch-Satterthwaite)}
#'   \item{p_value}{Unadjusted p-value}
#'   \item{p_adj}{Holm-adjusted p-value for multiple comparisons}
#'   \item{ci_lower}{Lower bound of confidence interval}
#'   \item{ci_upper}{Upper bound of confidence interval}
#'   \item{significant}{Logical; TRUE if p_adj < (1 - conf.level)}
#' }
#'
#' @details
#' The Games-Howell test uses the Welch-Satterthwaite approximation for
#' degrees of freedom and does not pool variances. P-values are adjusted
#' using the Holm method to control family-wise error rate.
#'
#' @examples
#' # Convert dose to factor
#' ToothGrowth$dose <- as.factor(ToothGrowth$dose)
#' 
#' # Perform Games-Howell test
#' result <- games_howell(ToothGrowth$len, ToothGrowth$dose)
#' print(result)
#'
#' @export

games_howell <- function(samples, groups, conf.level = 0.95) {
  
  # Input validation
  if (!is.numeric(samples)) {
    stop("samples must be numeric")
  }
  
  if (length(samples) != length(groups)) {
    stop("samples and groups must have the same length")
  }
  
  # Clean data
  complete_cases <- complete.cases(samples, groups)
  samples <- samples[complete_cases]
  groups <- as.factor(groups[complete_cases])
  
  # Check minimum requirements
  group_levels <- levels(groups)
  k <- length(group_levels)
  
  if (k < 2) {
    stop("At least 2 groups required")
  }
  
  # Calculate group statistics
  n <- tapply(samples, groups, length)
  means <- tapply(samples, groups, mean)
  vars <- tapply(samples, groups, var)
  
  # Generate all pairwise comparisons
  comparisons <- combn(k, 2)
  n_comparisons <- ncol(comparisons)
  
  # Initialize results
  results <- data.frame(
    group1 = character(n_comparisons),
    group2 = character(n_comparisons),
    mean_diff = numeric(n_comparisons),
    se = numeric(n_comparisons),
    t = numeric(n_comparisons),
    df = numeric(n_comparisons),
    p_value = numeric(n_comparisons),
    ci_lower = numeric(n_comparisons),
    ci_upper = numeric(n_comparisons),
    stringsAsFactors = FALSE
  )
  
  # Perform pairwise comparisons
  for (i in 1:n_comparisons) {
    g1_idx <- comparisons[1, i]
    g2_idx <- comparisons[2, i]
    
    g1 <- group_levels[g1_idx]
    g2 <- group_levels[g2_idx]
    
    # Mean difference
    mean_diff <- means[g1] - means[g2]
    
    # Standard error (Welch formula - no pooling)
    se <- sqrt(vars[g1]/n[g1] + vars[g2]/n[g2])
    
    # Degrees of freedom (Welch-Satterthwaite approximation)
    df <- (vars[g1]/n[g1] + vars[g2]/n[g2])^2 / 
      ((vars[g1]/n[g1])^2 / (n[g1] - 1) + 
         (vars[g2]/n[g2])^2 / (n[g2] - 1))
    
    # t-statistic
    t_stat <- mean_diff / se
    
    # Two-tailed p-value
    p_value <- 2 * pt(abs(t_stat), df, lower.tail = FALSE)
    
    # Confidence interval
    alpha <- 1 - conf.level
    t_crit <- qt(1 - alpha/2, df)
    ci_lower <- mean_diff - t_crit * se
    ci_upper <- mean_diff + t_crit * se
    
    # Store results
    results[i, ] <- list(
      group1 = g1,
      group2 = g2,
      mean_diff = mean_diff,
      se = se,
      t = t_stat,
      df = df,
      p_value = p_value,
      ci_lower = ci_lower,
      ci_upper = ci_upper
    )
  }
  
  # Adjust p-values using Holm method
  results$p_adj <- p.adjust(results$p_value, method = "holm")
  
  # Add significance indicator
  results$significant <- results$p_adj < (1 - conf.level)
  
  # Set class for potential print method
  class(results) <- c("games_howell", "data.frame")
  
  return(results)
}

#' Print method for games_howell objects
#'
#' @param x A games_howell object
#' @param digits Number of digits to display (default: 4)
#' @param ... Additional arguments (ignored)
#' @export

print.games_howell <- function(x, digits = 4, ...) {
  cat("\nGames-Howell Post-Hoc Test\n")
  cat("==========================\n\n")
  
  # Format output
  output <- x
  output$mean_diff <- round(output$mean_diff, digits)
  output$se <- round(output$se, digits)
  output$t <- round(output$t, digits)
  output$df <- round(output$df, 2)
  output$p_value <- format.pval(output$p_value, digits = digits)
  output$p_adj <- format.pval(output$p_adj, digits = digits)
  output$ci_lower <- round(output$ci_lower, digits)
  output$ci_upper <- round(output$ci_upper, digits)
  
  # Add significance stars
  output$sig <- ifelse(x$p_adj < 0.001, "***",
                       ifelse(x$p_adj < 0.01, "**",
                              ifelse(x$p_adj < 0.05, "*",
                                     ifelse(x$p_adj < 0.1, ".", ""))))
  
  print(as.data.frame(output), row.names = FALSE)
  
  cat("\n---\n")
  cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  cat("P-values adjusted using Holm method\n\n")
  
  invisible(x)
}

#' Get compact letter display from Games-Howell results
#'
#' Converts Games-Howell test results into compact letter display
#' using multcompView. Groups sharing a letter are not significantly different.
#'
#' @param x A games_howell object from \code{games_howell()}
#' @param alpha Significance level (default: 0.05)
#'
#' @return A named vector with group names and their letter codes
#'
#' @examples
#' ToothGrowth$dose <- as.factor(ToothGrowth$dose)
#' result <- games_howell(ToothGrowth$len, ToothGrowth$dose)
#' letters <- gh_letters(result)
#' print(letters)
#'
#' @export

gh_letters <- function(x, alpha = 0.05) {
  
  if (!inherits(x, "games_howell")) {
    stop("x must be a games_howell object")
  }
  
  # Create a matrix of p-values for multcompView
  all_groups <- unique(c(x$group1, x$group2))
  k <- length(all_groups)
  
  # Initialize p-value matrix
  p_matrix <- matrix(1, nrow = k, ncol = k)
  rownames(p_matrix) <- all_groups
  colnames(p_matrix) <- all_groups
  
  # Fill in p-values
  for (i in 1:nrow(x)) {
    g1 <- x$group1[i]
    g2 <- x$group2[i]
    p_matrix[g1, g2] <- x$p_adj[i]
    p_matrix[g2, g1] <- x$p_adj[i]
  }
  
  # Convert to multcompView format
  letters <- multcompView::multcompLetters(p_matrix, 
                                           threshold = alpha,
                                           Letters = letters)
  
  return(letters$Letters)
}

#' Extract compact letter display from Games-Howell results
#'
#' Uses multcompView to create compact letter display showing which groups
#' differ significantly from each other.
#'
#' @param gh_result A games_howell object
#' @param alpha Significance level (default: 0.05)
#'
#' @return Named vector of letters; groups sharing a letter are not significantly different
#'
#' @examples
#' result <- games_howell(ToothGrowth$len, ToothGrowth$dose)
#' letters <- gh_letters(result)
#' print(letters)
#'
#' @export

gh_letters <- function(gh_result, alpha = 0.05) {
  if (!inherits(gh_result, "games_howell")) {
    stop("Input must be a games_howell object")
  }
  
  # Create a matrix of p-values for multcompView
  groups <- unique(c(gh_result$group1, gh_result$group2))
  n_groups <- length(groups)
  
  # Initialize p-value matrix
  p_matrix <- matrix(1, nrow = n_groups, ncol = n_groups)
  rownames(p_matrix) <- groups
  colnames(p_matrix) <- groups
  
  # Fill in p-values
  for (i in 1:nrow(gh_result)) {
    g1 <- gh_result$group1[i]
    g2 <- gh_result$group2[i]
    p <- gh_result$p_adj[i]
    
    p_matrix[g1, g2] <- p
    p_matrix[g2, g1] <- p
  }
  
  # Convert to format for multcompView
  # multcompView expects logical matrix (TRUE = different)
  diff_matrix <- p_matrix < alpha
  
  # Use multcompView to get compact letter display
  letters <- multcompView::multcompLetters(diff_matrix)$Letters
  
  return(letters)
}