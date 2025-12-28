### Header vis_welch_normality -----

#' Visualisation of the normality assumption for Welch ANOVA/t-test
#'
#' \code{vis_welch_normality} checks for normality of each group separately using 
#' the Shapiro-Wilk and Anderson-Darling tests. The null hypothesis is that 
#' each group is normally distributed. The function generates histograms 
#' with normal distribution overlays and Q-Q plots to visually assess normality.
#' The layout is always 2 rows × k columns (histograms on top, Q-Q plots on bottom).
#'
#' @param samples Numeric vector; the dependent variable.
#' @param groups Factor or vector; the grouping variable (2 to 8 groups for visual display).
#' @param conf.level Numeric; confidence level (default: 0.95). Used to determine 
#'   alpha = 1 - conf.level for normality test interpretation.
#' @param samplename Character; label for the y-axis (default: "").
#' @param groupname Character; label for the x-axis (default: "").
#' @param cex Numeric; scaling factor for plot text and symbols (default: 1).
#'
#' @return A list containing:
#' \describe{
#'   \item{shapiro_tests}{List of Shapiro-Wilk test results for each group}
#'   \item{ad_tests}{List of Anderson-Darling test results for each group}
#'   \item{n_groups}{Number of groups}
#'   \item{group_names}{Names of the groups}
#' }
#'
#' @details
#' Layout is always 2 rows × k columns:
#' \itemize{
#'   \item Top row: Histograms with normal overlay for each group
#'   \item Bottom row: Q-Q plots for each group
#' }
#' For more than 8 groups, a tabular summary is provided instead of plots.
#'
#' @examples
#' # Two groups (like t-test)
#' vis_welch_normality(ToothGrowth$len, ToothGrowth$supp)
#' 
#' # Three groups
#' ToothGrowth$dose <- as.factor(ToothGrowth$dose)
#' vis_welch_normality(ToothGrowth$len, ToothGrowth$dose)
#'
#' @export

vis_welch_normality <- function(samples, 
                                groups, 
                                conf.level = 0.95, 
                                samplename = "", 
                                groupname = "", 
                                cex = 1) {
  
  # Store original par settings
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  # Rename for internal consistency (legacy variable names)
  y <- samples
  g <- groups
  
  # Clean data
  complete_cases <- complete.cases(y, g)
  y <- y[complete_cases]
  g <- as.factor(g[complete_cases])
  
  # Get group information
  group_levels <- levels(g)
  k <- length(group_levels)
  
  # Check minimum requirements
  if (k < 2) {
    stop("At least 2 groups required")
  }
  
  # Check if too many groups for meaningful visualization
  if (k > 8) {
    cat("\n")
    cat("===============================================\n")
    cat("  TOO MANY GROUPS FOR VISUAL DISPLAY (k = ", k, ")\n")
    cat("===============================================\n\n")
    cat("Visual plots are not meaningful with more than 8 groups.\n")
    cat("Providing normality test results in tabular format instead.\n\n")
    
    # Calculate test results for all groups
    shapiro_tests <- list()
    ad_tests <- list()
    
    results_table <- data.frame(
      Group = character(k),
      n = integer(k),
      Shapiro_W = numeric(k),
      Shapiro_p = numeric(k),
      AD_A = numeric(k),
      AD_p = numeric(k),
      stringsAsFactors = FALSE
    )
    
    for (i in seq_along(group_levels)) {
      gname <- group_levels[i]
      gdata <- y[g == gname]
      n <- length(gdata)
      
      # Shapiro-Wilk test
      if (n >= 3 && n <= 5000) {
        sh_test <- shapiro.test(gdata)
        shapiro_tests[[gname]] <- sh_test
        results_table$Shapiro_W[i] <- sh_test$statistic
        results_table$Shapiro_p[i] <- sh_test$p.value
      } else {
        shapiro_tests[[gname]] <- list(
          statistic = NA,
          p.value = NA,
          method = paste0("Shapiro-Wilk test requires n: 3-5000 (n = ", n, ")")
        )
        results_table$Shapiro_W[i] <- NA
        results_table$Shapiro_p[i] <- NA
      }
      
      # Anderson-Darling test
      if (n >= 7) {
        ad_test <- nortest::ad.test(gdata)
        ad_tests[[gname]] <- ad_test
        results_table$AD_A[i] <- ad_test$statistic
        results_table$AD_p[i] <- ad_test$p.value
      } else {
        ad_tests[[gname]] <- list(
          statistic = NA,
          p.value = NA,
          method = paste0("Anderson-Darling test requires n >= 7 (n = ", n, ")")
        )
        results_table$AD_A[i] <- NA
        results_table$AD_p[i] <- NA
      }
      
      results_table$Group[i] <- gname
      results_table$n[i] <- n
    }
    
    # Format and print table
    results_table$Shapiro_W <- round(results_table$Shapiro_W, 4)
    results_table$Shapiro_p <- format.pval(results_table$Shapiro_p, digits = 3, eps = 0.001)
    results_table$AD_A <- round(results_table$AD_A, 4)
    results_table$AD_p <- format.pval(results_table$AD_p, digits = 3, eps = 0.001)
    
    cat("Normality Test Results by Group:\n")
    cat("================================\n\n")
    print(results_table, row.names = FALSE)
    
    cat("\n")
    cat("Interpretation:\n")
    alpha <- 1 - conf.level
    cat("  - p >", alpha, ": Data consistent with normality\n")
    cat("  - p <=", alpha, ": Evidence against normality\n")
    cat("\n")
    
    # Count how many groups fail normality
    n_fail_shapiro <- sum(results_table$Shapiro_p != "NA" & 
                            as.numeric(sub("<", "", results_table$Shapiro_p)) < alpha, 
                          na.rm = TRUE)
    n_fail_ad <- sum(results_table$AD_p != "NA" & 
                       as.numeric(sub("<", "", results_table$AD_p)) < alpha, 
                     na.rm = TRUE)
    
    cat("Summary:\n")
    cat("  ", n_fail_shapiro, "out of", k, "groups fail Shapiro-Wilk test (p <", alpha, ")\n")
    cat("  ", n_fail_ad, "out of", k, "groups fail Anderson-Darling test (p <", alpha, ")\n")
    cat("\n")
    
    if (n_fail_shapiro > 0 || n_fail_ad > 0) {
      cat("Recommendation: Consider using Kruskal-Wallis test (non-parametric)\n")
    } else {
      cat("Recommendation: Normality assumption appears reasonable for Welch ANOVA\n")
    }
    cat("\n")
    
    # Return results invisibly
    result <- list(
      shapiro_tests = shapiro_tests,
      ad_tests = ad_tests,
      n_groups = k,
      group_names = group_levels,
      results_table = results_table,
      note = "Too many groups for visual display"
    )
    
    class(result) <- "vis_welch_normality"
    return(invisible(result))
  }
  
  # For k <= 8: Visual plots with 2 rows × k columns layout
  
  # Split data by groups
  group_data <- lapply(group_levels, function(lev) y[g == lev])
  names(group_data) <- group_levels
  
  # Run normality tests for each group
  shapiro_tests <- list()
  ad_tests <- list()
  
  for (i in seq_along(group_levels)) {
    gname <- group_levels[i]
    gdata <- group_data[[i]]
    n <- length(gdata)
    
    # Shapiro-Wilk test
    if (n >= 3 && n <= 5000) {
      shapiro_tests[[gname]] <- shapiro.test(gdata)
    } else if (n < 3) {
      shapiro_tests[[gname]] <- list(
        statistic = NA, 
        p.value = NA,
        method = paste0("Shapiro-Wilk test requires n >= 3 (n = ", n, ")")
      )
    } else {
      shapiro_tests[[gname]] <- list(
        statistic = NA,
        p.value = NA,
        method = paste0("Shapiro-Wilk test allows max n = 5000 (n = ", n, ")")
      )
    }
    
    # Anderson-Darling test
    if (n >= 7) {
      ad_tests[[gname]] <- nortest::ad.test(gdata)
    } else {
      ad_tests[[gname]] <- list(
        statistic = NA,
        p.value = NA,
        method = paste0("Anderson-Darling test requires n >= 7 (n = ", n, ")")
      )
    }
  }
  
  # Simple 2 × k layout: histograms on top row, Q-Q plots on bottom row
  # Adjust cex based on number of groups
  if (k <= 3) {
    plot_cex <- 0.6 * cex
    mar_val <- c(4, 4, 3, 1)
  } else if (k <= 5) {
    plot_cex <- 0.55 * cex
    mar_val <- c(3.5, 3.5, 2.5, 0.5)
  } else {
    # k = 6, 7, 8
    plot_cex <- 0.5 * cex
    mar_val <- c(3, 3, 2, 0.5)
  }
  
  par(mfrow = c(2, k),
      mar = mar_val,
      oma = c(0, 0, 2.5, 0),
      cex = plot_cex,
      font.main = 1,  # 1 = plain, not bold
      font.lab = 1,
      font.axis = 1)
  
  # Row 1: All histograms
  for (i in seq_along(group_levels)) {
    gname <- group_levels[i]
    gdata <- group_data[[i]]
    
    # Skip if no data
    if (length(gdata) == 0) {
      plot.new()
      next
    }
    
    # Calculate statistics
    gmean <- mean(gdata, na.rm = TRUE)
    gsd <- sd(gdata, na.rm = TRUE)
    
    # Get p-values for this group
    p_s <- shapiro_tests[[gname]]$p.value
    p_a <- ad_tests[[gname]]$p.value
    
    # --- Histogram with normal overlay ---
    hist_data <- hist(gdata, plot = FALSE)
    max_hist_density <- max(hist_data$density)
    max_normal_peak <- dnorm(gmean, mean = gmean, sd = gsd)
    y_max <- max(max_hist_density, max_normal_peak) * 1.1
    
    # Create title with p-values
    p_s_text <- if (!is.na(p_s)) signif(p_s, 2) else "N/A"
    p_a_text <- if (!is.na(p_a)) signif(p_a, 2) else "N/A"
    main_text <- paste0(gname, "\nSW p=", p_s_text, ", AD p=", p_a_text)
    
    hist(gdata, 
         freq = FALSE,
         main = main_text,
         xlab = "",
         ylab = if (i == 1) "Density" else "",
         ylim = c(0, y_max),
         col = "lightblue", 
         border = "black")
    
    # Overlay normal distribution
    data_range <- range(gdata)
    normal_range <- c(gmean - 3*gsd, gmean + 3*gsd)
    full_range <- c(min(data_range[1], normal_range[1]), 
                    max(data_range[2], normal_range[2]))
    x_seq <- seq(full_range[1], full_range[2], length = 200)
    normal_curve <- dnorm(x_seq, mean = gmean, sd = gsd)
    lines(x_seq, normal_curve, col = "red", lwd = 2)
  }
  
  # Row 2: All Q-Q plots
  for (i in seq_along(group_levels)) {
    gname <- group_levels[i]
    gdata <- group_data[[i]]
    
    # Skip if no data
    if (length(gdata) == 0) {
      plot.new()
      next
    }
    
    # --- Q-Q plot ---
    qqnorm(gdata, 
           main = paste0(gname, " Q-Q"),
           xlab = "Theoretical",
           ylab = if (i == 1) "Sample" else "")
    qqline(gdata, col = "red", lwd = 2)
  }
  
  # Outer title
  mtext("Normality Diagnostics by Group", outer = TRUE, cex = 1.0, line = 0.5)
  
  # Return results
  result <- list(
    shapiro_tests = shapiro_tests,
    ad_tests = ad_tests,
    n_groups = k,
    group_names = group_levels
  )
  
  class(result) <- "vis_welch_normality"
  return(invisible(result))
}