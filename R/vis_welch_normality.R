### Header vis_welch_normality -----

#' Visualisation of the normality assumption for Welch ANOVA/t-test
#'
#' \code{vis_welch_normality} checks for normality of each group separately using 
#' the Shapiro-Wilk and Anderson-Darling tests. The null hypothesis is that 
#' each group is normally distributed. The function generates histograms 
#' with normal distribution overlays and Q-Q plots to visually assess normality.
#' The layout automatically adapts based on the number of groups (k).
#'
#' @param samples Numeric vector; the dependent variable.
#' @param groups Factor or vector; the grouping variable (2 to 20 groups).
#' @param conf.level Numeric; confidence level, 0.95=default (currently unused, for future extensions).
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
#' Panel layouts adapt to number of groups:
#' \itemize{
#'   \item k=2: 2×2 layout (histogram + Q-Q for each group)
#'   \item k=3-4: 2×k layout (histogram + Q-Q for each group)
#'   \item k=5-6: 3×4 layout (histogram + Q-Q side-by-side for each group)
#'   \item k=7-12: 4×6 layout (histogram + Q-Q stacked for each group)
#'   \item k=13-20: 5×8 layout (histogram + Q-Q stacked for each group)
#' }
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
  if (k > 10) {
    cat("\n")
    cat("===============================================\n")
    cat("  TOO MANY GROUPS FOR VISUAL DISPLAY (k = ", k, ")\n")
    cat("===============================================\n\n")
    cat("Visual plots are not meaningful with more than 10 groups.\n")
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
    cat("  - p > 0.05: Data consistent with normality\n")
    cat("  - p <= 0.05: Evidence against normality\n")
    cat("\n")
    
    # Count how many groups fail normality
    n_fail_shapiro <- sum(results_table$Shapiro_p != "NA" & 
                            as.numeric(sub("<", "", results_table$Shapiro_p)) < 0.05, 
                          na.rm = TRUE)
    n_fail_ad <- sum(results_table$AD_p != "NA" & 
                       as.numeric(sub("<", "", results_table$AD_p)) < 0.05, 
                     na.rm = TRUE)
    
    cat("Summary:\n")
    cat("  ", n_fail_shapiro, "out of", k, "groups fail Shapiro-Wilk test (p < 0.05)\n")
    cat("  ", n_fail_ad, "out of", k, "groups fail Anderson-Darling test (p < 0.05)\n")
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
  
  # For k <= 10: Continue with visual plots
  
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
  
  # Determine layout based on number of groups
  # Design principle: All histograms in top row(s), all Q-Q plots in bottom row(s)
  if (k == 2) {
    # 2×2 layout: histograms on top, Q-Q plots on bottom
    layout_rows <- 2
    layout_cols <- 2
    par(mfrow = c(layout_rows, layout_cols), 
        mar = c(4, 4, 2, 1), 
        oma = c(0, 0, 2.5, 0),
        cex = 0.7 * cex)
  } else if (k <= 4) {
    # 2×k layout: top row = histograms, bottom row = Q-Q plots
    layout_rows <- 2
    layout_cols <- k
    par(mfrow = c(layout_rows, layout_cols),
        mar = c(4, 4, 2, 1),
        oma = c(0, 0, 2.5, 0),
        cex = 0.6 * cex)
  } else if (k <= 8) {
    # 2×8 layout: 5-8 groups fit in one row each
    layout_rows <- 2
    layout_cols <- 8
    par(mfrow = c(layout_rows, layout_cols),
        mar = c(3.5, 3.5, 2, 0.5),
        oma = c(0, 0, 2.5, 0),
        cex = 0.5 * cex)
  } else if (k <= 16) {
    # 4×8 layout: histograms in rows 1-2, Q-Q plots in rows 3-4
    layout_rows <- 4
    layout_cols <- 8
    par(mfrow = c(layout_rows, layout_cols),
        mar = c(3, 3, 1.5, 0.5),
        oma = c(0, 0, 2.5, 0),
        cex = 0.45 * cex)
  } else {
    # 6×8 layout for 17-20 groups: histograms in rows 1-3, Q-Q in rows 4-6
    layout_rows <- 6
    layout_cols <- 8
    par(mfrow = c(layout_rows, layout_cols),
        mar = c(3, 3, 1.5, 0.5),
        oma = c(0, 0, 2.5, 0),
        cex = 0.4 * cex)
  }
  
  # First pass: Plot all histograms
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
    p_s_text <- if (!is.na(p_s)) signif(p_s, 3) else "N/A"
    p_a_text <- if (!is.na(p_a)) signif(p_a, 3) else "N/A"
    main_text <- paste0("Hist. - ", gname, "\nSW p=", p_s_text, " | AD p=", p_a_text)
    
    hist(gdata, 
         freq = FALSE,
         main = main_text,
         xlab = "Values",
         ylab = "Density",
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
  
  # Fill remaining histogram slots if k > 8
  if (k > 8) {
    n_hist_rows <- ceiling(k / layout_cols)
    n_hist_slots <- n_hist_rows * layout_cols
    for (i in (k + 1):n_hist_slots) {
      plot.new()
    }
  }
  
  # Second pass: Plot all Q-Q plots
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
           main = paste("Q-Q Plot -", gname),
           xlab = "Theoretical Quantiles",
           ylab = "Sample Quantiles")
    qqline(gdata, col = "red", lwd = 2)
  }
  
  # Simple outer title
  mtext("Normality by Group", outer = TRUE, cex = 0.9, line = 0.5)
  
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