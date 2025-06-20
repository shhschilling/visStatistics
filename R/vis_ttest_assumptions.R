### Header vis_ttest_assumptions -----

#' Visualisation of the normality assumption for t-test analysis
#'
#' \code{vis_ttest_assumptions} checks for normality of each group using 
#' the Shapiro-Wilk test \code{shapiro.test()}. The null hypothesis is that 
#' each group is normally distributed. The function generates histograms 
#' with normal distribution overlays and a Q-Q plot to visually assess normality.
#'
#' @param samples vector containing dependent variable, datatype numeric
#' @param groups vector containing grouping variable, datatype factor (2 levels only)
#' @param conf.level confidence level, 0.95=default
#' @param samplename name of sample used in graphical output, datatype character
#'   , ''=default
#' @param groupname name of grouping variable used in graphical output, datatype character,
#'   ''=default
#' @param cex number indicating the amount by which plotting text and symbols
#'   should be scaled relative to the default. 1=default, 1.5 is 50\% larger,
#'   0.5 is 50\% smaller, etc.
#'
#' @return \code{list} containing the Shapiro-Wilk test results for each group.
#'
#' @examples
#' # Using built-in datasets
#' vis_ttest_assumptions(ToothGrowth$len, ToothGrowth$supp)
#' 
#' # Create sample data
#' group1 <- rnorm(30, mean = 10, sd = 2)
#' group2 <- rnorm(25, mean = 12, sd = 2.5)
#' groups <- factor(c(rep("A", 30), rep("B", 25)))
#' samples <- c(group1, group2)
#' vis_ttest_assumptions(samples, groups)
#'
#' @export vis_ttest_assumptions
#' 
vis_ttest_assumptions <- function(samples,
                                  groups,
                                  conf.level = 0.95,
                                  samplename = "",
                                  groupname = "",
                                  cex = 1) {
  
  # Store original par settings
  oldpar <- par(no.readonly = TRUE)

  oldpar$pin <- NULL  # Add this line
  oldpar$new <- FALSE  # Add this line too (removes the "new" warning)
  oldpar$fig <- NULL
  oldpar$mfg <- NULL
  oldpar$mai <- NULL
  oldpar$mar <- NULL
  on.exit(par(oldpar))
  
  # Clean data - remove NAs
  complete_cases <- complete.cases(samples, groups)
  samples_clean <- samples[complete_cases]
  groups_clean <- groups[complete_cases]
  
  # Check if exactly 2 groups
  group_levels <- levels(factor(groups_clean))
  if (length(group_levels) != 2) {
    stop("Exactly 2 groups required for t-test assumptions")
  }
  
  # Split data by groups
  group1_data <- samples_clean[groups_clean == group_levels[1]]
  group2_data <- samples_clean[groups_clean == group_levels[2]]
  
  # Normality tests for group 1
  if (length(group1_data) >= 3) {
    shapiro_group1 <- shapiro.test(group1_data)
    p_SH_group1 <- shapiro_group1$p.value
  } else {
    shapiro_group1 <- "Sample size too small for Shapiro-Wilk test"
    p_SH_group1 <- NA
  }
  
  # Normality tests for group 2
  if (length(group2_data) >= 3) {
    shapiro_group2 <- shapiro.test(group2_data)
    p_SH_group2 <- shapiro_group2$p.value
  } else {
    shapiro_group2 <- "Sample size too small for Shapiro-Wilk test"
    p_SH_group2 <- NA
  }
  
  # Create plots - 2x2 design
  #par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
  
  # Create plots - 2x2 design with explicit margins
  par(mar = c(4, 4, 2, 1), mfrow = c(2, 2), oma = c(0, 0, 3, 0))
  
  # Plot 1: Histogram with normal overlay for group 1
  hist(group1_data, 
       freq = FALSE, 
       main = paste("Histogram -", group_levels[1]),
       xlab = ifelse(samplename == "", "Values", samplename),
       ylab = "Density",
       cex.main = cex, cex.lab = cex, cex.axis = cex)
  
  # Overlay normal distribution - find proper range for full curve
  mean1 <- mean(group1_data)
  sd1 <- sd(group1_data)
  data_range1 <- range(group1_data)
  # Use 3 standard deviations or data range, whichever is wider
  normal_range1 <- c(mean1 - 3*sd1, mean1 + 3*sd1)
  full_range1 <- c(min(data_range1[1], normal_range1[1]), max(data_range1[2], normal_range1[2]))
  x_seq1 <- seq(full_range1[1], full_range1[2], length = 300)
  normal_curve1 <- dnorm(x_seq1, mean = mean1, sd = sd1)
  lines(x_seq1, normal_curve1, col = "red", lwd = 2)
  
  # Plot 2: Q-Q plot for group 1
  qqnorm(group1_data, main = paste("Q-Q Plot -", group_levels[1]), cex.main = cex)
  qqline(group1_data, col = "red", lwd = 2)
  
  # Plot 3: Histogram with normal overlay for group 2
  hist(group2_data, 
       freq = FALSE, 
       main = paste("Histogram -", group_levels[2]),
       xlab = ifelse(samplename == "", "Values", samplename),
       ylab = "Density",
       cex.main = cex, cex.lab = cex, cex.axis = cex)
  
  # Overlay normal distribution - find proper range for full curve
  mean2 <- mean(group2_data)
  sd2 <- sd(group2_data)
  data_range2 <- range(group2_data)
  # Use 3 standard deviations or data range, whichever is wider
  normal_range2 <- c(mean2 - 3*sd2, mean2 + 3*sd2)
  full_range2 <- c(min(data_range2[1], normal_range2[1]), max(data_range2[2], normal_range2[2]))
  x_seq2 <- seq(full_range2[1], full_range2[2], length = 300)
  normal_curve2 <- dnorm(x_seq2, mean = mean2, sd = sd2)
  lines(x_seq2, normal_curve2, col = "red", lwd = 2)
  
  # Plot 4: Q-Q plot for group 2
  qqnorm(group2_data, main = paste("Q-Q Plot -", group_levels[2]), cex.main = cex)
  qqline(group2_data, col = "red", lwd = 2)
  
  # Reset plotting parameters
  par(mfrow = c(1, 1))
  
  # Add main title with test results
  mtext(
    paste(
      "Check for normality of groups:",
      "\n", group_levels[1], " - Shapiro-Wilk: p = ",
      signif(p_SH_group1, 2),
      ", ", group_levels[2], " - Shapiro-Wilk: p = ",
      signif(p_SH_group2, 2)
    ),
    outer = TRUE
  )
  
  # Return results list
  results_list <- list(
    shapiro_group1 = shapiro_group1,
    shapiro_group2 = shapiro_group2
  )
  
  return(results_list)
}