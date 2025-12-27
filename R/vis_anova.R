#' ANOVA or Welch's ANOVA with appropriate post-hoc tests
#'
#' Internal function that performs ANOVA or Welch's one-way test and 
#' corresponding post-hoc comparisons. Uses TukeyHSD for equal variances 
#' (Fisher's ANOVA) and Games-Howell for unequal variances (Welch's ANOVA).
#'
#' @param samples Numeric vector; the dependent variable.
#' @param fact Factor; the grouping variable.
#' @param conf.level Numeric; confidence level for tests and intervals (default: 0.95).
#' @param samplename Character; label for y-axis (default: "").
#' @param factorname Character; label for x-axis (default: "").
#' @param cex Numeric; character expansion factor for plot elements (default: 1).
#'
#' @return A list with components:
#' \describe{
#'   \item{summary statistics of ANOVA}{Summary of Fisher's ANOVA or Welch's oneway test}
#'   \item{post-hoc analysis}{TukeyHSD object or Games-Howell results in compatible format}
#'   \item{conf.level}{The confidence level used}
#' }
#'
#' @details
#' The function first tests for homogeneity of variance using Bartlett's test.
#' If variances are equal (p > alpha), Fisher's one-way ANOVA with TukeyHSD 
#' post-hoc is performed. If variances are unequal (p <= alpha), Welch's 
#' heteroscedastic one-way ANOVA with Games-Howell post-hoc is performed.
#' 
#' The function produces a stripchart with means, confidence intervals, 
#' and compact letter display showing which groups differ significantly.

#' @keywords internal
vis_anova <- function(samples,
                      fact,
                      conf.level = conf.level,
                      samplename = "",
                      factorname = "",
                      cex = 1) {
  if (missing(conf.level)) {
    conf.level <- 0.95
  }
  
  oldparanova <- par(no.readonly = TRUE)
  on.exit(par(oldparanova))
  
  alpha <- 1 - conf.level
  
  samples3 <- na.omit(samples)
  fact <- subset(fact, !is.na(samples))
  samples <- samples3
  n_classes <- length(unique(fact))
  # https://en.wikipedia.org/wiki/Bonferroni_correction
  number_of_pairwise_comparisons <- n_classes * (n_classes - 1) / 2
  alpha_sidak <- 1 - conf.level^(1 / number_of_pairwise_comparisons) # Sidak correction, https://en.wikipedia.org/wiki/%C5%A0id%C3%A1k_correction
  # alpha_sidak=alpha #do not apply sidak correction
  sdna <- function(x) {
    sd(x, na.rm = T)
  }
  meanna <- function(x) {
    mean(x, na.rm = T)
  }
  
  s <- tapply(samples, fact, sdna)
  m <- tapply(samples, fact, meanna)
  
  samples_per_class <- integer(n_classes)
  for (i in 1:n_classes) {
    samples_per_class[i] <- sum(fact == unique(fact)[i])
  }
  # tests
  an <- aov(samples ~ fact)
  summaryAnova <- summary(an)
  oneway <- oneway.test(samples ~ fact)
  # check for homogeneity
  bartlett_test <- bartlett.test(samples ~ fact)
  p_bart <- bartlett_test$p.value
  levene_test <- levene.test(samples,fact)
  p_levene <- levene_test$p.value
  
  
  
  if (p_bart > 1 - conf.level) #changed logic
  {
    p_aov <- summaryAnova[[1]][["Pr(>F)"]][1]
    F_value <- round(summaryAnova[[1]]$`F value`[1],2)
    label_aov <- "Fisher's one-way ANOVA (TuckeyHSD post-hoc)"
    summarystat <- summaryAnova
    post_hoc_anova <- TukeyHSD(an, conf.level = conf.level)
  } else {
    # Unequal variances - use Welch's ANOVA with Games-Howell post-hoc
    p_aov <- oneway$p.value
    F_value <- round(oneway$statistic, 2)
    label_aov <- "Welch's one-way ANOVA (Games-Howell post-hoc)"
    summarystat <- oneway
    
    # Use Games-Howell for post-hoc (correct for unequal variances)
    gh_result <- games_howell(samples, fact, conf.level = conf.level)
    
    # Convert to format needed by multcompLetters (line 846)
    comparison_names <- paste0(gh_result$group2, "-", gh_result$group1)
    result_matrix <- cbind(
      diff = gh_result$mean_diff,
      lwr = gh_result$ci_lower,
      upr = gh_result$ci_upper,
      `p adj` = gh_result$p_adj
    )
    rownames(result_matrix) <- comparison_names
    post_hoc_anova <- list(fact = result_matrix)
  }
  
  
  
  
  maximum <- max(samples, na.rm = T)
  minimum <- min(samples, na.rm = T)
  
  spread <- maximum - minimum
  
  mi <- minimum - 1.2 * spread
  ma <- maximum + 1.2 * spread
  par(mfrow = c(1, 1), oma = c(0, 0, 3, 0))
  
  stripchart(
    samples ~ fact,
    vertical = TRUE,
    xlim = c(0, n_classes + 1),
    ylim = c(mi, ma),
    col = rep("grey30", n_classes),
    ylab = samplename,
    xlab = factorname,
    las = 2
  )
  
  
  
  # sd:
  for (i in 1:n_classes) {
    sn <- qt(1 - alpha_sidak / 2, samples_per_class[i] - 1) * s[[i]] / sqrt(samples_per_class[i])
    lines(
      x = c(i - 0.2, i - 0.2),
      # y = c(m[[i]] - s[[i]], m[[i]] + s[[i]]),
      y = c(m[[i]] - sn, m[[i]] + sn),
      col = colors()[131],
      lwd = 5
    )
  }
  
  
  for (i in 1:n_classes) {
    lines(
      x = c(i - 0.1, i + 0.1),
      y = c(m[[i]], m[[i]]),
      col = colors()[552],
      lwd = 3
    )
    arrows(
      i,
      m[[i]] + qt(1 - alpha / 2, samples_per_class[i] - 1) * s[[i]] / sqrt(samples_per_class[i]),
      # m[[i]] + qt(1 - alpha_sidak/2, samples_per_class[i] - 1) * s[[i]] / sqrt(samples_per_class[i]),
      i,
      m[[i]] - qt(1 - alpha / 2, samples_per_class[i] - 1) * s[[i]] / sqrt(samples_per_class[i]),
      angle = 90,
      code = 3,
      col = colors()[552],
      lty = 1,
      lwd = 2,
      length = 0.1
    )
  }
  
  
  
  s <- multcompLetters(post_hoc_anova[[1]][, 4], threshold = alpha)
  
  ord <- c()
  
  v <- attributes(s$Letters)$names
  f_levels <- sort(unique(fact))
  for (i in 1:n_classes) {
    ord[i] <- which(v == f_levels[i])
  }
  
  text(seq(1:n_classes + 1),
       mi,
       s$Letters[ord],
       col = colors()[81],
       lwd = 2)
  
  
  mtext(
    paste0(label_aov, "\nF = ", F_value, ", p = ", signif(p_aov, 2)),
    outer = TRUE)
  
  
  
  legend(
    "topleft",
    inset = 0.05,
    horiz = F,
    c(
      paste("mean with", conf.level * 100, "% conf. intervall "),
      paste("Sidak corrected" , round((1 - alpha_sidak) * 100, 2), "% conf. interval")
    ),
    col = c(colors()[131], colors()[552]),
    bty = "n",
    lwd = 3
  )
  
  
  
  
  my_list <-
    list(
      # "summary statistics of Fisher's one-way ANOVA" = summaryAnova,
      # "summary statistics of Welch's one-way ANOVA (not assuming equal  variances)" = oneway,
      "summary statistics of ANOVA" = summarystat,
      "post-hoc analysis " = post_hoc_anova,
      "conf.level" = conf.level
    )
  
  return(my_list)
}