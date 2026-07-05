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
#' @param variance_route Character; \code{"levene"} selects Fisher/Welch by
#'   Levene's test, \code{"welch"} always uses Welch's ANOVA.
#'
#' @return A list with components:
#' \describe{
#'   \item{summary statistics of ANOVA}{Summary of Fisher's ANOVA or Welch's oneway test}
#'   \item{post-hoc analysis}{TukeyHSD object or Games-Howell results in compatible format}
#'   \item{conf.level}{The confidence level used}
#' }
#'
#' @details
#' The function first tests for homogeneity of variance using Levene's and
#' Bartlett's tests applied to the internally studentised residuals
#' r_i = e_i / (SE_res sqrt(1 - h_i)), which remove the leverage-dependent
#' variance of the raw residuals (Var(e_i) = sigma^2 (1 - h_i)).
#' If variances are equal (p > alpha), Fisher's one-way ANOVA with Tukey's HSD
#' post-hoc is performed. If variances are unequal (p <= alpha), Welch's
#' one-way ANOVA with Games-Howell post-hoc is performed.
#'
#' The function produces a box plot with jittered points and group means
#' (red crosses for the parametric branches), annotated with a compact
#' letter display showing which groups differ significantly.
#' @examples
#' # Example with equal variances (uses Fisher's ANOVA + TukeyHSD)
#' data(PlantGrowth)
#' result1 <- vis_anova(PlantGrowth$weight, PlantGrowth$group,
#'   samplename = "Weight", factorname = "Group"
#' )
#'
#' # Example with unequal variances (uses Welch's ANOVA + Games-Howell)
#' # Create data with heterogeneous variances
#' set.seed(123)
#' group_a <- rnorm(20, mean = 10, sd = 1)
#' group_b <- rnorm(20, mean = 15, sd = 5) # Much larger variance
#' group_c <- rnorm(20, mean = 12, sd = 2)
#' values <- c(group_a, group_b, group_c)
#' groups <- factor(rep(c("A", "B", "C"), each = 20))
#' result2 <- vis_anova(values, groups,
#'   samplename = "Value", factorname = "Group"
#' )
#'
#' @noRd

vis_anova <- function(samples,
                      fact,
                      conf.level = conf.level,
                      samplename = "",
                      factorname = "",
                      variance_route = c("levene", "welch"),
                      plot_args = list()) {
  if (missing(conf.level)) {
    conf.level <- 0.95
  }
  variance_route <- match.arg(variance_route)
  if (is.null(plot_args)) plot_args <- list()

  oldparanova <- par(no.readonly = TRUE)
  on.exit(par(oldparanova))

  # Small label tier: 0.8 of the global par("cex") (relative, so it follows it).
  label_small <- 0.8
  if (!is.null(visstat_graphics_arg(plot_args, "cex", NULL))) {
    label_small <- visstat_graphics_arg(plot_args, "cex", label_small)
  }

  alpha <- 1 - conf.level

  samples3 <- na.omit(samples)
  fact <- subset(fact, !is.na(samples))
  samples <- samples3
  n_classes <- length(unique(fact))

  meanna <- function(x) {
    mean(x, na.rm = TRUE)
  }

  m <- tapply(samples, fact, meanna)
  # tests
  an <- aov(samples ~ fact)
  raw_residuals <- residuals(an)
  # Internally studentised residuals remove the leverage-induced
  # heteroscedasticity of the raw residuals (Var(e_i) = sigma^2 (1 - h_i)).
  scaled_residuals <- rstandard(an)
  if (any(!is.finite(scaled_residuals))) {
    scaled_residuals <- raw_residuals / max(sigma(an), 1e-8)
  }
  summaryAnova <- summary(an)
  oneway <- oneway.test(samples ~ fact)
  # check for homogeneity
  levene_test <- levene.test(scaled_residuals, fact)
  p_levene <- levene_test$p.value


  use_fisher <- variance_route == "levene" && p_levene > alpha

  if (use_fisher) {
    p_aov <- summaryAnova[[1]][["Pr(>F)"]][1]
    F_value <- sprintf("%.2f", summaryAnova[[1]]$`F value`[1])
    label_aov <- "Fisher's one-way ANOVA"
    summarystat <- summaryAnova
    post_hoc_anova <- TukeyHSD(an, conf.level = conf.level)
  } else {
    # Unequal variances - use Welch's ANOVA with Games-Howell post-hoc
    p_aov <- oneway$p.value
    F_value <- sprintf("%.2f", oneway$statistic)
    label_aov <- "Welch's one-way ANOVA"
    summarystat <- oneway

    # Use Games-Howell for post-hoc (correct for unequal variances)
    gh_result <- games.howell(samples, fact, conf.level = conf.level)

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


  # Y-axis limits (with extra lower margin for legend space)
  lower_margin <- 0.45
  upper_margin <- 0.2
  margins <- calc_min_max_of_y_axis(samples, lower_margin, upper_margin)
  mi <- margins[[1]]
  ma <- margins[[2]]

  box_cols <- rep_len(c(colorscheme(1), colorscheme(3)), n_classes)
  visstat_graphics_par(plot_args)

  show_jitter <- max(tapply(samples, fact, length)) <= jitter_max_n

  b <- boxplot(
    samples ~ fact,
    xlim = c(0, n_classes + 1),
    ylim = c(mi, ma),
    col = box_cols,
    ylab = visstat_graphics_arg(plot_args, "ylab", samplename),
    xlab = visstat_graphics_arg(plot_args, "xlab", factorname),
    las = 1,
    outline = !show_jitter # outliers via boxplot only when jitter is suppressed
  )

  if (show_jitter) {
    stripchart(
      samples ~ fact,
      vertical = TRUE,
      method = "jitter",
      col = adjustcolor(box_cols, red.f = 0.55, green.f = 0.55, blue.f = 0.55),
      pch = 1,
      cex = 0.7,
      add = TRUE
    )
  }

  # Group means -- parametric branch tests means, so mark them explicitly
  points(seq_len(n_classes), m,
    pch = 4,
    col = visstat_graphics_arg(plot_args, "col", "red"),
    cex = label_small,
    lwd = 1.5
  )

  if (n_classes > 6) {
    n_labels <- c(paste("n =", b$n[1]), as.character(b$n[-1]))
  } else {
    n_labels <- paste("n =", b$n)
  }
  text(seq_len(n_classes), ma - 0.03 * (ma - mi), n_labels, cex = label_small)


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
    cex = label_small,
    lwd = 2
  )


  mtext(
    visstat_graphics_arg(
      plot_args,
      "main",
      paste0(label_aov, "\nF = ", F_value, ", p = ", signif(p_aov, 2))
    )
  )


  # Legend: mean marker (top) + significance letters with post-hoc method
  # and alpha (split on two lines for compactness in narrow windows).
  # All post-hoc / alpha info lives here, so the title can stay compact.
  # Legend: significance letters only (the red x marks the mean, left
  # unlabelled like the box's median line).
  posthoc_name <- ifelse(use_fisher, "Tukey's HSD", "Games-Howell")
  legend("bottomleft",
    legend = c(
      "a, b, ...: significance letters",
      paste0("(", posthoc_name, ", alpha = ", signif(alpha, 2), ")")
    ),
    text.col = colors()[81],
    bty = "n",
    cex = label_small,
    inset = 0.04
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
