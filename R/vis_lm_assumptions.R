#' Visualisation of linear model assumption diagnostics
#'
#' Checks the residual diagnostics in the general linear model
#' Student's t-test 
#' (t.test,var=EQUAL) 
#' Fisher oneway ANOVA (aov) or simple linear regression.
#' Performs the Shapiro-Wilk and Anderson-Darling tests for normality, and
#' for grouped data also Levene's and Bartlett's tests for homogeneity of
#' variances. For simple linear regression, heteroscedasticity is assessed
#' with the Breusch-Pagan test [@Koenker:1981], which regresses squared raw
#' residuals on fitted values. The normality tests, the grouped variance tests,
#' and the histogram and Q-Q panels are computed from the internally
#' studentised residuals r_i = e_i / (SE_res sqrt(1 - h_i)), which remove the
#' leverage-dependent variance of the raw residuals (Var(e_i) = sigma^2
#' (1 - h_i)). The residuals-vs-fitted panel (regression mode) uses the
#' z-residuals z_i = e_i / SE_res, which retain the leverage-dependent spread.
#'
#' @param samples Numeric vector; the dependent variable.
#' @param fact Factor; the independent variable.
#' @param cex Numeric; scaling factor for plot text and symbols (default: 1).
#' @param correlation Logical. If \code{FALSE} and \code{fact} is numeric,
#'   regression diagnostics are shown. If \code{TRUE}, no regression
#'   diagnostics are shown. Default is \code{FALSE}.
#' @param conf.level Numeric confidence level for the simulated Q-Q envelopes.
#' @param qq_nsim Integer number of simulated refits for the Q-Q envelopes.
#'
#' @return A list with elements:
#' \describe{
#'   \item{summary_anova}{Summary of the ANOVA model.}
#'   \item{shapiro_test}{Result from \code{shapiro.test()}.}
#'   \item{ad_test}{Result from \code{nortest::ad.test()} or a character message if n < 7.}
#'   \item{levene_test}{Result from \code{levene.test()} (grouped diagnostics only).}
#'   \item{bartlett_test}{Result from \code{bartlett.test()} (grouped diagnostics only).}
#'   \item{bp_test}{Result from \code{bp.test()} (regression diagnostics only).}
#' }
#'
#' @examples
#' ToothGrowth$dose <- as.factor(ToothGrowth$dose)
#' vis_lm_assumptions(ToothGrowth$len, ToothGrowth$dose)
#'
#' @export
vis_lm_assumptions <- function(samples, fact, cex = 1, correlation = FALSE,
                               conf.level = 0.95,
                               qq_nsim = getOption("visStatistics.qq_nsim", 5000L)) {
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  # Clean the input
  samples3 <- na.omit(samples)
  fact <- subset(fact, !is.na(samples))
  samples <- samples3
  
  # Fit model
  anova_model <- aov(samples ~ fact) # needed for correct output structure, do not use lm(samples~fact)
  raw_residuals <- residuals(anova_model)
  # Internally studentised residuals r_i = e_i / (SE_res sqrt(1 - h_i)) for the
  # normality tests and the N(0,1)-reference panels (histogram, Q-Q).
  scaled_residuals <- rstandard(anova_model)
  if (any(!is.finite(scaled_residuals))) {
    residual_se <- sigma(anova_model)
    if (is.na(residual_se) || residual_se == 0) residual_se <- sd(raw_residuals, na.rm = TRUE)
    if (is.na(residual_se) || residual_se == 0) residual_se <- 1
    scaled_residuals <- raw_residuals / residual_se
  }
  # z-residuals z_i = e_i / SE_res for the residuals-vs-fitted panel only; these
  # retain the leverage-dependent spread Var(e_i) = sigma^2 (1 - h_i).
  z_residual_se <- sigma(anova_model)
  if (is.na(z_residual_se) || z_residual_se == 0) z_residual_se <- sd(raw_residuals, na.rm = TRUE)
  if (is.na(z_residual_se) || z_residual_se == 0) z_residual_se <- 1
  z_residuals <- raw_residuals / z_residual_se
  
  # Run assumption tests
  # Shapiro-Wilk test (for n >= 3 and n <= 5000)
  if (length(raw_residuals) >= 3 && length(raw_residuals) <= 5000) {
    shapiro_test <- shapiro.test(scaled_residuals)
  } else {
    if (length(raw_residuals) > 5000) {
      warning(
        "For samples > 5000 the Shapiro-Wilk test is undefined (p-value set ",
        "to NA); Anderson-Darling is the routing normality test.",
        call. = FALSE
      )
    }
    shapiro_test <- list(
      method = "Shapiro-Wilk normality test",
      statistic = NA,
      p.value = NA,
      data.name = "standardised model residuals"
    )
  }
  
  # Anderson-Darling test (for n >= 7)
  if (length(raw_residuals) >= 7) {
    ad_test <- nortest::ad.test(scaled_residuals)
  } else {
    ad_test <- "Sample size too small (n < 7) for Anderson-Darling test"
  }
  
  regression_mode <- !isTRUE(correlation) && (is.numeric(fact) || is.integer(fact))
  
  # Variance tests (only for grouped diagnostics)
  if (!regression_mode) {
    levene_test <- levene.test(scaled_residuals, fact)
    bartlett_test <- bartlett.test(scaled_residuals ~ fact)
     bp_test <- NULL
  } else {
    # For regression: use Breusch-Pagan test for heteroscedasticity
    # (tests if error variance depends on independent variables)
    levene_test <- NULL
    bartlett_test <- NULL
     bp_test <-   bp.test(anova_model)
  }

  qq_envelope <- tryCatch(
    qq_lm_envelope(anova_model, conf.level = conf.level, nsim = qq_nsim),
    error = function(e) NULL
  )
  
  plot_histogram <- function(x, xlab) {
    x_min <- min(min(x, na.rm = TRUE), -3.2)
    x_max <- max(max(x, na.rm = TRUE), 3.2)
    temp_hist <- hist(x, plot = FALSE)
    y_max <- max(max(temp_hist$density), 0.45)

    hist(x, freq = FALSE, main = "Histogram and Normal Density",
         xlab = xlab, col = "lightblue", border = "black",
         xlim = c(x_min, x_max), ylim = c(0, y_max))
    x_seq <- seq(x_min, x_max, length.out = 200)
    lines(x_seq, dnorm(x_seq), col = "red", lwd = 1)
  }
  
  plot_qq <- function(x, ylab) {
    diagnostic_col <- colorscheme(3)[5]
    if (is.null(qq_envelope)) {
      qq <- qqnorm(x, plot.it = FALSE)
    } else {
      qq <- list(x = qq_envelope$expected, y = qq_envelope$observed)
    }
    y_lim <- range(qq$y, na.rm = TRUE)
    if (!is.null(qq_envelope)) {
      y_lim <- range(y_lim, qq_envelope$pointwise, qq_envelope$global, na.rm = TRUE)
    }

    plot(qq$x, qq$y,
         type = "n",
         main = "Normal Q-Q Plot",
         xlab = "Theoretical quantiles",
         ylab = ylab,
         ylim = y_lim)
    if (!is.null(qq_envelope)) {
      polygon(
        c(qq$x, rev(qq$x)),
        c(qq_envelope$global[1, ], rev(qq_envelope$global[2, ])),
        border = NA,
        col = grDevices::adjustcolor(diagnostic_col, alpha.f = 0.25)
      )
      lines(qq$x, qq_envelope$pointwise[1, ], col = "black", lty = 2, lwd = 1)
      lines(qq$x, qq_envelope$pointwise[2, ], col = "black", lty = 2, lwd = 1)
      lines(qq$x, qq_envelope$global[1, ], col = diagnostic_col, lwd = 1)
      lines(qq$x, qq_envelope$global[2, ], col = diagnostic_col, lwd = 1)
    }
    qqline(x, col = "red", lwd = 1)
    points(qq$x, qq$y, pch = 1, col = "black")
    if (!is.null(qq_envelope)) {
      legend(
        "topleft",
        legend = sprintf(
          c("%.0f%% simultaneous tolerance band",
            "%.0f%% point-wise tolerance band"),
          100 * conf.level
        ),
        lty = c(1, 2),
        lwd = 1,
        col = c(diagnostic_col, "black"),
        bty = "n",
        cex = 0.65
      )
    }
  }
  
  plot_residuals_vs_fitted <- function() {
    y_lim <- extendrange(c(z_residuals, -3, 3), f = 0.08)
    y_ticks <- seq(floor(y_lim[1]), ceiling(y_lim[2]), by = 1)
    # smoother_col <- colorscheme(3)[5]
    fitted_values <- fitted(anova_model)
    plot(fitted_values, z_residuals,
         type = "n",
         main = "Residuals vs. Fitted",
         xlab = "Fitted response",
         ylab = "z residuals",
         ylim = y_lim,
         yaxt = "n")
    axis(2, at = y_ticks, las = 1)
    # ok <- is.finite(fitted_values) & is.finite(z_residuals)
    # smoother_fit <- NULL
    # smoother_x <- NULL
    # if (sum(ok) >= 3 && length(unique(fitted_values[ok])) >= 3) {
    #   smooth_dat <- data.frame(x = fitted_values[ok], z = z_residuals[ok])
    #   smoother <- tryCatch(
    #     suppressWarnings(stats::loess(
    #       z ~ x,
    #       data = smooth_dat,
    #       span = 2 / 3,
    #       degree = 1,
    #       family = "symmetric"
    #     )),
    #     error = function(e) NULL
    #   )
    #   if (!is.null(smoother)) {
    #     smoother_x <- seq(min(smooth_dat$x), max(smooth_dat$x), length.out = 200)
    #     pred <- tryCatch(
    #       stats::predict(smoother, data.frame(x = smoother_x), se = TRUE),
    #       error = function(e) NULL
    #     )
    #     if (!is.null(pred) && all(is.finite(pred$fit))) {
    #       smoother_fit <- pred$fit
    #       if (all(is.finite(pred$se.fit)) && is.finite(pred$df) && pred$df > 0) {
    #         crit <- stats::qt((1 + conf.level) / 2, df = pred$df)
    #         smoother_band <- crit * pred$se.fit
    #         polygon(
    #           c(smoother_x, rev(smoother_x)),
    #           c(smoother_fit - smoother_band,
    #             rev(smoother_fit + smoother_band)),
    #           border = NA,
    #           col = grDevices::adjustcolor(smoother_col, alpha.f = 0.2)
    #         )
    #       }
    #     }
    #   }
    # }
    abline(h = c(-3, 3), col = "grey85", lty = 2, lwd = 1)
    abline(h = 0, col = "red", lwd = 1)
    points(fitted_values, z_residuals, pch = 1, col = "black")
    # if (!is.null(smoother_x) && !is.null(smoother_fit)) {
    #   lines(smoother_x, smoother_fit, col = smoother_col, lwd = 1)
    #   legend(
    #     "topleft",
    #     legend = c(
    #       "lowess smoother",
    #       sprintf("%.0f%% smoother band", 100 * conf.level)
    #     ),
    #     lty = c(1, NA),
    #     lwd = c(1, NA),
    #     pch = c(NA, 15),
    #     pt.cex = c(NA, 1.5),
    #     col = c(smoother_col, grDevices::adjustcolor(smoother_col, alpha.f = 0.2)),
    #     bty = "n",
    #     cex = 0.65
    #   )
    # }
    outliers <- which(abs(z_residuals) > 3)
    if (length(outliers) > 0) {
      text(fitted_values[outliers], z_residuals[outliers],
           labels = outliers, pos = 3, cex = 0.7)
    }
  }
  
  if (regression_mode) {
    par(mfrow = c(1, 3), oma = c(0, 0, 3, 0), mar = c(4.5, 4, 3, 1),
        cex = 0.7 * cex)
    plot_histogram(scaled_residuals, "Standardised residuals")
    plot_qq(scaled_residuals, "Standardised residuals")
    plot_residuals_vs_fitted()
  } else {
    par(mfrow = c(1, 3), oma = c(0, 0, 3, 0), mar = c(4.5, 4, 3, 1),
        cex = 0.7 * cex)
    plot_histogram(scaled_residuals, "Standardised residuals")
    plot_qq(scaled_residuals, "Standardised residuals")

    abs_scaled_residuals <- abs(scaled_residuals)
    fact_plot <- factor(fact)
    group_id <- as.numeric(fact_plot)
    x_jitter <- jitter(group_id, amount = 0.08)
    y_lim <- c(0, max(3, abs_scaled_residuals, na.rm = TRUE) * 1.08)
    y_ticks <- seq(0, ceiling(y_lim[2]), by = 1)
    plot(x_jitter, abs_scaled_residuals,
         main = "Absolute residuals vs. Group",
         xlab = "Group",
         ylab = "Absolute standardised residuals",
         xlim = c(0.5, length(levels(fact_plot)) + 0.5),
         ylim = y_lim,
         xaxt = "n",
         yaxt = "n",
         pch = 1,
         col = "grey40")
    axis(1, at = seq_along(levels(fact_plot)), labels = levels(fact_plot))
    axis(2, at = y_ticks, las = 1)
    abline(h = 3, col = "grey85", lty = 2, lwd = 1)
    
    group_means <- tapply(abs_scaled_residuals, fact_plot, mean, na.rm = TRUE)
    points(seq_along(group_means), group_means, pch = 4, col = "red", cex = 0.8, lwd = 1.5)
    
    outliers <- which(abs_scaled_residuals > 3)
    if (length(outliers) > 0) {
      text(x_jitter[outliers], abs_scaled_residuals[outliers],
           labels = outliers, pos = 3, cex = 0.7)
    }

    legend("topright", legend = "group mean", pch = 4, pt.lwd = 1.5, col = "red",
           bty = "n", cex = 0.8)
  }

  # Add overall title with test results
  p_shapiro <- if (!is.na(shapiro_test$p.value)) signif(shapiro_test$p.value, 2) else "NA"
  p_AD <- if (is.list(ad_test) && !is.null(ad_test$p.value)) signif(ad_test$p.value, 2) else "N/A"
  
  if (regression_mode) {
    # Regression title with Breusch-Pagan test - split into two rows
    p_bp <- signif( bp_test$p.value, 2)
    title_line1 <- paste("Shapiro-Wilk p =", p_shapiro, 
                         "| Anderson-Darling p =", p_AD)
    title_line2 <- paste("Breusch-Pagan p =", p_bp)
    
    mtext(title_line1, side = 3, outer = TRUE, line = 1, cex = 0.7)
    mtext(title_line2, side = 3, outer = TRUE, line = 0, cex = 0.7)
  } else {
    # ANOVA title with Levene and Bartlett - split into two rows
    title_line1 <- paste("Shapiro-Wilk p =", p_shapiro,
                         "| Anderson-Darling p =", if(is.numeric(p_AD)) signif(p_AD, 2) else p_AD)
    title_line2 <- paste("Levene p =", signif(levene_test$p.value, 2),
                         "| Bartlett p =", signif(bartlett_test$p.value, 2))
    
    mtext(title_line1, side = 3, outer = TRUE, line = 1, cex = 0.7)
    mtext(title_line2, side = 3, outer = TRUE, line = 0, cex = 0.7)
  }
  
  # Return results
  result <- list(
    summary_anova = summary(anova_model),
    shapiro_test = shapiro_test,
    ad_test = ad_test,
    levene_test = if (!regression_mode) levene_test else NULL,
    bartlett_test = if (!regression_mode) bartlett_test else NULL,
    bp_test = if (regression_mode)  bp_test else NULL
  )
  
  class(result) <- "vis_lm_assumptions"
  return(invisible(result))
}

#' @noRd
vis_anova_assumptions <- function(...) {
  .Deprecated("vis_lm_assumptions")
  vis_lm_assumptions(...)
}
