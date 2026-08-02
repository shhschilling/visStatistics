### Header vis_group_normality -----

#' Group-wise normality display for the Welch route
#'
#' Draws, for each group, a histogram with the fitted normal density overlaid
#' and a normal Q-Q plot of the internally studentised residuals of an
#' intercept-only fit to that group, with the simulated pointwise and
#' simultaneous envelopes of \code{qq_lm_envelope()}. Shapiro-Wilk and
#' Anderson-Darling p-values for that group head each pair of panels.
#'
#' Welch-type tests assume normality within each group with group-specific
#' variances (Welch 1951), so the diagnostic belongs at the group level. The
#' pooled internally studentised residuals shown by \code{vis_lm_assumptions()}
#' are scaled by a single pooled \code{sigma} and, under unequal group
#' variances, form a scale mixture of normals even when every group is exactly
#' normal; a normality statistic computed on them is not a check of Welch's
#' assumption.
#'
#' Above \code{max_groups} groups the panel grid is unreadable and the
#' group-wise tests are reported as a table instead.
#'
#' @param samples Numeric vector of response values.
#' @param fact Factor giving the group membership.
#' @param conf.level Confidence level for the simulated Q-Q envelopes and for
#'   the alpha used in the tabular summary.
#' @param cex Character expansion applied to the panels.
#' @param qq_nsim Integer number of simulated refits per group.
#' @param max_groups Largest number of groups still drawn as panels.
#' @param plot_args Optional named list of base graphics parameters.
#'
#' @return Invisibly, a list with the per-group Shapiro-Wilk and
#'   Anderson-Darling results, the group names and the per-group sample sizes.
#' @noRd
vis_group_normality <- function(samples, fact, conf.level = 0.95, cex = 1,
                                qq_nsim = getOption(
                                  "visStatistics.qq_nsim", 5000L
                                ),
                                max_groups = 10L, plot_args = list()) {
  if (is.null(plot_args)) plot_args <- list()
  alpha <- 1 - conf.level

  keep <- stats::complete.cases(samples, fact)
  y <- samples[keep]
  g <- droplevels(as.factor(fact[keep]))

  group_levels <- levels(g)
  k <- length(group_levels)
  if (k < 2) {
    stop("At least 2 groups required")
  }

  group_data <- lapply(group_levels, function(lev) y[g == lev])
  names(group_data) <- group_levels
  group_n <- vapply(group_data, length, integer(1))

  ## Group-wise normality tests. These are per group by design: they test the
  ## assumption Welch actually makes. Nothing routes on them.
  shapiro_tests <- lapply(group_levels, function(gname) {
    x <- group_data[[gname]]
    n <- length(x)
    if (n >= 3 && n <= 5000) {
      stats::shapiro.test(x)
    } else {
      list(
        statistic = NA_real_, p.value = NA_real_,
        method = if (n < 3) {
          paste0("Shapiro-Wilk requires n >= 3 (n = ", n, ")")
        } else {
          paste0("Shapiro-Wilk allows n <= 5000 (n = ", n, ")")
        }
      )
    }
  })
  names(shapiro_tests) <- group_levels

  ad_tests <- lapply(group_levels, function(gname) {
    x <- group_data[[gname]]
    n <- length(x)
    if (n >= 7 && stats::sd(x) > 0) {
      tryCatch(nortest::ad.test(x), error = function(e) {
        list(statistic = NA_real_, p.value = NA_real_, method = conditionMessage(e))
      })
    } else {
      list(
        statistic = NA_real_, p.value = NA_real_,
        method = paste0("Anderson-Darling requires n >= 7 (n = ", n, ")")
      )
    }
  })
  names(ad_tests) <- group_levels

  ## The user-facing table. Built on every call, not only when the panel grid
  ## is dropped: on the Welch route this is the assumption summary the object
  ## carries, in place of the residual-based Shapiro-Wilk and Anderson-Darling
  ## of the automatic route.
  results_table <- data.frame(
    group = group_levels,
    n = as.integer(group_n),
    shapiro_W = round(
      vapply(shapiro_tests, function(z) as.numeric(z$statistic), numeric(1)), 4
    ),
    shapiro_p = signif(
      vapply(shapiro_tests, function(z) as.numeric(z$p.value), numeric(1)), 3
    ),
    anderson_darling_A = round(
      vapply(ad_tests, function(z) as.numeric(z$statistic), numeric(1)), 4
    ),
    anderson_darling_p = signif(
      vapply(ad_tests, function(z) as.numeric(z$p.value), numeric(1)), 3
    ),
    stringsAsFactors = FALSE, row.names = NULL
  )
  attr(results_table, "alpha") <- alpha
  attr(results_table, "note") <- paste(
    "Group-wise normality. Welch-type tests assume normality within each",
    "group with group-specific variances; nothing in the routing depends on",
    "these values."
  )

  result <- list(
    shapiro_tests = shapiro_tests,
    ad_tests = ad_tests,
    results_table = results_table,
    n_groups = k,
    group_names = group_levels,
    group_sizes = group_n
  )

  ## Too many groups for a readable grid: report the tests instead of drawing.
  if (k > max_groups) {
    n_fail_shapiro <- sum(results_table$shapiro_p < alpha, na.rm = TRUE)
    n_fail_ad <- sum(results_table$anderson_darling_p < alpha, na.rm = TRUE)

    cat("\nGroup-wise normality (", k, " groups; panels omitted above ",
      max_groups, ")\n", sep = ""
    )
    print(results_table, row.names = FALSE)
    cat("\n  ", n_fail_shapiro, "of", k, "groups reject Shapiro-Wilk at alpha =", alpha, "\n")
    cat("  ", n_fail_ad, "of", k, "groups reject Anderson-Darling at alpha =", alpha, "\n\n")

    result$panels_drawn <- FALSE
    result$note <- "Too many groups for the panel grid; group-wise tests reported as a table."
    class(result) <- "vis_group_normality"
    return(invisible(result))
  }

  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))

  plot_cex <- if (k <= 3) 0.6 * cex else if (k <= 5) 0.55 * cex else 0.5 * cex
  # Bottom and left must hold an axis title plus the tick labels at the reduced
  # cex; the top holds the two-line group heading with the SW/AD p-values.
  mar_val <- if (k <= 3) {
    c(5.0, 5.0, 3.6, 1.0)
  } else if (k <= 5) {
    c(4.8, 4.8, 3.4, 0.8)
  } else {
    c(4.4, 4.4, 3.2, 0.6)
  }

  visstat_graphics_par(plot_args, list(
    mfrow = c(2, k),
    mar = mar_val,
    # Bottom outer margin holds the figure-level key and its heading without
    # overprinting the per-panel axis titles; left outer margin keeps
    # "Density" / "Std. residuals" inside the device. No figure title is
    # drawn, so the top margin only needs to clear the panel headings.
    oma = c(3.2, 1.2, 0.6, 0.3),
    cex = plot_cex,
    font.main = 1,
    font.lab = 1,
    font.axis = 1
  ))

  fmt_p <- function(p) if (is.na(p)) "NA" else signif(p, 2)

  ## Row 1: histogram with the fitted normal, headed by the group-wise p-values.
  for (i in seq_along(group_levels)) {
    gname <- group_levels[i]
    gdata <- group_data[[i]]
    if (length(gdata) == 0) {
      graphics::plot.new()
      next
    }
    gmean <- mean(gdata)
    gsd <- stats::sd(gdata)

    hist_data <- graphics::hist(gdata, plot = FALSE)
    y_max <- max(hist_data$density) * 1.1
    if (is.finite(gsd) && gsd > 0) {
      y_max <- max(y_max, stats::dnorm(gmean, gmean, gsd) * 1.1)
    }

    graphics::hist(gdata,
      freq = FALSE,
      main = paste0(
        gname, " (n = ", length(gdata), ")\nSW p=",
        fmt_p(shapiro_tests[[gname]]$p.value), ", AD p=",
        fmt_p(ad_tests[[gname]]$p.value)
      ),
      xlab = "",
      ylab = if (i == 1) "Density" else "",
      ylim = c(0, y_max),
      col = "lightblue",
      border = "black"
    )

    if (is.finite(gsd) && gsd > 0) {
      rng <- range(c(range(gdata), gmean - 3 * gsd, gmean + 3 * gsd))
      x_seq <- seq(rng[1], rng[2], length.out = 200)
      graphics::lines(x_seq, stats::dnorm(x_seq, gmean, gsd), col = "red", lwd = 2)
    }
  }

  ## Row 2: Q-Q of the within-group studentised residuals with the simulated
  ## envelopes. No legend: at this panel size it is unreadable.
  ##
  ## Two passes: the first computes every group's envelope and Q-Q coordinates
  ## and tracks the range across all of them; the second draws the panels
  ## against that one shared y-axis. A per-panel range (as for the x-axis,
  ## which is left free since it depends on each group's own n) would make
  ## groups of different spread look alike, which is exactly what this panel
  ## exists to show.
  diagnostic_col <- colorscheme(3)[5]
  envelopes <- vector("list", k)
  names(envelopes) <- group_levels
  qq_data <- vector("list", k)
  names(qq_data) <- group_levels
  y_values <- list()

  for (i in seq_along(group_levels)) {
    gname <- group_levels[i]
    gdata <- group_data[[i]]
    if (length(gdata) < 3 || !is.finite(stats::sd(gdata)) || stats::sd(gdata) == 0) {
      next
    }

    fit_i <- stats::lm(gdata ~ 1)
    env <- tryCatch(
      qq_lm_envelope(fit_i, conf.level = conf.level, nsim = qq_nsim),
      error = function(e) NULL
    )
    envelopes[[gname]] <- env

    rs <- stats::rstandard(fit_i)
    if (is.null(env)) {
      qq <- stats::qqnorm(rs, plot.it = FALSE)
      y_values[[gname]] <- qq$y
    } else {
      qq <- list(x = env$expected, y = env$observed)
      y_values[[gname]] <- c(qq$y, env$pointwise, env$global)
    }
    qq_data[[gname]] <- list(qq = qq, rs = rs)
  }
  # range()'s na.rm drops NA/NaN but not +-Inf, so an Inf-seeded accumulator
  # would poison every update; collecting first and ranging once avoids that.
  y_lim_all <- range(unlist(y_values), na.rm = TRUE)

  for (i in seq_along(group_levels)) {
    gname <- group_levels[i]
    d <- qq_data[[gname]]
    if (is.null(d)) {
      graphics::plot.new()
      next
    }
    qq <- d$qq
    env <- envelopes[[gname]]

    plot(qq$x, qq$y,
      type = "n",
      main = paste0(gname, " Q-Q"),
      xlab = "Theoretical quantiles",
      ylab = if (i == 1) "Std. residuals" else "",
      ylim = y_lim_all
    )
    if (!is.null(env)) {
      graphics::polygon(
        c(qq$x, rev(qq$x)),
        c(env$global[1, ], rev(env$global[2, ])),
        border = NA,
        col = grDevices::adjustcolor(diagnostic_col, alpha.f = 0.25)
      )
      graphics::lines(qq$x, env$pointwise[1, ], col = "black", lty = 2, lwd = 1)
      graphics::lines(qq$x, env$pointwise[2, ], col = "black", lty = 2, lwd = 1)
      graphics::lines(qq$x, env$global[1, ], col = diagnostic_col, lwd = 1)
      graphics::lines(qq$x, env$global[2, ], col = diagnostic_col, lwd = 1)
    }
    stats::qqline(d$rs, col = "red", lwd = 1)
    graphics::points(qq$x, qq$y, pch = 1, col = "black")
  }

  have_envelope <- any(!vapply(envelopes, is.null, logical(1)))

  ## One key for the whole figure, in the wording of vis_lm_assumptions(). The
  ## band semantics are identical in every panel, so a per-panel key would
  ## repeat the same two entries k times at the size where they are least
  ## readable. The number of simulated refits is the provenance of the bands and
  ## so heads their key rather than the figure.
  if (have_envelope) {
    ## par("cex") still carries the per-panel expansion here, so a legend drawn
    ## at par("cex.lab") matches the axis titles exactly.
    label_cex <- graphics::par("cex.lab")
    graphics::par(
      fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
      new = TRUE
    )
    graphics::plot.new()
    ## Band names are written out: a figure key has room for them, so there is
    ## nothing for an abbreviation to buy here. The construction is credited in
    ## the documentation of qq_lm_envelope(), not on the figure. The third entry
    ## carries no line, so the key stays on one row.
    graphics::legend("bottom",
      horiz = TRUE,
      legend = c(
        sprintf("%.0f%% simultaneous tolerance band", 100 * conf.level),
        sprintf(
          "%.0f%% point-wise tolerance band, from %d simulated refits",
          100 * conf.level, as.integer(qq_nsim)
        )
      ),
      lty = c(1, 2),
      lwd = 1,
      col = c(diagnostic_col, "black"),
      bty = "n",
      cex = label_cex,
      # horiz = TRUE otherwise pads both entries to the width of the longer
      # one, leaving a gap between them wider than the labels themselves.
      text.width = NA
    )
  }

  result$qq_envelopes <- envelopes
  result$panels_drawn <- TRUE
  class(result) <- "vis_group_normality"
  invisible(result)
}


### Header welch_small_group_warning -----

#' Sample-size caution for the Welch route
#'
#' Warns when the smallest group falls below the size from which Welch's test
#' has been reported to control the Type I error rate in the presence of
#' skewness: fewer than 50 observations per group for at most four groups, fewer
#' than 100 for more than four groups (Delacre, Leys, Mora & Lakens 2019,
#' doi:10.5334/irsp.198).
#'
#' The condition is on the sample sizes alone. Gating it on a normality test is
#' not an option on this route: a pooled-residual test is invalid under the
#' unequal variances Welch exists for, and a per-group test has little power at
#' exactly these group sizes.
#'
#' @param fact Factor giving the group membership.
#'
#' @return Invisibly, \code{TRUE} when a warning was emitted.
#' @noRd
welch_small_group_warning <- function(fact) {
  g <- droplevels(as.factor(fact[!is.na(fact)]))
  counts <- table(g)
  if (length(counts) < 2) {
    return(invisible(FALSE))
  }

  threshold <- if (length(counts) <= 4) 50L else 100L
  smallest <- min(counts)
  if (smallest >= threshold) {
    return(invisible(FALSE))
  }

  warning(
    "Smallest group has ", smallest, " observations (", length(counts),
    " groups); Type I error control of Welch's test is not guaranteed below ",
    threshold, " observations per group. Type I error remains only between ",
    "2.5% and 7.5% for a minimum of ", threshold, " observations per group ",
    "under strong kurtosis and/or skewness ",
    "(Delacre et al. 2019 <doi:10.5334/irsp.198>). ",
    "The assumption plot shows the per-group distributions.",
    call. = FALSE
  )
  invisible(TRUE)
}
