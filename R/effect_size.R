# Internal effect-size helpers -------------------------------------------------

effect_size_record <- function(name, estimate, effect_size_method, conf.int = NULL) {
  out <- list(
    name = name,
    estimate = unname(estimate),
    effect_size_method = effect_size_method
  )
  if (!is.null(conf.int)) {
    out$conf.int <- unname(conf.int)
  }
  out
}

effect_size_unavailable <- function(effect_size_method) {
  effect_size_record(NA_character_, NA_real_, effect_size_method)
}

hedges_correction <- function(df) {
  if (!is.finite(df) || df <= 1) {
    return(NA_real_)
  }
  exp(lgamma(df / 2) - 0.5 * log(df / 2) - lgamma((df - 1) / 2))
}

effect_size_t_test <- function(samples, fact, var.equal = FALSE) {
  groups <- split(samples, fact)
  groups <- groups[seq_len(2)]
  x1 <- stats::na.omit(groups[[1]])
  x2 <- stats::na.omit(groups[[2]])
  n1 <- length(x1)
  n2 <- length(x2)
  if (n1 < 2 || n2 < 2) {
    return(effect_size_unavailable("Hedges' g requires at least two observations per group."))
  }
  m1 <- mean(x1)
  m2 <- mean(x2)
  s1 <- stats::var(x1)
  s2 <- stats::var(x2)
  if (isTRUE(var.equal)) {
    sd_std <- sqrt(((n1 - 1) * s1 + (n2 - 1) * s2) / (n1 + n2 - 2))
    df <- n1 + n2 - 2
    method <- "Hedges' g using pooled standard deviation"
  } else {
    sd_std <- sqrt((s1 + s2) / 2)
    df <- n1 + n2 - 2
    method <- "Hedges' g using unpooled standard deviation"
  }
  g <- hedges_correction(df) * ((m1 - m2) / sd_std)
  effect_size_record("Hedges' g", g, method)
}

effect_size_wilcoxon <- function(samples, fact) {
  groups <- split(samples, fact)
  groups <- groups[seq_len(2)]
  x1 <- stats::na.omit(groups[[1]])
  x2 <- stats::na.omit(groups[[2]])
  n1 <- length(x1)
  n2 <- length(x2)
  if (n1 == 0 || n2 == 0) {
    return(effect_size_unavailable("Rank-biserial correlation requires observations in both groups."))
  }
  wt <- suppressWarnings(stats::wilcox.test(x1, x2, exact = FALSE))
  u <- unname(wt$statistic)
  r_rb <- (2 * u) / (n1 * n2) - 1
  effect_size_record(
    "rank-biserial correlation",
    r_rb,
    "Rank-biserial correlation from the Wilcoxon rank-sum statistic"
  )
}

effect_size_anova <- function(samples, fact, result) {
  if (inherits(result[["summary statistics of ANOVA"]], "htest")) {
    test <- result[["summary statistics of ANOVA"]]
    f_value <- unname(test$statistic)
    df1 <- unname(test$parameter[1])
    df2 <- unname(test$parameter[2])
    omega <- (df1 * (f_value - 1)) / (df1 * f_value + df2 + 1)
    return(effect_size_record(
      "approximate omega-squared-type",
      max(0, omega, na.rm = TRUE),
      "Approximate omega-squared-type measure for Welch's one-way test, computed from F, df1, and df2"
    ))
  }

  fit <- stats::aov(samples ~ fact)
  tab <- summary(fit)[[1]]
  ss_between <- tab[["Sum Sq"]][1]
  ss_within <- tab[["Sum Sq"]][2]
  df_between <- tab[["Df"]][1]
  ms_within <- tab[["Mean Sq"]][2]
  omega <- (ss_between - df_between * ms_within) /
    (ss_between + ss_within + ms_within)
  effect_size_record(
    "omega-squared",
    max(0, omega, na.rm = TRUE),
    "Omega-squared for one-way ANOVA"
  )
}

effect_size_kruskal <- function(samples, fact, result) {
  test <- result[["Kruskal Wallis rank sum test"]]
  h <- unname(test$statistic)
  k <- length(unique(fact[!is.na(samples)]))
  n <- sum(!is.na(samples))
  eta2_h <- (h - k + 1) / (n - k)
  effect_size_record(
    "eta-squared based on H",
    max(0, eta2_h, na.rm = TRUE),
    "Eta-squared based on H for Kruskal-Wallis rank sum test"
  )
}

effect_size_chi_fisher <- function(result) {
  test <- result
  if (!is.null(result$statistic) && !is.null(result$observed)) {
    observed <- result$observed
    n <- sum(observed)
    dims <- dim(observed)
    if (all(dims == c(2, 2))) {
      phi <- sqrt(unname(result$statistic) / n)
      return(effect_size_record("phi", phi, "Phi coefficient for 2 x 2 contingency table"))
    }
    v <- sqrt(unname(result$statistic) / (n * (min(dims) - 1)))
    return(effect_size_record("Cramer's V", v, "Cramer's V for contingency table"))
  }
  if (!is.null(test$estimate)) {
    return(effect_size_record(
      names(test$estimate)[1],
      unname(test$estimate)[1],
      "Odds ratio from Fisher's exact test",
      conf.int = test$conf.int
    ))
  }
  effect_size_unavailable("No default effect size available for this contingency-table result.")
}

effect_size_kendall <- function(result) {
  test <- result$test
  effect_size_record(
    "Kendall's tau-b",
    unname(test$estimate),
    "Kendall rank correlation coefficient"
  )
}

#' Compute an effect-size estimate for a visstat result
#'
#' \code{effect_size()} returns the effect-size estimate associated with a
#' \code{visstat()} result. If \code{result$effect_size} is already present, it
#' is returned unchanged. Otherwise, the estimate is computed from the test
#' object stored in \code{result}; for some base R \pkg{stats} results, it is
#' extracted directly from the returned object.
#'
#' @details
#' Notation used below: \eqn{x} and \eqn{y} are the two variables entering
#' the selected analysis, \eqn{N} is the total number of non-missing
#' observations, \eqn{n_j} is the sample size in group \eqn{j}, \eqn{k} is
#' the number of groups, \eqn{\bar{y}_j} is the mean of numeric vector
#' \eqn{y} in group \eqn{j}, and \eqn{s_j^2} is the variance of numeric
#' vector \eqn{y} in group \eqn{j}.
#'
#' The following estimates are computed internally:
#'
#' \itemize{
#'   \item Student's two-sample \code{t.test(..., var.equal = TRUE)}:
#'     Hedges' \eqn{g_{s_p} = J(N-2)(\bar{y}_1-\bar{y}_2)/s_p}, where
#'     \eqn{s_p = \sqrt{((n_1-1)s_1^2+(n_2-1)s_2^2)/(N-2)}} and
#'     \eqn{J(\nu) = \Gamma(\nu/2)/(\sqrt{\nu/2}\Gamma((\nu-1)/2))}.
#'   \item Welch's two-sample \code{t.test(..., var.equal = FALSE)}:
#'     Hedges' \eqn{g_{s^*} = J(N-2)(\bar{y}_1-\bar{y}_2)/s^*}, where
#'     \eqn{s^* = \sqrt{(s_1^2+s_2^2)/2}}. This is the package's
#'     average-variance standardizer.
#'   \item Wilcoxon rank-sum test: signed rank-biserial correlation
#'     \eqn{r = 2W/(n_1 n_2)-1}, where \eqn{W} is the statistic returned by
#'     \code{wilcox.test()} for the first group.
#'   \item Fisher's one-way ANOVA: omega-squared
#'     \eqn{\omega^2 = \nu_1(F-1)/(\nu_1F+\nu_2+1)}, where \eqn{F} is the
#'     ordinary one-way ANOVA statistic, \eqn{\nu_1=k-1}, and
#'     \eqn{\nu_2=N-k}. Negative estimates are truncated to zero.
#'   \item Welch's one-way test: a package-defined approximate
#'     omega-squared-type estimate
#'     \eqn{\nu_1(F_W-1)/(\nu_1F_W+\nu_2+1)}, where \eqn{F_W} is the
#'     Welch ANOVA statistic, \eqn{\nu_1=k-1}, and \eqn{\nu_2} is the
#'     usually fractional denominator degree of freedom returned by
#'     \code{oneway.test()}. Negative estimates are truncated to zero.
#'   \item Kruskal-Wallis test: Kelley-adjusted eta-squared based on
#'     \eqn{H}, \eqn{\eta_H^2=(H-k+1)/(N-k)}, where \eqn{H} is the
#'     Kruskal-Wallis statistic. Negative estimates are truncated to zero.
#'   \item Pearson's chi-squared test: Cramer's \eqn{V} for general
#'     \eqn{R\times C} tables,
#'     \eqn{V=\sqrt{\chi^2/(N\cdot(\min(R,C)-1))}}, where \eqn{R} and \eqn{C}
#'     are the numbers of rows and columns. For \eqn{2\times 2} tables this
#'     is phi, \eqn{\sqrt{\chi^2/N}}. The chi-squared
#'     statistic is used as supplied by \code{chisq.test()}.
#' }
#'
#' The following estimates are extracted from existing result objects:
#' \itemize{
#'   \item \eqn{R^2} from \code{summary(lm())$r.squared}.
#'   \item Spearman's \eqn{\rho} from
#'     \code{cor.test(method = "spearman")$estimate}.
#'   \item Kendall's \eqn{\tau_b} from
#'     \code{cor.test(method = "kendall")$estimate}.
#'   \item The conditional maximum-likelihood odds ratio from
#'     \code{fisher.test()$estimate} and its confidence interval from
#'     \code{fisher.test()$conf.int} for \eqn{2\times 2} tables.
#' }
#'
#' @param result A list returned by \code{visstat()} or a compatible test
#'   result object.
#' @param x First input vector, matching the first argument of
#'   \code{visstat(x, y)}. Required when the effect size cannot be extracted
#'   from \code{result} alone.
#' @param y Second input vector, matching the second argument of
#'   \code{visstat(x, y)}. Required when the effect size cannot be extracted
#'   from \code{result} alone.
#' @return A list with components \code{name}, \code{estimate},
#'   \code{effect_size_method}, and optionally \code{conf.int}.
#' @references
#' Hedges, L. V. (1981). Distribution theory for Glass's estimator of effect
#' size and related estimators. \emph{Journal of Educational Statistics},
#' 6(2), 107--128. doi:10.3102/10769986006002107.
#'
#' Kerby, D. S. (2014). The simple difference formula: An approach to teaching
#' nonparametric correlation. \emph{Comprehensive Psychology}, 3.
#' doi:10.2466/11.IT.3.1.
#'
#' Olejnik, S., & Algina, J. (2003). Generalized eta and omega squared
#' statistics: Measures of effect size for some common research designs.
#' \emph{Psychological Methods}, 8(4), 434--447.
#' doi:10.1037/1082-989X.8.4.434.
#'
#' Ben-Shachar, M. S., Ludecke, D., & Makowski, D. (2020). effectsize:
#' Estimation of effect size indices and standardized parameters.
#' \emph{Journal of Open Source Software}, 5(56), 2815.
#' doi:10.21105/joss.02815.
#'
#' Kelley, T. L. (1935). An unbiased correlation ratio measure.
#' \emph{Proceedings of the National Academy of Sciences}, 21(9), 554--559.
#' doi:10.1073/pnas.21.9.554.
#'
#' Bergsma, W. (2013). A bias-correction for Cramer's V and Tschuprow's T.
#' \emph{Journal of the Korean Statistical Society}, 42(3), 323--328.
#' doi:10.1016/j.jkss.2012.10.002.
#' @examples
#' x <- ToothGrowth$supp
#' y <- ToothGrowth$len
#' tt <- list("t-test-statistics" = t.test(y ~ x, var.equal = TRUE))
#' effect_size(tt, x = x, y = y)
#'
#' kw <- list(
#'   "Kruskal Wallis rank sum test" = kruskal.test(Petal.Width ~ Species,
#'                                                data = iris)
#' )
#' effect_size(kw, x = iris$Species, y = iris$Petal.Width)
#'
#' tab <- matrix(c(10, 5, 4, 12), nrow = 2)
#' effect_size(chisq.test(tab))
#' @export
effect_size <- function(result, x = NULL, y = NULL) {
  if (!is.null(result$effect_size)) {
    return(result$effect_size)
  }
  if (!is.null(result[["t-test-statistics"]])) {
    method <- result[["t-test-statistics"]]$method
    return(effect_size_t_test(y, x, var.equal = !grepl("Welch", method, ignore.case = TRUE)))
  }
  if (!is.null(result[["statsWilcoxon"]])) {
    return(effect_size_wilcoxon(y, x))
  }
  if (!is.null(result[["summary statistics of ANOVA"]])) {
    return(effect_size_anova(y, x, result))
  }
  if (!is.null(result[["Kruskal Wallis rank sum test"]])) {
    return(effect_size_kruskal(y, x, result))
  }
  if (!is.null(result$test) && grepl("Kendall", result$test$method, ignore.case = TRUE)) {
    return(effect_size_kendall(result))
  }
  if (!is.null(result$method) && (grepl("Chi-squared", result$method, ignore.case = TRUE) ||
                                 grepl("Fisher", result$method, ignore.case = TRUE))) {
    return(effect_size_chi_fisher(result))
  }
  effect_size_unavailable("No effect size could be inferred for this result.")
}
