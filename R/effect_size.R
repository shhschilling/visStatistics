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
  1 - 3 / (4 * df - 1)
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
  eps2 <- (h - k + 1) / (n - k)
  effect_size_record(
    "epsilon-squared",
    max(0, eps2, na.rm = TRUE),
    "Epsilon-squared for Kruskal-Wallis rank sum test"
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

effect_size_for_visstat <- function(result, samples = NULL, fact = NULL) {
  if (!is.null(result$effect_size)) {
    return(result$effect_size)
  }
  if (!is.null(result[["t-test-statistics"]])) {
    method <- result[["t-test-statistics"]]$method
    return(effect_size_t_test(samples, fact, var.equal = !grepl("Welch", method, ignore.case = TRUE)))
  }
  if (!is.null(result[["statsWilcoxon"]])) {
    return(effect_size_wilcoxon(samples, fact))
  }
  if (!is.null(result[["summary statistics of ANOVA"]])) {
    return(effect_size_anova(samples, fact, result))
  }
  if (!is.null(result[["Kruskal Wallis rank sum test"]])) {
    return(effect_size_kruskal(samples, fact, result))
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
