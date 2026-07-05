library(testthat)

compare_appendix_stat <- function(rows, label, appendix, r_value,
                                  tolerance = 1e-10) {
  appendix <- unname(as.numeric(appendix))
  r_value <- unname(as.numeric(r_value))
  ok <- isTRUE(all.equal(appendix, r_value,
    tolerance = tolerance,
    check.attributes = FALSE
  ))
  if (!ok) {
    rows[[length(rows) + 1L]] <- data.frame(
      statistic = label,
      appendix = appendix,
      r_value = r_value,
      difference = appendix - r_value,
      stringsAsFactors = FALSE
    )
  }
  rows
}

manual_ad_stat <- function(x) {
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  z <- (x - mean(x)) / stats::sd(x)
  z_term <- stats::pnorm(z, log.p = TRUE) +
    rev(stats::pnorm(-z, log.p = TRUE))
  -n - mean((2 * seq_len(n) - 1) * z_term)
}

manual_fisher_anova_stat <- function(y, g) {
  g <- factor(g)
  n_i <- as.numeric(table(g))
  means <- as.numeric(tapply(y, g, mean))
  grand <- mean(y)
  ss_between <- sum(n_i * (means - grand)^2)
  ss_within <- sum(tapply(y, g, function(x) sum((x - mean(x))^2)))
  (ss_between / (length(n_i) - 1L)) / (ss_within / (length(y) - length(n_i)))
}

manual_levene_stat <- function(y, g) {
  g <- factor(g)
  group_means <- tapply(y, g, mean)
  z <- abs(y - group_means[g])
  manual_fisher_anova_stat(z, g)
}

manual_bartlett_stat <- function(y, g) {
  g <- factor(g)
  n_i <- as.numeric(table(g))
  vars <- as.numeric(tapply(y, g, stats::var))
  k <- length(n_i)
  n_total <- sum(n_i)
  pooled <- sum((n_i - 1) * vars) / (n_total - k)
  numerator <- (n_total - k) * log(pooled) - sum((n_i - 1) * log(vars))
  correction <- 1 + (sum(1 / (n_i - 1)) - 1 / (n_total - k)) /
    (3 * (k - 1))
  numerator / correction
}

manual_bp_stat <- function(fit) {
  aux <- stats::lm(stats::residuals(fit)^2 ~ stats::fitted(fit))
  length(stats::residuals(fit)) * summary(aux)$r.squared
}

manual_student_t_stat <- function(x, y) {
  n1 <- length(x)
  n2 <- length(y)
  sp2 <- ((n1 - 1) * stats::var(x) + (n2 - 1) * stats::var(y)) /
    (n1 + n2 - 2)
  (mean(x) - mean(y)) / sqrt(sp2 * (1 / n1 + 1 / n2))
}

manual_welch_t_stat <- function(x, y) {
  (mean(x) - mean(y)) / sqrt(stats::var(x) / length(x) + stats::var(y) / length(y))
}

manual_welch_anova_stat <- function(y, g) {
  g <- factor(g)
  n_i <- as.numeric(table(g))
  means <- as.numeric(tapply(y, g, mean))
  vars <- as.numeric(tapply(y, g, stats::var))
  k <- length(n_i)
  w_i <- n_i / vars
  w <- sum(w_i)
  weighted_mean <- sum(w_i * means) / w
  numerator <- sum(w_i * (means - weighted_mean)^2) / (k - 1)
  correction <- 1 + (2 * (k - 2) / (k^2 - 1)) *
    sum((1 - w_i / w)^2 / (n_i - 1))
  numerator / correction
}

manual_tukey_q <- function(y, g) {
  g <- factor(g)
  fit <- stats::aov(y ~ g)
  mse <- sum(stats::residuals(fit)^2) / fit$df.residual
  means <- tapply(y, g, mean)
  n_i <- table(g)
  comparisons <- utils::combn(names(means), 2)
  out <- data.frame(pair = character(), q = numeric())
  for (i in seq_len(ncol(comparisons))) {
    low <- comparisons[1L, i]
    high <- comparisons[2L, i]
    se <- sqrt((mse / 2) * (1 / n_i[low] + 1 / n_i[high]))
    out <- rbind(out, data.frame(
      pair = paste(high, low, sep = "-"),
      q = abs(means[high] - means[low]) / se
    ))
  }
  out
}

manual_wilcox_w <- function(x, y) {
  n1 <- length(x)
  pooled_ranks <- rank(c(x, y), ties.method = "average")
  sum(pooled_ranks[seq_len(n1)]) - n1 * (n1 + 1) / 2
}

manual_kruskal_h <- function(y, g) {
  g <- factor(g)
  ranks <- rank(y, ties.method = "average")
  n_total <- length(y)
  n_i <- as.numeric(table(g))
  mean_ranks <- as.numeric(tapply(ranks, g, mean))
  expected_rank <- (n_total + 1) / 2
  h <- 12 / (n_total * (n_total + 1)) *
    sum(n_i * (mean_ranks - expected_rank)^2)
  tie_blocks <- table(y)
  tie_factor <- 1 - sum(tie_blocks^3 - tie_blocks) /
    (n_total^3 - n_total)
  h / tie_factor
}

manual_kendall_tau_b <- function(x, y) {
  n <- length(x)
  concordant <- 0
  discordant <- 0
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      pair_sign <- sign(x[i] - x[j]) * sign(y[i] - y[j])
      concordant <- concordant + as.integer(pair_sign > 0)
      discordant <- discordant + as.integer(pair_sign < 0)
    }
  }
  n0 <- n * (n - 1) / 2
  n1 <- sum(table(x) * (table(x) - 1) / 2)
  n2 <- sum(table(y) * (table(y) - 1) / 2)
  (concordant - discordant) / sqrt((n0 - n1) * (n0 - n2))
}

manual_spearman_rho <- function(x, y) {
  stats::cor(rank(x), rank(y))
}

manual_chisq_stat <- function(tab) {
  expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  sum((tab - expected)^2 / expected)
}

manual_fisher_obs_prob <- function(tab) {
  a <- tab[1, 1]
  b <- tab[1, 2]
  c <- tab[2, 1]
  n <- sum(tab)
  choose(a + b, a) * choose(c + tab[2, 2], c) / choose(n, a + c)
}

manual_fisher_two_sided_p <- function(tab) {
  a <- tab[1, 1]
  row1 <- sum(tab[1, ])
  row2 <- sum(tab[2, ])
  col1 <- sum(tab[, 1])
  support <- max(0, col1 - row2):min(row1, col1)
  probs <- stats::dhyper(support, row1, row2, col1)
  observed <- stats::dhyper(a, row1, row2, col1)
  sum(probs[probs <= observed + .Machine$double.eps^0.5])
}

appendix_stat_discrepancies <- function() {
  rows <- list()

  x_norm <- c(-1.4, -0.8, -0.3, 0.1, 0.2, 0.5, 1.1, 1.7, 2.4)
  shapiro_internal <- .Call(
    getFromNamespace("C_SWilk", "stats"),
    sort(x_norm)
  )[1]
  rows <- compare_appendix_stat(
    rows, "Shapiro-Wilk W", shapiro_internal,
    stats::shapiro.test(x_norm)$statistic
  )

  rows <- compare_appendix_stat(
    rows, "Anderson-Darling A", manual_ad_stat(x_norm),
    nortest::ad.test(x_norm)$statistic
  )

  g3 <- factor(rep(c("A", "B", "C"), times = c(5, 6, 7)))
  y3 <- c(
    4.9, 5.1, 5.5, 5.0, 5.3,
    6.2, 5.9, 6.8, 6.5, 6.1, 6.4,
    7.4, 8.0, 7.8, 8.3, 7.2, 8.5, 7.9
  )

  rows <- compare_appendix_stat(
    rows, "Levene F", manual_levene_stat(y3, g3),
    levene.test(y3, g3)$statistic
  )
  rows <- compare_appendix_stat(
    rows, "Bartlett K-squared", manual_bartlett_stat(y3, g3),
    stats::bartlett.test(y3, g3)$statistic
  )

  fit <- stats::lm(
    c(2.1, 2.9, 3.7, 4.2, 5.1, 6.0, 6.4, 7.1, 7.9, 9.2, 9.8, 11.3) ~
      seq_len(12)
  )
  rows <- compare_appendix_stat(
    rows, "Breusch-Pagan BP", manual_bp_stat(fit), bp.test(fit)$statistic
  )

  x1 <- c(4.2, 5.1, 5.4, 4.8, 5.0)
  x2 <- c(6.1, 5.8, 6.5, 6.0, 6.2, 6.7)
  rows <- compare_appendix_stat(
    rows, "Student t", manual_student_t_stat(x1, x2),
    stats::t.test(x1, x2, var.equal = TRUE)$statistic
  )
  rows <- compare_appendix_stat(
    rows, "Welch t", manual_welch_t_stat(x1, x2),
    stats::t.test(x1, x2)$statistic
  )
  rows <- compare_appendix_stat(
    rows, "Fisher ANOVA F", manual_fisher_anova_stat(y3, g3),
    summary(stats::aov(y3 ~ g3))[[1]][["F value"]][1]
  )
  rows <- compare_appendix_stat(
    rows, "Welch ANOVA F", manual_welch_anova_stat(y3, g3),
    stats::oneway.test(y3 ~ g3)$statistic
  )

  tukey_q <- manual_tukey_q(y3, g3)
  tukey_r <- stats::TukeyHSD(stats::aov(y3 ~ g3))$g3
  tukey_p <- stats::ptukey(tukey_q$q,
    nmeans = nlevels(g3),
    df = stats::aov(y3 ~ g3)$df.residual,
    lower.tail = FALSE
  )
  for (i in seq_len(nrow(tukey_q))) {
    rows <- compare_appendix_stat(
      rows, paste("Tukey p from q", tukey_q$pair[i]),
      tukey_p[i], tukey_r[tukey_q$pair[i], "p adj"]
    )
  }

  gh <- games.howell(y3, g3)
  for (i in seq_len(nrow(gh))) {
    g1 <- gh$group1[i]
    g2 <- gh$group2[i]
    values1 <- y3[g3 == g1]
    values2 <- y3[g3 == g2]
    rows <- compare_appendix_stat(
      rows, paste("Games-Howell t", g1, g2),
      manual_welch_t_stat(values1, values2), gh$t[i]
    )
  }

  wx <- c(1, 2, 2, 4)
  wy <- c(2, 3, 5, 5, 6)
  rows <- compare_appendix_stat(
    rows, "Wilcoxon W", manual_wilcox_w(wx, wy),
    suppressWarnings(stats::wilcox.test(wx, wy)$statistic)
  )

  y_rank <- c(1, 2, 2, 4, 2, 3, 5, 5, 6, 1, 4, 4)
  g_rank <- factor(rep(c("A", "B", "C"), times = c(4, 5, 3)))
  rows <- compare_appendix_stat(
    rows, "Kruskal-Wallis H", manual_kruskal_h(y_rank, g_rank),
    stats::kruskal.test(y_rank ~ g_rank)$statistic
  )

  x_ord <- c(1, 2, 2, 4, 5, 6)
  y_ord <- c(1, 3, 2, 4, 4, 5)
  rows <- compare_appendix_stat(
    rows, "Kendall tau-b", manual_kendall_tau_b(x_ord, y_ord),
    stats::cor.test(x_ord, y_ord,
      method = "kendall",
      exact = FALSE
    )$estimate
  )

  x_s <- c(3, 1, 4, 2, 5, 7, 6)
  y_s <- c(1, 2, 3, 5, 4, 6, 7)
  rows <- compare_appendix_stat(
    rows, "Spearman rho", manual_spearman_rho(x_s, y_s),
    stats::cor.test(x_s, y_s, method = "spearman")$estimate
  )

  tab <- matrix(c(18, 11, 9, 14, 12, 16), nrow = 2, byrow = TRUE)
  rows <- compare_appendix_stat(
    rows, "Pearson chi-squared", manual_chisq_stat(tab),
    suppressWarnings(stats::chisq.test(tab, correct = FALSE)$statistic)
  )

  fisher_tab <- matrix(c(1, 9, 11, 3), nrow = 2, byrow = TRUE)
  rows <- compare_appendix_stat(
    rows, "Fisher observed table probability",
    manual_fisher_obs_prob(fisher_tab),
    stats::dhyper(
      fisher_tab[1, 1], sum(fisher_tab[1, ]),
      sum(fisher_tab[2, ]), sum(fisher_tab[, 1])
    )
  )
  rows <- compare_appendix_stat(
    rows, "Fisher two-sided p-value", manual_fisher_two_sided_p(fisher_tab),
    stats::fisher.test(fisher_tab)$p.value
  )

  if (length(rows) == 0L) {
    return(data.frame(
      statistic = character(),
      appendix = numeric(),
      r_value = numeric(),
      difference = numeric()
    ))
  }
  do.call(rbind, rows)
}

test_that("appendix test statistics match the documented R functions", {
  discrepancies <- appendix_stat_discrepancies()
  if (nrow(discrepancies) > 0L) {
    fail(paste(c(
      "Appendix/R statistic discrepancies:",
      capture.output(print(discrepancies, row.names = FALSE))
    ), collapse = "\n"))
  }
  expect_equal(nrow(discrepancies), 0L)
})
