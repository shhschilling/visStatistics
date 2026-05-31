rank_biserial_from_pairs <- function(x1, x2) {
  wins <- sum(outer(x1, x2, ">"))
  losses <- sum(outer(x1, x2, "<"))
  (wins - losses) / (length(x1) * length(x2))
}

rank_biserial_from_mean_ranks <- function(x1, x2) {
  n_total <- length(x1) + length(x2)
  ranks <- rank(c(x1, x2), ties.method = "average")
  r1 <- ranks[seq_along(x1)]
  r2 <- ranks[-seq_along(x1)]
  2 * (mean(r1) - mean(r2)) / n_total
}

rank_biserial_from_wilcox_w <- function(x1, x2) {
  w <- unname(suppressWarnings(
    wilcox.test(x1, x2, exact = FALSE)$statistic
  ))
  2 * w / (length(x1) * length(x2)) - 1
}

test_that("Wilcoxon rank-biserial implementations are equivalent", {
  examples <- list(
    untied = list(
      group = factor(c(rep("a", 4), rep("b", 4))),
      response = c(8, 7, 5, 3, 6, 4, 2, 1)
    ),
    tied = list(
      group = factor(c(rep("a", 5), rep("b", 5))),
      response = c(6, 5, 5, 3, 2, 5, 4, 3, 3, 1)
    )
  )

  for (example in examples) {
    x1 <- example$response[example$group == "a"]
    x2 <- example$response[example$group == "b"]
    result <- list(statsWilcoxon = suppressWarnings(
      wilcox.test(example$response ~ example$group, exact = FALSE)
    ))

    estimate <- effect_size(result, x = example$group, y = example$response)$estimate
    from_wilcox_w <- rank_biserial_from_wilcox_w(x1, x2)
    from_pairs <- rank_biserial_from_pairs(x1, x2)
    from_mean_ranks <- rank_biserial_from_mean_ranks(x1, x2)

    expect_equal(estimate, from_wilcox_w, tolerance = 1e-12)
    expect_equal(estimate, from_pairs, tolerance = 1e-12)
    expect_equal(estimate, from_mean_ranks, tolerance = 1e-12)
  }
})
