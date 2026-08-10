## Dunn's test: agreement with the standard implementations, and with the
## primary source.
##
## The reference packages are Suggests-level at most, so every comparison
## against them is skipped when they are absent. The checks that do not need
## them -- Eq. (2) and (3) of Dunn (1964) evaluated by hand, structural
## properties, and the relationship to Bonferroni -- always run.

suppress_graphics <- function() {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
}

make_data <- function(seed = 42) {
  set.seed(seed)
  data.frame(
    y = c(rnorm(12), rnorm(15, 0.8), rnorm(10, 1.6), rnorm(13, 0.4)),
    g = factor(rep(c("A", "B", "C", "D"), c(12, 15, 10, 13)))
  )
}

make_tied_data <- function(seed = 11) {
  set.seed(seed)
  ## Rounding forces a large number of ties, so the Eq. (3) correction is
  ## actually exercised rather than collapsing to Eq. (2).
  data.frame(
    y = round(c(rnorm(14), rnorm(14, 1), rnorm(14, 2))),
    g = factor(rep(c("A", "B", "C"), each = 14))
  )
}

pair_key <- function(g1, g2) paste(pmin(g1, g2), pmax(g1, g2), sep = "-")

test_that("z statistic reproduces Dunn (1964) Eq. (2) computed by hand", {
  d <- make_data()
  res <- dunn.test(d$y, d$g)

  R <- rank(d$y)
  N <- length(d$y)
  n <- tapply(R, d$g, length)
  Rbar <- tapply(R, d$g, mean)
  ## no ties in continuous data, so Eq. (2) applies
  expect_equal(length(unique(d$y)), N)

  for (i in seq_len(nrow(res))) {
    g1 <- res$group1[i]
    g2 <- res$group2[i]
    sigma <- sqrt(N * (N + 1) / 12 * (1 / n[[g1]] + 1 / n[[g2]]))
    expect_equal(res$z[i], (Rbar[[g1]] - Rbar[[g2]]) / sigma, tolerance = 1e-12)
  }
})

test_that("tie correction reproduces Dunn (1964) Eq. (3) computed by hand", {
  d <- make_tied_data()
  res <- dunn.test(d$y, d$g)

  R <- rank(d$y)
  N <- length(d$y)
  n <- tapply(R, d$g, length)
  Rbar <- tapply(R, d$g, mean)
  tie <- table(d$y)
  tie <- tie[tie > 1]
  expect_gt(length(tie), 0) # the fixture really does contain ties

  sigma_sq <- N * (N + 1) / 12 - sum(tie^3 - tie) / (12 * (N - 1))
  for (i in seq_len(nrow(res))) {
    g1 <- res$group1[i]
    g2 <- res$group2[i]
    sigma <- sqrt(sigma_sq * (1 / n[[g1]] + 1 / n[[g2]]))
    expect_equal(res$z[i], (Rbar[[g1]] - Rbar[[g2]]) / sigma, tolerance = 1e-12)
  }
})

test_that("agrees with rstatix::dunn_test", {
  skip_if_not_installed("rstatix")
  for (dat in list(make_data(), make_tied_data())) {
    own <- dunn.test(dat$y, dat$g)
    ref <- as.data.frame(rstatix::dunn_test(y ~ g, data = dat,
                                            p.adjust.method = "holm"))
    m <- merge(
      transform(own, key = pair_key(group1, group2)),
      transform(ref, key = pair_key(group1, group2)),
      by = "key"
    )
    expect_equal(nrow(m), nrow(own))
    ## rstatix orders the difference the other way round, so compare |z|
    expect_equal(abs(m$z), abs(m$statistic), tolerance = 1e-8)
    expect_equal(m$p_value, m$p, tolerance = 1e-8)
    expect_equal(m$p_adj, m$p.adj, tolerance = 1e-8)
  }
})

test_that("agrees with PMCMRplus::kwAllPairsDunnTest", {
  skip_if_not_installed("PMCMRplus")
  for (dat in list(make_data(), make_tied_data())) {
    own <- dunn.test(dat$y, dat$g)
    ## PMCMRplus emits an informational message when it applies the tie
    ## correction; that is the behaviour we want, not a problem.
    ref <- suppressWarnings(
      PMCMRplus::kwAllPairsDunnTest(y ~ g, data = dat, p.adjust.method = "holm")
    )
    for (i in seq_len(nrow(own))) {
      ## PMCMRplus returns a lower-triangular matrix; the pair may appear in
      ## either orientation.
      a <- own$group1[i]
      b <- own$group2[i]
      p <- if (b %in% rownames(ref$p.value) && a %in% colnames(ref$p.value) &&
               !is.na(ref$p.value[b, a])) {
        ref$p.value[b, a]
      } else {
        ref$p.value[a, b]
      }
      expect_equal(own$p_adj[i], p, tolerance = 1e-8)
    }
  }
})

test_that("agrees with dunn.test::dunn.test once its conventions are matched", {
  skip_if_not_installed("dunn.test")
  dat <- make_data()
  own <- dunn.test(dat$y, dat$g)

  ## The dunn.test package defaults to p = P(Z >= |z|), i.e. half the two-sided
  ## p-value, unless altp = TRUE. Its Holm output is also not made monotone, so
  ## only the UNADJUSTED p-values are compared here; the adjustment itself is
  ## checked against p.adjust() below.
  ref <- utils::capture.output(
    out <- dunn.test::dunn.test(dat$y, dat$g, method = "none", altp = TRUE,
                                table = FALSE, list = FALSE, kw = FALSE,
                                interpret = FALSE)
  )
  keys <- vapply(strsplit(out$comparisons, " - ", fixed = TRUE),
                 function(p) pair_key(p[1], p[2]), character(1))
  m <- merge(
    transform(own, key = pair_key(group1, group2)),
    data.frame(key = keys, z_ref = out$Z, p_ref = out$altP),
    by = "key"
  )
  expect_equal(nrow(m), nrow(own))
  expect_equal(abs(m$z), abs(m$z_ref), tolerance = 1e-6)
  expect_equal(m$p_value, m$p_ref, tolerance = 1e-6)
})

test_that("Holm adjustment is p.adjust(), and dominates Bonferroni", {
  d <- make_data()
  res <- dunn.test(d$y, d$g)

  expect_equal(res$p_adj, p.adjust(res$p_value, "holm"), tolerance = 1e-12)

  ## Dunn (1964) p. 242 prescribes Bonferroni, alpha/(2p) two-sided. Holm is
  ## uniformly more powerful: it never gives a larger adjusted p-value.
  bonf <- p.adjust(res$p_value, "bonferroni")
  expect_true(all(res$p_adj <= bonf + 1e-12))
})

test_that("uses one global ranking, not pairwise re-ranking", {
  ## A group that is irrelevant to a given pair still changes that pair's
  ## statistic under global ranking, and cannot under pairwise ranking. This is
  ## the property that makes the test the counterpart of kruskal.test().
  set.seed(3)
  base <- data.frame(y = c(rnorm(10), rnorm(10, 1)),
                     g = factor(rep(c("A", "B"), each = 10)))
  extended <- rbind(base, data.frame(y = rnorm(10, 50), g = factor("C")))

  ab_alone <- dunn.test(base$y, base$g)
  ab_with_c <- subset(dunn.test(extended$y, extended$g),
                      group1 == "A" & group2 == "B")
  expect_false(isTRUE(all.equal(ab_alone$z[1], ab_with_c$z[1])))

  ## whereas pairwise.wilcox.test is unaffected by the extra group
  p_alone <- pairwise.wilcox.test(base$y, base$g)$p.value["B", "A"]
  p_with_c <- pairwise.wilcox.test(extended$y, extended$g)$p.value["B", "A"]
  expect_equal(p_alone, p_with_c)
})

test_that("structure, ordering and edge cases behave", {
  d <- make_data()
  res <- dunn.test(d$y, d$g)

  expect_s3_class(res, "dunn.test")
  expect_equal(nrow(res), choose(nlevels(d$g), 2))
  expect_true(all(res$p_value >= 0 & res$p_value <= 1))
  expect_true(all(res$p_adj >= res$p_value - 1e-12))
  expect_true(all(res$se > 0))

  ## conf.level moves only the significance flag, never the p-values
  strict <- dunn.test(d$y, d$g, conf.level = 0.99)
  expect_equal(res$p_adj, strict$p_adj)
  expect_true(all(strict$significant <= res$significant))

  ## NAs are dropped pairwise-complete, unused levels are removed
  d2 <- d
  d2$y[c(1, 5, 30)] <- NA
  expect_silent(dunn.test(d2$y, d2$g))
  d3 <- subset(d, g %in% c("A", "B"))
  expect_equal(nrow(dunn.test(d3$y, d3$g)), 1)

  expect_error(dunn.test(as.character(d$y), d$g), "must be numeric")
  expect_error(dunn.test(d$y, d$g[-1]), "same length")
  expect_error(dunn.test(d$y[d$g == "A"], droplevels(d$g[d$g == "A"])),
               "At least 2 groups")
})

test_that("a two-group Dunn test matches the normal-approximation Wilcoxon", {
  ## With k = 2 the global ranking is the pairwise ranking, so Dunn reduces to
  ## the normal approximation of the rank-sum test without continuity
  ## correction.
  set.seed(5)
  d <- data.frame(y = c(rnorm(20), rnorm(25, 1)),
                  g = factor(rep(c("A", "B"), c(20, 25))))
  own <- dunn.test(d$y, d$g)
  w <- wilcox.test(y ~ g, data = d, correct = FALSE, exact = FALSE)
  expect_equal(own$p_value[1], w$p.value, tolerance = 1e-8)
})
