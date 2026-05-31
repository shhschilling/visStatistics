test_that("effect_size agrees with formulae in the effect-size table", {
  hedges_j <- function(nu) {
    exp(lgamma(nu / 2) - 0.5 * log(nu / 2) - lgamma((nu - 1) / 2))
  }

  grouped <- function(y, g) {
    split(y, g)[seq_len(2)]
  }

  # Student's t-test: Hedges' g_s_p
  y <- ToothGrowth$len
  g <- ToothGrowth$supp
  x <- grouped(y, g)
  n1 <- length(x[[1]])
  n2 <- length(x[[2]])
  n_total <- n1 + n2
  sp <- sqrt(((n1 - 1) * var(x[[1]]) + (n2 - 1) * var(x[[2]])) /
               (n_total - 2))
  ref <- hedges_j(n_total - 2) * (mean(x[[1]]) - mean(x[[2]])) / sp
  res <- list("t-test-statistics" = t.test(y ~ g, var.equal = TRUE))
  expect_equal(effect_size(res, x = g, y = y)$estimate, ref, tolerance = 1e-12)

  # Welch's t-test: Hedges' g_s*
  y <- mtcars$mpg
  g <- factor(mtcars$am)
  x <- grouped(y, g)
  s_star <- sqrt((var(x[[1]]) + var(x[[2]])) / 2)
  n1 <- length(x[[1]])
  n2 <- length(x[[2]])
  s1 <- var(x[[1]])
  s2 <- var(x[[2]])
  nu_star <- ((n1 - 1) * (n2 - 1) * (s1 + s2)^2) /
    ((n2 - 1) * s1^2 + (n1 - 1) * s2^2)
  ref <- hedges_j(nu_star) * (mean(x[[1]]) - mean(x[[2]])) / s_star
  res <- list("t-test-statistics" = t.test(y ~ g, var.equal = FALSE))
  expect_equal(effect_size(res, x = g, y = y)$estimate, ref, tolerance = 1e-12)

  # Wilcoxon rank-sum: signed rank-biserial r
  y <- warpbreaks$breaks
  g <- warpbreaks$wool
  x <- grouped(y, g)
  w <- unname(suppressWarnings(wilcox.test(x[[1]], x[[2]], exact = FALSE)$statistic))
  ref <- 2 * w / (length(x[[1]]) * length(x[[2]])) - 1
  res <- list(statsWilcoxon = suppressWarnings(wilcox.test(y ~ g, exact = FALSE)))
  expect_equal(effect_size(res, x = g, y = y)$estimate, ref, tolerance = 1e-12)

  # Fisher's ANOVA: omega-squared from F, df1, df2
  y <- PlantGrowth$weight
  g <- PlantGrowth$group
  fit <- aov(y ~ g)
  tab <- summary(fit)[[1]]
  f_value <- unname(tab[["F value"]][1])
  nu1 <- unname(tab[["Df"]][1])
  nu2 <- unname(tab[["Df"]][2])
  ref <- max(0, nu1 * (f_value - 1) / (nu1 * f_value + nu2 + 1))
  res <- list("summary statistics of ANOVA" = summary(fit))
  expect_equal(effect_size(res, x = g, y = y)$estimate, ref, tolerance = 1e-12)

  # Welch's ANOVA: approximate omega-squared-type F conversion
  y <- iris$Sepal.Length
  g <- iris$Species
  ow <- oneway.test(y ~ g)
  welch_anova_den_df <- function(y, g) {
    g <- factor(g)
    n_i <- tapply(y, g, length)
    s2_i <- tapply(y, g, var)
    w_i <- n_i / s2_i
    k <- length(n_i)
    tmp <- sum((1 - w_i / sum(w_i))^2 / (n_i - 1)) / (k^2 - 1)
    1 / (3 * tmp)
  }
  f_value <- unname(ow$statistic)
  expect_named(ow$parameter, c("num df", "denom df"))
  nu1 <- unname(ow$parameter[["num df"]])
  nu2 <- unname(ow$parameter[["denom df"]])
  expect_equal(nu1, length(unique(g)) - 1, tolerance = 1e-12)
  expect_equal(nu2, welch_anova_den_df(y, g), tolerance = 1e-12)
  expect_false(nu2 == round(nu2))
  ref <- max(0, nu1 * (f_value - 1) / (nu1 * f_value + nu2 + 1))
  res <- list("summary statistics of ANOVA" = ow)
  expect_equal(effect_size(res, x = g, y = y)$estimate, ref, tolerance = 1e-12)

  # Kruskal-Wallis: Kelley-adjusted eta_H^2
  y <- iris$Petal.Width
  g <- iris$Species
  kw <- kruskal.test(y ~ g)
  h <- unname(kw$statistic)
  k <- length(unique(g))
  n_total <- length(y)
  ref <- max(0, (h - k + 1) / (n_total - k))
  res <- list("Kruskal Wallis rank sum test" = kw)
  expect_equal(effect_size(res, x = g, y = y)$estimate, ref, tolerance = 1e-12)

  # Linear regression: coefficient of determination R^2
  fit <- lm(Fertility ~ Examination, data = swiss)
  y <- swiss$Fertility
  ref <- 1 - sum(residuals(fit)^2) / sum((y - mean(y))^2)
  res <- list(effect_size = list(
    name = "R-squared",
    estimate = summary(fit)$r.squared,
    effect_size_method = "Coefficient of determination"
  ))
  expect_equal(effect_size(res)$estimate, ref, tolerance = 1e-12)

  # Spearman rank correlation: rho
  ok <- complete.cases(airquality$Wind, airquality$Ozone)
  ct <- suppressWarnings(cor.test(
    airquality$Wind[ok], airquality$Ozone[ok],
    method = "spearman"
  ))
  res <- list(effect_size = list(
    name = "Spearman's rho",
    estimate = unname(ct$estimate),
    effect_size_method = "Spearman rank correlation coefficient"
  ))
  expect_equal(effect_size(res)$estimate, unname(ct$estimate), tolerance = 1e-12)

  # Kendall's tau_b
  set.seed(42)
  n <- 150
  xs <- sample(1:5, n, replace = TRUE)
  ys <- pmin(5, pmax(1, (6 - xs) + sample(-1:1, n, replace = TRUE)))
  x_levels <- c("never", "rarely", "sometimes", "often", "always")
  y_levels <- c("poor", "fair", "ok", "good", "great")
  x_ord <- ordered(x_levels[xs], levels = x_levels)
  y_ord <- ordered(y_levels[ys], levels = y_levels)
  ct <- cor.test(as.numeric(y_ord), as.numeric(x_ord),
                 method = "kendall", exact = FALSE)
  res <- list(test = ct)
  expect_equal(effect_size(res)$estimate, unname(ct$estimate), tolerance = 1e-12)

  # Pearson chi-squared R x C: Cramer's V
  tab <- margin.table(HairEyeColor, c(1, 2))
  chi <- suppressWarnings(chisq.test(tab))
  ref <- sqrt(unname(chi$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
  expect_equal(effect_size(chi)$estimate, ref, tolerance = 1e-12)

  # Pearson chi-squared 2 x 2: unsigned phi magnitude
  tab <- matrix(c(10, 5, 4, 12), nrow = 2)
  chi <- suppressWarnings(chisq.test(tab))
  ref <- sqrt(unname(chi$statistic) / sum(tab))
  expect_equal(effect_size(chi)$estimate, ref, tolerance = 1e-12)

  # Fisher's exact 2 x 2: odds ratio and confidence interval
  tab <- matrix(c(8, 2, 1, 5), nrow = 2)
  ft <- fisher.test(tab)
  got <- effect_size(ft)
  expect_equal(got$estimate, unname(ft$estimate), tolerance = 1e-12)
  expect_equal(got$conf.int, unname(ft$conf.int), tolerance = 1e-12)
})

test_that("Welch-Satterthwaite df matches oneway.test for two groups", {
  welch_satterthwaite_df <- function(y, g) {
    x <- split(y, g)
    n1 <- length(x[[1]])
    n2 <- length(x[[2]])
    s1 <- stats::var(x[[1]])
    s2 <- stats::var(x[[2]])
    ((s1 / n1 + s2 / n2)^2) /
      ((s1 / n1)^2 / (n1 - 1) + (s2 / n2)^2 / (n2 - 1))
  }

  y <- mtcars$mpg
  for (g in list(factor(mtcars$am),
                 factor(mtcars$am, levels = c("1", "0")))) {
    ow <- stats::oneway.test(y ~ g)
    df_from_equation <- welch_satterthwaite_df(y, g)
    df_from_oneway <- unname(ow$parameter[["denom df"]])

    # oneway.test() returns c("num df", "denom df"); the equation is denom df.
    expect_named(ow$parameter, c("num df", "denom df"))
    expect_equal(unname(ow$parameter[["num df"]]), 1, tolerance = 1e-12)
    expect_equal(df_from_oneway, df_from_equation, tolerance = 1e-12)
    expect_equal(df_from_oneway,
                 unname(stats::t.test(y ~ g)$parameter),
                 tolerance = 1e-12)
  }
})
