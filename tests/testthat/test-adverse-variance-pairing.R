library(testthat)

# Helper function to suppress graphics during tests to avoid opening windows
suppress_graphics <- function(code) {
  pdf(NULL)
  on.exit(if (dev.cur() > 1) dev.off())
  force(code)
}

# Data with prescribed group sizes and group standard deviations, means all 0.
make_pairing_data <- function(n, sd, seed = 1) {
  set.seed(seed)
  data.frame(
    g = factor(rep(LETTERS[seq_along(n)], times = n)),
    y = unlist(mapply(function(ni, si) rnorm(ni, 0, si), n, sd, SIMPLIFY = FALSE))
  )
}

detect <- visStatistics:::detect_adverse_variance_pairing
warn <- visStatistics:::warn_adverse_variance_pairing
adverse_warning <- "smallest groups"

test_that("the adverse size/variance pairing is detected", {
  dat <- make_pairing_data(c(5, 8, 12, 15), c(2.2, 1.7, 1.3, 1.0))
  res <- detect(dat$y, dat$g)
  expect_true(res$adverse)
  expect_gte(res$sd_ratio, 1.5)
  expect_equal(as.integer(res$n), c(5L, 8L, 12L, 15L))
})

test_that("benign configurations are not detected", {
  benign <- list(
    # larger groups carry the larger standard deviations
    list(n = c(5, 8, 12, 15), sd = c(1.0, 1.3, 1.7, 2.2)),
    list(n = c(50, 80, 120, 150), sd = c(1.0, 1.3, 1.7, 2.2)),
    # balanced, so no size/variance pairing exists
    list(n = rep(20, 4), sd = c(1.0, 1.3, 1.7, 2.2)),
    # unbalanced but homoscedastic
    list(n = c(5, 8, 12, 15), sd = rep(1, 4)),
    list(n = rep(20, 4), sd = rep(1, 4))
  )
  for (case in benign) {
    dat <- make_pairing_data(case$n, case$sd)
    expect_false(detect(dat$y, dat$g)$adverse)
  }
})

test_that("two groups are covered", {
  dat <- make_pairing_data(c(8, 30), c(2.5, 1.0))
  expect_true(detect(dat$y, dat$g)$adverse)
})

test_that("only the vulnerable routes are warned about", {
  pairing <- list(adverse = TRUE, n = c(5L, 8L, 12L, 15L), sd_ratio = 2.7)
  expect_warning(warn(pairing, "fisher"), adverse_warning)
  expect_warning(warn(pairing, "rank"), adverse_warning)
  expect_no_warning(warn(list(adverse = FALSE), "fisher"))
  expect_no_warning(warn(list(adverse = FALSE), "rank"))
})

test_that("the equal-variance route on adverse data warns end to end", {
  dat <- make_pairing_data(c(5, 8, 12, 15), c(2.2, 1.7, 1.3, 1.0))
  expect_warning(
    suppress_graphics(visstat(dat$g, dat$y)),
    adverse_warning
  )
})

test_that("a selected Welch route on adverse data does not warn", {
  # mtcars has 13 manual cars with sd(mpg) = 6.17 against 19 automatic with
  # sd(mpg) = 3.83: the pairing is adverse, but Levene detects the
  # heteroscedasticity and Welch's t-test is selected, which is not affected.
  dat <- data.frame(g = as.factor(mtcars$am), y = mtcars$mpg)
  expect_true(detect(dat$y, dat$g)$adverse)
  w <- NULL
  withCallingHandlers(
    suppress_graphics(visstat(dat$g, dat$y)),
    warning = function(cond) {
      w <<- c(w, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl(adverse_warning, w)))
})

test_that("explicit group_test overrides never warn about the pairing", {
  dat <- make_pairing_data(c(5, 8, 12, 15), c(2.2, 1.7, 1.3, 1.0))
  for (choice in c("welch", "rank")) {
    w <- NULL
    withCallingHandlers(
      suppress_graphics(visstat(dat$g, dat$y, group_test = choice)),
      warning = function(cond) {
        w <<- c(w, conditionMessage(cond))
        invokeRestart("muffleWarning")
      }
    )
    expect_false(any(grepl(adverse_warning, w)))
  }
})
