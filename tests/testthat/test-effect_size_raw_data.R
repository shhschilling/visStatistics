test_that("effect_size() raw-data mode equals visstat()'s effect_size record", {
  # In raw-data mode effect_size(x, y) runs visstat() internally and returns
  # its $effect_size component. For each routing branch the record must match
  # the one visstat() attaches to its own result.

  check_branch <- function(xx, yy) {
    res <- suppressWarnings(visstat(xx, yy))
    direct <- suppressWarnings(effect_size(xx, yy))
    ref <- res$effect_size
    ref$selected_test <- selected_test_title(res)
    expect_equal(direct, ref)
    expect_true(is.character(direct$selected_test))
  }

  # two-sample numeric vs two-level factor (t-test / Wilcoxon branch)
  check_branch(ToothGrowth$supp, ToothGrowth$len)
  # numeric vs multi-level factor (ANOVA / Kruskal branch)
  check_branch(PlantGrowth$group, PlantGrowth$weight)
  # numeric vs numeric (regression branch)
  check_branch(trees$Girth, trees$Height)
  # factor vs factor (chi-squared / Fisher branch)
  check_branch(factor(mtcars$cyl), factor(mtcars$gear))
})

test_that("effect_size() errors when raw-data input lacks the second vector", {
  expect_error(
    effect_size(ToothGrowth$len),
    "two vectors"
  )
})

test_that("effect_size() errors when raw-data input is given a third vector", {
  expect_error(
    effect_size(ToothGrowth$supp, ToothGrowth$len, ToothGrowth$len),
    "exactly two vectors"
  )
})

test_that("effect_size() errors when raw-data vectors differ in length", {
  expect_error(
    effect_size(ToothGrowth$supp, "hans"),
    "equal length"
  )
})

test_that("effect_size() raw-data mode forwards visstat() warnings", {
  # correlation = TRUE is ignored when the data route to a non-correlation
  # test; visstat() warns and the warning must surface through effect_size().
  expect_warning(
    effect_size(PlantGrowth$group, PlantGrowth$weight, correlation = TRUE),
    "correlation = TRUE was ignored"
  )
})

test_that("effect_size() raw-data mode leaves no open graphics device", {
  before <- grDevices::dev.list()
  suppressWarnings(effect_size(ToothGrowth$supp, ToothGrowth$len))
  expect_identical(grDevices::dev.list(), before)
})
