# Compare branching of visStatistics against related automatic-test packages.
# The script searches for a reproducible example where pooled-residual
# normality is not rejected, but at least one input/group-wise normality test is.

alpha <- 0.05
out_dir <- "competitor_branching_plots"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

print_branch_table <- function(title, rows) {
  cat("\n", title, "\n", sep = "")
  print(as.data.frame(rows), row.names = FALSE)
}

required <- c("visStatistics", "automatedtests", "autotestR", "boxTest",
              "compareGroups")
available <- vapply(required, requireNamespace, logical(1), quietly = TRUE)
print(available)

simulate_candidate <- function(seed, n = 35, k = 4) {
  set.seed(seed)
  group <- factor(rep(LETTERS[seq_len(k)], each = n))
  shift <- rep(seq(0, by = 0.8, length.out = k), each = n)

  ## Same non-Gaussian error distribution in every group.
  ## The common linear model removes group means; visStatistics tests the
  ## pooled standardised residuals, whereas competitors test input vectors
  ## or groups separately.
  err <- rt(n * k, df = 8)
  y <- shift + err
  data.frame(group = group, y = y)
}

simulate_very_large_candidate <- function(seed, n = 1000, k = 4) {
  set.seed(seed)
  group <- factor(rep(LETTERS[seq_len(k)], each = n))
  shift <- rep(seq(0, by = 0.8, length.out = k), each = n)

  ## Tiny contamination: practically small, but visible to normality tests
  ## at very large n.
  err <- rnorm(n * k)
  contaminated <- runif(n * k) < 0.02
  err[contaminated] <- err[contaminated] + rnorm(sum(contaminated), 0, 4)

  data.frame(group = group, y = shift + err)
}

branch_visstat <- function(dat) {
  fit <- lm(y ~ group, data = dat)
  std_resid <- rstandard(fit)
  p_sw <- shapiro.test(std_resid)$p.value
  p_ad <- nortest::ad.test(std_resid)$p.value
  p_lev <- visStatistics::levene.test(dat$y, dat$group)$p.value
  p_bartlett <- bartlett.test(dat$y, dat$group)$p.value

  k <- nlevels(dat$group)

  if (p_sw <= alpha) {
    branch <- if (k == 2) "Wilcoxon rank-sum" else "Kruskal-Wallis"
  } else if (p_lev <= alpha) {
    branch <- if (k == 2) "Welch t-test" else "Welch ANOVA"
  } else {
    branch <- if (k == 2) "Student t-test" else "Fisher ANOVA"
  }

  list(branch = branch, p_sw_residuals = p_sw, p_ad_residuals = p_ad,
       p_levene = p_lev, p_bartlett = p_bartlett)
}

branch_groupwise <- function(dat) {
  p_group <- tapply(dat$y, dat$group, function(z) shapiro.test(z)$p.value)
  p_ad_group <- tapply(dat$y, dat$group, function(z) nortest::ad.test(z)$p.value)
  p_lev <- visStatistics::levene.test(dat$y, dat$group)$p.value
  p_bartlett <- bartlett.test(dat$y, dat$group)$p.value

  k <- nlevels(dat$group)

  if (any(p_group <= alpha)) {
    branch <- if (k == 2) "Wilcoxon rank-sum" else "Kruskal-Wallis"
  } else if (p_lev <= alpha) {
    branch <- if (k == 2) "Welch t-test" else "Welch ANOVA"
  } else {
    branch <- if (k == 2) "Student t-test" else "Fisher ANOVA"
  }

  list(branch = branch, p_sw_groups = p_group, p_ad_groups = p_ad_group,
       p_levene = p_lev, p_bartlett = p_bartlett)
}

branch_automatedtests_style <- function(dat) {
  p_y <- shapiro.test(dat$y)$p.value
  p_ad_y <- nortest::ad.test(dat$y)$p.value
  p_bartlett <- bartlett.test(dat$y, dat$group)$p.value

  k <- nlevels(dat$group)

  if (p_y <= alpha) {
    branch <- if (k == 2) "Mann-Whitney U" else "Kruskal-Wallis"
  } else if (p_bartlett <= alpha) {
    branch <- if (k == 2) "Welch t-test" else "Welch ANOVA"
  } else {
    branch <- if (k == 2) "Student t-test" else "Fisher ANOVA"
  }

  list(branch = branch, p_sw_input_y = p_y, p_ad_input_y = p_ad_y,
       p_bartlett = p_bartlett)
}

## compareGroups::compareGroups() with method = NA applies shapiro.test() to the
## whole response vector, ignoring the grouping (see compareGroups:::compare.i),
## then reports a parametric or a non-parametric test accordingly. With its
## default method = 1 the response is assumed normal and no test is performed.
branch_comparegroups_style <- function(dat) {
  p_y <- shapiro.test(dat$y)$p.value
  k <- nlevels(dat$group)
  if (p_y <= alpha) {
    branch <- if (k == 2) "Kruskal-Wallis (2 groups)" else "Kruskal-Wallis"
  } else {
    branch <- if (k == 2) "Student t-test" else "Fisher ANOVA"
  }
  list(branch = branch, p_sw_input_y = p_y,
       note = "method = NA; shapiro.test() on the ungrouped response")
}

find_automatedtests_example <- function(max_seed = 50000) {
  for (seed in seq_len(max_seed)) {
    dat <- simulate_candidate(seed)
    vis <- branch_visstat(dat)
    at <- branch_automatedtests_style(dat)

    if (vis$branch != "Kruskal-Wallis" && at$branch == "Kruskal-Wallis") {
      return(list(seed = seed, data = dat, vis = vis,
                  automatedtests_style = at))
    }
  }
  stop("No automatedtests-style example found up to seed ", max_seed,
       call. = FALSE)
}

find_groupwise_example <- function(k = 4, max_seed = 50000) {
  for (seed in seq_len(max_seed)) {
    dat <- simulate_candidate(seed, k = k)
    vis <- branch_visstat(dat)
    grp <- branch_groupwise(dat)

    rank_branch <- if (k == 2) "Wilcoxon rank-sum" else "Kruskal-Wallis"
    if (vis$branch != rank_branch && grp$branch == rank_branch) {
      return(list(seed = seed, data = dat, vis = vis, groupwise = grp))
    }
  }
  stop("No group-wise example found up to seed ", max_seed, call. = FALSE)
}

find_large_n_example <- function(n = 200, k = 4, max_seed = 50000) {
  for (seed in seq_len(max_seed)) {
    dat <- simulate_candidate(seed, n = n, k = k)
    vis <- branch_visstat(dat)
    grp <- branch_groupwise(dat)
    at <- branch_automatedtests_style(dat)

    ## visStatistics applies the residual-normality gate at every sample size;
    ## there is no size threshold that bypasses it. We require the residual-based
    ## branch to differ from at least one input/group-wise normality logic.
    rank_branch <- if (k == 2) "Wilcoxon rank-sum" else "Kruskal-Wallis"
    if (vis$branch != rank_branch &&
        (grp$branch == rank_branch || at$branch == rank_branch)) {
      return(list(seed = seed, data = dat, vis = vis, groupwise = grp,
                  automatedtests_style = at))
    }
  }
  stop("No large-n example found up to seed ", max_seed, call. = FALSE)
}

find_very_large_n_example <- function(n = 1000, k = 4, max_seed = 2000) {
  seed <- 1
  dat <- simulate_very_large_candidate(seed, n = n, k = k)
  list(seed = seed, data = dat, vis = branch_visstat(dat),
       groupwise = branch_groupwise(dat),
       automatedtests_style = branch_automatedtests_style(dat))
}

automatedtests_example <- find_automatedtests_example()
groupwise_example <- find_groupwise_example()
boxTest_example <- find_groupwise_example(k = 2)
large_n_example <- find_large_n_example()
very_large_n_example <- find_very_large_n_example(n = 1000)

cat("\n=== automatedtests-style input-vector case ===\n")
cat("\nSeed:", automatedtests_example$seed, "\n")

cat("\nvisStatistics-style branch:\n")
print(automatedtests_example$vis)
cat("\nautomatedtests-style input-vector branch:\n")
print(automatedtests_example$automatedtests_style)
cat("\ncompareGroups-style input-vector branch (method = NA):\n")
print(branch_comparegroups_style(automatedtests_example$data))

cat("\n=== group-wise Shapiro case, relevant for autotestR/boxTest logic ===\n")
cat("\nSeed:", groupwise_example$seed, "\n")
cat("\nvisStatistics-style branch:\n")
print(groupwise_example$vis)
cat("\nGroup-wise branch:\n")
print(groupwise_example$groupwise)

cat("\n=== two-group group-wise Shapiro case, relevant for boxTest ===\n")
cat("\nSeed:", boxTest_example$seed, "\n")
cat("\nvisStatistics-style branch:\n")
print(boxTest_example$vis)
cat("\nGroup-wise branch:\n")
print(boxTest_example$groupwise)

cat("\n=== large-n case, relevant for CLT bypass in visStatistics ===\n")
cat("\nSeed:", large_n_example$seed, "\n")
cat("\nGroup sizes:\n")
print(table(large_n_example$data$group))
cat("\nvisStatistics-style branch:\n")
print(large_n_example$vis)
cat("\nGroup-wise branch:\n")
print(large_n_example$groupwise)
cat("\nautomatedtests-style input-vector branch:\n")
print(large_n_example$automatedtests_style)

print_branch_table(
  "Large-n normality decisions shown as a standard group-wise workflow:",
  data.frame(
    group = names(large_n_example$groupwise$p_sw_groups),
    n = as.integer(table(large_n_example$data$group)),
    shapiro_p = unname(large_n_example$groupwise$p_sw_groups),
    ad_p = unname(large_n_example$groupwise$p_ad_groups),
    normal_by_shapiro = unname(large_n_example$groupwise$p_sw_groups) > alpha,
    normal_by_ad = unname(large_n_example$groupwise$p_ad_groups) > alpha
  )
)

print_branch_table(
  "Large-n visStatistics pooled-residual diagnostics:",
  data.frame(
    diagnostic = c("Shapiro-Wilk, pooled standardised residuals",
                   "Anderson-Darling, pooled standardised residuals",
                   "Levene-Brown-Forsythe",
                   "Bartlett"),
    p_value = c(large_n_example$vis$p_sw_residuals,
                large_n_example$vis$p_ad_residuals,
                large_n_example$vis$p_levene,
                large_n_example$vis$p_bartlett)
  )
)

cat("\n=== very-large-n case, n = 1000 per group ===\n")
cat("\nSeed:", very_large_n_example$seed, "\n")
cat("\nGroup sizes:\n")
print(table(very_large_n_example$data$group))
cat("\nvisStatistics-style branch:\n")
print(very_large_n_example$vis)
cat("\nGroup-wise branch:\n")
print(very_large_n_example$groupwise)
cat("\nautomatedtests-style input-vector branch:\n")
print(very_large_n_example$automatedtests_style)

print_branch_table(
  "Very-large-n normality decisions shown as a standard group-wise workflow:",
  data.frame(
    group = names(very_large_n_example$groupwise$p_sw_groups),
    n = as.integer(table(very_large_n_example$data$group)),
    shapiro_p = unname(very_large_n_example$groupwise$p_sw_groups),
    ad_p = unname(very_large_n_example$groupwise$p_ad_groups),
    normal_by_shapiro = unname(very_large_n_example$groupwise$p_sw_groups) > alpha,
    normal_by_ad = unname(very_large_n_example$groupwise$p_ad_groups) > alpha
  )
)

print_branch_table(
  "Very-large-n visStatistics pooled-residual diagnostics:",
  data.frame(
    diagnostic = c("Shapiro-Wilk, pooled standardised residuals",
                   "Anderson-Darling, pooled standardised residuals",
                   "Levene-Brown-Forsythe",
                   "Bartlett"),
    p_value = c(very_large_n_example$vis$p_sw_residuals,
                very_large_n_example$vis$p_ad_residuals,
                very_large_n_example$vis$p_levene,
                very_large_n_example$vis$p_bartlett)
  )
)

cat("\nRunning installed packages where available:\n")

if (available[["visStatistics"]]) {
  vs <- visStatistics::visstat(automatedtests_example$data$group,
                               automatedtests_example$data$y)
  cat("\nvisStatistics::visstat() method:\n")
  print(vs$method)
  cat("\nvisStatistics effect_size:\n")
  print(vs$effect_size)

  pdf(file.path(out_dir, "visStatistics_automatedtests_case.pdf"),
      width = 11, height = 7)
  plot(vs)
  dev.off()

  vs_large <- visStatistics::visstat(large_n_example$data$group,
                                     large_n_example$data$y)
  cat("\nvisStatistics::visstat() method for large-n case:\n")
  print(vs_large$method)
  cat("\nvisStatistics effect_size for large-n case:\n")
  print(vs_large$effect_size)

  pdf(file.path(out_dir, "visStatistics_large_n_case.pdf"),
      width = 11, height = 7)
  plot(vs_large)
  dev.off()

  vs_very_large <- visStatistics::visstat(very_large_n_example$data$group,
                                          very_large_n_example$data$y)
  cat("\nvisStatistics::visstat() method for very-large-n case:\n")
  print(vs_very_large$method)
  cat("\nvisStatistics effect_size for very-large-n case:\n")
  print(vs_very_large$effect_size)

  pdf(file.path(out_dir, "visStatistics_very_large_n_case.pdf"),
      width = 11, height = 7)
  plot(vs_very_large)
  dev.off()
}

if (available[["automatedtests"]]) {
  cat("\nautomatedtests::automatical_test() output:\n")
  at <- automatedtests::automatical_test(automatedtests_example$data$group,
                                         automatedtests_example$data$y)
  print(at)

  cat("\nautomatedtests::automatical_test() output for large-n case:\n")
  at_large <- automatedtests::automatical_test(large_n_example$data$group,
                                               large_n_example$data$y)
  print(at_large)

  cat("\nautomatedtests::automatical_test() output for very-large-n case:\n")
  at_very_large <- automatedtests::automatical_test(
    very_large_n_example$data$group,
    very_large_n_example$data$y
  )
  print(at_very_large)
}

if (available[["autotestR"]]) {
  cat("\nautotestR::pre.test() output for group-wise case:\n")
  wide <- split(groupwise_example$data$y, groupwise_example$data$group)
  wide <- as.data.frame(wide)
  print(autotestR::pre.test(wide))

  cat("\nautotestR::test.anova() output for group-wise case:\n")
  ar <- autotestR::test.anova(wide, verbose = TRUE)
  print(ar)
  if (!is.null(ar$plot)) {
    grDevices::pdf(file.path(out_dir, "autotestR_groupwise_case.pdf"),
                   width = 8, height = 6)
    print(ar$plot)
    grDevices::dev.off()
  }

  cat("\nautotestR::pre.test() output for large-n case:\n")
  large_wide <- split(large_n_example$data$y, large_n_example$data$group)
  large_wide <- as.data.frame(large_wide)
  print(autotestR::pre.test(large_wide))

  cat("\nautotestR::pre.test() output for very-large-n case:\n")
  very_large_wide <- split(very_large_n_example$data$y,
                           very_large_n_example$data$group)
  very_large_wide <- as.data.frame(very_large_wide)
  print(autotestR::pre.test(very_large_wide))
}

if (available[["compareGroups"]]) {
  cat("\ncompareGroups::compareGroups(method = NA) on the input-vector case:\n")
  cg <- compareGroups::compareGroups(group ~ y,
                                     data = automatedtests_example$data,
                                     method = NA)
  print(compareGroups::createTable(cg))
  cat("\nMethod resolved by compareGroups for the response:\n")
  print(attr(cg$y, "method"))
}

if (available[["boxTest"]]) {
  cat("\nboxTest::compare_two_groups() output for two-group case:\n")
  bx <- boxTest::compare_two_groups(boxTest_example$data, continuous = "y",
                                    group = "group")
  print(bx)
  if (!is.null(bx$plot)) {
    grDevices::pdf(file.path(out_dir, "boxTest_groupwise_AB_case.pdf"),
                   width = 8, height = 6)
    print(bx$plot)
    grDevices::dev.off()
  }
}

cat("\nPlot files written to:\n")
print(normalizePath(out_dir))
