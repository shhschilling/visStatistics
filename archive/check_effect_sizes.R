# Verify visStatistics effect sizes against effectsize / base-R computations.
#
# For each test branch:
#   1. Run visstat() and read its $effect_size$estimate
#   2. Compute the same effect size independently via effectsize or by hand
#   3. Compare with all.equal()
#
# Usage:
#   install.packages("effectsize")  # if needed
#   source("tools/check_effect_sizes.R")
#
# Tolerance applies elementwise via all.equal(tolerance = 1e-6).

suppressPackageStartupMessages({
  library(visStatistics)
  library(effectsize)
})

compare <- function(label, visstat_val, reference_val, tol = 1e-6) {
  eq <- isTRUE(all.equal(unname(visstat_val), unname(reference_val), tolerance = tol))
  cat(sprintf("%-50s visstat = %12.6f   ref = %12.6f   %s\n",
              label, visstat_val, reference_val, if (eq) "OK" else "MISMATCH"))
  invisible(eq)
}

# ---------------------------------------------------------------------------
# 1. Student's t-test  -> Hedges' g (pooled)
# ---------------------------------------------------------------------------
res <- visstat(ToothGrowth$supp, ToothGrowth$len)
g_vis <- res$effect_size$estimate
g_ref <- effectsize::hedges_g(len ~ supp, data = ToothGrowth,
                              pooled_sd = TRUE)$Hedges_g
compare("Student t-test: Hedges' g (pooled)", g_vis, g_ref)

# ---------------------------------------------------------------------------
# 2. Welch's t-test  -> Hedges' g (unpooled, average-variance)
# ---------------------------------------------------------------------------
res <- visstat(mtcars$am, mtcars$mpg)
g_vis <- res$effect_size$estimate
# visStatistics uses sd_std = sqrt((s1^2 + s2^2)/2); reproduce manually
x1 <- mtcars$mpg[mtcars$am == 0]
x2 <- mtcars$mpg[mtcars$am == 1]
n1 <- length(x1); n2 <- length(x2)
sd_std <- sqrt((var(x1) + var(x2)) / 2)
df <- n1 + n2 - 2
J  <- exp(lgamma(df / 2) - 0.5 * log(df / 2) - lgamma((df - 1) / 2))
g_ref <- J * (mean(x1) - mean(x2)) / sd_std
compare("Welch t-test: Hedges' g (unpooled, avg-var)", g_vis, g_ref)

# ---------------------------------------------------------------------------
# 3. Wilcoxon rank-sum  -> rank-biserial correlation
# ---------------------------------------------------------------------------
res <- visstat(warpbreaks$wool, warpbreaks$breaks)
r_vis <- res$effect_size$estimate
r_ref <- effectsize::rank_biserial(breaks ~ wool, data = warpbreaks)$r_rank_biserial
# effectsize convention may flip sign relative to which group is "first"
compare("Wilcoxon: rank-biserial",       abs(r_vis), abs(r_ref))

# ---------------------------------------------------------------------------
# 4. Fisher's one-way ANOVA  -> omega-squared
# ---------------------------------------------------------------------------
res <- visstat(PlantGrowth$group, PlantGrowth$weight)
o_vis <- res$effect_size$estimate
o_ref <- effectsize::omega_squared(aov(weight ~ group, data = PlantGrowth))$Omega2
compare("Fisher ANOVA: omega-squared",   o_vis, o_ref)

# ---------------------------------------------------------------------------
# 5. Welch's heteroscedastic ANOVA  -> approximate omega-squared
# ---------------------------------------------------------------------------
res <- visstat(iris$Species, iris$Sepal.Length)
o_vis <- res$effect_size$estimate
ow    <- oneway.test(Sepal.Length ~ Species, data = iris)
F  <- unname(ow$statistic); df1 <- unname(ow$parameter[1]); df2 <- unname(ow$parameter[2])
o_ref <- max(0, df1 * (F - 1) / (df1 * F + df2 + 1))
compare("Welch ANOVA: approx omega-squared", o_vis, o_ref)

# ---------------------------------------------------------------------------
# 6. Kruskal-Wallis  -> visStatistics' "epsilon-squared"
#                       (formula = (H - k + 1) / (n - k);
#                        more commonly called adjusted eta-H^2 / Kelley's eta^2)
# ---------------------------------------------------------------------------
res <- visstat(iris$Species, iris$Petal.Width)
e_vis <- res$effect_size$estimate
kw <- kruskal.test(Petal.Width ~ Species, data = iris)
H  <- unname(kw$statistic)
k  <- length(unique(iris$Species))
n  <- nrow(iris)
e_ref_kelley  <- max(0, (H - k + 1) / (n - k))
e_ref_tomczak <- H / (n - 1)
compare("Kruskal-Wallis: Kelley adj. eta^2 (vs visstat)", e_vis, e_ref_kelley)
cat(sprintf("%-50s             tomczak eps^2 = %12.6f (different definition)\n",
            "Kruskal-Wallis: Tomczak epsilon^2", e_ref_tomczak))

# ---------------------------------------------------------------------------
# 7. Linear regression  -> R^2
# ---------------------------------------------------------------------------
res <- visstat(swiss$Examination, swiss$Fertility)
r2_vis <- res$effect_size$estimate
r2_ref <- summary(lm(Fertility ~ Examination, data = swiss))$r.squared
compare("Linear regression: R^2", r2_vis, r2_ref)

# ---------------------------------------------------------------------------
# 8. Spearman rank correlation  -> rho
# ---------------------------------------------------------------------------
res <- visstat(airquality$Wind, airquality$Ozone, correlation = TRUE)
rho_vis <- res$effect_size$estimate
rho_ref <- suppressWarnings(
  cor.test(airquality$Wind, airquality$Ozone, method = "spearman")$estimate)
compare("Spearman: rho", rho_vis, rho_ref)

# ---------------------------------------------------------------------------
# 9. Kendall's tau_b
# ---------------------------------------------------------------------------
set.seed(42)
n  <- 150
xs <- sample(1:5, n, replace = TRUE)
ys <- pmin(5, pmax(1, (6 - xs) + sample(-1:1, n, replace = TRUE)))
likert_alc  <- c("never", "rarely", "sometimes", "often", "always")
likert_perf <- c("poor",  "fair",   "ok",        "good",  "great")
alcohol     <- ordered(likert_alc[xs],  levels = likert_alc)
performance <- ordered(likert_perf[ys], levels = likert_perf)
res <- visstat(performance, alcohol, correlation = TRUE)
tau_vis <- res$effect_size$estimate
tau_ref <- cor.test(as.numeric(performance), as.numeric(alcohol),
                    method = "kendall", exact = FALSE)$estimate
compare("Kendall: tau_b", tau_vis, tau_ref)

# ---------------------------------------------------------------------------
# 10. Pearson chi^2  R x C  -> Cramer's V (HairEyeColor 4x4)
# ---------------------------------------------------------------------------
hair_eye_df <- counts_to_cases(as.data.frame(HairEyeColor))
res <- visstat(hair_eye_df$Eye, hair_eye_df$Hair)
v_vis <- res$effect_size$estimate
v_ref <- effectsize::cramers_v(table(hair_eye_df$Eye, hair_eye_df$Hair),
                               adjust = FALSE)$Cramers_v
compare("Pearson chi^2 RxC: Cramer's V", v_vis, v_ref)

# ---------------------------------------------------------------------------
# 11. Pearson chi^2  2 x 2 (with Yates)  -> phi
# ---------------------------------------------------------------------------
sub <- HairEyeColor[1:2, 1:2, ]
sub_df <- counts_to_cases(as.data.frame(sub))
res <- visstat(sub_df$Eye, sub_df$Hair)
phi_vis <- res$effect_size$estimate
chi  <- chisq.test(table(sub_df$Eye, sub_df$Hair))$statistic
N    <- sum(table(sub_df$Eye, sub_df$Hair))
phi_ref <- sqrt(unname(chi) / N)
compare("Pearson chi^2 2x2: phi", phi_vis, phi_ref)

# ---------------------------------------------------------------------------
# 12. Fisher's exact 2x2  -> odds ratio (+ CI)
# ---------------------------------------------------------------------------
male <- HairEyeColor[, , 1]
bbhg <- male[1:2, 3:4]
bbhg_df <- counts_to_cases(as.data.frame(bbhg))
res <- visstat(bbhg_df$Eye, bbhg_df$Hair)
or_vis <- res$effect_size$estimate
ft <- fisher.test(table(bbhg_df$Eye, bbhg_df$Hair))
or_ref <- unname(ft$estimate)
compare("Fisher exact 2x2: odds ratio", or_vis, or_ref)
ci_vis <- res$effect_size$conf.int
ci_ref <- ft$conf.int
cat(sprintf("%-50s  CI visstat = [%.4f, %.4f]  ref = [%.4f, %.4f]   %s\n",
            "Fisher exact 2x2: OR confidence interval",
            ci_vis[1], ci_vis[2], ci_ref[1], ci_ref[2],
            if (isTRUE(all.equal(ci_vis, ci_ref, tolerance = 1e-6))) "OK" else "MISMATCH"))
