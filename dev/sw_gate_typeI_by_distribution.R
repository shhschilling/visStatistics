## ---------------------------------------------------------------------------
## Type I error of test-selection strategies, BY EXPLICIT NULL HYPOTHESIS.
##
## Three strategies are scored on the SAME data, each tied to its own H0:
##   mean = Welch t-test        -> H0: equal population MEANS
##   rank = Wilcoxon rank-sum   -> H0: P(X > Y) = 0.5   (stochastic ordering)
##   gate = SW-routed procedure -> SW rejects -> rank ; SW passes -> Levene -> Welch/Student
##          (so the gate's H0 is data-dependent: means when SW passes,
##           ordering when SW rejects)
##
## A reported rejection rate is a TYPE I ERROR only for the strategy whose H0
## is TRUE in that data-generating process (DGP). Each block states which H0s
## are true. Where a strategy's H0 is FALSE, its number is POWER, not error
## (printed in the same cell -- read the block caption to know which is which).
##
## Target alpha = 0.05 ; Bradley (1978) liberal interval = [0.025, 0.075].
## No data are standardised; raw draws are tested.
## ---------------------------------------------------------------------------

set.seed(1)
NREP  <- 8000
ALPHA <- 0.05

levene_p <- function(y, g) anova(lm(abs(y - ave(y, g, FUN = median)) ~ g))[["Pr(>F)"]][1]

reject <- function(a, b) {
  y <- c(a, b); g <- factor(rep(c("A", "B"), c(length(a), length(b))))
  rs <- suppressWarnings(rstandard(lm(y ~ g))); rs <- rs[is.finite(rs)]
  sw <- tryCatch(if (length(rs) >= 3 && length(rs) <= 5000) shapiro.test(rs)$p.value else 1,
                 error = function(e) 1)
  p_mean <- t.test(a, b, var.equal = FALSE)$p.value                       # H0: equal means
  p_rank <- suppressWarnings(wilcox.test(a, b)$p.value)                   # H0: P(X>Y)=0.5
  p_gate <- if (sw < ALPHA) p_rank
            else if (levene_p(y, g) < ALPHA) p_mean
            else t.test(a, b, var.equal = TRUE)$p.value
  c(gate = p_gate < ALPHA, mean = p_mean < ALPHA, rank = p_rank < ALPHA)
}

SHAPES <- list(
  "normal      (skew 0)" = function(m) rnorm(m),
  "uniform     (skew 0)" = function(m) runif(m),
  "Laplace     (skew 0)" = function(m){u<-runif(m)-.5;-sign(u)*log(1-2*abs(u))},
  "t df=3      (skew 0)" = function(m) rt(m, 3),
  "gamma k=4   (skew 1)" = function(m) rgamma(m, 4),
  "exp         (skew 2)" = function(m) rexp(m),
  "gamma k=.25 (skew 4)" = function(m) rgamma(m, 0.25),
  "lognormal   (skew 6)" = function(m) rlnorm(m)
)

## precompute POPULATION mean and median per shape (one big pilot sample each),
## so B is built with equal population mean/median and 4x variance WITHOUT
## leaking A's per-replicate sample statistics (the earlier bug).
POP <- lapply(SHAPES, function(rg){ x <- rg(2e6); list(mean = mean(x), median = median(x)) })

## run one DGP across all shapes at n = 50 and n = 200; print gate/mean/rank
run_block <- function(draw) {
  ## Rejection rate P(reject H0) at alpha=0.05, per test, at two group sizes.
  ## Columns: the two fixed-null tests (Welch=means, Wilcox=ordering) then the
  ## SW-routed gate. Sub-headers repeat under each group size n.
  line <- strrep("-", 22 + 2 + 3*7 + 2 + 3*7)
  cat(line, "\n")
  cat(sprintf("%-22s | %-20s | %-20s\n",
              "rejection rate (alpha=.05)", "group size n = 50", "group size n = 200"))
  cat(sprintf("%-22s | %6s %6s %6s | %6s %6s %6s\n", "input shape (skew)",
              "Welch","Wilcox","gate", "Welch","Wilcox","gate"))
  cat(sprintf("%-22s | %6s %6s %6s | %6s %6s %6s\n", "  [H0 tested]:",
              "means","order","mixed", "means","order","mixed"))
  cat(line, "\n")
  for (nm in names(SHAPES)) {
    rg <- SHAPES[[nm]]; pop <- POP[[nm]]
    r50  <- rowMeans(replicate(NREP, { ab <- draw(rg, 50,  pop); reject(ab$a, ab$b) }))
    r200 <- rowMeans(replicate(NREP, { ab <- draw(rg, 200, pop); reject(ab$a, ab$b) }))
    cat(sprintf("%-22s | %6.3f %6.3f %6.3f | %6.3f %6.3f %6.3f\n",
                nm, r50["mean"],r50["rank"],r50["gate"], r200["mean"],r200["rank"],r200["gate"]))
  }
  cat(line, "\n")
}

cat("Cells = rejection rate at alpha = 0.05.  Columns name the TEST and its fixed H0:\n")
cat("   Welch   -> tests  H0: equal MEANS        (mu_A = mu_B)\n")
cat("   Wilcox  -> tests  H0: equal ORDERING     (P(X>Y) = 0.5)\n")
cat("   gate    -> SW-routed: reports Welch if SW passes, Wilcox if SW rejects (H0 = whichever it picked)\n")
cat("Bradley liberal interval [0.025, 0.075].  A cell is TYPE I only where its H0 is TRUE in the DGP (see caption).\n")

## --- DGP A: identical groups, equal n --------------------------------------
cat("\n=== DGP A: identical distributions, equal n ===\n")
cat("    TRUE H0:  equal means = YES   |   ordering P(X>Y)=0.5 = YES\n")
cat("    => ALL THREE columns are Type I error rates.\n")
run_block(function(rg, n, pop) list(a = rg(n), b = rg(n)))

## --- DGP B: equal MEANS, unequal variance (var_B = 4 var_A), equal n -------
cat("\n=== DGP B: equal MEANS, unequal variance (4x), equal n ===\n")
cat("    TRUE H0:  equal means = YES   |   ordering P(X>Y)=0.5 = NO (generally)\n")
cat("    => 'mean' column = Type I (valid target);  'rank' column = POWER vs a real\n")
cat("       ordering difference (NOT error);  'gate' inherits whichever route SW picks.\n")
run_block(function(rg, n, pop) {
  ## B = popmean + 2*(draw - popmean): same population mean, variance x4, skew preserved
  list(a = rg(n), b = pop$mean + 2 * (rg(n) - pop$mean))
})

## --- DGP C: equal MEDIANS, unequal variance, equal n -----------------------
cat("\n=== DGP C: equal MEDIANS, unequal variance (4x), equal n ===\n")
cat("    TRUE H0:  ordering P(X>Y)=0.5 ~ YES (exact for symmetric shapes) | equal means = NO for skewed\n")
cat("    => 'rank' column = Type I (valid target);  'mean' column = POWER vs a real\n")
cat("       mean difference for skewed shapes (NOT error);  'gate' inherits SW's route.\n")
run_block(function(rg, n, pop) {
  ## B = popmedian + 2*(draw - popmedian): same population median, variance x4
  list(a = rg(n), b = pop$median + 2 * (rg(n) - pop$median))
})

cat("\nReading guide:\n")
cat("- DGP A: every shape ~0.05 in all columns -> skew does not break level under a\n")
cat("  balanced common null (a-b symmetric; ranks exchangeable). No normality needed.\n")
cat("- DGP B: 'mean' stays ~0.05 (Welch valid); 'rank' is large because P(X>Y)!=0.5 is\n")
cat("  TRUE -> rank correctly detects ordering; 'gate' tracks 'rank' for skewed shapes\n")
cat("  (SW rejects) -> high *if read as a mean test*, correct *as an ordering test*.\n")
cat("- DGP C: mirror image -- 'rank' ~0.05 (valid for ordering); 'mean' large for skewed\n")
cat("  shapes because the means really differ. Each test holds its OWN null.\n")

cat("\nsessionInfo:\n"); print(sessionInfo())
