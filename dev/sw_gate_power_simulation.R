## ---------------------------------------------------------------------------
## Supplementary note: does the Shapiro-Wilk routing gate cost power?
##
## Compares three decision strategies on the SAME simulated data:
##   parametric : always t-test (2 groups) / Fisher one-way ANOVA (>2 groups)
##   rank       : always Wilcoxon (2 groups) / Kruskal-Wallis (>2 groups)
##   gate       : Shapiro-Wilk on the model residuals decides
##                (SW rejects normality -> rank ; SW passes -> parametric)
##
## Reported per skewness level:
##   - statistical POWER (fraction of NREP runs with p < alpha under a true shift)
##   - empirical TYPE I error of the gate (same design, shift = 0)
##   - SW pass rate (fraction of runs routed to the parametric branch)
##
## Data-generating process:
##   smooth, unimodal: gamma with shape k chosen so skew = 2/sqrt(k);
##   skew = 0 uses the normal distribution.
##   A pure location shift (in SD units) is added to ONE group; raw values are
##   tested (no standardisation).
##
## NOTE ON SCOPE: results below hold for smooth unimodal distributions only.
## Heavy-tailed, discrete or multimodal shapes are NOT covered and can behave
## differently (the SW pass/reject decision may decouple from which test is
## more powerful). See discussion.
##
## Reproduces the tables in the supplementary discussion of visStatistics'
## n > 50 routing gate.
## ---------------------------------------------------------------------------

set.seed(1)

## ---- parameters -----------------------------------------------------------
NREP   <- 10000          # Monte Carlo replications per cell
ALPHA  <- 0.05           # nominal significance level
SKEWS  <- c(0, 0.3, 0.5, 0.7, seq(1, 3, by = 0.2))
SHIFT  <- 0.75           # location shift in SD units (power scenario)

## ---- helpers --------------------------------------------------------------

## random generator + SD for a target skewness (gamma; normal at skew 0)
make_rgen <- function(skew) {
  if (skew == 0) return(list(rgen = function(m) rnorm(m), sd = 1))
  k <- (2 / skew)^2                      # gamma shape: skew = 2/sqrt(k)
  list(rgen = function(m) rgamma(m, shape = k), sd = sqrt(k))
}

## one simulation cell: returns power/Type-I of the three strategies + SW pass
simulate_cell <- function(skew, n, n_groups, shift, nrep = NREP, alpha = ALPHA) {
  g    <- make_rgen(skew)
  gf   <- factor(rep(seq_len(n_groups), each = n))
  last <- levels(gf)[n_groups]

  out <- replicate(nrep, {
    y <- g$rgen(n * n_groups)
    y[gf == last] <- y[gf == last] + shift * g$sd   # shift one group

    if (n_groups == 2) {
      a <- y[gf == levels(gf)[1]]; b <- y[gf == last]
      p_par  <- t.test(a, b)$p.value                # Welch t (default)
      p_rank <- suppressWarnings(wilcox.test(a, b)$p.value)
    } else {
      p_par  <- anova(lm(y ~ gf))[["Pr(>F)"]][1]     # Fisher one-way ANOVA
      p_rank <- kruskal.test(y, gf)$p.value          # Kruskal-Wallis
    }
    sw     <- shapiro.test(rstandard(lm(y ~ gf)))$p.value
    p_gate <- if (sw < alpha) p_rank else p_par

    c(parametric = p_par  < alpha,
      rank       = p_rank < alpha,
      gate       = p_gate < alpha,
      sw_pass    = sw >= alpha)
  })
  rowMeans(out)
}

## full sweep over skewness for one design
run_sweep <- function(n, n_groups, shift = SHIFT, skews = SKEWS) {
  rows <- lapply(skews, function(s) {
    pwr <- simulate_cell(s, n, n_groups, shift = shift)   # power
    t1  <- simulate_cell(s, n, n_groups, shift = 0)       # Type I
    data.frame(skew = s,
               parametric = round(pwr["parametric"], 3),
               rank       = round(pwr["rank"], 3),
               gate       = round(pwr["gate"], 3),
               sw_pass    = round(pwr["sw_pass"], 3),
               gate_typeI = round(t1["gate"], 3),
               row.names  = NULL)
  })
  do.call(rbind, rows)
}

## ---- run the two scenarios ------------------------------------------------

cat("\n=== Two groups, n = 30 per group (t-test vs Wilcoxon) ===\n")
two_group <- run_sweep(n = 30, n_groups = 2)
print(two_group, row.names = FALSE)

cat("\n=== Four groups, n = 51 per group (Fisher ANOVA vs Kruskal-Wallis) ===\n")
cat("    (n > 50: in visstat() the gate is bypassed; here SW is kept active\n")
cat("     to show what routing by SW would do)\n")
four_group <- run_sweep(n = 51, n_groups = 4)
print(four_group, row.names = FALSE)

## ---- optional: save -------------------------------------------------------
## write.csv(two_group,  "dev/sw_gate_two_group.csv",  row.names = FALSE)
## write.csv(four_group, "dev/sw_gate_four_group.csv", row.names = FALSE)

## ---- reading the output ---------------------------------------------------
## - "gate" tracks max(parametric, rank) across the whole skew range:
##   it sides with the parametric test near normality and with the rank test
##   under skew, because the SW pass rate slides from ~95% to ~0% as skew grows.
## - The parametric column is flat in skew (the mean test gains nothing from
##   skew); the rank column climbs steeply -> the gate captures that gain.
## - gate_typeI stays near ALPHA; a mild upward drift can appear at the
##   crossover skew (where SW pass ~ 50-70%), the documented conditional-test
##   distortion (Zimmerman; Franc). It remains within Bradley's [0.025, 0.075].
## ---------------------------------------------------------------------------

cat("\nsessionInfo:\n"); print(sessionInfo())
