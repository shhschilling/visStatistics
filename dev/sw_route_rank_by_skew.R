## ---------------------------------------------------------------------------
## How often does the Shapiro-Wilk gate route to the RANK branch, by skewness?
##
## CELL = P(route to rank-based test)
##      = P( Shapiro-Wilk on the pooled model residuals rejects normality at
##            alpha = 0.05 ),
##   estimated over NREP simulated data sets.
##
## DATA-GENERATING PROCESS (per replicate):
##   two equal-sized groups, each drawn from  Gamma(shape = k, scale = 1),
##   with NO location difference between groups (routing depends on residual
##   shape, which is location-invariant). Residuals = rstandard(lm(y ~ group)),
##   so SW sees N = 2 * n_per_group residuals.
##
## GAMMA(shape = k, scale = 1) moments:
##   mean = k,  variance = k,
##   skewness          gamma1 = 2 / sqrt(k),
##   excess kurtosis   gamma2 = 6 / k = 1.5 * gamma1^2.
##
## IMPORTANT CAVEAT (must be reported with the table):
##   For the gamma family skewness and kurtosis are NOT independent:
##   gamma2 = 1.5 * gamma1^2. Varying the shape k therefore varies skewness
##   AND kurtosis jointly. This table characterises the gate's behaviour along
##   the gamma one-parameter path only; it does NOT isolate the effect of
##   skewness at fixed kurtosis. Separating the two requires a richer family
##   (e.g. Johnson SU, Pearson, or Fleishman power-method distributions).
## ---------------------------------------------------------------------------

set.seed(1)
NREP  <- 4000
ALPHA <- 0.05
SKEWS <- seq(0.1, 2.5, by = 0.2)            # population skewness gamma1
NS    <- c(10, 20, 30, 50, 75, 100, 200)     # n per group (total residuals N = 2n)

shape_from_skew <- function(skew) (2 / skew)^2          # k = (2/gamma1)^2
exkurt_from_skew <- function(skew) 1.5 * skew^2          # gamma2 = 1.5 gamma1^2

## P(route to rank) = P(SW on pooled residuals rejects at ALPHA)
p_route_rank <- function(skew, n, nrep = NREP) {
  k <- shape_from_skew(skew)
  g <- factor(rep(1:2, each = n))
  mean(replicate(nrep, {
    y <- rgamma(2 * n, shape = k, scale = 1)             # equal groups, no shift
    shapiro.test(rstandard(lm(y ~ g)))$p.value < ALPHA
  }))
}

## ---- mapping skew -> shape -> excess kurtosis (full characterisation) ------
cat("Gamma(shape = k, scale = 1):  skew = 2/sqrt(k),  excess kurtosis = 6/k = 1.5*skew^2\n")
cat(sprintf("%-8s %-12s %-16s\n", "skew", "shape k", "excess kurtosis"))
for (s in SKEWS)
  cat(sprintf("%-8.1f %-12.3f %-16.3f\n", s, shape_from_skew(s), exkurt_from_skew(s)))

## ---- table -----------------------------------------------------------------
cat("\nCELL = P(route to RANK branch) = P(Shapiro-Wilk rejects residual normality, alpha = 0.05)\n")
cat("DGP  = two equal groups ~ Gamma(shape k, scale 1), no location difference; N = 2n residuals.\n")
cat("NOTE = gamma couples skew and kurtosis (excess kurtosis = 1.5*skew^2); the table follows\n")
cat("       the gamma path and does NOT isolate skew at fixed kurtosis.\n\n")

line <- strrep("-", 18 + length(NS) * 7)
cat(line, "\n")
cat(sprintf("%-18s", "pop. skew (exk.)"))
for (n in NS) cat(sprintf("%7d", n)); cat("   n per group\n")
cat(line, "\n")
for (s in SKEWS) {
  cat(sprintf("%-4.1f (%5.2f)      ", s, exkurt_from_skew(s)))
  for (n in NS) cat(sprintf("%7.2f", p_route_rank(s, n)))
  cat("\n")
}
cat(line, "\n")

cat("\nsessionInfo:\n"); print(sessionInfo())
