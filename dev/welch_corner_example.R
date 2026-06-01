## Welch-corner example for visStatistics
##
## Construction: k = 5 lognormal groups with EQUAL population means but
## unequal variance + skew (and therefore DIFFERENT medians). For a
## lognormal, mean = exp(mu + sigma^2/2), so fixing mu_i = M - sigma_i^2/2
## holds every group mean at exp(M) while the spread/skew rises with sigma_i.
## Side effect: medians = exp(mu_i) differ across groups.
##
## Why this construction: under equal means, every rejection by a mean-based
## test (Welch's ANOVA) is a FALSE POSITIVE. This lets us ask honestly whether
## raising the n_i bypass threshold from >50 to >100 controls Welch's Type I
## error -- it cannot be faked by a built-in mean difference.
##
## Source interactively (RStudio): welch_case() lets visstat() draw to the
## active device, as it does by default.

suppressMessages(library(visStatistics))

sdlog <- c(A = 0.3, B = 0.5, C = 0.7, D = 0.9, E = 1.1)  # rising spread/skew
M     <- 1                                               # every group mean = exp(M)
meanlog <- M - sdlog^2 / 2                               # equalise means

make_data <- function(n) {
  grp <- factor(rep(names(n), times = n))
  val <- unlist(lapply(names(n),
    function(k) rlnorm(n[k], meanlog[k], sdlog[k])))
  list(grp = grp, val = val)
}

# pull the test visstat() actually ran (method + p) out of its result object,
# whatever the branch-specific slot name happens to be
visstat_test <- function(res) {
  for (el in res)
    if (is.list(el) && !is.null(el$method) && !is.null(el$p.value))
      return(list(method = trimws(el$method), p = el$p.value))
  list(method = "(test not found in result)", p = NA_real_)
}

welch_case <- function(n, label) {
  set.seed(1)
  d <- make_data(n)
  res <- visstat(d$grp, d$val)        # default behaviour: plots go to the screen

  cat("\n===========", label, "===========\n")
  cat("group sizes:", paste(n, collapse = " / "), "\n")
  cat("means      :", paste(round(tapply(d$val, d$grp, mean),   2), collapse = " / "), "\n")
  cat("medians    :", paste(round(tapply(d$val, d$grp, median), 2), collapse = " / "), "\n")
  cat("SDs        :", paste(round(tapply(d$val, d$grp, sd),     2), collapse = " / "), "\n")

  # contrast the test visstat() chose with Kruskal-Wallis, side by side
  vt <- visstat_test(res)
  kw <- kruskal.test(d$val ~ d$grp)
  cat("\n  --------------------------- result contrast ---------------------------\n")
  cat(sprintf("  visstat()  %-52s p = %.4g\n", vt$method,    vt$p))
  cat(sprintf("  Kruskal    %-52s p = %.4g\n", kw$method,    kw$p.value))
  cat("  -----------------------------------------------------------------------\n")
  invisible(res)
}

## Monte Carlo Type I error: equal means => every rejection is a false positive.
## Bradley's liberal robustness bound for nominal alpha = .05 is [.025, .075].
welch_typeI <- function(n, nrep = 5000, alpha = 0.05) {
  grp <- factor(rep(names(n), times = n))
  mean(replicate(nrep, {
    val <- unlist(lapply(names(n),
      function(k) rlnorm(n[k], meanlog[k], sdlog[k])))
    oneway.test(val ~ grp, var.equal = FALSE)$p.value < alpha
  }))
}

# --- single-draw display (open the visstat panels) ---
welch_case(c(A=55,  B=65,  C=75,  D=85,  E=95),  "n in (50,100)")
welch_case(c(A=110, B=130, C=150, D=170, E=190), "n > 100")
welch_case(c(A=510, B=530, C=550, D=570, E=590), "n > 500")

# --- Type I error across the threshold (uncomment to run; ~seconds) ---
set.seed(1)
cat("\nWelch Type I error (nominal .05, Bradley OK = [.025,.075]):\n")
cat("  n in (50,100):", round(welch_typeI(c(A=55, B=65, C=75, D=85, E=95)),    4), "\n")
cat("  n > 100      :", round(welch_typeI(c(A=110,B=130,C=150,D=170,E=190)),   4), "\n")
cat("  n > 500      :", round(welch_typeI(c(A=510,B=530,C=550,D=570,E=590)),   4), "\n")
