## Interactive viewer for the bypass-threshold flip (k = 5, equal means,
## heteroscedastic, tunable skewness). Run in RStudio: source this file once,
## then call see_case(...) at the console. Each call draws visstat()'s
## assumptions plot and result plot to the ACTIVE device (your plot pane) --
## no PDFs, no fixed dimensions.
##
##   source("dev/see_gate_interactive.R")
##   see_case(s = 4, regime = "small")   # n in (50,100)  -> Kruskal
##   see_case(s = 4, regime = "large")   # n > 100         -> Welch
##   see_case(s = 8, regime = "small")   # crank the skew up

## Load the package FROM SOURCE so the applied gate (50 for k<=4, 100 for k>4)
## is what you see. Requires the working directory to be the package root.
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".")
} else {
  library(visStatistics)
}

scale_default <- c(A = 0.3, B = 0.5, C = 0.7, D = 0.9, E = 1.1)  # per-group SD
target        <- 10

## target skewness -> lognormal log-sd (lognormal skew = (w+2)sqrt(w-1))
skew_to_sdlog <- function(s) {
  stopifnot(s > 0)
  uniroot(function(sd) { w <- exp(sd^2); (w + 2) * sqrt(w - 1) - s }, c(1e-4, 5))$root
}

## standardised skewed base: mean 0, sd 1, skewness s (scale/shift invariant)
make_zfun <- function(s) {
  sdlog <- skew_to_sdlog(s)
  m  <- exp(sdlog^2 / 2)
  sd <- sqrt((exp(sdlog^2) - 1) * exp(sdlog^2))
  function(n) (rlnorm(n, 0, sdlog) - m) / sd
}

REGIMES <- list(
  small = c(A = 55,  B = 65,  C = 75,  D = 85,  E = 95),   # n in (50,100)
  large = c(A = 110, B = 130, C = 150, D = 170, E = 190)   # n > 100
)

## Draw one case interactively. Returns the visstat() object invisibly.
see_case <- function(s = 4, regime = c("small", "large"), seed = 1) {
  regime <- match.arg(regime)
  n   <- REGIMES[[regime]]
  set.seed(seed)
  zf  <- make_zfun(s)
  grp <- factor(rep(names(n), times = n))
  val <- unlist(lapply(names(n), function(g) target + scale_default[g] * zf(n[g])))

  cat(sprintf("\nskewness = %g | regime = %s | n = %s\n",
              s, regime, paste(n, collapse = "/")))
  cat("means  :", paste(round(tapply(val, grp, mean),   2), collapse = " / "), "\n")
  cat("medians:", paste(round(tapply(val, grp, median), 2), collapse = " / "), "\n")
  cat("sds    :", paste(round(tapply(val, grp, sd),     2), collapse = " / "), "\n")

  ## graphicsoutput = NULL -> draw to the active device (RStudio plot pane).
  ## visstat() emits the assumptions plot first, then the result plot; step
  ## through them with the plot-history arrows in RStudio.
  res <- visstat(grp, val)
  cat("routed to:", visStatistics:::selected_test_title(res), "\n")
  invisible(res)
}

message("Loaded. Try:  see_case(s = 4, regime = \"small\")  then  see_case(s = 4, regime = \"large\")")
