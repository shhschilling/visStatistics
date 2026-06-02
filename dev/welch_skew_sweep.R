## Welch corner: Type I error vs target skewness, across the bypass threshold.
##
## Construction: k = 5 groups with EQUAL means but unequal SD (heteroscedastic)
## and a TUNABLE common skewness. Because skewness is invariant under positive
## scaling and shifting, we build a standardised skewed base z (mean 0, sd 1,
## skewness = s) from a lognormal, then set
##        y_i = target + scale_i * z
## so every group has mean = target (=> equal means => every rejection by a
## mean-based test is a FALSE POSITIVE), sd = scale_i (=> heteroscedastic), and
## skewness = s (the same in every group; Delacre's "skew distribution whose SD
## ratio varies without changing skewness" property, achieved here by scaling).
##
## Bradley (1978) liberal robustness bound for nominal alpha = .05: [.025, .075].
##
## Usage (interactive):
##   source("dev/welch_skew_sweep.R")
##   tab <- skew_sweep()            # prints + returns the Type I table
##   show_densities(s = 4)          # eyeball the 5 group densities at skew 4
##   welch_case(c(A=55,B=65,C=75,D=85,E=95), s = 4)  # let visstat() draw

## Load from SOURCE so the routed-test column reflects the applied gate
## (visstat_core.R), not whatever version happens to be installed.
if (requireNamespace("pkgload", quietly = TRUE)) {
  suppressMessages(pkgload::load_all(".", quiet = TRUE))
} else {
  suppressMessages(library(visStatistics))
}

## Which test did visstat() route to, and its p-value, for one realised
## dataset. The name comes from the package's own selected_test_title();
## the p-value mirrors the same result keys.
test_pvalue <- function(result) {
  if (!is.null(result[["t-test-statistics"]]$p.value))
    return(result[["t-test-statistics"]]$p.value)
  if (!is.null(result[["statsWilcoxon"]]$p.value))
    return(result[["statsWilcoxon"]]$p.value)
  if (!is.null(result[["Kruskal Wallis rank sum test"]]$p.value))
    return(result[["Kruskal Wallis rank sum test"]]$p.value)
  a <- result[["summary statistics of ANOVA"]]
  if (!is.null(a)) {
    if (inherits(a, "htest")) return(a$p.value)
    s <- summary(a); return(s[[1]][["Pr(>F)"]][1])
  }
  if (!is.null(result$p.value)) return(result$p.value)
  NA_real_
}

short_test <- function(x) {
  if (grepl("Welch",   x)) return("Welch")
  if (grepl("Fisher",  x)) return("Fisher")
  if (grepl("Kruskal", x)) return("KW")
  if (grepl("Wilcox",  x)) return("Wilcox")
  if (grepl("Student|t.test|t test", x, ignore.case = TRUE)) return("t")
  x
}

route_case <- function(n, s, scale = scale_default, seed = 1) {
  set.seed(seed); zfun <- make_zfun(s)
  grp <- factor(rep(names(n), times = n))
  val <- make_val(n, zfun, scale)
  pdf(tempfile(fileext = ".pdf")); on.exit(dev.off())  # swallow the plots
  res <- suppressMessages(visstat(grp, val))
  list(test = visStatistics:::selected_test_title(res),
       p    = test_pvalue(res))
}

scale_default <- c(A = 0.3, B = 0.5, C = 0.7, D = 0.9, E = 1.1)  # per-group SD
target        <- 10

## target skewness s -> lognormal log-sd. Lognormal skewness = (w+2)sqrt(w-1),
## w = exp(sdlog^2); strictly increasing in sdlog, so uniroot is unique.
skew_to_sdlog <- function(s) {
  stopifnot(s > 0)
  uniroot(function(sdlog) {
    w <- exp(sdlog^2); (w + 2) * sqrt(w - 1) - s
  }, c(1e-4, 5))$root
}

## standardised skewed base: mean 0, sd 1, skewness s (scale/shift invariant)
make_zfun <- function(s) {
  sdlog <- skew_to_sdlog(s)
  m  <- exp(sdlog^2 / 2)
  sd <- sqrt((exp(sdlog^2) - 1) * exp(sdlog^2))
  function(n) (rlnorm(n, 0, sdlog) - m) / sd
}

make_val <- function(n, zfun, scale = scale_default) {
  unlist(lapply(names(n), function(k) target + scale[k] * zfun(n[k])))
}

welch_typeI <- function(n, s, nrep = 3000, alpha = 0.05, scale = scale_default) {
  zfun <- make_zfun(s)
  grp  <- factor(rep(names(n), times = n))
  mean(replicate(nrep,
    oneway.test(make_val(n, zfun, scale) ~ grp, var.equal = FALSE)$p.value < alpha))
}

## main sweep: skewness {2..8} x size regime -> Welch Type I
skew_sweep <- function(skews = 2:8, nrep = 3000, seed = 1) {
  sizes <- list(
    "n in (50,100)" = c(A = 55,  B = 65,  C = 75,  D = 85,  E = 95),
    "n > 100"       = c(A = 110, B = 130, C = 150, D = 170, E = 190),
    "n > 500"       = c(A = 510, B = 530, C = 550, D = 570, E = 590)
  )
  tab   <- matrix(NA_real_, nrow = length(skews), ncol = length(sizes),
                  dimnames = list(skewness = skews, regime = names(sizes)))
  route <- matrix("",        nrow = length(skews), ncol = length(sizes),
                  dimnames = list(skewness = skews, regime = names(sizes)))
  for (i in seq_along(skews))
    for (j in seq_along(sizes)) {
      set.seed(seed)
      tab[i, j] <- welch_typeI(sizes[[j]], skews[i], nrep = nrep)
      rc <- route_case(sizes[[j]], skews[i], seed = seed)
      route[i, j] <- sprintf("%-6s p=%.3f", short_test(rc$test), rc$p)
    }
  cat("\nWelch ANOVA Type I error (k = 5, equal means, heteroscedastic).\n")
  cat("Nominal alpha = .05; Bradley liberal bound [.025, .075]; nrep =", nrep, "\n\n")
  print(round(tab, 4))
  cat("\n('*' marks cells above Bradley's upper bound .075)\n")
  flagged <- tab
  flagged[] <- ifelse(tab > 0.075, paste0(format(round(tab, 3), nsmall = 3), "*"),
                                   format(round(tab, 3), nsmall = 3))
  print(noquote(flagged))
  cat("\nTest that visstat() actually routes to (one seeded dataset per cell),\n")
  cat("under the applied gate (50 for k<=4, 100 for k>4):\n\n")
  print(noquote(route))
  invisible(tab)
}

## visual inspection helpers ---------------------------------------------------

show_densities <- function(s = 4, scale = scale_default, n_each = 5000) {
  set.seed(1); zfun <- make_zfun(s)
  cols <- seq_along(scale)
  xs <- lapply(names(scale), function(k) target + scale[k] * zfun(n_each))
  dens <- lapply(xs, density)
  plot(NULL, xlim = range(sapply(dens, function(d) range(d$x))),
       ylim = c(0, max(sapply(dens, function(d) max(d$y)))),
       xlab = "value", ylab = "density",
       main = sprintf("k=5 groups, equal mean=%g, skewness=%g", target, s))
  for (i in cols) lines(dens[[i]], col = i, lwd = 2)
  abline(v = target, lty = 3)
  legend("topright", legend = sprintf("%s (sd=%.1f)", names(scale), scale),
         col = cols, lwd = 2, bty = "n")
}

## grid of the 5 group densities across skewness 2..8, written to a PDF so the
## shapes can be eyeballed next to the Type I table.
show_density_grid <- function(skews = 2:8, scale = scale_default,
                              n_each = 20000,
                              file = "dev/welch_skew_densities.pdf") {
  if (!is.null(file)) { pdf(file, width = 10, height = 7); on.exit(dev.off()) }
  op <- par(mfrow = c(2, 4), mar = c(4, 4, 3, 1)); on.exit(par(op), add = TRUE)
  cols <- seq_along(scale)
  for (s in skews) {
    set.seed(1); zfun <- make_zfun(s)
    xs   <- lapply(names(scale), function(k) target + scale[k] * zfun(n_each))
    dens <- lapply(xs, density)
    plot(NULL, xlim = c(target - 2, target + 6),
         ylim = c(0, max(sapply(dens, function(d) max(d$y)))),
         xlab = "value", ylab = "density",
         main = sprintf("skewness = %g", s))
    for (i in cols) lines(dens[[i]], col = i, lwd = 1.8)
    abline(v = target, lty = 3)
  }
  plot.new()
  legend("center", legend = sprintf("%s (sd=%.1f)", names(scale), scale),
         col = cols, lwd = 1.8, bty = "n", title = "equal mean = 10")
  if (!is.null(file)) message("wrote ", file)
}

welch_case <- function(n, s = 4, scale = scale_default) {
  set.seed(1); zfun <- make_zfun(s)
  grp <- factor(rep(names(n), times = n))
  val <- make_val(n, zfun, scale)
  res <- visstat(grp, val)   # default: draw to active device for inspection
  cat("\nskewness target:", s, "| group sizes:", paste(n, collapse = " / "), "\n")
  cat("means  :", paste(round(tapply(val, grp, mean), 2), collapse = " / "), "\n")
  cat("sds    :", paste(round(tapply(val, grp, sd),   2), collapse = " / "), "\n")
  invisible(res)
}

## Run on source: write the density grid (figures) and print the Type I table.
## Comment out if you only want the helper functions.
#show_density_grid()
#invisible(skew_sweep())
