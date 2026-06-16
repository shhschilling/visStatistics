## Correct Fleishman (1978) coefficient solver with Monte Carlo validation.
##
## Y = -c + b Z + c Z^2 + d Z^3,  Z ~ N(0,1)
##
## Fleishman system (Fleishman 1978; Vale & Maurelli 1983):
##   f1: b^2 + 2c^2 + 6bd + 15d^2 - 1               = 0   (variance = 1)
##   f2: 2c(b^2 + 24bd + 105d^2 + 2) - g1           = 0   (skewness)
##   f3: 24[bd + c^2(1+b^2+28bd)
##          + d^2(12+48bd+141c^2+225d^2)] - g2      = 0   (excess kurtosis)

fleishman_eqs <- function(par, g1, g2) {
  b <- par[1]; c <- par[2]; d <- par[3]
  f1 <- b^2 + 2*c^2 + 6*b*d + 15*d^2 - 1
  f2 <- 2*c*(b^2 + 24*b*d + 105*d^2 + 2) - g1
  f3 <- 24*(b*d + c^2*(1 + b^2 + 28*b*d) +
              d^2*(12 + 48*b*d + 141*c^2 + 225*d^2)) - g2
  c(f1, f2, f3)
}

## Numerical Jacobian
jac <- function(par, g1, g2, h = 1e-6) {
  n <- length(par)
  J <- matrix(0, n, n)
  f0 <- fleishman_eqs(par, g1, g2)
  for (j in seq_len(n)) {
    pp <- par; pp[j] <- pp[j] + h
    J[, j] <- (fleishman_eqs(pp, g1, g2) - f0) / h
  }
  J
}

## Newton-Raphson with multiple starts
solve_fleishman <- function(g1, g2, tol = 1e-12, maxit = 200) {
  if (g1 == 0 && g2 == 0) {
    return(list(b = 1, c = 0, d = 0, converged = TRUE, resid = 0))
  }
  starts <- expand.grid(
    b = c(0.8, 0.9, 1.0, 1.1),
    c = c(-0.3, -0.1, g1/6, 0.1, 0.3),
    d = c(-0.1, 0, 0.05, 0.1, 0.2)
  )
  best <- NULL; best_resid <- Inf
  for (s in seq_len(nrow(starts))) {
    par <- as.numeric(starts[s, ])
    ok <- TRUE
    for (it in seq_len(maxit)) {
      f <- fleishman_eqs(par, g1, g2)
      if (any(!is.finite(f))) { ok <- FALSE; break }
      if (sqrt(sum(f^2)) < tol) break
      J <- jac(par, g1, g2)
      step <- tryCatch(solve(J, f), error = function(e) NULL)
      if (is.null(step)) { ok <- FALSE; break }
      par <- par - step
      if (any(!is.finite(par)) || max(abs(par)) > 1e3) { ok <- FALSE; break }
    }
    if (!ok) next
    r <- sqrt(sum(fleishman_eqs(par, g1, g2)^2))
    if (r < best_resid) { best_resid <- r; best <- par }
  }
  if (is.null(best)) return(list(b=NA, c=NA, d=NA, converged=FALSE, resid=Inf))
  list(b = best[1], c = best[2], d = best[3],
       converged = best_resid < 1e-8, resid = best_resid)
}

## Monte Carlo validation: draw Y, check moments
validate_mc <- function(b, c, d, n = 2e6, seed = 1) {
  set.seed(seed)
  z <- rnorm(n)
  y <- -c + b*z + c*z^2 + d*z^3
  m  <- mean(y); v <- mean((y-m)^2)
  sk <- mean((y-m)^3) / v^(3/2)
  ku <- mean((y-m)^4) / v^2 - 3
  c(mean = m, var = v, skew = sk, exkurt = ku)
}

## Universal lower bound on excess kurtosis: g2 >= g1^2 - 2
feasible_bound <- function(g1) g1^2 - 2

## ---- Target cases ----------------------------------------------------------
targets <- data.frame(
  case   = c("normal", "mild_skew_low_kurt", "high_skew",
             "no_skew_high_kurt", "moderate"),
  g1     = c(0, 0.4, 2.0, 0.0, 1.0),
  g2     = c(0, 0.4, 6.0, 6.0, 1.0)
)

cat("Universal bound check (excess kurtosis must be >= skew^2 - 2):\n")
for (i in seq_len(nrow(targets))) {
  g1 <- targets$g1[i]; g2 <- targets$g2[i]
  lb <- feasible_bound(g1)
  cat(sprintf("  %-20s skew=%.1f exkurt=%.1f  bound=%.2f  %s\n",
              targets$case[i], g1, g2, lb,
              if (g2 >= lb) "OK" else "INFEASIBLE"))
}
cat("\n")

results <- list()
for (i in seq_len(nrow(targets))) {
  g1 <- targets$g1[i]; g2 <- targets$g2[i]
  sol <- solve_fleishman(g1, g2)
  val <- if (sol$converged) validate_mc(sol$b, sol$c, sol$d) else c(NA,NA,NA,NA)
  results[[i]] <- data.frame(
    case = targets$case[i],
    target_skew = g1, target_exkurt = g2,
    b = sol$b, c = sol$c, d = sol$d,
    converged = sol$converged, resid = sol$resid,
    mc_skew = val["skew"], mc_exkurt = val["exkurt"], mc_var = val["var"],
    row.names = NULL
  )
  cat(sprintf("%-20s b=%+.8f c=%+.8f d=%+.8f | resid=%.2e | MC skew=%.4f exkurt=%.4f var=%.4f\n",
              targets$case[i], sol$b, sol$c, sol$d, sol$resid,
              val["skew"], val["exkurt"], val["var"]))
}

out <- do.call(rbind, results)
write.csv(out, file.path("dev", "fleishman_5panel_coefficients.csv"), row.names = FALSE)
cat("\nSaved: dev/fleishman_5panel_coefficients.csv\n")
