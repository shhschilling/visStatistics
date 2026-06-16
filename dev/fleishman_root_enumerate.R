## Enumerate ALL roots of the Fleishman system for a target (skew, exkurt)
## and report which are monotonic (valid power transform: c^2 <= 3 b d).

fleishman_eqs <- function(par, g1, g2) {
  b <- par[1]; c <- par[2]; d <- par[3]
  f1 <- b^2 + 2*c^2 + 6*b*d + 15*d^2 - 1
  f2 <- 2*c*(b^2 + 24*b*d + 105*d^2 + 2) - g1
  f3 <- 24*(b*d + c^2*(1 + b^2 + 28*b*d) +
              d^2*(12 + 48*b*d + 141*c^2 + 225*d^2)) - g2
  c(f1, f2, f3)
}
jac <- function(par, g1, g2, h = 1e-7) {
  f0 <- fleishman_eqs(par, g1, g2)
  sapply(1:3, function(j){ pp<-par; pp[j]<-pp[j]+h; (fleishman_eqs(pp,g1,g2)-f0)/h })
}
newton <- function(par, g1, g2, maxit = 200, tol = 1e-13) {
  for (it in seq_len(maxit)) {
    f <- fleishman_eqs(par, g1, g2)
    if (any(!is.finite(f))) return(NULL)
    if (sqrt(sum(f^2)) < tol) return(par)
    step <- tryCatch(solve(jac(par,g1,g2), f), error=function(e) NULL)
    if (is.null(step)) return(NULL)
    par <- par - step
    if (any(!is.finite(par)) || max(abs(par)) > 1e4) return(NULL)
  }
  if (sqrt(sum(fleishman_eqs(par,g1,g2)^2)) < 1e-9) par else NULL
}

enumerate <- function(g1, g2) {
  grid <- expand.grid(b = seq(0.3, 1.6, 0.1),
                      c = seq(-0.8, 0.8, 0.1),
                      d = seq(-0.4, 0.4, 0.05))
  found <- list()
  for (s in seq_len(nrow(grid))) {
    r <- newton(as.numeric(grid[s,]), g1, g2)
    if (is.null(r)) next
    dup <- FALSE
    for (f in found) if (max(abs(f - r)) < 1e-6) { dup <- TRUE; break }
    if (!dup) found[[length(found)+1]] <- r
  }
  found
}

mono_ok <- function(b, c, d) {
  ## monotone increasing for all z  <=>  p'(z)=b+2cz+3dz^2 has no real sign change
  ## with d>0: need discriminant (2c)^2 - 4*(3d)*b <= 0  ->  c^2 <= 3 b d
  d > 0 && c^2 <= 3*b*d
}

validate_mc <- function(b,c,d,n=2e6,seed=1){
  set.seed(seed); z<-rnorm(n); y<- -c+b*z+c*z^2+d*z^3
  m<-mean(y); v<-mean((y-m)^2)
  c(skew=mean((y-m)^3)/v^1.5, exkurt=mean((y-m)^4)/v^2-3, var=v)
}

for (tgt in list(c(1,1), c(0.4,0.4), c(2,6), c(0,6))) {
  g1<-tgt[1]; g2<-tgt[2]
  cat(sprintf("\n=== target skew=%.1f exkurt=%.1f ===\n", g1, g2))
  roots <- enumerate(g1, g2)
  for (r in roots) {
    b<-r[1]; c<-r[2]; d<-r[3]
    ok <- mono_ok(b,c,d)
    v <- validate_mc(b,c,d)
    cat(sprintf("  b=%+.6f c=%+.6f d=%+.6f  c^2=%.4f 3bd=%+.4f  %s | MC skew=%.3f exkurt=%.3f\n",
                b,c,d, c^2, 3*b*d,
                if(ok) "MONOTONIC" else "fold", v["skew"], v["exkurt"]))
  }
}
