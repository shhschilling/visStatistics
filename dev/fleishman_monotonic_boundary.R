## Monotonic-Fleishman feasibility boundary.
## For each skew g1, find the minimum excess kurtosis g2 for which a MONOTONIC
## Fleishman cubic exists (boundary: c^2 = 3 b d, i.e. p'(z) has a double root).
##
## Unknowns b,c,d solve:
##   variance: b^2 + 2c^2 + 6bd + 15d^2 = 1
##   skew:     2c(b^2 + 24bd + 105d^2 + 2) = g1
##   monotone boundary: c^2 = 3 b d
## then read off excess kurtosis from the cubic.

kurt_of <- function(b,c,d) {
  24*(b*d + c^2*(1 + b^2 + 28*b*d) +
        d^2*(12 + 48*b*d + 141*c^2 + 225*d^2))
}

eqs_boundary <- function(par, g1) {
  b<-par[1]; c<-par[2]; d<-par[3]
  c(b^2 + 2*c^2 + 6*b*d + 15*d^2 - 1,
    2*c*(b^2 + 24*b*d + 105*d^2 + 2) - g1,
    c^2 - 3*b*d)
}
solve_boundary <- function(g1) {
  obj <- function(p) sum(eqs_boundary(p, g1)^2)
  best<-NULL; br<-Inf
  for (b0 in c(0.7,0.9,1.0,1.1)) for (c0 in c(0,0.1,0.2,g1/6)) for (d0 in c(0.02,0.05,0.1)) {
    s<-optim(c(b0,c0,d0), obj, method="Nelder-Mead",
             control=list(maxit=8000, reltol=1e-12))
    if (s$value<br){br<-s$value; best<-s$par}
  }
  if (br>1e-8) return(c(skew=g1, min_exkurt=NA))
  c(skew=g1, min_exkurt=kurt_of(best[1],best[2],best[3]),
    b=best[1], c=best[2], d=best[3])
}

cat("Minimum monotonic-Fleishman excess kurtosis by skew:\n")
cat(sprintf("%6s %12s %12s\n","skew","min_exkurt","univ_bound"))
for (g1 in c(0,0.2,0.4,0.6,0.8,1.0,1.2,1.5,2.0)) {
  r<-solve_boundary(g1)
  cat(sprintf("%6.1f %12.3f %12.3f\n", g1, r["min_exkurt"], g1^2-2))
}
