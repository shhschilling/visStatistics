## Skew = 1, lowest feasible cubic-Fleishman excess kurtosis.
## Convention: Y = -c + b Z + c Z^2 + d Z^3, Z ~ N(0,1).

fleishman_eqs <- function(par, g1, g2) {
  b<-par[1]; c<-par[2]; d<-par[3]
  c(b^2 + 2*c^2 + 6*b*d + 15*d^2 - 1,
    2*c*(b^2 + 24*b*d + 105*d^2 + 2) - g1,
    24*(b*d + c^2*(1+b^2+28*b*d) + d^2*(12+48*b*d+141*c^2+225*d^2)) - g2)
}
jac <- function(par,g1,g2,h=1e-7){ f0<-fleishman_eqs(par,g1,g2)
  sapply(1:3,function(j){pp<-par;pp[j]<-pp[j]+h;(fleishman_eqs(pp,g1,g2)-f0)/h}) }
newton <- function(par,g1,g2,maxit=300,tol=1e-13){
  for(i in seq_len(maxit)){ f<-fleishman_eqs(par,g1,g2)
    if(any(!is.finite(f))) return(NULL)
    if(sqrt(sum(f^2))<tol) return(par)
    s<-tryCatch(solve(jac(par,g1,g2),f),error=function(e)NULL); if(is.null(s))return(NULL)
    par<-par-s; if(any(!is.finite(par))||max(abs(par))>1e4)return(NULL) }
  if(sqrt(sum(fleishman_eqs(par,g1,g2)^2))<1e-9) par else NULL }
solve_any <- function(g1,g2){
  for(b0 in seq(0.5,1.4,0.15)) for(c0 in seq(-0.4,0.4,0.1)) for(d0 in seq(-0.2,0.25,0.05)){
    r<-newton(c(b0,c0,d0),g1,g2); if(!is.null(r) && r[1]>0) return(r)
  }
  NULL
}
moms <- function(b,c,d,n=3e6,seed=1){ set.seed(seed); z<-rnorm(n); y<- -c+b*z+c*z^2+d*z^3
  m<-mean(y); v<-mean((y-m)^2); c(skew=mean((y-m)^3)/v^1.5, exkurt=mean((y-m)^4)/v^2-3, var=v) }

g1 <- 1
## bisect the feasibility floor between universal bound (g1^2-2=-1) and a feasible point
lo <- g1^2 - 2          # -1, definitely infeasible for cubic
hi <- 3.0               # feasible
stopifnot(!is.null(solve_any(g1, hi)))
for(it in 1:40){ mid<-(lo+hi)/2; if(is.null(solve_any(g1,mid))) lo<-mid else hi<-mid }
floor_kurt <- hi
r <- solve_any(g1, floor_kurt)
v <- moms(r[1],r[2],r[3])
cat(sprintf("Skew=1 cubic-Fleishman floor: excess kurtosis = %.4f\n", floor_kurt))
cat(sprintf("  b=%+.8f  c=%+.8f  d=%+.8f\n", r[1],r[2],r[3]))
cat(sprintf("  c^2=%.5f  3bd=%.5f  -> %s\n", r[2]^2, 3*r[1]*r[3],
            if(r[2]^2<=3*r[1]*r[3]) "monotonic" else "fold"))
cat(sprintf("  MC: skew=%.4f exkurt=%.4f var=%.4f\n", v["skew"],v["exkurt"],v["var"]))

write.csv(data.frame(skew=g1, floor_exkurt=floor_kurt,
                     a=-r[2], b=r[1], c=r[2], d=r[3],
                     mono=(r[2]^2<=3*r[1]*r[3]),
                     mc_skew=v["skew"], mc_exkurt=v["exkurt"]),
          file.path("dev","fleishman_skew1_floor.csv"), row.names=FALSE)
cat("\nSaved: dev/fleishman_skew1_floor.csv\n")
