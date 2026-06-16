## Solve the 2x2 factorial Fleishman targets under the helper convention
##   Y = -c + b Z + c Z^2 + d Z^3,  Z ~ N(0,1)
## and MC-validate. Compares against the existing preview CSV.

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

## prefer the canonical root: d>0 and smallest |c|, real, b>0
solve_canonical <- function(g1,g2){
  grid<-expand.grid(b=seq(0.4,1.5,0.1),c=seq(-0.6,0.6,0.1),d=seq(-0.3,0.3,0.05))
  roots<-list()
  for(s in seq_len(nrow(grid))){ r<-newton(as.numeric(grid[s,]),g1,g2)
    if(is.null(r))next
    if(!any(vapply(roots,function(x)max(abs(x-r))<1e-6,logical(1)))) roots[[length(roots)+1]]<-r }
  ## pick b>0, then d>0 if available, else smallest fold severity
  if(!length(roots)) return(NULL)   ## infeasible: no real cubic root
  cand<-Filter(function(r) r[1]>0, roots)
  if(!length(cand)) cand<-roots
  dpos<-Filter(function(r) r[3]>0, cand)
  pick<-if(length(dpos)) dpos[[which.min(sapply(dpos,function(r) abs(r[2])))]]
        else cand[[which.min(sapply(cand,function(r) r[2]^2 - 3*r[1]*r[3]))]]
  pick
}
## minimum cubic-Fleishman excess kurtosis at a given skew (feasibility floor)
min_feasible_kurt <- function(g1){
  for(g2 in seq(max(0,g1^2-2), 12, by=0.05)){
    if(!is.null(solve_canonical(g1,g2))) return(g2)
  }
  NA
}
moms <- function(b,c,d,n=2e6,seed=1){ set.seed(seed); z<-rnorm(n); y<- -c+b*z+c*z^2+d*z^3
  m<-mean(y); v<-mean((y-m)^2); c(skew=mean((y-m)^3)/v^1.5, exkurt=mean((y-m)^4)/v^2-3, var=v) }

targets <- data.frame(
  label = c("mild skew lower tail","mild skew higher tail",
            "high skew lower tail","high skew higher tail"),
  g1 = c(0.5, 0.5, 2.0, 2.0),
  g2 = c(1.0, 3.0, 3.0, 6.0)
)

cat(sprintf("%-22s %5s %5s | %9s %9s %9s | %5s %s\n",
            "cell","g1","g2","b","c","d","c2<=3bd","MC(skew,kurt)"))
out <- list()
for(i in seq_len(nrow(targets))){
  g1<-targets$g1[i]; g2<-targets$g2[i]
  r<-solve_canonical(g1,g2)
  if(is.null(r)){
    floor<-min_feasible_kurt(g1)
    cat(sprintf("%-22s %5.1f %5.1f | %s (min feasible ex.kurt at skew %.1f = %.2f)\n",
                targets$label[i],g1,g2,"INFEASIBLE for cubic Fleishman",g1,floor))
    out[[i]]<-data.frame(label=targets$label[i],skew=g1,exkurt=g2,
                         a=NA,b=NA,c=NA,d=NA,mono=NA,mc_skew=NA,mc_exkurt=NA)
    next
  }
  b<-r[1]; c<-r[2]; d<-r[3]
  v<-moms(b,c,d); fold<- if(c^2<=3*b*d) "mono" else "FOLD"
  cat(sprintf("%-22s %5.1f %5.1f | %+9.5f %+9.5f %+9.5f | %6s  skew=%.3f kurt=%.3f\n",
              targets$label[i],g1,g2,b,c,d,fold,v["skew"],v["exkurt"]))
  out[[i]]<-data.frame(label=targets$label[i],skew=g1,exkurt=g2,
                       a=-c,b=b,c=c,d=d,mono=(c^2<=3*b*d),
                       mc_skew=v["skew"],mc_exkurt=v["exkurt"])
}
res<-do.call(rbind,out)
write.csv(res, file.path("dev","fleishman_factorial_solved.csv"), row.names=FALSE)
cat("\nSaved: dev/fleishman_factorial_solved.csv\n")
