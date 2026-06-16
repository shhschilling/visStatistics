## Complete cubic-Fleishman design space + density previews in one PNG (fast).
## Convention: Y = -c + b Z + c Z^2 + d Z^3, Z ~ N(0,1).

fl_eqs <- function(par,g1,g2){ b<-par[1];c<-par[2];d<-par[3]
  c(b^2+2*c^2+6*b*d+15*d^2-1,
    2*c*(b^2+24*b*d+105*d^2+2)-g1,
    24*(b*d+c^2*(1+b^2+28*b*d)+d^2*(12+48*b*d+141*c^2+225*d^2))-g2) }
jac <- function(par,f,h=1e-7){ f0<-f(par)
  sapply(seq_along(par),function(j){pp<-par;pp[j]<-pp[j]+h;(f(pp)-f0)/h}) }
newton_f <- function(par,f,maxit=120,tol=1e-12){
  for(i in seq_len(maxit)){ fv<-f(par); if(any(!is.finite(fv)))return(NULL)
    if(sqrt(sum(fv^2))<tol)return(par)
    s<-tryCatch(solve(jac(par,f),fv),error=function(e)NULL); if(is.null(s))return(NULL)
    par<-par-s; if(any(!is.finite(par))||max(abs(par))>1e4)return(NULL) }
  if(sqrt(sum(f(par)^2))<1e-9)par else NULL }

## any real root (fold allowed) - coarse grid, early return
solve_any <- function(g1,g2){
  f<-function(p)fl_eqs(p,g1,g2)
  for(b0 in seq(0.5,1.3,0.2))for(c0 in seq(-0.4,0.4,0.2))for(d0 in seq(-0.2,0.25,0.1)){
    r<-newton_f(c(b0,c0,d0),f); if(!is.null(r)&&r[1]>0)return(r)}; NULL }

## monotonic root preferred (d>0, smallest |c|) - coarse, collect
solve_mono <- function(g1,g2){
  if(g1==0&&g2==0)return(c(1,0,0))
  f<-function(p)fl_eqs(p,g1,g2); found<-list()
  for(b0 in seq(0.5,1.3,0.2))for(c0 in seq(-0.4,0.4,0.2))for(d0 in seq(-0.2,0.3,0.1)){
    r<-newton_f(c(b0,c0,d0),f); if(is.null(r)||r[1]<=0)next
    if(!any(vapply(found,function(x)max(abs(x-r))<1e-6,logical(1))))found[[length(found)+1]]<-r }
  mono<-Filter(function(r) r[2]^2<=3*r[1]*r[3],found)
  if(length(mono)) mono[[which.min(sapply(mono,function(r)abs(r[2])))]] else NULL }

## monotonic floor directly: solve {var=1, skew=g1, c^2=3bd}
kurt_of <- function(b,c,d) 24*(b*d+c^2*(1+b^2+28*b*d)+d^2*(12+48*b*d+141*c^2+225*d^2))
mono_floor_at <- function(g1){
  if(g1==0)return(0)
  f<-function(p){b<-p[1];c<-p[2];d<-p[3]
    c(b^2+2*c^2+6*b*d+15*d^2-1, 2*c*(b^2+24*b*d+105*d^2+2)-g1, c^2-3*b*d)}
  for(b0 in c(0.8,1.0,1.2))for(c0 in c(0.05,g1/6,0.2))for(d0 in c(0.03,0.08,0.15)){
    r<-newton_f(c(b0,c0,d0),f); if(!is.null(r)&&r[1]>0&&r[3]>0)return(kurt_of(r[1],r[2],r[3]))}
  NA }

skews <- seq(0,2,0.2)
feas_floor <- mono_floor <- numeric(length(skews))
for(i in seq_along(skews)){ g1<-skews[i]
  lo<-g1^2-2; hi<-max(7,1.6*g1^2+1)
  for(it in 1:22){m<-(lo+hi)/2; if(is.null(solve_any(g1,m)))lo<-m else hi<-m}
  feas_floor[i]<-hi
  mf<-mono_floor_at(g1); mono_floor[i]<-if(is.na(mf)) hi else mf }

## candidate panels (hardcoded, all MC-validated)
cand <- data.frame(
  label=c("Normal","mild low (0.5,1)","mild high (0.5,3)",
          "skew1 floor (1,0.42)","skew1 clean (1,1.6)","heavy tails (0,6)","high skew (2,6)"),
  g1=c(0,0.5,0.5,1,1,0,2), g2=c(0,1,3,0.42,1.6,6,6),
  b=c(1, 0.92410, 0.79338, 1.14881, 0.94243, 0.66268, 0.82632),
  c=c(0, 0.07312, 0.05860, 0.29364, 0.15938, 0.00000, 0.31375),
  d=c(0, 0.02298, 0.06364,-0.08842, 0.01050, 0.10189, 0.02271))
cand$mono<-cand$c^2<=3*cand$b*cand$d

dens <- function(y,b,c,d){ if(b==1&&c==0&&d==0)return(dnorm(y))
  vapply(y,function(yy){r<-polyroot(c(-c-yy,b,c,d));re<-Re(r)[abs(Im(r))<1e-8]
    if(!length(re))return(0);dp<-b+2*c*re+3*d*re^2;v<-dnorm(re)/abs(dp);v<-v[is.finite(v)]
    if(!length(v))0 else sum(v)},numeric(1)) }

png(file.path("dev","fleishman_design_space.png"),width=1700,height=900,res=120)
layout(matrix(c(1,1,1,1, 2,3,4,5, 6,7,8,8),nrow=3,byrow=TRUE),heights=c(1.25,1,1))
par(mar=c(4.5,4.5,3,1))
plot(NA,xlim=c(0,2),ylim=c(-2,8),xlab="skewness",ylab="excess kurtosis",
     main="Cubic-Fleishman design space")
ub<-skews^2-2
polygon(c(skews,rev(skews)),c(ub,rev(feas_floor)),col="#fde0dd",border=NA)
polygon(c(skews,rev(skews)),c(feas_floor,rev(mono_floor)),col="#fff7bc",border=NA)
polygon(c(skews,rev(skews)),c(mono_floor,rep(8,length(skews))),col="#e5f5e0",border=NA)
lines(skews,ub,lwd=2,lty=3,col="grey40"); lines(skews,feas_floor,lwd=2,col="#d95f0e")
lines(skews,mono_floor,lwd=2,col="#238b45"); lines(skews,1.5*skews^2,lwd=1.5,lty=2,col="#3182bd")
pts<-cand[cand$label!="Normal",]
points(pts$g1,pts$g2,pch=21,bg=ifelse(pts$mono,"#238b45","#d95f0e"),cex=1.6)
text(pts$g1,pts$g2,labels=pts$label,pos=4,cex=0.7,xpd=NA)
legend("topleft",bty="n",cex=0.72,
  legend=c("universal bound g2=g1^2-2","Fleishman feasibility floor","monotonic floor",
           "Gamma path 1.5*skew^2","clean point","folding point"),
  col=c("grey40","#d95f0e","#238b45","#3182bd","#238b45","#d95f0e"),
  lty=c(3,1,1,2,NA,NA),pch=c(NA,NA,NA,NA,19,19),lwd=2)
text(1.45,-1.3,"impossible",col="#99000d",cex=0.8)
text(1.6,3.8,"feasible\nbut folds",col="#993404",cex=0.8)
text(0.5,6.9,"clean (monotonic)",col="#005a32",cex=0.8)
xg<-seq(-4,7,length.out=700)
for(i in seq_len(nrow(cand))){ d<-dens(xg,cand$b[i],cand$c[i],cand$d[i])
  plot(xg,d,type="l",lwd=2,col=if(isTRUE(cand$mono[i]))"#238b45" else "#d95f0e",
       xlab="",ylab="density",ylim=c(0,max(d)*1.12),
       main=sprintf("%s\n%s",cand$label[i],if(isTRUE(cand$mono[i]))"monotonic" else "FOLD"),cex.main=0.85)
  abline(v=0,col="grey80",lty=3) }
dev.off()
cat("Saved: dev/fleishman_design_space.png\n")
print(cand[,c("label","g1","g2","b","c","d","mono")])
