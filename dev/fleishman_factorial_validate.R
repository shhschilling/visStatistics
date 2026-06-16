## Validate the user's factorial Fleishman design and render densities.
## Reads the existing parameters CSV; does NOT recompute coefficients.

src <- file.path("dev", "fleishman_factorial_story_preview",
                 "visstatistics_fleishman_factorial_story_parameters.csv")
p <- read.csv(src, stringsAsFactors = FALSE)

## a + b z + c z^2 + d z^3  (uses the a column as stored)
draw <- function(a,b,c,d,n=2e6,seed=1){ set.seed(seed); z<-rnorm(n); a+b*z+c*z^2+d*z^3 }
moms <- function(y){ m<-mean(y); v<-mean((y-m)^2)
  c(mean=m, sd=sqrt(v), skew=mean((y-m)^3)/v^1.5, exkurt=mean((y-m)^4)/v^2-3) }

cat("Monte Carlo validation of the factorial parameters:\n")
cat(sprintf("%-24s %7s %7s | %8s %8s %8s\n",
            "panel","tgtSkew","tgtKurt","MC.mean","MC.skew","MC.kurt"))
for (i in seq_len(nrow(p))) {
  v <- moms(draw(p$a[i],p$b[i],p$c[i],p$d[i]))
  cat(sprintf("%-24s %7.2f %7.2f | %8.3f %8.3f %8.3f\n",
              p$label[i], p$skew[i], p$exkurt[i], v["mean"], v["skew"], v["exkurt"]))
}

## monotonicity check: c^2 <= 3 b d
cat("\nFold check (need c^2 <= 3bd):\n")
for (i in seq_len(nrow(p))) {
  lhs<-p$c[i]^2; rhs<-3*p$b[i]*p$d[i]
  cat(sprintf("  %-24s c^2=%.4f 3bd=%.4f  %s\n",
              p$label[i], lhs, rhs, if(lhs<=rhs) "monotonic" else "FOLD"))
}

## exact density via root inversion of a + b z + c z^2 + d z^3 = y
dens <- function(y,a,b,c,d){
  if (b==1 && c==0 && d==0) return(dnorm(y))
  vapply(y,function(yy){
    r<-polyroot(c(a-yy,b,c,d)); re<-Re(r)[abs(Im(r))<1e-8]
    if(!length(re)) return(0)
    dp<-b+2*c*re+3*d*re^2; val<-dnorm(re)/abs(dp); val<-val[is.finite(val)]
    if(!length(val)) 0 else sum(val)
  },numeric(1))
}

xg<-seq(-4,7,length.out=1000)
png(file.path("dev","fleishman_factorial_validate.png"),
    width=1600,height=360,res=110)
op<-par(mfrow=c(1,5),mar=c(4,4,3,1))
for (i in seq_len(nrow(p))) {
  dd<-dens(xg,p$a[i],p$b[i],p$c[i],p$d[i])
  plot(xg,dd,type="l",lwd=2,col="#185FA5",xlab="standardised value",
       ylab="density",ylim=c(0,max(dd)*1.1),
       main=sprintf("%d. %s\nskew=%.1f kurt=%.1f",p$panel[i],p$label[i],
                    p$skew[i],p$exkurt[i]),cex.main=0.9)
  abline(v=0,col="grey70",lty=3)
}
par(op);dev.off()
cat("\nSaved: dev/fleishman_factorial_validate.png\n")
