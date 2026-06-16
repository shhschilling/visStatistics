## Compute exact Fleishman densities and validate against MC histograms.
## Reads coefficients from fleishman_5panel_coefficients.csv,
## writes density curves to fleishman_5panel_density_curves.csv.

coef <- read.csv(file.path("dev", "fleishman_5panel_coefficients.csv"),
                 stringsAsFactors = FALSE)

## Exact density via change of variables:
##   Y = p(Z) = -c + bZ + cZ^2 + dZ^3
##   f_Y(y) = sum over real roots z_i of p(z)=y  of  phi(z_i) / |p'(z_i)|
fleishman_density <- function(y, b, c, d) {
  if (b == 1 && c == 0 && d == 0) return(dnorm(y))
  vapply(y, function(yy) {
    ## solve d z^3 + c z^2 + b z + (-c - yy) = 0
    roots <- polyroot(c(-c - yy, b, c, d))
    re <- Re(roots); im <- Im(roots)
    real <- re[abs(im) < 1e-8]
    if (!length(real)) return(0)
    dp <- b + 2*c*real + 3*d*real^2
    val <- dnorm(real) / abs(dp)
    val <- val[is.finite(val)]
    if (!length(val)) 0 else sum(val)
  }, numeric(1))
}

## ---- build curves and validate ---------------------------------------------
xg <- seq(-4, 7, length.out = 1000)
curves <- list()
cat("Validation: integral of density (should be ~1)\n")
for (i in seq_len(nrow(coef))) {
  b <- coef$b[i]; c <- coef$c[i]; d <- coef$d[i]
  dens <- fleishman_density(xg, b, c, d)
  integral <- sum((dens[-1] + dens[-length(dens)]) / 2 * diff(xg))
  cat(sprintf("  %-20s integral=%.4f  max=%.4f\n",
              coef$case[i], integral, max(dens)))
  curves[[i]] <- data.frame(case = coef$case[i], x = xg, density = dens)
}

out <- do.call(rbind, curves)
write.csv(out, file.path("dev", "fleishman_5panel_density_curves.csv"),
          row.names = FALSE)
cat("\nSaved: dev/fleishman_5panel_density_curves.csv\n")

## ---- also draw a base-R PNG so it exists on disk ---------------------------
labels <- c(
  normal             = "1. Normal (0, 0)",
  mild_skew_low_kurt = "2. Mild skew (0.4, 0.4)",
  high_skew          = "3. High skew (2, 6)",
  no_skew_high_kurt  = "4. Heavy tails (0, 6)",
  moderate           = "5. Moderate (1, 1)"
)
png(file.path("dev", "fleishman_5panel_density.png"),
    width = 1500, height = 350, res = 110)
op <- par(mfrow = c(1, 5), mar = c(4, 4, 3, 1))
for (i in seq_len(nrow(coef))) {
  sub <- subset(out, case == coef$case[i])
  plot(sub$x, sub$density, type = "l", lwd = 2, col = "#185FA5",
       xlab = "standardised value", ylab = "density",
       main = labels[[coef$case[i]]],
       ylim = c(0, max(sub$density) * 1.1))
  abline(v = 0, col = "grey70", lty = 3)
}
par(op); dev.off()
cat("Saved: dev/fleishman_5panel_density.png\n")
