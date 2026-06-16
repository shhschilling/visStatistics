## Run this script directly in R/RStudio
## Plots display immediately on your machine

library(ggplot2)

# Fleishman coefficients
candidates <- list(
  "1. Normal (0, 0)" = list(skew = 0, exkurt = 0, b = 1, c = 0, d = 0),
  "2. Mild skew, low kurt (0.4, 0.4)" = list(skew = 0.4, exkurt = 0.4, b = 0.97364644, c = 0.06398517, d = 0.00736258),
  "3. High skew, low kurt (2.0, 0.4)" = list(skew = 2.0, exkurt = 0.4, b = 0.8763, c = 0.3145, d = 0.0227),
  "4. Moderate skew/kurt (1.0, 1.0)" = list(skew = 1.0, exkurt = 1.0, b = 1.01748519, c = 0.19099508, d = -0.01857700),
  "5. No skew, high kurt (0, 6.0)" = list(skew = 0, exkurt = 6.0, b = 1, c = 0, d = 0),
  "Alt: Mild skew, high kurt (0.4, 1.0)" = list(skew = 0.4, exkurt = 1.0, b = 0.97364644, c = 0.06398517, d = 0.00736258),
  "Alt: High skew, mod kurt (2.0, 1.0)" = list(skew = 2.0, exkurt = 1.0, b = 0.8763, c = 0.3145, d = 0.0227)
)

fleishman_density <- function(x, b, c, d) {
  vapply(x, function(y) {
    roots <- polyroot(c(-c - y, b, c, d))
    real_roots <- Re(roots)[abs(Im(roots)) < 1e-7]
    if (!length(real_roots)) return(NA_real_)
    deriv <- b + 2 * c * real_roots + 3 * d * real_roots^2
    dens <- dnorm(real_roots) / abs(deriv)
    dens <- dens[is.finite(dens)]
    if (!length(dens)) NA_real_ else sum(dens)
  }, numeric(1))
}

# Generate density data
x_grid <- seq(-4, 6, length.out = 800)
dens_data <- do.call(rbind, lapply(names(candidates), function(name) {
  params <- candidates[[name]]
  dens <- fleishman_density(x_grid, params$b, params$c, params$d)
  data.frame(
    case = name,
    skew = params$skew,
    exkurt = params$exkurt,
    x = x_grid,
    density = dens,
    stringsAsFactors = FALSE
  )
}))

# PLOT 1: All candidates
p1 <- ggplot(dens_data, aes(x = x, y = density)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(~ case, scales = "free_y", nrow = 3) +
  labs(title = "Fleishman candidates for 5-panel design",
       subtitle = "Select 4 non-normal cases",
       x = "standardised value", y = "density") +
  theme_minimal(base_size = 10)
print(p1)

# PLOT 2: Parameter space
param_data <- data.frame(
  case = names(candidates),
  skew = sapply(candidates, "[[", "skew"),
  exkurt = sapply(candidates, "[[", "exkurt")
)

p2 <- ggplot(param_data, aes(x = skew, y = exkurt, label = case)) +
  geom_point(size = 4) +
  geom_text(hjust = -0.05, size = 3) +
  labs(title = "Parameter space: (skew, excess kurtosis)",
       x = "skewness", y = "excess kurtosis") +
  theme_minimal(base_size = 11) +
  theme(panel.border = element_rect(colour = "grey50", fill = NA))
print(p2)

cat("\n\nNow tell me: which 4 Fleishman cases (+ normal) for the 5-panel design?\n")
