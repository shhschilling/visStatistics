## Multiple linear regression: cantonal drivers of EV market share (2025)
## Data source: LG332_Team1_Excel.xlsx, sheet "3" (Blatt 3)
##   Y  : EV-Marktanteil Neuzulassungen 2025 (%)
##   X1 : Ladedichte (LP / 1'000 PW)
##   X2 : Steuerrabatt (%)
##   X3 : Tourismusintensität (Logiernächte / Einw.)
##   X4 : Kaufkraft (EUR / Einw.)

df <- data.frame(
  Kanton = c("ZH", "LU", "SZ", "OW", "NW", "ZG", "BL", "AI", "GR", "GE"),
  Y  = c(30.67, 27.38, 23.92, 22.16, 25.04, 26.00, 26.86,  6.07, 25.86, 13.36),
  X1 = c(14.94, 20.21, 18.79, 24.43, 25.96, 20.20, 15.11, 17.27, 46.44, 16.43),
  X2 = c(100.00, 79.94,  0.00, 50.00, 100.00, 50.00, 39.69,  0.00, 80.00,  0.00),
  X3 = c(39.58, 58.79, 34.72, 131.49, 75.31, 22.25,  9.17, 147.73, 216.73, 62.48),
  X4 = c(60861, 52159, 78904, 57859, 65999, 89607, 54361, 57094, 52969, 54789)
)

# Multiple linear regression
fit <- lm(Y ~ X1 + X2 + X3 + X4, data = df)
summary(fit)

# Diagnostics
par(mfrow = c(2, 2))
plot(fit)
par(mfrow = c(1, 1))

## Vorbehalt: Zu wenige Beobachtungen im Verhältnis zur Anzahl Prädiktoren.
## Die Schätzungen sind instabil, das Modell ist nahezu gesättigt (R^2 überhöht),
## und die einzelnen Koeffizienten sind nicht zuverlässig interpretierbar.
