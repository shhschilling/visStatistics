Four-group Gamma/normal skew sweep for Route 1 power
NREP per cell: 4000
Maximum Monte Carlo SE for a percentage estimate: 0.79 percentage points
alpha: 0.05
Group mean offsets:
  moderate ordered effect: 0, 0.25, 0.50, 0.75 SD

Power means: probability of p < alpha for detecting the omnibus group pattern.
F: Fisher's one-way ANOVA.
W: Welch's heteroscedastic ANOVA.
L: mean-centred Levene selects F vs W.
KW: Kruskal-Wallis branch.
SW: Shapiro-Wilk selects W vs KW.
SW+L: Shapiro-Wilk selects KW or, if parametric branch is retained,
  Levene selects F vs W.
In this homoscedastic design, Welch selections are false-positive Levene routes.

Skew = 0 is normal. Skew > 0 uses the standardised Gamma path.
Gamma path caveat: skewness and excess kurtosis vary together.

Files:
  gamma_skew_sweep_4groups_power.csv
  gamma_skew_sweep_4groups_power.rds
  gamma_skew_sweep_4groups_power.png
  gamma_skew_sweep_4groups_parametric_branch_power.png
  gamma_skew_sweep_4groups_route_probability.png
  gamma_skew_sweep_4groups_fisher_welch_route_probability.png
