Route 1 equal-mean rejection simulation inspired by Blanca:2017 and Zimmerman:2004
NREP per cell: 50000
Maximum Monte Carlo SE for a percentage estimate: 0.22 percentage points

Local PDFs read before simulation:
  Blanca:2017, DOI 10.7334/psicothema2016.383
  Zimmerman:2004, DOI 10.1348/000711004849222

Ground truth:
  all four group means are equal.

Design adaptation:
  Blanca-style: zero group effect, balanced/unbalanced one-way layouts,
  non-normal distributions, empirical rejection rates.
  Zimmerman-style: unconditional Fisher/Welch tests versus Levene-conditioned
  choice, unequal variances, and variance-size pairing.

Gamma caveat:
  skewness and excess kurtosis vary together on the Gamma path.

Files:
  route1_equal_mean_blanca_zimmerman.csv
  route1_equal_mean_blanca_zimmerman.rds
  route1_equal_mean_gate_heatmap.png
  route1_equal_mean_strategy_rejection.png
