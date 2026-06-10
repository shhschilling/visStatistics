Mild-skew Route 1 stress test
NREP per cell: 2000
alpha: 0.05

Data-generating distribution:
  Identical standardised Gamma groups unless shift_sd > 0.
  Gamma shape = (2 / skew)^2, scale = 1; then standardised to mean 0 and SD 1.
  Target skewness values: 0.1, 0.2, 0.3, 0.4, 0.5.
  Caveat: along this Gamma path, skewness and kurtosis vary together.

Figure X:
  figure_x_route_to_rank_mild_skew.png
  Percentage of simulations routed to Wilcoxon/Kruskal-Wallis by Shapiro-Wilk
  on internally studentised residuals.
  DGP: all groups are identical; no group shift; equal means and equal ordering.

Figure Y:
  figure_y_mean_rank_agreement_mild_skew.png
  Percentage of simulations where the mean-based test and rank-based test
  give the same significant/not-significant decision.
  DGP: same standardised Gamma shape in all groups; last group shifted by
  0, 0.3, or 0.5 SD.
  Mean tests: Student t-test for 2 groups; Fisher ANOVA for 4 groups.
  Rank tests: Wilcoxon for 2 groups; Kruskal-Wallis for 4 groups.
