Gamma high-skew power simulation
NREP per cell: 2000
alpha: 0.05
effect: 0.5 SD shift added to the last group

Power means: probability of p < alpha for detecting the 0.5 SD shift.
Mean test: Student/Welch for 2 groups; Fisher/Welch ANOVA for 4 groups,
  with Levene selecting equal-variance versus Welch.
Rank test: Wilcoxon for 2 groups; Kruskal-Wallis for 4 groups.
Gate: Shapiro-Wilk on standardised residuals; if rejected, use rank test;
  otherwise use the mean test.

Gamma path caveat: skewness and excess kurtosis vary together.

Files:
  gamma_high_skew_power.csv
  gamma_high_skew_power.rds
  gamma_high_skew_power.png
  gamma_high_skew_route_probability.png
