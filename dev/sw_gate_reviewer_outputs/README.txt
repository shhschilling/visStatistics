Reviewer-facing Route 1 gate simulation
NREP per cell: 2000
alpha: 0.05

Files:
  common_null_rates.csv
  location_shift_power.csv
  equal_mean_lognormal_conflict.csv
  common_null_typeI.png
  location_shift_power.png
  route_to_rank_probability.png
  equal_mean_lognormal_conflict.png

Interpretation:
  common_null_rates.csv: all three tests have a true null.
  location_shift_power.csv: all three tests have a false null.
  equal_mean_lognormal_conflict.csv: the mean null is true, but the
    ordering/rank null is false; gate rejections are route-dependent.
