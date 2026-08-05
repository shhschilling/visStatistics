# What is in this directory (state of 2026-08-03)

Everything here comes from one run of `route1_simulations.R` at `B = 50000` on
8 cores, finished 2 August 2026. Earlier runs, their logs, the superseded
figures and the abandoned SD-sensitivity check have been deleted; nothing older
survives in this directory.

## The run

| | |
|---|---|
| Type I | 6 sizes x 5 designs x 5 input shapes = 150 cells |
| power | 5 designs x 6 sizes x 5 shapes = 150 cells |
| sizes | 10, 20, 30, 50, 100, 200, both parts |
| replications | 50 000 per cell, verified by inverting the recorded standard errors |
| seed | `RNGkind("L'Ecuyer-CMRG")`, `set.seed(20260615)`, one stream per replication |

Reproducible from the single script: a reviewer running it obtains these
numbers. The three heteroscedastic power designs are appended last in
`POWER_DESIGNS`, so every cell that existed before keeps its stream position —
checked, the 150 Type I cells reproduce bit-identically.

### Designs

Type I and power now share the same five:

1. `balanced n, equal SD`
2. `unbalanced n, equal SD`
3. `balanced n, unequal SD`
4. `unbalanced n, larger n with larger SD`
5. `unbalanced n, larger n with smaller SD`

Unbalanced sizes are `n_bar * (0.5, 0.8, 1.2, 1.5)`; the SD vectors are
`(1.0, 1.3, 1.7, 2.2)` and its reverse.

### Heteroscedastic power block

The mean shifts are scaled by `sqrt(mean(SD^2))` so that the population
`omega^2` matches the homoscedastic panels and the comparison isolates unequal
variances: the base `(0, 0.25, 0.50, 0.75)` becomes `(0, 0.404, 0.807, 1.211)`.
`sd_per_group` and `shift_scale` record this per row.

### New columns

Both files carry `resid_skewness` and `resid_excess_kurtosis`, the mean over
replications of the moments of the standardised residual vector that the
Shapiro-Wilk gate is applied to. A scale mixture of symmetric distributions
induces excess kurtosis but no skewness; skewed input carries both.

## Files

| file | what |
|---|---|
| `route1_simulations.R` | the simulation; writes into `fleishman_route1_power_B50000_outputs/` |
| `route1_typeI_figures.R` | the two Type I figures |
| `route1_power_figure.R` | the power figure |
| `fleishman_route1_residual_helpers.R`, `fleishman_figure_typography.R` | shared helpers |
| `route1_equal_mean_simulations.{csv,rds}` | Type I results |
| `fleishman_4groups_power.{csv,rds}` | power results |
| `full_run_hetero.log` | run log, 300 completion lines |

The CSV/RDS pairs exist twice: in `fleishman_route1_power_B50000_outputs/`,
where the simulation writes them, and in this directory, where the figure
scripts read them, since both scripts set `OUTDIR <- "."` and `FIGDIR <- "."`.
The copies are byte-identical.

## Figures

Three, all regenerated 2026-08-03:

- `fleishman_4groups_power.png` — panels A to F: input densities, then one power
  panel per design in the order listed above.
- `route1_identical_distributions_typeI_with_kw_fleishman_B50000.png`
- `route1_equal_means_unequal_distributions_fleishman_B50000.png`

The Type I figures picked up the new sizes automatically, since they facet on
the mean group size: six rows per block instead of four.

The power figure is 20 x 53.5 inches, so its `ggsave` call passes
`limitsize = FALSE` to clear ggplot2's 50-inch guard. At an aspect of 2.675
against the 1.512 an A4 portrait text block allows, it no longer fits one page
at `\textwidth` and needs splitting or shorter panels before it can go into the
vignette. `NS_TO_PLOT` excludes 200, which is simulated but saturated.

Five figures produced by earlier versions are no longer generated, and their
plotting code has been removed from `route1_power_figure.R`:
`fleishman_4groups_power_pdf.png`, `fleishman_4groups_power_with_pdf.png` (which
was a byte-identical duplicate of the combined figure),
`fleishman_4groups_route_probability.png`,
`fleishman_4groups_fisher_welch_route_probability.png` and
`fleishman_4groups_parametric_branch_power.png`.

## Previous state, archived

`visstatisticsArchive/20260729_route1_simulations_pre_unbalanced/` holds the
`inst/simulations/` tree before the unbalanced power series was added.
`visstatisticsArchive/20260801_route1_pre_n30/` holds the four CSV/RDS files of
the 29 July grid, before `n_bar = 30` and 200 were added.

## The vignette does not read from here

`vignettes/children/_simulations.Rmd` reads `figures/*.png`, i.e.
`vignettes/figures/`. Both scripts set `FIGDIR <- "."`, so their copy step never
fires and nothing propagates automatically. As of 2026-08-03 `vignettes/figures/`
still holds the **27 July** versions of all three simulation figures. Copy them
across when the new ones are accepted.
