# Route 1 replication material

Code and Monte Carlo output behind the three Route 1 simulation figures of the
vignette *visStatistics: automated test selection, visualised*
(Section "Route 1 simulations").

Locate the directory from R with

```r
system.file("simulations", package = "visStatistics")
```

and copy its contents to a writable working directory before running anything;
the scripts read and write in the current working directory.

## Files

| file | purpose |
|---|---|
| `route1_simulations.R` | runs the Type I and power simulations and writes the `.csv`/`.rds` results |
| `route1_typeI_figures.R` | builds the two Type I figures from the saved results |
| `route1_power_figure.R` | builds the power figure from the saved results |
| `fleishman_route1_residual_helpers.R` | Fleishman coefficients, densities and group colours |
| `fleishman_figure_typography.R` | shared figure typography |
| `route1_equal_mean_simulations.csv`, `.rds` | Type I results, 5 designs x 4 sample sizes x 5 input distributions |
| `fleishman_4groups_power.csv`, `.rds` | power results, 5 sample sizes x 5 input distributions |
| `sim_README.txt` | run log of the saved output: replications, cores, Monte Carlo SE, input distributions |

## Reproducing

The saved results use `B = 50000` replications per cell and take hours on
several cores. Re-run them with

```r
source("route1_simulations.R")
```

which accepts the number of replications and the number of cores as command-line
arguments, e.g. `Rscript route1_simulations.R 1000 4` for a fast check. The seed
at the top of the script fixes one L'Ecuyer-CMRG stream per replication, so the
published output is reproduced exactly at `B = 50000`, whatever the number of
cores.

The figures are rebuilt from the saved results without re-running the
simulation:

```r
source("route1_typeI_figures.R")
source("route1_power_figure.R")
```

`route1_typeI_figures.R` names its two output files after the number of
replications behind the results. Set `NREP` before sourcing it when the results
come from a run with a reduced `B`.

These require `ggplot2`, `patchwork`, `scales`, `colorspace` and `ggtext`, none
of which is a dependency of `visStatistics` itself.
