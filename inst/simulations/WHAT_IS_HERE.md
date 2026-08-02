# What is in this directory (state of 2026-07-29)

Two sets of Route 1 figures exist. They differ, and the filenames do not say so.

## Current figures, in this directory

Regenerated 2026-07-29 from `route1_simulations.R` at `B = 50000`, 8 cores.

| file | panels | changed on 2026-07-29 |
|---|---|---|
| `fleishman_4groups_power.png` | **A, B, C** | **C is new**: unbalanced power series |
| `fleishman_4groups_power_with_pdf.png` | identical file to the above | — |
| `fleishman_4groups_power_pdf.png` | A alone | unchanged |
| `route1_identical_distributions_typeI_with_kw_fleishman_B50000.png` | A, B, C | panel B/C **header wording** only; numbers unchanged |
| `route1_equal_means_unequal_distributions_fleishman_B50000.png` | A, B, C, D | panel B **header wording** only; numbers unchanged |
| `fleishman_4groups_parametric_branch_power.png` | — | unchanged; balanced subset only |
| `fleishman_4groups_route_probability.png` | — | unchanged; balanced subset only |
| `fleishman_4groups_fisher_welch_route_probability.png` | — | unchanged; balanced subset only |

The power figure is now 20 x 24.3 inches (was 20 x 15.2). Ratio 1.215 against the
1.512 that an A4 portrait text block allows, so it fits one page at
`\textwidth`. The width is deliberately unchanged, so printed point sizes are
unchanged.

## What panel C of the power figure shows

Second power design, `unbalanced n, equal SD`, with
`(n1, n2, n3, n4) = n_bar * (0.5, 0.8, 1.2, 1.5)` and unit SDs. Findings:

- Gate routing is identical to the balanced series, to within 1 percentage
  point in every cell. This is structural: the Shapiro gate reads `N - k` pooled
  residuals, and the multipliers sum to 4, so both designs have the same `N`.
- Power lost to imbalance is **not** uniform. It tracks the steepness of the
  power curve: 3-5 pp at `n_bar = 10`, peaking at 5.4-7.8 pp at `n_bar = 20`,
  falling at 50, at most 0.6 pp at 100 and exactly 0 at 200. Monte Carlo SE of
  such a difference is 0.32 pp. The small values at `n_bar = 50` for the two
  high-kurtosis panels are ceiling effects, those cells being at .987 and .996
  already.
- The loss is common across strategies **except** for Welch at `n_bar = 10`
  under skew, where imbalance *helps* it: -4.62 pp at skew 2 and -0.97 pp at
  skew 1, against +3.3 to +5.0 pp for every other strategy. The ordering is
  therefore preserved wherever the loss is common, and changes exactly where it
  is not: those two cells are the only ones in either design where Welch is the
  best fixed test. Whether groups of 5, 8, 12, 15 support any inference is a
  judgement call.

Panel C is therefore close to a null result, kept because a claim of invariance
that is not shown has to be cited from an unshown file instead.

## Previous figures, archived

`visstatisticsArchive/20260729_route1_simulations_pre_unbalanced/` holds the
complete `inst/simulations/` tree as of the last commit before these changes,
taken from git rather than from disk. That is where the two-panel power figure
and the pre-edit Type I headers live.

## The vignette does not read from here

`vignettes/children/_simulations.Rmd` reads `figures/*.png`, i.e.
`vignettes/figures/`. Both scripts set `OUTDIR <- "."` and `FIGDIR <- "."`, so
their copy step never fires and nothing is propagated automatically. As of
2026-07-29 `vignettes/figures/` still holds the **27 July** versions: the
two-panel power figure and the pre-edit Type I headers. Copy the three files
across when the new figures are accepted.
