# ANOVA Visualization Update: Šidák CIs Restored

## Decision Made

**Option 1: Restore Šidák CIs with clear legend separation**

The circular logic about confidence intervals has been resolved by restoring Šidák-corrected CIs around group means in `vis_anova()`, with explicit visual and textual separation between:
1. **Spread visualization** (blue error bars) — shows the uncertainty and distributional assumptions that parametric tests depend on
2. **Significance determination** (compact letters) — shows which pairs actually differ

## Changes to `R/vis_anova.R`

### What was changed:

1. **Added Šidák CI calculation** (lines 149-191)
   - For each group, computes the Šidák-corrected critical value: `t_crit = qt(1 - alpha_sidak/2, df = n-1)`
   - CI bounds: `mean ± t_crit × SE`
   - Uses Šidák adjustment to control family-wise error across all pairwise comparisons

2. **Visual representation**
   - Red horizontal lines: group means (unchanged)
   - Blue vertical error bars with caps: Šidák CIs around each group (NEW)
   - Letters at bottom: post-hoc significance (unchanged)

3. **Updated legend and explanatory text** (lines 218-246)
   - Explicit legend shows "group means" and "Šidák CIs (spread)"
   - Detailed explanatory text at bottom clarifies:
     - What each visual element represents
     - That CIs show spread/assumptions, not significance
     - That letters show which pairs differ
     - Displays the post-hoc test name and alpha level

## Why this approach works:

- **Parametric tests depend on variance**: ANOVA assumes homogeneity of variance. Showing the spread (via Šidák CIs) makes the data's conformity to this assumption visually apparent.

- **No ambiguity between visualization and significance**: The legend and text explicitly separate CI interpretation (spread) from significance determination (letters). Users cannot confuse them.

- **Consistency**: Kruskal-Wallis shows full distribution via boxplot; ANOVA now shows distributional assumptions via CIs. Both provide distributional context.

- **Conservative but transparent**: Šidák correction is more conservative than standard CIs, reducing false visual alarms from overlapping intervals that don't reflect true significance.

## What was NOT changed:

- Post-hoc test selection logic (still Levene → TukeyHSD or Welch → Games-Howell)
- Significance letter determination (still uses post-hoc p-values)
- Overall structure and return values
- Function signature or documentation

## Testing

The change modifies only the visualization layer. All statistical computations (ANOVA, Welch, post-hoc tests) remain identical. When the updated package is built and run:

```R
devtools::document()  # Update roxygen docs
devtools::test(filter = 'anova')  # Run ANOVA-specific tests
devtools::check()     # Full CRAN check
```

The visualization will now show blue error bars around each group mean, with a clear legend explaining that these represent Šidák CIs for spread and distributional assumptions, distinct from the significance letters below.
