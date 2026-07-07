# Vignette Updates: Šidák CI Visualization Documentation

## Changes Made

The `vignettes/visStatistics.Rmd` file has been updated in three locations to document the new Šidák CI visualization in ANOVA plots.

### 1. **Post-hoc test following Fisher's ANOVA: TukeyHSD** (lines 566-574)

**Old:**
```
For graphical display, `visstat()` shows raw data points, group means (red line), 
and compact letters indicating which groups differ significantly.
```

**New:**
```
For graphical display, `visstat()` shows:
- Raw data points (grey dots)
- Group means (red horizontal lines)
- Šidák-corrected confidence intervals around each group mean (blue vertical error bars with caps), 
  which visualize the spread and distributional assumptions that parametric tests depend on
- Compact letters indicating which groups differ significantly

Significance is determined exclusively by the `TukeyHSD()` post-hoc test results, 
with each group assigned letters such that groups sharing the same letter do not differ significantly.
The Šidák CIs are purely descriptive visualizations of spread and do not determine significance
—the compact letters provide the only significance information.
```

### 2. **Post-hoc test following Welch's ANOVA: Games-Howell** (lines 586-594)

Same update structure as TukeyHSD, with identical bullets and clarification about CI purpose.

### 3. **General graphical output section** (lines 629-638)

**Old:**
```
The graph shows raw data points, group means (red line), and green letters indicating 
which groups differ significantly.
When variances are equal (`aov()`), significance is determined by `TukeyHSD()` post-hoc test.
When variances are unequal (`oneway.test()`), significance is determined by `games.howell()` post-hoc test.
A significant test result between two groups is graphically represented by different green letters 
below a pair of group means.
```

**New:**
```
The graph shows:
- Raw data points (grey dots)
- Group means (red horizontal lines)
- Šidák-corrected confidence intervals around each group mean (blue vertical error bars with caps)
- Green letters indicating which groups differ significantly

When variances are equal (`aov()`), significance is determined by `TukeyHSD()` post-hoc test.
When variances are unequal (`oneway.test()`), significance is determined by `games.howell()` post-hoc test.
A significant test result between two groups is graphically represented by different green letters 
below a pair of group means.
The blue Šidák CIs visualize the distributional spread that parametric tests depend on and are 
separate from significance determination (which is shown by the letters).
```

## Key Documentation Points

All three updates:
1. **Break down visualization elements** with clear bullets (raw data, means, CIs, letters)
2. **Explicitly distinguish CIs from significance**: CIs show spread/assumptions, letters show significance
3. **Prevent confusion**: Make crystal clear that overlapping CIs ≠ insignificant difference
4. **Support the implementation**: Document what users will actually see in the plots

## Testing

When the vignette is rendered (via `pkgdown` or direct `rmarkdown::render()`), the updated text will be visible in the HTML/PDF output alongside the actual plot examples.
