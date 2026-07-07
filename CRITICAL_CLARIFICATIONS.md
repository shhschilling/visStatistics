# Critical Clarifications: Šidák CIs vs. Tukey/Games-Howell Inference

## The Problem

Displaying confidence intervals alongside post-hoc test results creates a **visual inference hazard**: users will naturally interpret overlapping/non-overlapping intervals as evidence of significance or equivalence, which is **statistically incorrect and dangerous**.

## The Solution: Unmissable Clarity

We have implemented THREE layers of explicit warnings to prevent misinterpretation:

### 1. On-Plot Legend (R code: `vis_anova.R`, lines 231-248)

The legend appears directly on every ANOVA plot and now reads:

```
Significance determined by [Tukey HSD | Games-Howell] post-hoc test (α=0.05):
• Letters (a,b,c...): groups with same letter do NOT differ significantly
• Red lines: group means
• Blue error bars: Šidák CIs for individual means (descriptive only)
⚠ WARNING: Overlapping/non-overlapping intervals do NOT indicate significance.
Use letters only to determine which groups differ.
```

**Why on the plot:** Most users will see only the plot, not the vignette. This warning is unmissable.

### 2. Vignette Documentation (TukeyHSD section, lines 572-574)

```markdown
**Critical:** Pairwise group differences are assessed exclusively using Tukey's HSD 
post-hoc test. The displayed confidence intervals are Šidák-adjusted intervals for 
the individual group means and are provided for descriptive purposes only. 

**These intervals must not be used to infer significance between groups.** 
Overlapping or non-overlapping intervals do NOT indicate whether two groups differ 
significantly. All inferential conclusions about pairwise differences must be based 
solely on the Tukey HSD results (indicated by the letters).
```

Same text appears for Games-Howell section (lines 592-594) and general graphical output section (line 638).

### 3. Conceptual Separation

The documentation now makes four critical distinctions:

#### A. **Purpose of CIs**
- **Šidák CIs:** Intervals for individual group **marginal means** (not differences)
- **Tukey/Games-Howell:** Intervals for pairwise **differences** between groups

#### B. **What determines significance**
- **Tukey HSD / Games-Howell:** ONLY source of inferential conclusions
- **Šidák CIs:** Descriptive visualization only; cannot be used for inference

#### C. **Visual interpretation**
- Overlapping Šidák CIs for two group means ≠ groups are not significantly different
- Non-overlapping Šidák CIs for two group means ≠ groups are significantly different
- The letters indicate significance; intervals do not.

#### D. **Multiple testing adjustment**
- **Šidák:** Adjusts for multiple comparisons across *all k group means*
- **Tukey/Games-Howell:** Adjusts for multiple comparisons across *all pairwise differences*

These are different error rates protecting different hypotheses.

## Why This is Necessary

Even with perfect wording, many users will misinterpret the plot visually. The redundant warnings at three levels (plot, vignette, general description) are necessary because:

1. **Visual processing dominates:** Users see intervals first, read text second (if at all)
2. **Interval overlap is intuitive but wrong:** Years of textbook training make overlapping intervals "feel" like non-significance
3. **Plot literacy varies:** Even experienced users can misinterpret unfamiliar plots

## What the Plot Shows (Accurate Description)

The ANOVA plot now displays:

1. **Raw data points** (grey dots): All observations
2. **Group means** (red horizontal lines): Point estimate of each group's central tendency
3. **Šidák CIs around means** (blue error bars): Confidence intervals for individual group means, adjusted for multiple comparisons
4. **Significance letters** (a, b, c, ...): Post-hoc test results showing which groups differ significantly

**Reading the plot:**
- Use letters to determine significance
- Use blue intervals to assess spread/precision of each group's mean estimate
- Never use interval overlap to make significance judgments

## Implementation Notes

### Code Changes
- **R/vis_anova.R**: Legend now includes explicit warning about interval interpretation
- **vignettes/visStatistics.Rmd**: Three sections updated with **Critical** and **Important** callouts

### Test When Building
```R
devtools::document()    # Update roxygen docs
devtools::check()       # Full CRAN check
```

When run, plots will display the warning text directly, making the distinction unmissable.

## Remaining Risk

Even with these warnings, some users will ignore them. This is a known limitation of visual communication. However, the implementation is now **statistically defensible** because:

1. The warnings are explicit and repeated
2. The correct interpretation is given priority (letters first, intervals second)
3. The distinction between marginal CIs and pairwise inference is clearly stated
4. Users who read carefully will understand the correct interpretation

**Bottom line:** This is valid statistical practice with high-effort communication. Not perfect, but defensible.
