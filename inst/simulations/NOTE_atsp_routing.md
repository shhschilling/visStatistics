# Note: should the rank branch of Route 1 route to ATSp instead of Kruskal–Wallis?

**Conclusion: no. The simulations do not justify replacing Kruskal–Wallis.**
Reasoning in Section 6; the evidence it rests on is Sections 1, 2 and 4.

Status: everything below is from the B = 50,000 grids in `inst/simulations/`;
nothing here has been written into the vignette.

Monte Carlo error at B = 50,000: SE = 0.0010 at p = 0.05, so a 2-SE band around
the nominal level is [0.0481, 0.0519]. Differences smaller than that measure
nothing.

Tests compared (naming as in `route1_power_figure.R:352`):

| Abbrev. | Test | Null |
|---|---|---|
| KW   | Kruskal–Wallis, mid-ranks (`stats::kruskal.test`) | H0F |
| RK   | Kruskal–Wallis, pseudo-ranks (`rankFD`, `effect="unweighted"`) | H0F |
| ATS  | ANOVA-type statistic (`rankFD`) | H0F |
| ATSp | ANOVA-type statistic (`rankFD`, `hypothesis="H0p"`) | H0p |

## 1. Level, under a true rank null

The rank null holds only in the two symmetric columns
(`rankfd_route1_typeI.R:137`: `rank_null_true = panel %in% c(1, 2)`). In the
equal-SD designs the distributions there are identical; in the heteroscedastic
ones they are symmetric about a common location with differing scales, so the
locations agree and the relative effects are ½, which is what makes those
columns a Type I check for the rank tests.

Maximum rejection rate over those 60 cells of
`rankfd_route1_typeI_B50000.csv` / `rankfd_route1_typeI_h0p_B50000.csv`:

| design | KW | RK | ATS | ATSp |
|---|---|---|---|---|
| balanced n, equal SD | 0.0509 | 0.0504 | 0.0512 | 0.0512 |
| balanced n, unequal SD | 0.0561 | 0.0567 | 0.0533 | 0.0518 |
| unbalanced n, equal SD | 0.0508 | 0.0633 | 0.0565 | 0.0644 |
| unbalanced n, larger n with larger SD | 0.0358 | 0.0378 | 0.0472 | 0.0535 |
| unbalanced n, larger n with smaller SD | 0.0850 | **0.1203** | 0.0716 | 0.0747 |

The direction with n matters more than the maximum. In the negative-pairing
corner (larger groups with smaller SDs), at n̄ = 10, 20, 30, 50, 100, 200:

```
RK    0.1097  0.1148  0.1162  0.1179  0.1203  0.1200   -> grows, settles ~0.12
ATS   0.0716  0.0600  0.0589  0.0574  0.0579  0.0573   -> falls, settles ~0.057
ATSp  0.0747  0.0613  0.0550  0.0535  0.0508  0.0503   -> falls to nominal
```

ATSp is the only one of the four whose level reaches nominal in **every** design
by n̄ = 200 (0.0512, 0.0502, 0.0501, 0.0505, 0.0503). Its failures are confined
to n̄ = 10 and shrink monotonically. RK's inflation is asymptotic, not a
small-sample artefact: it is larger at n̄ = 200 than at n̄ = 10.

## 2. Power

Mean over panels and sample sizes, 150 cells of
`rankfd_route1_power_B50000.csv` / `rankfd_route1_power_h0p_B50000.csv`:

| design | KW | RK | ATS | ATSp |
|---|---|---|---|---|
| balanced n, equal SD | 0.7802 | 0.7801 | 0.7827 | 0.7812 |
| balanced n, unequal SD | 0.7261 | 0.7271 | 0.7266 | 0.7218 |
| unbalanced n, equal SD | 0.7458 | 0.7840 | 0.7051 | 0.7101 |
| unbalanced n, larger n with larger SD | 0.6536 | 0.6784 | 0.6999 | 0.7113 |
| unbalanced n, larger n with smaller SD | 0.8118 | 0.8503 | 0.7014 | 0.6927 |

Design means hide the spread. Cellwise against the incumbent KW, over the same
150 cells (2 MC SE = 0.0045 for a rate near 0.5):

- **RK vs KW**: higher in 77 cells beyond 2 MC SE, lower in 5, worst deficit
  0.0080. RK dominates KW on power almost everywhere, so **KW is not the best
  rank test on this axis**. But RK's largest leads sit in the two designs where
  its own level is inflated (0.0633 and 0.1203), so they are partly size.
- **ATSp vs KW**: lower in 53 cells beyond 2 MC SE, higher in 30. Median
  deficit where it loses is 0.0142, but the distribution has a long tail —
  the 90th percentile of the deficit is 0.1910 and the worst cell is
  **0.2898**.

| design | mean(ATSp − KW) | cells worse beyond 2 MC SE | worst cell |
|---|---|---|---|
| balanced n, equal SD | +0.0010 | 1/30 | −0.0069 |
| balanced n, unequal SD | −0.0043 | 13/30 | −0.0159 |
| unbalanced n, equal SD | −0.0357 | 17/30 | −0.1640 |
| unbalanced n, larger n with larger SD | +0.0578 | 0/30 | — |
| unbalanced n, larger n with smaller SD | −0.1191 | 22/30 | −0.2898 |

The power cost of ATSp is therefore neither modest nor confined to one design:
it is concentrated in negative pairing, which is the same corner ATSp was
considered for in the first place. In that corner KW's own level is 0.0850, so
part of KW's apparent power there is size — but a level of 0.085 against 0.050
cannot account for a 29-point gap.

## 3. ATSp does not reduce to KW

Checked directly against `rankFD` on identical datasets, not asserted:

| data | KW p | ATSp p | ATS df₁ | ATS df₂ |
|---|---|---|---|---|
| balanced, homoscedastic | 0.0599 | 0.0582 | 2.9998 | 155.82 |
| balanced, homoscedastic (2nd draw) | 0.9576 | 0.9584 | 2.9918 | 154.89 |
| unbalanced, homoscedastic | 0.1151 | 0.2243 | 2.6248 | 116.13 |
| unbalanced, negative pairing | 0.1119 | 0.2771 | 2.2060 | 75.89 |

Under balance *and* homoscedasticity the two agree closely but are not the same
statistic: df₁ → k − 1 and df₂ ≈ N − k, so the Box-type F reference approaches
χ²/(k − 1). Once balance is broken the estimated df fall well below k − 1 and
the p-values diverge by a factor of about two, with equal variances.

The H0F/H0p distinction appears purely as a different df₂ (155.92 vs 155.82 when
balanced; 77.1 vs 116.1 when not) — the Behrens–Fisher correction.

## 4. Why not default to a rank test everywhere?

Because every rank test in the table, ATSp included, answers a different
question from the mean-based branch, and the gap **widens with n**. Over the 90
cells with equal group means but unequal distributions
(`rank_null_true == FALSE`), rejection rates by n̄:

```
n̄       10     20     30     50    100    200
KW    0.127  0.180  0.229  0.329  0.588  0.895
ATSp  0.101  0.141  0.195  0.323  0.602  0.909
```

The means are equal in every one of these cells. A user asking whether the group
*locations* differ receives a rejection in 91 % of them at n̄ = 200, and
collecting more data makes this worse, not better. ATSp is no help: it is
built to detect exactly this (relative effects ≠ ½), so it is not misbehaving —
it is answering the question it was given.

This is the argument for the gate rather than a fixed rank default: routing to a
rank test has to be a decision that the mean-based question is not the one being
asked, or that its assumptions have failed. It cannot be the default for data
that a mean-based test handles correctly.

## 5. What this does not establish

- The Box-type degrees-of-freedom construction itself is **not** verified here:
  Brunner, Dette & Munk (1997), which derives it, was not read. Konietschke &
  Brunner (2023) attribute it there; everything else in Section 7 comes from
  sources that were read.
- `rankFD` is a dependency `visStatistics` does not currently take
  (`rankfd_route1_power.R:24`).
- The post-hoc step is untouched. Route 1 currently pairs KW with pairwise
  Wilcoxon; what pairs with ATSp has not been simulated.
- Whether the negative-pairing design is common enough in the package's target
  use to justify a default change is a judgement the simulations cannot make.

## 6. Conclusion

No rank test in the comparison dominates, and each alternative to
Kruskal–Wallis fails severely on the axis it does not optimise:

- **RK** has the best power but its size inflation under negative pairing is
  asymptotic — 0.1097 at n̄ = 10 rising to 0.1200 at n̄ = 200. A user cannot
  outgrow it by collecting more data, and cannot see it.
- **ATSp** attains nominal level in every design but pays for it with a power
  deficit that reaches 0.2898 in a single cell and averages 0.1191 across the
  negative-pairing design — the very corner it would be adopted to protect.
- **KW** is best on neither axis. Its maximum level is 0.0850 and RK beats it on
  power in 77 of 150 cells.

Kruskal–Wallis is therefore retained not because it wins, but because its
failure on each axis is bounded where the alternatives' are not: it never
inflates past 0.085 and never trails by more than 0.008 against the
power-optimal rank test. Replacing a CRAN default carries costs the simulations
cannot weigh, and they supply no evidence strong enough to bear them.

Two qualifications, so the record is not read as stronger than it is:

1. This is an argument for **sufficiency under the conditions the routing gate
   establishes**, not for superiority. In the balanced homoscedastic design all
   four tests are indistinguishable (0.7802 / 0.7801 / 0.7827 / 0.7812, within
   Monte Carlo error) — the default is doing no work there, and the gate is
   carrying the argument.
2. The corner where all four degrade — larger groups paired with smaller SDs —
   is a real failure of the rank branch as a whole, not a reason to prefer one
   member of it. It belongs in the vignette's limitations, whichever test is
   the default.

## 7. Sources

Read for this note; equation numbers are those of the cited paper. Following the
project convention, journal articles are cited by equation, not page.

**Konietschke, F. and Brunner, E. (2023).** rankFD: An R Software Package for
Nonparametric Analysis of General Factorial Designs. *The R Journal* 15(1),
142–158. <doi:10.32614/RJ-2023-029>

- Model and effects: $X_{ik}\sim F_i$, $N=\sum_i n_i$ (eq. 1); weighted relative
  effects $\theta_i$ (eq. 2) from the weighted average $H_N=\frac1N\sum_i n_iF_i$;
  unweighted $\psi_i$ (eq. 3) from the unweighted average $G$.
- Ranks $R_{ik}=\tfrac12+N\widehat H(X_{ik})$; pseudo-ranks
  $R^\psi_{ik}=\tfrac12+N\widehat G(X_{ik})$, with
  $\widehat\psi_i=\frac1N\overline{R}{}^\psi_{i\cdot}-\tfrac12$ (eq. 4).
  Hence: *rank* tests estimate $\theta_i$, *pseudo-rank* tests estimate $\psi_i$.
- $H_{0F}: F_1=\dots=F_a$ (eq. 5); "equal distribution functions imply equal
  variances if $H_{0F}$ is true (if second moments exist)".
- $H_{0P}: \psi_1=\dots=\psi_a$ (eq. 9). **The key remark for this note:** the
  alternative form $H_{0P}: \theta_1=\dots=\theta_a$ (eq. 10) "depends on the
  relative sample sizes $n_i/N$ ... the rejection region of such a test is not
  invariant, but it changes with the ratios $n_i/N$ of the sample sizes."
- $H_{0P}$ "neither implies variance homogeneity nor equal shapes of the
  distributions"; for two samples this is the nonparametric Behrens–Fisher
  problem.
- ANOVA-type statistic (eq. 15):
  $A_N(C)=N\,\widehat\psi^\top A\widehat\psi\,/\,\mathrm{trace}(A\widehat V_N)$
  with $A=C^\top[CC^\top]^{+}C$, approximated by an $F$ distribution with
  $\widehat f_1,\widehat f_2$ degrees of freedom obtained via the Box-type
  approximation of Brunner et al. (1997), and attributed to Akritas et al.
  (1997) and Brunner et al. (2017).
- Sample-size guidance: the Wald-type statistic needs $n_i\ge 50$; the ATS
  "controls the type-I error much better in small sample sizes; $n_i \ge 15$
  depending on the design and hypothesis of interest".
- The $F$ approximation of $A_N(C)$ "is also valid under the more general
  hypothesis $H_{0P}$" — which is what makes ATSp available at all.

**Brunner, E., Konietschke, F., Pauly, M. and Puri, M. L. (2017).** Rank-based
procedures in factorial designs: hypotheses about non-parametric treatment
effects. *JRSS-B* 79(5), 1463–1485. <doi:10.1111/rssb.12222>
— proposes the ATS jointly with Akritas et al. (1997), per the attribution above.

**Brunner, E., Bathke, A. C. and Konietschke, F. (2018).** *Rank and Pseudo-Rank
Procedures for Independent Observations in Factorial Designs.* Springer.
<doi:10.1007/978-3-030-02914-2> — the textbook treatment; consulted for
orientation only, no claim here rests on it.

**R documentation read:** `rankFD::rankFD` (v0.1.1) and `stats::kruskal.test`.
The latter states its null as "the location parameters of the distribution of
`x` are the same in each group", which is the location reading of $H_{0F}$.

### What the sources explain about the simulation

Two results in Sections 1–2 stop being surprising once eq. (10) is read:

1. **Why KW and RK are sensitive to imbalance.** They estimate the *weighted*
   relative effects $\theta_i$, whose null depends on $n_i/N$. The rejection
   region moves with the allocation, so pairing large groups with small SDs
   changes what is being tested — the inflation is not a small-sample artefact,
   which is consistent with it persisting at $\bar n = 200$.
2. **Why ATSp is inflated at $\bar n = 10$.** The paper's own guidance is
   $n_i \ge 15$. Our worst ATSp levels (0.0747, 0.0644) occur exactly at
   $\bar n = 10$ and fall to nominal by $\bar n = 50$–100.

Note that the paper locates the severe rank/pseudo-rank divergence in *two- and
higher-way* layouts, calling one-way trouble the "extreme cases". Route 1 is
one-way, and the negative-pairing design is such an extreme case — which is an
argument that the corner is narrow, not that it is harmless.

## 8. Correction, 10 Aug 2026: omega^2 was mislabelled in every generated figure

Found by asking a question the construction itself answers: **if omega^2 is
held fixed, the parametric tests should have near-identical power across
designs.** They did not, and the discrepancy located a real error.

`omega_scaling_helpers.R` was rewritten during this session to the cited
Shieh (2012) / Kulinskaya & Staudte (2006) construction. Its previous version
used a simple allocation-weighted average of variances in the denominator,
which is not that formula. Every omega^2 computed before the rewrite is wrong
in the three heteroscedastic designs, and those values had been baked into the
figure labels.

**Main power figures** (`route1_power_figure.R`, via
`effect_sizes_by_design_panel_legacy.csv`, regenerated):

| design | labelled | correct | error |
|---|---|---|---|
| balanced n, equal SD | 0.0725 | 0.0725 | 0 |
| unbalanced n, equal SD | 0.0626 | 0.0626 | 0 |
| balanced n, unequal SD | 0.0725 | 0.0803 | +0.0078 |
| unbalanced n, larger n with larger SD | 0.0525 | 0.0748 | **+0.0224** |
| unbalanced n, larger n with smaller SD | 0.0778 | 0.0679 | −0.0098 |

The two homoscedastic designs are unaffected: the three formulas coincide when
all sigma_j are equal.

**omega^2-fixed reference grid**: worse, because the *simulation itself* used
the superseded scale factors. The CSV stores 0.0725 for all five designs, but
the shifts it actually simulated give:

```
unbalanced, larger n with smaller SD   0.0633
balanced, equal SD                     0.0725
unbalanced, equal SD                   0.0725
balanced, unequal SD                   0.0803
unbalanced, larger n with larger SD    0.1024
```

so that grid does not hold omega^2 fixed at all outside the homoscedastic
designs. The figure now recomputes omega^2 from the shifts and SDs actually
simulated and prints
`[NOT fixed: the grid was generated for 0.0725]` on the three affected rows,
rather than repeating the stored claim. Reproducing the intended grid needs a
rerun of `route1_power_omega_fixed.R` with the corrected `scale_omega_fixed()`.

### The diagnostic, which is worth keeping

Where omega^2 genuinely is fixed, the prediction holds exactly: across the two
homoscedastic designs Fisher's power at n = 20 is 0.5272 against 0.5305, a
difference of 0.0033 — at the Monte Carlo floor. Across all five designs
Welch's power orders **perfectly** with the recomputed omega^2 and not with the
stored value (Spearman rho = 1.000, n = 20):

| omega^2 (recomputed) | Welch power, n = 20 |
|---|---|
| 0.0633 | 0.4944 |
| 0.0725 | 0.5271 |
| 0.0725 | 0.5426 |
| 0.0803 | 0.5522 |
| 0.1024 | 0.6920 |

This is independent confirmation that the corrected heteroscedastic formula is
the right one: the simulated parametric power tracks it and ignores the number
the file claimed.

**Rule this establishes for the figure scripts:** never label a panel from an
effect-size column stored in a results file. Recompute it from the design
constants the simulation actually used, with the same function that defines the
quantity everywhere else, and fail loudly when the two disagree.
