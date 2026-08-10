# Audit: what rests on a read source, what rests on my assertion

Written 10 Aug 2026, after a session in which several claims turned out to be
derived rather than read. The point is that you should not have to re-derive
anything to know what to trust.

Three categories: **read** (I opened the source and the claim is in it),
**computed** (verified numerically by a script you can rerun, but *not* in any
literature I have read), and **unsourced** (rests on my assertion, or on a
source I have not read).

---

## A. Read — source opened, claim confirmed

| Claim | Source | What I read |
|---|---|---|
| `levene.test()` matches the standard R implementation | Gastwirth, Gel & Miao (2009) <doi:10.1214/09-STS301> | pp. 343–346; their Eq. 3 is the package's mean-centred formula |
| rank-biserial $r = 2W/(n_1n_2)-1$ | Wendt (1972) <doi:10.1002/ejsp.2420020412> | page located by you |
| Games–Howell procedure | Games & Howell (1976) <doi:10.2307/1164979> | named procedure only; implementation is original |
| Rank tests' level depends on $n_i/N$ under unequal variances | Brunner et al. (2017) <doi:10.1111/rssb.12222> | Table 2 p. 1477; power design p. 1480 |
| Design σ vectors, sample-size patterns | Brunner et al. (2017), Table 2, p. 1477 | σ ∈ {(1,1,1,1), (1,√2,2,√5), (√5,2,√2,1)}; n₁=(5,5,5,5), n₂=(10,20,30,40)+m |
| Brunner's power alternatives | Brunner et al. (2017), p. 1480 | $X_{ik}\sim N(\mu_i,1)$, $n_i\equiv n\in\{15,20\}$; one-point $(0,0,0,\delta)$ and trend $\delta(\tfrac14,\tfrac12,\tfrac34,1)$, $\delta=0,0.1,\dots,1.6$ |
| ATS definition and its $F$ reference | Konietschke & Brunner (2023) <doi:10.32614/RJ-2023-029> | eq. (15); Box-type df attributed to Brunner, Dette & Munk (1997) |
| ATS valid under H0p; WTS needs $n_i\ge50$, ATS $n_i\ge15$ | Konietschke & Brunner (2023) | same section |
| Weighted vs unweighted relative effects; rank tests target $\theta_i$, pseudo-rank tests $\psi_i$ | Konietschke & Brunner (2023) | eqs. (2)–(4), p. 145 |
| $H_{0P}$ in weighted effects "depends on the relative sample sizes $n_i/N$" | Konietschke & Brunner (2023) | remark after eq. (10) |
| KW consistency non-centrality $c^R_{KW}=\boldsymbol p'\boldsymbol P_a\boldsymbol p$, $c^\psi_{KW}=\boldsymbol\psi'\boldsymbol P_a\boldsymbol\psi$ | Brunner, Bathke & Konietschke (2018) <doi:10.1007/978-3-030-02914-2> | Result 4.14, p. 201 |
| $H(x)$, $G(x)$, $p_i$, $\psi_i$ definitions | Brunner et al. (2018) | eqs. (4.1)–(4.4), p. 186 |
| Nonparametric effects in factorial designs are described by $\psi$ and contrasts $\boldsymbol M\boldsymbol\psi$ | Brunner et al. (2018), §6.2 | eq. (6.1) and Schematic 6.2, pp. 336–337 |
| `kruskal.test()` null stated as equal location parameters | `stats::kruskal.test` Rd | read |
| `rankFD()` options, H0F/H0p semantics | `rankFD::rankFD` Rd v0.1.1 | read |

**Not read, and a claim rests on it:** Brunner, Dette & Munk (1997), which
derives the Box-type degrees of freedom. Not in the Zotero library. The ATS df
construction is therefore attributed, not verified.

**Not confirmed read in this session:** Steiger (2004), Carroll & Nordholm
(1975) and Shieh (2012), the three citations attached to the population $\omega^2$
equations in `_effect_size_table.Rmd`. The *formulas* are verified numerically
(section B), but I cannot confirm from this session's record that I opened those
three papers. Treat the attributions as unchecked until you or I read them.

---

## B. Computed — verified numerically, not in any literature I have read

Rerunnable; none of these may be presented as a citation.

| Result | Check |
|---|---|
| `population_omega_sq()` reduces exactly to the balanced and unbalanced-homoscedastic formulas | agreement to 6 decimals |
| $\widehat p_i=(\bar R_{i\cdot}-\tfrac12)/N$ estimates $\int H\,\mathrm dF_i$ | max abs error 0.001, unbalanced heteroscedastic |
| $\widehat\eta_H^2 \to 12\sum_i q_i(p_i-\tfrac12)^2$, $q_i=n_i/N$ | 7 configurations (balanced, unbalanced, heteroscedastic, both pairings, $a=3,4,5$); relative errors −0.57 % … +0.44 % |
| $\widehat\eta_H^2$ is stable in $N$ | 0.0681, 0.0699, 0.0701, 0.0700, 0.0697, 0.0702 at $n=10\dots200$ |
| ATSp does not reduce to KW; $df_1\to k-1$, $df_2\approx N-k$ only when balanced | direct `rankFD` comparison on identical data |
| Brunner's trend alternative at $\delta=1$ equals the package's shifts plus a constant | ω² identical to 6 decimals in 4 configurations |

The third row is the formula I defended for many turns as though it were
established. It is not. It is mine, it verifies numerically, and it is
deliberately **not** in the vignette.

---

## C. Unsourced — still resting on assertion

1. **`route1_simulations.R` design inputs.** SD = (1, 1.3, 1.7, 2.2) and
   multipliers (0.5, 0.8, 1.2, 1.5), against Brunner Table 2's
   (1, √2, 2, √5) and (10, 20, 30, 40). The design *names* follow the table;
   the numbers do not. Everything derived from `fleishman_4groups_power.rds`
   inherits this.
2. **`route1_simulations.R:40-42` comment is false.** It states that scaling
   shifts by `sqrt(mean(sd^2))` makes ω² "match the homoscedastic blocks". It
   gives 0.0803 against 0.0725. **Not yet corrected — outstanding.**
3. **Why Brunner writes σ as roots.** The variances are the integers
   (1, 2, 4, 5), which is an inference from the numbers. The paper does not
   state a rationale.

---

## D. Corrected during this session

- ω² in the main power figures: 3 of 5 rows were wrong (worst +0.0224).
  `effect_sizes_by_design_panel_legacy.csv` regenerated, figures rebuilt.
- `omega_scaling_helpers.R`: rewritten to the cited heteroscedastic
  construction; the previous version used a plain average of variances.
- Fabricated Levene (1960) citation removed from `R/levene.test.R`.
- Zimmerman (2004) → Brunner et al. (2017) in roxygen, the runtime warning
  string in `visstat_core.R`, and `_simulations.Rmd`.
- Kerby (2014) → Wendt (1972) for rank-biserial $r$.
- Three population ω² now carry distinct subscripts (bal/unbal/het) in both
  vignette and figures, derived from `omega_sq_regime()`.
- Figure labels never read a stored effect-size column; they recompute.

---

## E. New grid, completed 10 Aug 2026

`fleishman_4groups_power_design_brunner_B50000.csv` and
`rankfd_route1_power_design_brunner_B50000.csv`, 90 cells each: Brunner's σ,
shifts identical in every design, ω² reported as the consequence. The two
homoscedastic designs were not rerun — their inputs are identical to the
existing grid. **These figures have not been made yet and nothing has been
written from them.**

Holding the shifts fixed puts the heteroscedastic designs at roughly 40 % of the
homoscedastic effect (ω² = 0.0301, 0.0287, 0.0243 against 0.0725), which is what
the old 1.614 rescaling was concealing.
