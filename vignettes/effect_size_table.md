Statistical significance is strongly affected by sample size, while effect-size
estimates are intended to support comparisons across studies regardless of
sample size [@Levine:2002].

To avoid additional package dependencies, `effect_size()` extracts, where possible, the effect sizes
from base R `stats` output where available and implements the remaining
formulae internally [@Hedges:1981; @Kerby:2014; @Olejnik:2003; @BenShachar:2020; @Kelley:1935; @Bergsma:2013].

| Analysis | R call | Effect size | Formula | Source |
|:---|:---|:---|:---|:---|
| [Student's $t$-test](#eq:student-t) | `t.test(..., var.equal = TRUE)` | Hedges' $g_{s_p}$ (pooled) | $g_{s_p} = J\cdot(\bar{x}_1-\bar{x}_2)/s_p$ | [@Hedges:1981] |
| [Welch's $t$-test](#eq:welch-t) | `t.test(..., var.equal = FALSE)` | Hedges' $g_{s^{*}}$ (non-pooled) | $g_{s^{*}} = J\cdot(\bar{x}_1-\bar{x}_2)/s^{*}$ | [@Hedges:1981] |
| [Wilcoxon rank-sum](#eq:wilcoxon-w) | `wilcox.test()` | rank-biserial $r$ | $r = 2\cdot W/(n_1\cdot n_2) - 1$ | [@Kerby:2014] |
| [Fisher's ANOVA](#eq:fisher-f) | `aov()` | $\omega^2$ | $\nu_1\cdot(F-1)/(\nu_1\cdot F + \nu_2 + 1)$ | [@Olejnik:2003] |
| [Welch's ANOVA](#eq:welch-f) | `oneway.test()` | $\omega^2$ (approx.) | $\nu_1\cdot(F_W-1)/(\nu_1\cdot F_W + \nu_2 + 1)$ | [@BenShachar:2020] |
| [Kruskal--Wallis](#eq:kruskal-h) | `kruskal.test()` | Kelley-adjusted $\eta_H^2$ | $(H - k + 1)/(N - k)$ | [@Kelley:1935] |
| Linear regression | `lm()` | $R^2$ | $R^2 = 1 - SS_\text{res}/SS_\text{tot}$ | `summary(lm())$r.squared` |
| [Spearman](#eq:spearman-rho) | `cor.test(method = "spearman")` | $\rho$ | $\rho = r(\operatorname{rank}(x),\operatorname{rank}(y))$, Eq. \@ref(eq:spearman-rho) | `cor.test()$estimate` |
| [Kendall](#eq:kendall-tau-b) | `cor.test(method = "kendall")` | $\tau_b$ | $\tau_b = (C-D)/\sqrt{(n_0-n_1)(n_0-n_2)}$, Eq. \@ref(eq:kendall-tau-b) | `cor.test()$estimate` |
| [Pearson $\chi^2$ ($R\times C$)](#eq:pearson-chi) | `chisq.test()` | Cramér's $V$ | $V_{R\times C} = \sqrt{\chi^2/(N\cdot(\min(R,C)-1))}$ | [@Bergsma:2013] |
| [Pearson $\chi^2$ ($2\times 2$)](#eq:pearson-chi) | `chisq.test()` | $\phi$ | $V_{2\times 2} = \sqrt{\chi^2/N}$ | [@Bergsma:2013] |
| [Fisher's exact ($2\times 2$)](#eq:fisher-exact) | `fisher.test()` | odds ratio | $\widehat{\text{OR}} = n_{11}n_{22}/(n_{12}n_{21})$, Eq. \@ref(eq:odds-ratio) | `fisher.test()$estimate` |

Here:

- $J$: Hedges' small-sample correction factor,
  $J = \Gamma((N-2)/2)\,/\,(\sqrt{(N-2)/2}\;\Gamma((N-3)/2))$.
- $s^{*}$: non-pooled average-variance standardizer,
  $s^{*} = \sqrt{(s_1^2+s_2^2)/2}$.
- $\nu_1$, $\nu_2$: numerator and denominator degrees of freedom; for
  Fisher's ANOVA, $\nu_1=k-1$ and $\nu_2=N-k$; for Welch's ANOVA,
  $\nu_1$ and $\nu_2$ are returned by `oneway.test()`.
- $SS_\text{res}=\sum_i(Y_i-\hat{Y}_i)^2$ is the residual sum of squares,
where $\hat{Y}_i$ is the predicted value, and
- $SS_\text{tot}=\sum_i(Y_i-\bar{Y})^2$ is the total sum of squares.

All other variables used in the formulae above are defined in the corresponding "Analysis" section.
