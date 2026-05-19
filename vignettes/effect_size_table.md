Statistical significance is strongly affected by sample size, while effect-size
estimates are intended to support comparisons across studies regardless of
sample size [@Levine:2002].

To avoid additional package dependencies, `effect_size()` extracts, where possible, the effect sizes
from base R `stats` output where available and implements the remaining
formulae internally [@Hedges:1981; @Kerby:2014; @Olejnik:2003; @BenShachar:2020; @Kelley:1935; @Bergsma:2013].

| Analysis | R call | Effect size | Formula | Source |
|:---|:---|:---|:---|:---|
| [Student's $t$-test](#student-t) | `t.test(..., var.equal = TRUE)` | Hedges' $g_{s_p}$ (pooled) | $g_{s_p} = J\cdot(\bar{x}_1-\bar{x}_2)/s_p$ | [@Hedges:1981] |
| [Welch's $t$-test](#welch-t) | `t.test(..., var.equal = FALSE)` | Hedges' $g_{s^{*}}$ (non-pooled) | $g_{s^{*}} = J\cdot(\bar{x}_1-\bar{x}_2)/s^{*}$ | [@Hedges:1981] |
| [Wilcoxon rank-sum](#wilc) | `wilcox.test()` | rank-biserial $r$ | $r = 2\cdot W/(n_1\cdot n_2) - 1$ | [@Kerby:2014] |
| [Fisher's ANOVA](#fisher-aov) | `aov()` | $\omega^2$ | $\nu_1\cdot(F-1)/(\nu_1\cdot F + \nu_2 + 1)$ | [@Olejnik:2003] |
| [Welch's ANOVA](#welch-aov) | `oneway.test()` | $\omega^2$ (approx.) | $\nu_1\cdot(F_W-1)/(\nu_1\cdot F_W + \nu_2 + 1)$ | [@BenShachar:2020] |
| [Kruskal--Wallis](#kw) | `kruskal.test()` | Kelley-adjusted $\eta_H^2$ | $(H - k + 1)/(N - k)$ | [@Kelley:1935] |
| [Linear regression](#lin-reg) | `lm()` | $R^2$ | see [coefficient of determination](#r2) | `summary(lm())$r.squared` |
| [Spearman](#rho) | `cor.test(method = "spearman")` | $\rho$ | see [Spearman rank correlation](#rho) | `cor.test()$estimate` |
| [Kendall](#tau) | `cor.test(method = "kendall")` | $\tau_b$ | see [Kendall's $\tau_b$](#tau) | `cor.test()$estimate` |
| [Pearson $\chi^2$ ($R\times C$)](#pearson-chi) | `chisq.test()` | Cramér's $V$ | $V_{R\times C} = \sqrt{\chi^2/(N\cdot(\min(R,C)-1))}$ | [@Bergsma:2013] |
| [Pearson $\chi^2$ ($2\times 2$)](#pearson-chi) | `chisq.test()` | $\phi$ | $V_{2\times 2} = \sqrt{\chi^2/N}$ | [@Bergsma:2013] |
| [Fisher's exact ($2\times 2$)](#fisher-exact) | `fisher.test()` | odds ratio | see [odds ratio](#or) | `fisher.test()$estimate` |

where

$J$: Hedges' small-sample correction factor, $J = \Gamma((N-2)/2)\,/\,(\sqrt{(N-2)/2}\;\Gamma((N-3)/2))$,\
$s^{*}$: non-pooled average-variance standardizer, $s^{*} = \sqrt{(s_1^2+s_2^2)/2}$,\
$\nu_1$, $\nu_2$: numerator and denominator degrees of freedom; for Fisher's ANOVA, $\nu_1=k-1$ and $\nu_2=N-k$; for Welch's ANOVA, $\nu_1$ and $\nu_2$ are returned by `oneway.test()`.

All other variables used in the formulae above are defined in the corresponding "Analysis" section.
