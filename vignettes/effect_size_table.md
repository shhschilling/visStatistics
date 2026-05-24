Statistical significance is strongly affected by sample size, while effect-size
estimates are intended to support comparisons across studies regardless of
sample size [@Levine:2002].

To avoid additional package dependencies, `effect_size()` extracts, where possible, the effect sizes
from base R `stats` output where available and implements the remaining
formulae internally [@Hedges:1981; @Kerby:2014; @Olejnik:2003; @BenShachar:2020; @Kelley:1935; @Bergsma:2013].

```{r effect-size-table, echo=FALSE, results='asis'}
if (knitr::is_latex_output()) {
  cat(r"(
\begingroup
\scriptsize
\setlength{\tabcolsep}{2pt}
\setlength{\arrayrulewidth}{0.2pt}
\renewcommand{\arraystretch}{1.18}
\newcommand{\tbdoi}[2]{\href{https://doi.org/#1}{#2}}
\noindent
\begin{tabular}{@{}>{\raggedright\arraybackslash}p{0.23\textwidth}>{\raggedright\arraybackslash}p{0.17\textwidth}>{\raggedright\arraybackslash}p{0.37\textwidth}>{\raggedright\arraybackslash}p{0.19\textwidth}@{}}
\hline
\textbf{Analysis} & \textbf{Effect size} & \textbf{Formula} &
\textbf{Source} \\
\hline
Student's $t$-test, Eq.~\eqref{eq:student-t} &
Hedges' $g_{s_p}$ & $g_{s_p}=J(\bar{x}_1-\bar{x}_2)/s_p$ &
\tbdoi{10.3102/10769986006002107}{Hedges, 1981} \\
\hline
Welch's $t$-test, Eq.~\eqref{eq:welch-t} &
Hedges' $g_{s^*}$ & $g_{s^*}=J(\bar{x}_1-\bar{x}_2)/s^*$ &
\tbdoi{10.3102/10769986006002107}{Hedges, 1981} \\
\hline
Wilcoxon rank-sum, Eq.~\eqref{eq:wilcoxon-w} &
rank-biserial $r$ & $r=2W/(n_1n_2)-1$ &
\tbdoi{10.2466/11.IT.3.1}{Kerby, 2014} \\
\hline
Fisher's ANOVA, Eq.~\eqref{eq:fisher-f} & $\omega^2$ &
$\nu_1(F-1)/(\nu_1F+\nu_2+1)$ &
\tbdoi{10.1037/1082-989X.8.4.434}{Olejnik and Algina, 2003} \\
\hline
Welch's ANOVA, Eq.~\eqref{eq:welch-f} &
$\omega^2$ (approx.) &
$\nu_1(F_W-1)/(\nu_1F_W+\nu_2+1)$ &
\tbdoi{10.21105/joss.02815}{Ben-Shachar et al., 2020} \\
\hline
Kruskal--Wallis, Eq.~\eqref{eq:kruskal-h} &
Kelley-adjusted $\eta_H^2$ & $(H-k+1)/(N-k)$ &
\tbdoi{10.1073/pnas.21.9.554}{Kelley, 1935} \\
\hline
Linear regression, Section~\ref{sec:general-linear-model} &
$R^2$ &
$R^2=1-SS_\text{res}/SS_\text{tot}$ &
\texttt{summary(lm())\$r.squared} \\
\hline
Spearman, Eq.~\eqref{eq:spearman-rho} &
$\rho$ &
$\rho=r(\operatorname{rank}(x),\operatorname{rank}(y))$ &
\texttt{cor.test()} \\
\hline
Kendall, Eq.~\eqref{eq:kendall-tau-b} &
$\tau_b$ &
$\tau_b=(C-D)/\sqrt{\left(n_0-n_1\right)\left(n_0-n_2\right)}$ &
\texttt{cor.test()} \\
\hline
Pearson $\chi^2$ ($R\times C$), Eq.~\eqref{eq:pearson-chi} &
Cramer's $V$ &
$V=\sqrt{\chi^2/\left(N\left(\min(R,C)-1\right)\right)}$ &
\tbdoi{10.1016/j.jkss.2012.10.002}{Bergsma, 2013} \\
\hline
Pearson $\chi^2$ ($2\times2$), Eq.~\eqref{eq:pearson-chi} &
$\phi$ & $V=\sqrt{\chi^2/N}$ &
\tbdoi{10.1016/j.jkss.2012.10.002}{Bergsma, 2013} \\
\hline
Fisher's exact ($2\times2$), Eq.~\eqref{eq:fisher-exact} &
odds ratio &
$\widehat{\mathrm{OR}}=n_{11}n_{22}/(n_{12}n_{21})$ &
\texttt{fisher.test()} \\
\hline
\end{tabular}
\endgroup
)")
} else {
  cat(r"(
| Analysis | Effect size | Formula | Source |
|:---|:---|:---|:---|
| [Student's $t$-test](#eq:student-t) | Hedges' $g_{s_p}$ (pooled) | $g_{s_p} = J\cdot(\bar{x}_1-\bar{x}_2)/s_p$ | [@Hedges:1981] |
| [Welch's $t$-test](#eq:welch-t) | Hedges' $g_{s^{*}}$ (non-pooled) | $g_{s^{*}} = J\cdot(\bar{x}_1-\bar{x}_2)/s^{*}$ | [@Hedges:1981] |
| [Wilcoxon rank-sum](#eq:wilcoxon-w) | rank-biserial $r$ | $r = 2\cdot W/(n_1\cdot n_2) - 1$ | [@Kerby:2014] |
| [Fisher's ANOVA](#eq:fisher-f) | $\omega^2$ | $\nu_1\cdot(F-1)/(\nu_1\cdot F + \nu_2 + 1)$ | [@Olejnik:2003] |
| [Welch's ANOVA](#eq:welch-f) | $\omega^2$ (approx.) | $\nu_1\cdot(F_W-1)/(\nu_1\cdot F_W + \nu_2 + 1)$ | [@BenShachar:2020] |
| [Kruskal--Wallis](#eq:kruskal-h) | Kelley-adjusted $\eta_H^2$ | $(H - k + 1)/(N - k)$ | [@Kelley:1935] |
| Linear regression | $R^2$ | $R^2 = 1 - SS_\text{res}/SS_\text{tot}$ | `summary(lm())$r.squared` |
| [Spearman](#eq:spearman-rho) | $\rho$ | $\rho = r(\operatorname{rank}(x),\operatorname{rank}(y))$, Eq. \@ref(eq:spearman-rho) | `cor.test()$estimate` |
| [Kendall](#eq:kendall-tau-b) | $\tau_b$ | $\tau_b = (C-D)/\sqrt{\left(n_0-n_1\right)\left(n_0-n_2\right)}$, Eq. \@ref(eq:kendall-tau-b) | `cor.test()$estimate` |
| [Pearson $\chi^2$ ($R\times C$)](#eq:pearson-chi) | Cramér's $V$ | $V_{R\times C} = \sqrt{\chi^2/\left(N\cdot(\min(R,C)-1)\right)}$ | [@Bergsma:2013] |
| [Pearson $\chi^2$ ($2\times 2$)](#eq:pearson-chi) | $\phi$ | $V_{2\times 2} = \sqrt{\chi^2/N}$ | [@Bergsma:2013] |
| [Fisher's exact ($2\times 2$)](#eq:fisher-exact) | odds ratio | $\widehat{\text{OR}} = n_{11}n_{22}/(n_{12}n_{21})$, Eq. \@ref(eq:odds-ratio) | `fisher.test()$estimate` |
)")
}
```

Here:

- $J$: Hedges' small-sample correction factor,
  $J = \Gamma((N-2)/2)\,/\,(\sqrt{(N-2)/2}\;\Gamma((N-3)/2))$.
- $s^{*}$: non-pooled average-variance standardizer,
  $s^{*} = \sqrt{(s_1^2+s_2^2)/2}$.
- $\nu_1$, $\nu_2$: numerator and denominator degrees of freedom; for
  Fisher's ANOVA, $\nu_1=k-1$ and $\nu_2=N-k$; for Welch's ANOVA,
  $\nu_1=k-1$ and $\nu_2$ is the usually fractional denominator degree
  of freedom returned by `oneway.test()`.
- $SS_\text{res}=\sum_i(Y_i-\hat{Y}_i)^2$ is the residual sum of squares,
where $\hat{Y}_i$ is the predicted value, and
- $SS_\text{tot}=\sum_i(Y_i-\bar{Y})^2$ is the total sum of squares.

All other variables used in the formulae above are defined in the corresponding "Analysis" section.
