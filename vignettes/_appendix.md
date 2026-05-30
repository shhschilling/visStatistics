# (APPENDIX) Appendix {.unnumbered}

```{=latex}
\numberwithin{equation}{section}
```

# Assumption tests {#sec:assumption-statistics}

## Normality tests {#sec:norm}






### Shapiro--Wilk test `shapiro.test()` {#sec:shap}

The Shapiro--Wilk test evaluates whether a sample $x_1,\ldots,x_n$ comes from a normal distribution.
Let $x_{(1)}\le \cdots \le x_{(n)}$ be its order statistics.
Introduce a reference sample $Z_1,\ldots,Z_n$ of independent standard normal random variables, i.e. $Z_i \sim N(0,1)$ for all $i$, and let $Z_{(1)}\le \cdots \le Z_{(n)}$ be their order statistics used to construct the Shapiro--Wilk weights.

Let $m_i = \operatorname{E}(Z_{(i)})$ and $v_{ij} = \operatorname{Cov}(Z_{(i)}, Z_{(j)})$ for $i,j = 1,\ldots,n$.
Define $\mathbf{m} = (m_1,\ldots,m_n)^\top$ and $V = (v_{ij})_{i,j=1}^n$.

The vector $\mathbf{m}$ contains the expected standard-normal order statistics, and $V$ is their covariance matrix.
Let $\mathbf{a}=(a_1,\ldots,a_n)^\top$ be the resulting vector of normalised weights for the ordered observed sample values

$$\mathbf{a}
=\frac{V^{-1}\mathbf{m}}
{\sqrt{\left(\mathbf{m}^\top V^{-1}V^{-1}\mathbf{m}\right)}}.$$ Then the Shapiro--Wilk statistic [@Shapiro:1965] is

\begin{equation}
W=\frac{\left(\sum_{i=1}^{n} a_i x_{(i)}\right)^2}
{\sum_{i=1}^{n} (x_i-\bar{x})^2}
(\#eq:shapiro-w)
\end{equation}

$W$ takes values in $(0, 1]$; values close to 1 indicate normality.

### Anderson--Darling test `ad.test()` {#sec:adar}


Let $z_i = (x_{(i)} - \bar{x})/s,\; i=1,2,\ldots,n$ be the standardised order statistics of $x_i$, where $s$ is the sample standard deviation, and let $\Phi$ denote the standard normal cumulative distribution function.
The test statistic is

\begin{equation}
A^2 = -n - \frac{1}{n}\sum_{i=1}^{n}(2i-1)
        \left[\ln\Phi(z_i) + \ln\!\left(1 - \Phi(z_{n+1-i})\right)\right]
(\#eq:anderson-a2)
\end{equation}

The implementation uses `ad.test()` from `nortest` [@Gross:2015].

## Homoscedasticity tests {#sec:homo}

### The mean-centred Levene test `levene.test()` {#sec:lev}

The package implementation uses Levene's original mean-centred proposal [@Levene:1960].

The Levene test statistic is the one-way ANOVA $F$ statistic, computed on the
absolute residuals $|e_{ij}|$ in place of the responses $x_{ij}$; the
corresponding Fisher ANOVA formula is given in Eq.\ \@ref(eq:fisher-f):

\begin{equation}
F_L = \frac{\displaystyle\sum_{i=1}^{k} n_i (\overline{|e|}_i - \overline{|e|})^2\;/\;(k-1)}
         {\displaystyle\sum_{i=1}^{k}\sum_{j=1}^{n_i}(|e_{ij}| - \overline{|e|}_i)^2\;/\;(N-k)},
(\#eq:levene-f)
\end{equation}

where $\overline{|e|}_i$ is the within-group mean of the absolute residuals and $\overline{|e|}$ is their overall mean.

### Bartlett's test `bartlett.test()` {#sec:bart}

Bartlett's test statistic [@Bartlett:1937] is

\begin{equation}
K^2 = \frac{(N-k)\ln s_p^2 - \displaystyle\sum_{i=1}^k (n_i-1)\ln s_i^2}
{1 + \dfrac{1}{3(k-1)}\!\left(\displaystyle\sum_{i=1}^k \frac{1}{n_i-1} - \frac{1}{N-k}\right)},
(\#eq:bartlett-k2)
\end{equation}

where $k$ is the number of groups, $N = \sum_{i=1}^k n_i$ is the total sample size, $n_i$ is the sample size of group $i$, $s_i^2$ is the sample variance of group $i$, and $s_p^2$ is the pooled variance

$$s_p^2 = \frac{1}{N-k}\sum_{i=1}^k (n_i-1)\,s_i^2.$$

Under the null hypothesis the statistic approximately follows $\chi^2(k-1)$.

### Breusch--Pagan test `bp.test()` {#sec:bp}

For simple linear regression, group-based variance tests are not applicable.
The package implementation `bp.test()` performs the Koenker variant [@Koenker:1981] of the Breusch--Pagan test [@Breusch:1979], which tests whether the $N$ squared residuals $e_i^2$ vary systematically with the fitted values from the regression model $\hat{y}_i$.

The Breusch--Pagan statistic is defined as:

\begin{equation}
BP = N R^2_\text{aux}
(\#eq:breusch-pagan-bp),
\end{equation}

where $R^2_\text{aux}$ denotes the coefficient of determination from regressing $e_i^2$ on $\hat{y}_i$:

$$R^2_\text{aux}
= 1 -
\frac{\sum_{i=1}^{N} (e_i^2 - \widehat{e_i^2})^2}
     {\sum_{i=1}^{N} (e_i^2 - \overline{e^2})^2}.$$

Here $\widehat{e_i^2}$ are the fitted values from this auxiliary regression and $\overline{e^2}$ is the mean of the squared residuals.

Under the null hypothesis of homoscedasticity, $BP$ is compared asymptotically to a $\chi^2(k-1)$ distribution.

# Parametric tests {#sec:tests}

In the numeric-response, categorical-predictor branch (Route 1), parametric tests
are selected when residual normality is not rejected, or when all
group-specific sample sizes are greater than 50. The Levene variance gate
then separates equal-variance tests from Welch-type tests.

## Student's t-test and Fisher's one-way ANOVA

### Student's t-test `t.test(..., var.equal = TRUE)`{#sec:tt}

Student's t-test tests the null hypothesis that the means of two
unpaired groups are equal.
The test statistic for Student's t-test
(`t.test(..., var.equal = TRUE)`) is

\begin{equation}
t = \frac{\bar{x}_1 - \bar{x}_2}
{s_p \sqrt{\dfrac{1}{n_1} + \dfrac{1}{n_2}}},
(\#eq:student-t)
\end{equation}

where $\bar{x}_1$ and $\bar{x}_2$ are the sample means, $n_1$ and $n_2$
the sample sizes, and $s_p$ the pooled standard deviation

$$s_p =
\sqrt{\frac{(n_1-1)s_1^2 + (n_2-1)s_2^2}{n_1+n_2-2}},$$

with $s_1^2$ and $s_2^2$ the sample variances.
The statistic follows a $t$-distribution with
$\nu = n_1 + n_2 - 2$ degrees of freedom.

### Fisher's one-way ANOVA `aov()`{#sec:fisher-aov}

Fisher's one-way ANOVA generalises the mean comparison to more than two
groups and tests the null hypothesis that the means of
\(k\) groups are equal.
Fisher's ANOVA test statistic is

\begin{equation}
\begin{aligned}
F &= \frac{MS_\text{between}}{MS_\text{within}}
   = \frac{SS_\text{between}/(k-1)}{SS_\text{within}/(N-k)}
   = \frac{\displaystyle\sum_{i=1}^{k} n_i
          (\bar{x}_i - \bar{x})^2\;/\;(k-1)}
          {\displaystyle\sum_{i=1}^{k}\sum_{j=1}^{n_i}
          (x_{ij}-\bar{x}_i)^2\;/\;(N-k)}
\end{aligned},
(\#eq:fisher-f)
\end{equation}

where $MS_\text{between}$ and $MS_\text{within}$ are the between-group
and within-group mean squares, $SS_\text{between}$ and
$SS_\text{within}$ are the corresponding sums of squares, $k$ is the
number of groups, $N = \sum_{i=1}^k n_i$ is the total sample size,
$\bar{x}_i$ is the mean of group $i$, $\bar{x}$ is the overall mean,
and $x_{ij}$ is observation $j$ in group $i$.

From Eq.\ \@ref(eq:fisher-f) follows that in the two-sample case
($k=2$), the squared test statistic of Student's t-test equals the
Fisher ANOVA test statistic, $t^2 = F$, resulting in identical
$p$-values for `t.test(var.equal = TRUE)` and `aov()`.

Under \(H_0: \mu_1 = \cdots = \mu_k\), the statistic follows
\(F(k-1, N-k)\).

#### Post-hoc comparison {#sec:tukey}

`visstat()` follows `aov()` with Tukey's
Honest Significant Differences procedure `TukeyHSD()` [@Tukey:1949].
The procedure is designed for pairwise mean comparisons following ANOVA.

`TukeyHSD()` returns adjusted p-values and confidence intervals for all
pairwise differences between factor-level means.
For two groups \(i\) and \(j\), let
\(d_{ij} = \bar{x}_i - \bar{x}_j\). The studentised range statistic is

\begin{equation}
q_{ij} =
\frac{|d_{ij}|}
{\sqrt{\dfrac{MS_\text{within}}{2}
\left(\dfrac{1}{n_i} + \dfrac{1}{n_j}\right)}},
(\#eq:tukey-hsd-q)
\end{equation}

where \(MS_\text{within}\) is defined in Eq.\ \@ref(eq:fisher-f).
Adjusted p-values are computed from the studentised range distribution
with \(k\) groups and \(N-k\) residual degrees of freedom.

## Welch's t-test and Welch's heteroscedastic ANOVA

Welch's heteroscedastic ANOVA generalises the unequal-variance mean
comparison to more than two groups.

### Welch's t-test `t.test()`{#sec:welch-tt}

Welch's t-test (`t.test(..., var.equal = FALSE)`) compares the means of
two independent groups when homogeneous variances cannot be assumed.
Its statistic is

\begin{equation}
t = \frac{\bar{x}_1 - \bar{x}_2}
{\sqrt{s_1^2/n_1 + s_2^2/n_2}}
(\#eq:welch-t)
\end{equation}

with degrees of freedom approximated by the Welch--Satterthwaite
equation [@Welch:1947; @Satterthwaite:1946]:

\begin{equation}
\nu \approx
\frac{\left(\dfrac{s_1^2}{n_1} + \dfrac{s_2^2}{n_2}\right)^2}
{\dfrac{(s_1^2/n_1)^2}{n_1-1}
 + \dfrac{(s_2^2/n_2)^2}{n_2-1}}.
(\#eq:welch-satterthwaite-df)
\end{equation}

Welch's methods outperform their classical counterparts when variances
differ [@Moser:1992; @Fagerland:2009; @Delacre:2017].

### Welch's heteroscedastic ANOVA `oneway.test()`{#sec:welch-aov}

Welch's heteroscedastic ANOVA (`oneway.test()`) generalises Welch's
t-test to more than two groups by down-weighting groups with large
variance.
It compares group means using weights based on sample sizes and
variances when homogeneous variances cannot be assumed.
Its test statistic is

\begin{equation}
F_W =
\frac{\displaystyle\sum_{i=1}^{k} w_i
(\bar{x}_i - \bar{x}_w)^2\;/\;(k-1)}
{1 + \dfrac{2(k-2)}{k^2-1}
\displaystyle\sum_{i=1}^{k} \dfrac{(1-w_i/w)^2}{n_i-1}},
(\#eq:welch-f)
\end{equation}

where $w_i = n_i/s_i^2$ are the inverse-variance weights,
$w = \sum_{i=1}^{k} w_i$, and
$\bar{x}_w = \sum_{i=1}^{k} w_i \bar{x}_i / w$ is the weighted grand
mean. The numerator degree of freedom is $k-1$; the denominator degree
of freedom is the Satterthwaite-type approximation returned by
`oneway.test()`.

#### Post-hoc comparison `games.howell()`{#sec:gh}

Post-hoc comparisons use the package implementation `games.howell()`
[@Games:1976].
The Games--Howell procedure is used for pairwise mean comparisons under
unequal variances and unequal sample sizes.
For each pairwise comparison, the two groups are denoted as 1 and 2.
The test statistic is

\begin{equation}
t = \frac{d}{SE},
(\#eq:games-howell-t)
\end{equation}

where $d = \bar{x}_1 - \bar{x}_2$ is the mean difference and
$SE = \sqrt{s_1^2/n_1 + s_2^2/n_2}$ its standard error.

Eq.\ \@ref(eq:games-howell-t) is evaluated against a $t$ distribution
with $\nu$ degrees of freedom from the Welch--Satterthwaite
approximation in Eq.\ \@ref(eq:welch-satterthwaite-df).
The resulting two-sided $p$-values are adjusted with Holm's method
[@Holm:1979].

# Non-parametric tests

In the numeric-response, categorical-predictor branch, non-parametric
tests are selected when residual normality is rejected. They are also
used for an ordered response with a categorical predictor.

## Wilcoxon rank-sum test and Kruskal--Wallis test

The Wilcoxon rank-sum test is a two-group rank-based location test;
Kruskal--Wallis generalises this location comparison to more than two
groups.

### Wilcoxon rank-sum test `wilcox.test()`{#sec:wilc}

The two-sample Wilcoxon rank-sum test, also known as the Mann--Whitney
test, tests for a difference in location between two independent
distributions.
<!-- The Wilcoxon rank-sum test should not generally be read as a median -->
<!-- test. -->
<!-- Only under additional distributional assumptions, such as a common-shape -->
<!-- location shift, does a location-shift interpretation also imply a median -->
<!-- shift [@Fay:2010]. -->

To compare the two groups on one common rank scale, all observations are
pooled before ranking. For two independent groups with sample sizes
$n_1$ and $n_2$, all $N = n_1 + n_2$ observations are assigned ranks
$1$ to $N$.
Let $W_1 = \sum_{i=1}^{n_1} R(x_{1,i})$ denote the rank sum of the first
group, where $R(x_{1,i})$ is the rank of observation $x_{1,i}$ in the
pooled sample. The test statistic returned by `wilcox.test()` is the
Mann--Whitney $U$ statistic [@Mann:1947] of the first group:

\begin{equation}
W = U_1 = W_1 - \frac{n_1(n_1+1)}{2}
(\#eq:wilcoxon-w)
\end{equation}

An exact $p$-value is computed when both groups contain fewer than
50 observations and the data contain no ties; otherwise a normal
approximation with continuity correction is used.

### Kruskal--Wallis test `kruskal.test()`{#sec:kw}

The Kruskal--Wallis test compares group distributions based on ranked
values and tests the null hypothesis that the groups come from the same
population,
specifically that the distributions have the same location [@Kruskal:1952].
If the group distributions are sufficiently similar in shape and scale,
the test can be interpreted as testing equality of medians across groups
[@Hollander:2014].

\begin{equation}
H = \frac{12}{N(N+1)} \sum_{i=1}^{k}
n_i \left(\bar{R}_i - \bar{R}\right)^2,
(\#eq:kruskal-h)
\end{equation}

where $n_i$ is the sample size of group $i$, $k$ is the number of groups,
$\bar{R}_i$ is the average rank of group $i$, $N$ is the total sample
size, and $\bar{R} = (N+1)/2$ is the expected average rank under the
null hypothesis. The statistic approximately follows $\chi^2(k-1)$.

#### Post-hoc comparison `pairwise.wilcox.test()`{#sec:pairwise-wilcox}

`pairwise.wilcox.test()` compares each pair of factor levels via the
Wilcoxon rank-sum test on ranks rather than means.
The resulting p-values are adjusted for multiplicity using Holm's
step-down method [@Holm:1979].

# Tests for comparing proportions

For two unordered factors (route 4), `visstat()` tests the null hypothesis that
the variables are independent using Pearson's \(\chi^2\) test or
Fisher's exact test, depending on expected cell counts following
Cochran's rule [@Cochran:1954].

## Pearson's $\chi^2$ test `chisq.test()`{#sec:chi}

Pearson's \(\chi^2\) test evaluates the null hypothesis that two
categorical variables are independent.

Let $O_{ij}$ and $E_{ij}$ denote the observed and expected frequencies in
row $i$ and column $j$ of an $R \times C$ contingency table, where rows
index the $R$ levels of the response $y$ and columns the $C$ levels of
the predictor $x$.
The Pearson residual for cell $(i,j)$ is
\begin{equation}
r_{ij} = \frac{O_{ij} - E_{ij}}{\sqrt{E_{ij}}},
\quad i = 1,\ldots,R,\quad j = 1,\ldots,C.
(\#eq:pearson-residual)
\end{equation}
<!--
# The tiles in the mosaic plots [@Meyer:2006; @Meyer:2024] accompanying 
# Pearson's $\chi^2$ tests selection 
# represent Pearson residual values on a blue--red colour scale.
-->
The test statistic of Pearson's $\chi^2$ test is

\begin{equation}
\chi^2 = \sum_{i=1}^{R}\sum_{j=1}^{C} r_{ij}^2,
(\#eq:pearson-chi)
\end{equation}

Under the null hypothesis of independence, the statistic is compared with a
\(\chi^2\) distribution with $(R-1)(C-1)$ degrees of freedom.

For $2\times 2$ tables, Yates' continuity correction is applied by
default.



## Fisher's exact test `fisher.test()` {#sec:fisher-exact-test}

Fisher's exact test (`fisher.test()`) is applied when Cochran's rule
[@Cochran:1954] is violated. The test calculates an exact $p$-value by
conditioning on the observed margins of an $R \times C$ contingency
table under the null hypothesis of independence.
Let $T = (n_{ij})$ denote the observed table.
To maintain consistency with the `y ~ x` (response \~ predictor)
framework used throughout `visStatistics`, rows ($i=1,\ldots,R$)
represent the levels of the response variable $y$ and columns
($j=1,\ldots,C$) represent the levels of the predictor $x$.
The row totals are $n_{i\cdot} = \sum_{j=1}^C n_{ij}$ and the column
totals are $n_{\cdot j} = \sum_{i=1}^R n_{ij}$.

In the $2 \times 2$ case ($R=2, C=2$), the table is structured as
follows:

$$\begin{array}{c|cc|c}
& x_1 & x_2 & \text{Row sums} \\
\hline
y_1 & n_{11} & n_{12} & n_{1\cdot} \\
y_2 & n_{21} & n_{22} & n_{2\cdot} \\
\hline
\text{Column sums} & n_{\cdot 1} & n_{\cdot 2} & N
\end{array}$$

The exact probability of observing this table under the null hypothesis
of independence, given the fixed margins, is given by the hypergeometric
probability mass function:

\begin{equation}
\mathbb{P}(T \mid n_{1\cdot}, n_{2\cdot}, n_{\cdot 1}, n_{\cdot 2})
= \frac{\binom{n_{1\cdot}}{n_{11}}
        \binom{n_{2\cdot}}{n_{21}}}{\binom{N}{n_{\cdot 1}}},
(\#eq:fisher-exact)
\end{equation}

where $N = n_{1\cdot} + n_{2\cdot}
= n_{\cdot 1} + n_{\cdot 2}$ is the total sample size.
The $p$-value is computed by summing the probabilities of all tables with
the same margins whose probabilities under the null are less than or
equal to that of the observed table.
For general $R \times C$ tables, `fisher.test()` generalises this
approach using the multivariate hypergeometric distribution.
For $2 \times 2$ tables, `fisher.test()` additionally returns the
conditional maximum likelihood estimate of the odds ratio

\begin{equation}
\widehat{\mathrm{OR}} = n_{11}n_{22}/(n_{12}n_{21})
(\#eq:odds-ratio)
\end{equation}

and its confidence interval.

# Rank correlations {#sec:rank-correlations}

Rank correlations are used when `correlation = TRUE`.

## Kendall rank correlation `cor.test(..., method="kendall")`{#sec:tau}

Kendall's \(\tau_b\) tests the null hypothesis of no monotone
association between two ordered variables.
For two ordinal variables with $n$ joint observations, let $C$ denote the
number of concordant pairs (those whose ranks agree in both variables)
and $D$ the number of discordant pairs.
Kendall's $\tau_b$ is defined as

\begin{equation}
\tau_b \;=\; \frac{C - D}
{\sqrt{\left(n_0 - n_1\right)\left(n_0 - n_2\right)}},
(\#eq:kendall-tau-b)
\end{equation}

where $n_0 = n(n-1)/2$ is the total number of observation pairs,
$n_1 = \sum_i t_i(t_i-1)/2$ is the number of pairs tied in the response,
and $n_2 = \sum_j u_j(u_j-1)/2$ is the number of pairs tied in the
predictor. The denominator correction makes $\tau_b$ attain $\pm 1$ even
with ties, which Spearman's $\rho$ does not [@Kendall:1945].
With few ordered levels (e.g., five-point Likert items), ties are
unavoidable; this is the principal reason to prefer $\tau_b$ over
Spearman's $\rho$ in this setting [@Agresti:2010].

`visstat()` calls
`cor.test(as.numeric(y), as.numeric(x), method = "kendall",
exact = FALSE)` and reports $\tau_b$, the asymptotic test statistic
$z = \tau_b / \operatorname{SE}(\tau_b)$, and the two-sided $p$-value.

## Spearman rank correlation `cor.test(..., method="spearman")` {#sec:rho}

For two numeric variables with `correlation = TRUE`, `visstat()` calls
`cor.test(x, y, method = "spearman")` to test for a monotone
association between $x$ and $y$ using ranks.
Spearman's $\rho$ is Pearson's $r$ applied to the ranks:

\begin{equation}
\rho = r(\operatorname{rank}(x), \operatorname{rank}(y)),
(\#eq:spearman-rho)
\end{equation}

where $r(u, v)$ denotes Pearson's correlation coefficient:

$$r(u,v)
=
\frac{\sum_{i=1}^{n}(u_i-\bar u)(v_i-\bar v)}
{\sqrt{\sum_{i=1}^{n}(u_i-\bar u)^2}\,
 \sqrt{\sum_{i=1}^{n}(v_i-\bar v)^2}}.$$

Here $u_i = \operatorname{rank}(x_i)$ and
$v_i = \operatorname{rank}(y_i)$ are the ranks of the $n$ paired
observations, and $\bar{u}$ and $\bar{v}$ are their sample means.

For inference, `cor.test(..., method = "spearman")` computes an exact
$p$-value for small samples without ties by evaluating all $n!$ rank
permutations. For larger samples or when ties are present, it uses an
approximation to the null distribution of the rank association measure
or its asymptotic transformation. No distributional assumptions on the
original data are required.

A separate Pearson-correlation branch is not implemented.
In simple linear regression with an intercept, the two-sided test of zero
slope and the two-sided test of zero Pearson correlation return the same
$p$-value. Pearson correlation would therefore not add a separate
inferential route to the default regression branch.


<!--
# Influence diagnostics: Cook's distance {#sec:cooks-distance}

For simple linear regression, the leverage plot visualises whether individual
observations may exert disproportionate influence on the fitted line. Cook's
distance combines residual size and leverage for this purpose [@Cook:1982].
In `visStatistics`, the contours are drawn on the z residual scale used in the
diagnostic panel. With $z_i$ defined in Eq.\ \@ref(eq:z-residual), Cook's
distance for observation $i$ is

\begin{equation}
D_i = \frac{z_i^2 h_i}{k(1-h_i)^2},
(\#eq:cooks-distance-z)
\end{equation}

with leverage

\begin{equation}
h_i = \frac{1}{N} +
      \frac{(x_i-\bar{x})^2}{\sum_{r=1}^{N}(x_r-\bar{x})^2}.
(\#eq:leverage-simple-regression)
\end{equation}

Here $x_i$ is the predictor value of observation $i$, $\bar{x}$ is the
predictor mean, $N$ is the total sample size, and $k = 2$ is the number of
fitted model parameters.
-->

# Effect size `effect_size()` {#sec:effect-size}



The effect size  takes the value zero when the null hypothesis is true and some other, test- specific non-zero value when the null hypothesis is false, it is an an index of degree of departure from the null hypothesis [@Cohen:2013; page 10]

While  statistical significance is strongly affected by sample size, effect-size, estimates are intended to support comparisons across studies regardless of
sample size [@Levine:2002]   Effect size is therefore an important determinant of power or required sample size or both  [@Cohen:2013; page 10].

To avoid additional package dependencies, `effect_size()` 
extracts, where possible, the effect sizes 
from base R `stats` output, Otherwise it implements the remaining
formulae internally.


The following tables summarises the statistical analysis with their respective effect sizes and formulae. 
<!-- The internal function  `effect_size()` extracts, where possible, the effect sizes -->
<!-- from base R `stats` output, Otherwise it implements the remaining -->
<!-- formulae internally. -->

```{r effect-size-table, echo=FALSE, results='asis'}
if (knitr::is_latex_output()) {
  cat(r"(
\begin{table}[!htbp]
\caption{Effect sizes returned by \texttt{effect\_size()}.}
\label{tab:effect-size-formulae}
\centering
\begingroup
\scriptsize
\setlength{\tabcolsep}{2pt}
\setlength{\arrayrulewidth}{0.2pt}
\renewcommand{\arraystretch}{1.18}
\newcommand{\tbdoi}[2]{\href{https://doi.org/#1}{#2}}
\begin{tabular}{@{}>{\raggedright\arraybackslash}p{0.23\textwidth}>{\raggedright\arraybackslash}p{0.17\textwidth}>{\raggedright\arraybackslash}p{0.37\textwidth}>{\raggedright\arraybackslash}p{0.19\textwidth}@{}}
\hline
\textbf{Analysis} & \textbf{Effect size} & \textbf{Formula} &
\textbf{Source} \\
\hline
Student's $t$-test &
Hedges' $g_{s_p}$ & $g_{s_p}=J(N-2)(\bar{x}_1-\bar{x}_2)/s_p$ &
\tbdoi{10.3102/10769986006002107}{Hedges, 1981} \\
\hline
Welch's $t$-test &
Hedges' $g_{s^{*}}$ & $g_{s^{*}}=J(\nu^{*})(\bar{x}_1-\bar{x}_2)/s^{*}$ &
\tbdoi{10.31234/osf.io/tu6mp}{Delacre et al., 2021} \\
\hline
Wilcoxon rank-sum &
rank-biserial $r$ & $r=2W/(n_1n_2)-1$ &
\href{https://journals.sagepub.com/doi/abs/10.2466/11.it.3.1}{Kerby, 2014} \\
\hline
Fisher's ANOVA & $\omega^2$ &
$\nu_1(F-1)/(\nu_1F+\nu_2+1)$ &
\tbdoi{10.1016/j.jesp.2017.09.004}{Albers and Lakens, 2018, Appendix A} \\
\hline
Welch's ANOVA &
$\omega^2$ (approx.) &
$\nu_1(F_W-1)/(\nu_1F_W+\nu_2+1)$ &
\tbdoi{10.1016/j.jesp.2017.09.004}{F-form from Albers and Lakens, 2018, Appendix A} \\
\hline
Kruskal--Wallis &
$\eta_H^2$ & $(H-k+1)/(N-k)$ &
\tbdoi{10.1073/pnas.21.9.554}{Kelley, 1935} \\
\hline
Simple linear regression &
$R^2$ &
$R^2=1-SS_\text{res}/SS_\text{tot}$ &
\texttt{summary(lm())\$r.squared} \\
\hline
Spearman &
$\rho$ &
$\rho=r(\operatorname{rank}(x),\operatorname{rank}(y))$ &
\texttt{cor.test()} \\
\hline
Kendall &
$\tau_b$ &
$\tau_b=(C-D)/\sqrt{\left(n_0-n_1\right)\left(n_0-n_2\right)}$ &
\texttt{cor.test()} \\
\hline
Pearson $\chi^2$ ($R\times C$) &
Cramer's $V$ &
$V_{R\times C}=\sqrt{\chi^2/\left(N\left(\min(R,C)-1\right)\right)}$ &
\tbdoi{10.4324/9780203771587}{Cohen 2013, p. 223} \\
\hline
Pearson $\chi^2$ ($2\times2$) &
$\phi$ & $\phi=\sqrt{\chi^2/N}$ &
\tbdoi{10.4324/9780203771587}{Cohen 2013, p. 223} \\
\hline
Fisher's exact ($2\times2$) &
odds ratio &
$\widehat{\mathrm{OR}}=n_{11}n_{22}/(n_{12}n_{21})$ &
\texttt{fisher.test()} \\
\hline
\end{tabular}
\endgroup
\end{table}
)")
} else {
  cat(r"(
Table: (\#tab:effect-size-formulae) Effect sizes returned by `effect_size()`.

| Analysis | Effect size | Formula | Source |
|:---|:---|:---|:---|
| Student's $t$-test | Hedges' $g_{s_p}$ (pooled) | $g_{s_p} = J(N-2)\cdot(\bar{x}_1-\bar{x}_2)/s_p$ | [Hedges 1981](https://doi.org/10.3102/10769986006002107) |
| Welch's $t$-test | Hedges' $g_{s^{*}}$ (non-pooled) | $g_{s^{*}} = J(\nu^{*})\cdot(\bar{x}_1-\bar{x}_2)/s^{*}$ | [Delacre et al. 2021](https://doi.org/10.31234/osf.io/tu6mp) |
| Wilcoxon rank-sum | rank-biserial $r$ | $r = 2\cdot W/(n_1\cdot n_2) - 1$ | [Kerby 2014](https://journals.sagepub.com/doi/abs/10.2466/11.it.3.1) |
| Fisher's ANOVA | $\omega^2$ | $\nu_1\cdot(F-1)/(\nu_1\cdot F + \nu_2 + 1)$ | [Albers and Lakens 2018, Appendix A](https://doi.org/10.1016/j.jesp.2017.09.004) |
| Welch's ANOVA | $\omega^2$ (approx.) | $\nu_1\cdot(F_W-1)/(\nu_1\cdot F_W + \nu_2 + 1)$ | [F-form from Albers and Lakens 2018, Appendix A](https://doi.org/10.1016/j.jesp.2017.09.004) |
| Kruskal--Wallis | $\eta_H^2$ | $(H - k + 1)/(N - k)$ | [Kelley 1935](https://doi.org/10.1073/pnas.21.9.554) |
| Simple linear regression | $R^2$ | $R^2 = 1 - SS_\text{res}/SS_\text{tot}$ | `summary(lm())$r.squared` |
| Spearman | $\rho$ | $\rho = r(\operatorname{rank}(x),\operatorname{rank}(y))$ | `cor.test()$estimate` |
| Kendall | $\tau_b$ | $\tau_b = (C-D)/\sqrt{\left(n_0-n_1\right)\left(n_0-n_2\right)}$ | `cor.test()$estimate` |
| Pearson $\chi^2$ ($R\times C$) | Cramér's $V$ | $V_{R\times C} = \sqrt{\chi^2/\left(N\cdot(\min(R,C)-1)\right)}$ | [Cohen 2013, p. 223](https://doi.org/10.4324/9780203771587) |
| Pearson $\chi^2$ ($2\times 2$) | $\phi$ | $\phi = \sqrt{\chi^2/N}$ | [Cohen 2013, p. 223](https://doi.org/10.4324/9780203771587) |
| Fisher's exact ($2\times 2$) | odds ratio | $\widehat{\mathrm{OR}} = n_{11}n_{22}/(n_{12}n_{21})$ | `fisher.test()$estimate` |
)")
}
```

Here, Hedges' small-sample correction factor is

\begin{equation*}
J(\nu) =
\frac{\Gamma(\nu/2)}
     {\sqrt{\nu/2}\;\Gamma((\nu-1)/2)},
\end{equation*}

where $J$ denotes Hedges' correction factor. For Student's $t$-test,
$\nu=N-2$; for Welch's $t$-test,
$\nu=\nu^{*}$, with

\begin{equation*}
\nu^{*} =
\frac{(n_1-1)(n_2-1)(s_1^2+s_2^2)^2}
     {(n_2-1)s_1^4+(n_1-1)s_2^4}.
\end{equation*}

The non-pooled average-variance standardizer is

\begin{equation*}
s^{*} = \sqrt{\frac{s_1^2+s_2^2}{2}},
\end{equation*}

where $s^{*}$ denotes the average-variance standardizer.

$\nu_1$ and $\nu_2$ denote the numerator and denominator degrees of
freedom; for Fisher's ANOVA, $\nu_1=k-1$ and $\nu_2=N-k$; for Welch's
ANOVA, $\nu_1=k-1$ and $\nu_2$ is the usually fractional denominator
degree of freedom returned by `oneway.test()`.

For simple linear regression, the coefficient of determination is

\begin{equation}
R^2 = 1 - \frac{SS_\text{res}}{SS_\text{tot}},
(\#eq:r-squared)
\end{equation}

where $SS_\text{res}=\sum_{i=1}^{N}(y_i-\hat{y}_i)^2$ is the residual
sum of squares, $\hat{y}_i$ is the predicted value, and
$SS_\text{tot}=\sum_{i=1}^{N}(y_i-\bar{y})^2$ is the total sum of
squares.

All other variables used in Table \@ref(tab:effect-size-formulae) are
defined in the corresponding "Analysis" section.
