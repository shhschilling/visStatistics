# Parametric tests {#sec:tests}

In the numeric-response, categorical-predictor branch (Route 1), parametric tests
are selected when residual normality is not rejected, or when all
group-specific sample sizes are greater than 50. The Levene variance gate
then separates equal-variance tests from Welch-type tests.

## Student's t-test and Fisher's one-way ANOVA


### Student's t-test {#sec:tt}

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

### Fisher's one-way ANOVA {#sec:fisher-aov}

Fisher's one-way ANOVA generalises the mean comparison to more than two
groups.
Fisher's one-way ANOVA tests the null hypothesis that the means of
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

### Welch's t-test {#sec:welch-tt}

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

### Welch's heteroscedastic ANOVA {#sec:welch-aov}

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

#### Post-hoc comparison {#sec:gh}

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

### Wilcoxon rank-sum test {#sec:wilc}

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

### Kruskal--Wallis test {#sec:kw}

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

#### Post-hoc comparison {#sec:pairwise-wilcox}

`pairwise.wilcox.test()` compares each pair of factor levels via the
Wilcoxon rank-sum test on ranks rather than means.
The resulting p-values are adjusted for multiplicity using Holm's
step-down method [@Holm:1979].

# Tests for comparing proportions

For two unordered factors (route 4), `visstat()` tests the null hypothesis that
the variables are independent using Pearson's \(\chi^2\) test or
Fisher's exact test, depending on expected cell counts following
Cochran's rule [@Cochran:1954].

## Pearson's $\chi^2$ test {#sec:chi}

Pearson's \(\chi^2\) test evaluates the null hypothesis that two
categorical variables are independent.

Let $O_{ij}$ and $E_{ij}$ denote the observed and expected frequencies in
row $i$ and column $j$ of an $R \times C$ contingency table, where rows
index the $R$ levels of the response $y$ and columns the $C$ levels of
the predictor $x$.
The Pearson residual for cell $(i,j)$ is

$$r_{ij} = \frac{O_{ij} - E_{ij}}{\sqrt{E_{ij}}},
\quad i = 1,\ldots,R,\quad j = 1,\ldots,C.$$

For Pearson's $\chi^2$ tests, `visstat()` also generates a mosaic plot
in which cell colours represent Pearson residual values (see Section
\@ref(sec:route-4-examples)) on a blue--red colour scale.


The test statistic of Pearson's $\chi^2$ test is

\begin{equation}
\chi^2 = \sum_{i=1}^{R}\sum_{j=1}^{C} r_{ij}^2,
(\#eq:pearson-chi)
\end{equation}

Under the null hypothesis of independence, the statistic is compared with a
\(\chi^2\) distribution with $(R-1)(C-1)$ degrees of freedom.


For $2\times 2$ tables, Yates' continuity correction is applied by
default.













Pearson's \(\chi^2\) test evaluates the null hypothesis that two
categorical variables are independent.
Let $O_{ij}$ and $E_{ij}$ denote the observed and expected frequencies in
row $i$ and column $j$ of an $R \times C$ contingency table, where rows
index the $R$ levels of the response $y$ and columns the $C$ levels of
the predictor $x$.
The Pearson residual for cell $(i,j)$ is

$$r_{ij} = \frac{O_{ij} - E_{ij}}{\sqrt{E_{ij}}},
\quad i = 1,\ldots,R,\quad j = 1,\ldots,C.$$

From this $$r_{ij} the  p-values, shown in the mosaic plots link are generated 


The test statistic of Pearson's $\chi^2$ test is

\begin{equation}
\chi^2 = \sum_{i=1}^{R}\sum_{j=1}^{C} r_{ij}^2
       = \sum_{i=1}^{R}\sum_{j=1}^{C}
         \frac{(O_{ij}-E_{ij})^2}{E_{ij}}
(\#eq:pearson-chi)
\end{equation}

compared to $\chi^2\!\left((R-1)(C-1)\right)$.
For $2\times 2$ tables, Yates' continuity correction is applied by
default. For general $R \times C$ tables, `visstat()` supplements the bar
chart with a mosaic plot in which tiles are coloured by $r_{ij}$
(blue: positive, red: negative).

## Fisher's exact test {#sec:fisher-exact-test}

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

## Kendall rank correlation {#sec:tau}

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

## Spearman rank correlation {#sec:rho}

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
