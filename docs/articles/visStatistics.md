# visStatistics: The right test, visualised

## Abstract

`visStatistics` provides a workflow for routine two-variable frequentist
inference in R. Given two vectors,
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
first dispatches by variable classes, factor levels, sample sizes,
expected cell counts, and explicit user options.

Its main assumption-driven branch concerns tests of central tendency for
a numeric response grouped by a factor. There, sample size and residual
diagnostics from a fitted linear model choose between rank-based and
mean-based tests and, within the latter, between equal-variance and
Welch-type variants.

The output is deliberately visual: diagnostic plots are shown together
with assumption-test p-values, the selected test, effect size, and
post-hoc results where applicable. This shifts attention from ad-hoc
test selection to visual diagnostic assessment and statistical
interpretation.

The automated workflow of `visStatistics` is particularly suited for
server-side R applications, where users select variables through a web
interface and receive a complete visual statistical analysis. It also
supports time-constrained work such as statistical consulting, where
less time spent on test selection leaves more room for interpretation.

## Introduction

In the frequentist tradition, most routine data analyses reduce to a
comparatively small set of inferential frameworks, including group
comparisons, regression models and contingency-table analyses Hayat et
al. ([2017](#ref-Hayat:2017)); their correct use depends on assumptions
that are often checked informally or not at all. `visStatistics` targets
this gap by making routine frequentist test selection explicit,
assumption-aware, visual, and reproducible. Rather than requiring users
to choose the test function first,
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
starts from two variables and routes common two-variable settings
through a fixed decision workflow. It selects a test from the variable
classes, distributional assumptions, sample size, and expected cell
counts; displays the diagnostics that led to the selected route; and
returns an R object whose [`print()`](https://rdrr.io/r/base/print.html)
and [`summary()`](https://rdrr.io/r/base/summary.html) methods expose
the complete test results, including the reported effect size. The
scripted workflow is well suited for browser-based applications where
sensitive data (such as highly confidential medical records) are stored
securely on a server and cannot be directly accessed by users. This
approach was already successfully applied to develop a medical scoring
tool ([Bijlenga et al. 2017](#ref-Bijlenga:2017)).

For group comparisons, packages with related scope include
`compareGroups` ([Subirana et al. 2014](#ref-Subirana:2014)), `boxTest`
([Sau et al. 2025](#ref-Sau:2025)), `autotestR` ([Garcia
2026](#ref-Garcia:2026)), and `automatedtests` ([Zeevat
2025](#ref-Zeevat:2025)). `compareGroups` is primarily designed for
bivariate descriptive tables and reports, not diagnostic plots and test
visualisations. `boxTest` covers only the two-group numeric-response
case. `autotestR` provides automated recommendations for t-tests, ANOVA,
correlation, and contingency-table analyses. `automatedtests` provides
the broadest routing among these packages, including one-sample, paired,
repeated-measures, regression, correlation, and contingency-table cases.

For mean-based tests in the general linear-model framework, the
normality assumption concerns the model errors, not the marginal
response distribution. None of these packages bases routing on pooled
model residuals from the common linear model: `autotestR` and `boxTest`
test the response separately within groups, whereas `automatedtests` and
`compareGroups` test the ungrouped numeric response.

Among the reviewed automated test-selection packages, `visStatistics` is
thus the only one that bases the central-tendency route on explicit
residual diagnostics from the common linear model rather than on
marginal or groupwise normality checks. The package is deliberately
limited to common two-variable settings in which the route can be
visualised and audited: pretesting where applied, residual-based
diagnostics for the linear-model branch, the selected test, full test
statistics, effect size, result plots, and post-hoc comparisons where
required.

## Package overview

`visStatistics` ([Schilling 2026](#ref-Schilling:2026)) is available on
[CRAN](https://CRAN.R-project.org/package=visStatistics) as the latest
stable release. This article refers to the latest development state in
the [GitHub repository](https://github.com/shhschilling/visStatistics),
which may include minor changes between CRAN submissions.

Given two input vectors `x` and `y` of class `"numeric"`, `"integer"`,
or `"factor"`, its main function
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
can be called in two equivalent forms:

``` r

# Recommended form:
visstat(x, y)

# Formula interface:
visstat(y ~ x, data = dataframe)
```

An examplary function call is

``` r

#Standardised form
visstat(npk$block,npk$yield)
```

From this single entry point, the package automatically selects among
the implemented hypothesis tests,

[`t.test()`](https://rdrr.io/r/stats/t.test.html),
[`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html),
[`aov()`](https://rdrr.io/r/stats/aov.html),
[`oneway.test()`](https://rdrr.io/r/stats/oneway.test.html),
[`kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html),
[`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html),
[`fisher.test()`](https://rdrr.io/r/stats/fisher.test.html),
[`lm()`](https://rdrr.io/r/stats/lm.html).

The underlying selection algorithm is detailed in Section
@ref(sec:decision). The function returns an object of class `"visstat"`
with [`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html), and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods
described in Section @ref(sec:visstat-methods)

Among the returned components,
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
reports the p-value of the selected test and an `effect_size` field
(Section @ref(sec:p-values-effect-sizes)).

Unless stated otherwise, R function names for selected tests refer to
functions from the `stats` package distributed with R ([R Core Team
2026](#ref-R:2026)).

To reduce dependencies on other packages, `visStatistics` implements
[`levene.test()`](https://shhschilling.github.io/visStatistics/reference/levene.test.md)
for the variance gate in grouped mean-based tests
(Eq. @ref(eq:levene-f)),
[`bp.test()`](https://shhschilling.github.io/visStatistics/reference/bp.test.md)
for regression diagnostics (Eq. @ref(eq:breusch-pagan-bp)), and
[`games.howell()`](https://shhschilling.github.io/visStatistics/reference/games.howell.md)
for Welch-ANOVA post-hoc comparisons (Eq. @ref(eq:games-howell-t)).

Definitions of all implemented test statistics, rank-correlation
coefficients, and effect sizes are given in Appendices
@ref(sec:tests)–@ref(sec:rank-correlations).

## Decision logic

The decision logic of
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
is layered: The general branching, summarised in Figure
@ref(fig:overview), is driven by the `class` and number of factor levels
of its input vectors.

Only for a numeric response with a categorical predictor, the selection
among tests of central tendency further depends on group-specific sample
size and residual diagnostics from a fitted linear model (Section
@ref(sec:route-1)).

### Top-level routing by input class

The main branching logic consists of four default routes summarised in
Figure @ref(fig:overview).

- Route 1. Numeric responses with categorical predictors enter the
  central-tendency branch detailed in Section @ref(sec:route-1).
- Route 2. Ordered categorical responses with categorical predictors
  follow the non-parametric Wilcoxon or Kruskal–Wallis route (Section
  @ref(sec:route-2)).
- Route 3. Two numeric variables enter simple linear regression (Section
  @ref(sec:route-3)).
- Route 4. Two unordered factors enter the proportion-comparison branch
  (Section @ref(sec:route-4)).

Rank-correlation analyses are optional user-requested alternatives for
ordered–ordered and numeric–numeric inputs. They are reached only when
`correlation = TRUE` is set explicitly.

![Flowchart showing all implemented statistical tests organised by the
class of the input vectors.](figures/overview.png)

Overview of all implemented tests selected based on input class.

### General linear model framework

Student’s t-test, Fisher’s ANOVA (both belonging to Route 1) and simple
linear regression (in Route 3) are special cases of the general linear
model framework ([Thompson 2015](#ref-Thompson:2015)) and share the same
model assumptions: the expected value of the response is a linear
function of the predictors, the error terms are mutually independent and
normally distributed with expectation 0, and the error variance is
constant.

Residuals are the empirical realisations of these error terms. To check
whether the residuals fulfil the linear model assumptions,
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
both *vis*ualises (see Section @ref(sec:graphical)) and formally
assesses the normality and homoscedasticity of the residuals by
assumption tests (see Section @ref(sec:assumption-tests)) for tests
belonging to Route 1 and Route 3. Note that only in Route 1,
\\p\\-values derived from these assumption tests influence the test
selection (Section @ref(sec:route-1)).

Below, Section @ref(sec:math-glm) formally defines the general linear
model framework in the context of the implemented tests.

#### Model definition

In the general linear model, a response \\Y\\ is modelled as a linear
combination of \\k-1\\ predictors \\x_j\\. The general linear model for
observation \\i,\\i = 1, \ldots, N\\ is then

\\\begin{equation} (\\eq:glm) Y_i = \beta_0 + \beta_1 x\_{i1} + \cdots +
\beta\_{k-1} x\_{i,k-1} + \varepsilon_i, \end{equation}\\

where \\Y_i\\ is the response for observation \\i\\, \\x\_{ij}\\ is the
value of predictor \\j\\ for observation \\i\\, \\\beta_0, \beta_1,
\ldots, \beta\_{k-1}\\ are the \\k\\ parameters, and \\\varepsilon_i\\
is the model error term. The model error terms \\\varepsilon_i\\ are
assumed to be independent and normally distributed with expectation 0
and constant variance \\\sigma^2\\, in short \\\varepsilon_i \sim
\mathscr{N}(0, \sigma^2), \quad\mathrm{mutually\\ independent} \\

From Eq.  @ref(eq:glm), the special cases used by
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
follow from the predictor structure:

**Student’s t-test** uses one binary indicator variable \\x\_{i1}\\,
with \\x\_{i1}=0\\ for group 1 and \\x\_{i1}=1\\ for group 2. Let
\\\mu_1\\ and \\\mu_2\\ denote the expected values in the two population
groups. For the expected values of the response, we then obtain \\E(Y_i
\mid x\_{i1}=0)=\mu_1=\beta_0\\ for group 1 and \\E(Y_i \mid
x\_{i1}=1)=\mu_2=\beta_0+\beta_1\\ for group 2. Testing \\H_0: \beta_1 =
0\\ is therefore equivalent to testing \\H_0: \mu_1 = \mu_2\\.

**Fisher’s ANOVA** generalises this coding to \\k-1\\ binary indicator
variables for \\k\\ groups; testing \\H_0: \beta_1 = \cdots =
\beta\_{k-1} = 0\\ is equivalent to testing equality of all group means.

**Simple linear regression** uses one continuous predictor; \\H_0:
\beta_1 = 0\\ examines whether a linear relationship exists.

The model error terms \\\varepsilon_i\\ in Eq.  @ref(eq:glm) are not
observed. They observable counterpart are the residuals.  
After fitting the corresponding linear model, the residual is

\\\begin{equation} e_i = y_i - \hat{y}\_i, (\\eq:raw-residual)
\end{equation}\\

where \\y_i\\ is the observed value and \\\hat{y}\_i\\ the fitted value
for observation \\i\\.

#### General linear model assumption tests

**Normality tests** The normality of the residuals is formally assessed
using both the Shapiro–Wilk test ([Shapiro and Wilk
1965](#ref-Shapiro:1965))
([`shapiro.test()`](https://rdrr.io/r/stats/shapiro.test.html); Eq. 
@ref(eq:shapiro-w)) and the Anderson–Darling test ([Anderson and Darling
1952](#ref-Anderson:1952)) (`ad.test()`; Eq.  @ref(eq:anderson-a2)). The
Anderson–Darling test adds sensitivity to deviations in the tails of the
distribution ([Yap and Sim 2011](#ref-Yap:2011)).

**Homoscedasticity tests** For grouped central-tendency analyses,
variance homogeneity is assessed using the package-implemented
mean-centred Levene test ([Levene 1960](#ref-Levene:1960))
([`levene.test()`](https://shhschilling.github.io/visStatistics/reference/levene.test.md);
Eq.  @ref(eq:levene-f)) and Bartlett’s test ([Bartlett
1937](#ref-Bartlett:1937))
([`bartlett.test()`](https://rdrr.io/r/stats/bartlett.test.html); Eq. 
@ref(eq:bartlett-k2)). Bartlett’s test has greater power when normality
holds, but the Levene test is more robust to departures from normality
([Allingham and Rayner 2012](#ref-Allingham:2012)). Therefore, Levene’s
test is used as the variance gate in the automated workflow.

For simple linear regression, group-based variance tests are not
applicable. There, `visStatistics` uses its package implementation
[`bp.test()`](https://shhschilling.github.io/visStatistics/reference/bp.test.md)
of the Breusch–Pagan test ([Breusch and Pagan 1979](#ref-Breusch:1979))
(Eq. @ref(eq:breusch-pagan-bp)).

#### Visualisation of the assumptions of the general linear model

[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
*vis*ualises the assumptions of the underlying linear model for the
selection of tests of central tendency (Route 1) and for simple linear
regression (`correlation = FALSE`) (Route 3).

It fits a simple linear model between response and predictor and
extracts the model residuals. Standardising the pooled raw model
residuals with their standard error, we obtain the z-residuals used in
the diagnostic plots to facilitate model comparison: \\\begin{equation}
z_i = \frac{e_i}{SE\_\text{res}}, \\ \mathrm{with} \\ SE\_\text{res} =
\sqrt{\frac{\sum\_{r=1}^{N} e_r^2}{N-k}}, (\\eq:z-residual)
\end{equation}\\

Here \\e_i\\ is the raw model residual for observation \\i\\ defined in
Eq. @ref(eq:raw-residual), \\N\\ is the total sample size, \\k\\ is the
number of fitted linear model parameters, and \\SE\_\text{res}\\ is the
residual standard error.  

##### Layout of the diagnostic panel

In Route 1 and Route 3,
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
always generates a diagnostic panel of the model residuals.

**Route 1** For numeric responses with categorical predictors, the
diagnostic panel displays the residual histogram, the normal Q–Q plot,
and the absolute z-residuals \\\|z_i\|\\ (Eq. @ref(eq:z-residual)) by
group. The last panel shows whether residual spread is comparable across
factor levels, the pattern assessed formally by the Levene and Bartlett
variance checks.

**Route 3** For simple linear regression, the diagnostic panel displays
the residual histogram, the normal Q–Q plot, z-scaled residuals versus
fitted values, and z-scaled residuals versus leverage
(Eq. @ref(eq:leverage-simple-regression) with Cook’s distance contours
computed on the same z-residual scale (see Section
@ref(sec:cooks-distance)).

**Outer title for both routes**

The first row of the outer title reports p-values of residual-normality
checks with the Shapiro–Wilk Shapiro–Wilk test and Anderson–Darling
tests. The second row reports p-values of variance checks: Levene and
Bartlett for grouped central-tendency analyses, or Breusch–Pagan for
simple regression.

Note that among the displayed assumption tests, only the Shapiro–Wilk
and Levene test results enter automated routing, and only in the
central-tendency branch (see Section @ref(sec:route-1)).
Anderson–Darling, Bartlett, and Breusch–Pagan are diagnostic output
only.

Because the displayed z-residuals are a constant rescaling of the raw
residuals fed to the assumption tests, the visual evidence and the
displayed p-values in the outer titles refer to the same residual
object.

The Route 1 and Route 3 diagnostic-panel designs are illustrated in the
examples in Figures @ref(fig:welch-anova-example), left, and
@ref(fig:regression-example), left.

### Route-specific decision rules

The general branching is driven by input class and factor levels
(Section @ref(sec:top-level)). Within the selected route, additional
rules determine the selected test and output; these route-specific rules
are detailed below.

#### Route 1: Numeric response, categorical predictor

Figure @ref(fig:decision-tree) summarises the test decision logic in the
central-tendency branch.

![Decision tree selecting among Welch t-test, Student t-test, Wilcoxon,
Fisher ANOVA, Welch ANOVA, and Kruskal-Wallis tests based on the
all-groups n_i \> 50 rule, the Shapiro-Wilk test on model residuals, and
the Levene test for variance homogeneity.](figures/decision_tree.png)

Decision tree for tests of central tendency. If all group-specific
sample sizes are greater than 50, the formal residual normality test is
bypassed and variance homogeneity is assessed directly. Otherwise,
Shapiro–Wilk on model residuals determines whether the route remains
mean-based or switches to rank-based tests; the Levene test then selects
equal-variance or Welch-type procedures.

The first split checks group size. If all groups contain more than 50
observations,
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
treats the parametric (mean-based) path as justified by the Central
Limit Theorem ([Lumley et al. 2002](#ref-Lumley:2002); [Rasch et al.
2011](#ref-Rasch:2011)). This avoids switching to non-parametric
(rank-based) tests because of negligible residual-normality deviations
in large samples ([Ghasemi and Zahediasl 2012](#ref-Ghasemi:2012);
[Fagerland 2012](#ref-Fagerland:2012); [Shatz 2024](#ref-Shatz:2024)).

Otherwise, the p-value of the Shapiro–Wilk (SW) test determines whether
the route remains mean-based: If normality is rejected (\\p\_\text{SW}
\le \alpha\\), rank-based tests are selected:
[`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html)
(Eq. @ref(eq:wilcoxon-w)) for two groups, or
[`kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html)
(Eq. @ref(eq:kruskal-h)) followed by Holm-adjusted
[`pairwise.wilcox.test()`](https://rdrr.io/r/stats/pairwise.wilcox.test.html)
for more than two groups. If normality is not rejected, variance
homogeneity is assessed with the package-implemented mean-centred Levene
test (L) on model residuals ([Levene 1960](#ref-Levene:1960))
(Eq. @ref(eq:levene-f)).

For homoscedastic data, `t.test(var.equal = TRUE)`
(Eq. @ref(eq:student-t)) is applied for two groups, or Fisher’s
[`aov()`](https://rdrr.io/r/stats/aov.html) (Eq. @ref(eq:fisher-f)) with
[`TukeyHSD()`](https://rdrr.io/r/stats/TukeyHSD.html)
(Eq. @ref(eq:tukey-hsd-q)) for more than two groups. For heteroscedastic
data, Welch’s [`t.test()`](https://rdrr.io/r/stats/t.test.html)
(Eq. @ref(eq:welch-t)) is applied for two groups, or Welch’s
[`oneway.test()`](https://rdrr.io/r/stats/oneway.test.html)
(Eq. @ref(eq:welch-f)) with the package implementation
[`games.howell()`](https://shhschilling.github.io/visStatistics/reference/games.howell.md)
(([Games and Howell 1976](#ref-Games:1976)); see Section @ref(sec:gh))
for more than two groups.

##### Post-hoc tests

ANOVA, Welch ANOVA, and Kruskal–Wallis are omnibus tests: a significant
test result tells us that *some* group differs, but not which.

To identify the differing pairs,
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
tests all pairwise comparisons among the factor levels, defining a
family of tests. Because the three omnibus tests rest on different
assumptions, each branch uses a matching post-hoc procedure:

- [`TukeyHSD()`](https://rdrr.io/r/stats/TukeyHSD.html)
  (Eq. @ref(eq:tukey-hsd-q)) after
  [`aov()`](https://rdrr.io/r/stats/aov.html) controls the family-wise
  error rate through the studentised range distribution under a
  common-variance assumption.

- [`games.howell()`](https://shhschilling.github.io/visStatistics/reference/games.howell.md)
  (Eq. @ref(eq:games-howell-t)) after
  [`oneway.test()`](https://rdrr.io/r/stats/oneway.test.html) uses
  separate variance estimates and Welch-adjusted degrees of freedom for
  each pair, making it the appropriate post-hoc procedure for the
  heteroscedastic Welch branch.

- `pairwise.wilcox.test(p.adjust.method = "holm")` after
  [`kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html) uses
  Holm’s step-down adjustment for the pairwise Wilcoxon tests in the
  Kruskal–Wallis branch.

The graphical results panel of these omnibus tests consists of box plots
(see examples in Section @ref(sec:examples-route1)) enriched with
significance letters to visualise the post-hoc analysis: Pairs whose
adjusted post-hoc \\p\\-value falls below \\\alpha\\ are marked with
different green significance letters below the box plots; pairs sharing
a letter are not significantly different.

#### Route 2: Ordered response

An ordered categorical response with a categorical predictor or ordered
categorical predictor is treated as a rank-based group comparison . The
ordered response is converted to integer level codes and analysed with
the Wilcoxon rank-sum test for two groups or the Kruskal–Wallis test for
more than two groups.

#### Route 3: Numeric response, numeric predictor

Two numeric variables ask whether a numeric response changes with a
numeric predictor. By default,
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
fits a simple linear regression (Eq. @ref(eq:simple-regression-fit)).
and the diagnostic panel described in Section @ref(sec:graphical) is
displayed. If general linear model assumptions are violated, the
corresponding p-values trigger warnings and recommendations, but no
automatic model replacement. The regression output is shown in Section
@ref(sec:lin-reg).

#### Route 4: Two unordered factors

Two unordered factors ask whether two categorical variables are
independent.
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
uses Pearson’s \\\chi^2\\ test or Fisher’s exact test, depending on
expected cell counts following Cochran’s rule ([Cochran
1954](#ref-Cochran:1954)): the \\\chi^2\\ approximation is used if no
expected cell count is less than 1 and no more than 20% of cells have
expected counts below 5. Yates’ continuity correction is applied by
default to \\2 \times 2\\ tables when the \\\chi^2\\ approximation is
used.

#### Optional rank-correlation mode

The four routes above describe the default, automatic test selection
behaviour. For ordered–ordered and numeric–numeric input vectors, the
user can instead request a rank-correlation analysis by setting
`correlation = TRUE`.

Both optional analyses test monotone association and are computed by
[`cor.test()`](https://rdrr.io/r/stats/cor.test.html): Kendall’s
\\\tau_b\\ (Eq. @ref(eq:kendall-tau-b)) with
`method = "kendall", exact = FALSE` for two ordered variables, and
Spearman’s \\\rho\\ (Eq. @ref(eq:spearman-rho)) with
`method = "spearman"` for two numeric variables. Kendall’s \\\tau_b\\
corrects for ties present with few ordered levels ([Agresti
2010](#ref-Agresti:2010); [Xu et al. 2013](#ref-Xu:2013)).

Note, that for numeric–numeric input, Pearson correlation is not
implemented as a separate optional mode as in simple linear regression
with an intercept, the two-sided test of zero slope and the two-sided
Pearson correlation test return the same \\p\\-value.

### Reported p-values and effect sizes

The selected test output includes the p-value for the corresponding null
hypothesis. As a complementary output component, the `effect_size` field
reports the effect-size name, numeric estimate, and method description.

The exported package function
[`effect_size()`](https://shhschilling.github.io/visStatistics/reference/effect_size.md)
computes these estimates; implemented formulae are given in Appendix
@ref(sec:effect-size).

The p-value quantifies evidence against the null hypothesis, whereas the
effect-size estimate describes the magnitude of the selected comparison,
association, or model fit on the scale defined in Appendix
@ref(sec:effect-size) ([Fritz et al. 2012](#ref-Fritz:2012); [Levine and
Hullett 2002](#ref-Levine:2002)).

## Examples

The examples follow the routes outlined in Section @ref(sec:top-level)
and are chosen to trigger every branch.

Within the group-comparison routes, examples are ordered such that the
two-group case is followed by its generalisation to more than two
groups: Student’s t-test by Fisher’s one-way ANOVA, Welch’s t-test by
Welch’s one-way ANOVA, and Wilcoxon rank-sum by Kruskal–Wallis.

Where needed, the example descriptions add interpretive details on the
graphical output, such as significance letters, regression bands, or
mosaic plots.

### Route 1: Numeric response, categorical predictor

#### Student’s t-test and Fisher’s one-way ANOVA

##### Student’s t-test

The `ToothGrowth` dataset records odontoblast length in 60 guinea pigs
given vitamin C by orange juice (`OJ`) or ascorbic acid (`VC`). With
delivery method as predictor and length as response, the
assumption-diagnostic panel shows no residual-normality or
variance-homogeneity violation.
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
therefore selects Student’s t-test, and the result panel shows the
two-group box plot with the selected test result.

``` r

student_ttest <- visstat(ToothGrowth$supp, ToothGrowth$len)
```

![Student's t-test applied to the \`ToothGrowth\` dataset (\`len\` vs.\\
\`supp\`). Assumption diagnostics (Shapiro--Wilk does not reject
residual normality; Levene does not reject residual variance
homogeneity) select the equal-variance mean-based path, followed by box
plots with the Student t-test
result.](visStatistics_files/figure-html/student-ttest-example-1.png)![Student's
t-test applied to the \`ToothGrowth\` dataset (\`len\` vs.\\ \`supp\`).
Assumption diagnostics (Shapiro--Wilk does not reject residual
normality; Levene does not reject residual variance homogeneity) select
the equal-variance mean-based path, followed by box plots with the
Student t-test
result.](visStatistics_files/figure-html/student-ttest-example-2.png)

Student’s t-test applied to the `ToothGrowth` dataset (`len`
vs. `supp`). Assumption diagnostics (Shapiro–Wilk does not reject
residual normality; Levene does not reject residual variance
homogeneity) select the equal-variance mean-based path, followed by box
plots with the Student t-test result.

##### Fisher’s one-way ANOVA with Tukey HSD post-hoc comparisons

The `PlantGrowth` dataset records yields (as measured by dried weight of
plants) for a control group and two treatment groups. With control and
treatment groups as predictor and plant weight as response, the
assumption-diagnostic panel shows that Shapiro–Wilk does not reject
normality of the model residuals and
[`levene.test()`](https://shhschilling.github.io/visStatistics/reference/levene.test.md)
does not reject homoscedasticity.
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
therefore applies Fisher’s one-way ANOVA followed by Tukey HSD post-hoc
comparisons. The result panel shows the box plots and post-hoc
significance letters. The omnibus F-test is significant at \\\alpha =
0.05\\, and the Tukey HSD post-hoc comparison finds no significant
difference between the control group and either treatment, but the
difference between `trt1` and `trt2` is significant.

``` r

anova_plantgrowth <- visstat(PlantGrowth$group, PlantGrowth$weight)
```

![Fisher's one-way ANOVA applied to the \`PlantGrowth\` dataset
(\`weight\` vs.\\ \`group\`). Assumption diagnostics (Shapiro--Wilk does
not reject residual normality; Levene does not reject residual variance
homogeneity) select the equal-variance mean-based path, followed by box
plots with Tukey HSD significance letters (\$\alpha =
0.05\$).](visStatistics_files/figure-html/anova-example-1.png)![Fisher's
one-way ANOVA applied to the \`PlantGrowth\` dataset (\`weight\` vs.\\
\`group\`). Assumption diagnostics (Shapiro--Wilk does not reject
residual normality; Levene does not reject residual variance
homogeneity) select the equal-variance mean-based path, followed by box
plots with Tukey HSD significance letters (\$\alpha =
0.05\$).](visStatistics_files/figure-html/anova-example-2.png)

Fisher’s one-way ANOVA applied to the `PlantGrowth` dataset (`weight`
vs. `group`). Assumption diagnostics (Shapiro–Wilk does not reject
residual normality; Levene does not reject residual variance
homogeneity) select the equal-variance mean-based path, followed by box
plots with Tukey HSD significance letters (\\\alpha = 0.05\\).

#### Welch’s t-test and Welch’s one-way ANOVA

##### Welch’s t-test

The *Motor Trend Car Road Tests* dataset (`mtcars`) contains 32
observations, where `mpg` denotes miles per (US) gallon and `am`
represents the transmission type (`0` = automatic, `1` = manual). With
binary factor `am` and continuous response `mpg`, the
assumption-diagnostic panel shows that Shapiro–Wilk does not reject
normality of the model residuals, while the Levene test detects
heteroscedasticity. The routing therefore leads to Welch’s t-test rather
than Student’s t-test, and the result panel shows the corresponding
two-group comparison.

``` r

mtcars$am <- as.factor(mtcars$am)
t_test_stats <- visstat(mtcars$am, mtcars$mpg)
```

![Welch's t-test applied to the \`mtcars\` dataset (\`mpg\` vs.\\
\`am\`). Assumption diagnostics (Shapiro--Wilk does not reject residual
normality; Levene rejects residual variance homogeneity) select the
unequal-variance mean-based path, followed by box plots with the Welch
t-test
result.](visStatistics_files/figure-html/ttest-example-1.png)![Welch's
t-test applied to the \`mtcars\` dataset (\`mpg\` vs.\\ \`am\`).
Assumption diagnostics (Shapiro--Wilk does not reject residual
normality; Levene rejects residual variance homogeneity) select the
unequal-variance mean-based path, followed by box plots with the Welch
t-test result.](visStatistics_files/figure-html/ttest-example-2.png)

Welch’s t-test applied to the `mtcars` dataset (`mpg` vs. `am`).
Assumption diagnostics (Shapiro–Wilk does not reject residual normality;
Levene rejects residual variance homogeneity) select the
unequal-variance mean-based path, followed by box plots with the Welch
t-test result.

##### Welch’s heteroscedastic one-way ANOVA with Games–Howell post-hoc comparisons

In the `iris` dataset, using `Species` as predictor and `Sepal.Length`
as response, the assumption-diagnostic panel shows that Shapiro–Wilk
does not reject normality of the model residuals, whereas the Levene
test rejects homoscedasticity at the given \\\alpha = 5\\\\.
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
therefore selects Welch’s heteroscedastic one-way ANOVA
([`oneway.test()`](https://rdrr.io/r/stats/oneway.test.html)) and
applies Games–Howell post-hoc comparisons. The result panel shows the
box plots and Games–Howell significance letters.

``` r

welch_anova_iris <- visstat(iris$Species, iris$Sepal.Length)
```

![Welch's heteroscedastic one-way ANOVA applied to the \`iris\` dataset
(\`Sepal.Length\` vs.\\ \`Species\`). Assumption diagnostics
(Shapiro--Wilk does not reject residual normality; Levene rejects
residual variance homogeneity) select the unequal-variance mean-based
path, followed by box plots with Games--Howell significance letters
(\$\alpha =
0.05\$).](visStatistics_files/figure-html/welch-anova-example-1.png)![Welch's
heteroscedastic one-way ANOVA applied to the \`iris\` dataset
(\`Sepal.Length\` vs.\\ \`Species\`). Assumption diagnostics
(Shapiro--Wilk does not reject residual normality; Levene rejects
residual variance homogeneity) select the unequal-variance mean-based
path, followed by box plots with Games--Howell significance letters
(\$\alpha =
0.05\$).](visStatistics_files/figure-html/welch-anova-example-2.png)

Welch’s heteroscedastic one-way ANOVA applied to the `iris` dataset
(`Sepal.Length` vs. `Species`). Assumption diagnostics (Shapiro–Wilk
does not reject residual normality; Levene rejects residual variance
homogeneity) select the unequal-variance mean-based path, followed by
box plots with Games–Howell significance letters (\\\alpha = 0.05\\).

#### Wilcoxon rank-sum test and Kruskal–Wallis test

##### Wilcoxon rank-sum test

The `warpbreaks` dataset records thread breaks during weaving. Using
wool type (`A` or `B`) as predictor and the number of breaks as
response, the assumption-diagnostic panel shows that the Shapiro–Wilk
test rejects normality of the model residuals.
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
therefore selects the Wilcoxon rank-sum test, and the result panel shows
the rank-based two-group comparison.

``` r

wilcoxon_stats <- visstat(warpbreaks$wool, warpbreaks$breaks)
```

![Wilcoxon rank-sum test applied to the \`warpbreaks\` dataset
(\`breaks\` vs.\\ \`wool\`). Assumption diagnostics (Shapiro--Wilk
rejects residual normality; non-parametric path selected) and box plots
with the Wilcoxon test
result.](visStatistics_files/figure-html/wilcoxon-example-1.png)![Wilcoxon
rank-sum test applied to the \`warpbreaks\` dataset (\`breaks\` vs.\\
\`wool\`). Assumption diagnostics (Shapiro--Wilk rejects residual
normality; non-parametric path selected) and box plots with the Wilcoxon
test result.](visStatistics_files/figure-html/wilcoxon-example-2.png)

Wilcoxon rank-sum test applied to the `warpbreaks` dataset (`breaks`
vs. `wool`). Assumption diagnostics (Shapiro–Wilk rejects residual
normality; non-parametric path selected) and box plots with the Wilcoxon
test result.

##### Kruskal–Wallis rank sum test with pairwise Wilcoxon post-hoc comparisons

In the `iris` data set, `Petal.Width` by `Species` follows a different
route than `Sepal.Length` by `Species` above (Figure
@ref(fig:welch-anova-example)), because the assumption diagnostics
differ. The assumption-diagnostic panel shows clear departures from
normality, and both normality tests return very small \\p\\-values.
Since Shapiro–Wilk falls below \\\alpha\\,
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
switches to
[`kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html) followed
by Holm-adjusted
[`pairwise.wilcox.test()`](https://rdrr.io/r/stats/pairwise.wilcox.test.html).
The result panel shows the box plots and Holm-adjusted significance
letters; all three species differ significantly in petal width, as
indicated by distinct letters.

``` r

kruskal_iris <- visstat(iris$Species, iris$Petal.Width)
```

![Kruskal-Wallis test applied to the \`iris\` dataset (\`Petal.Width\`
vs.\\ \`Species\`). Assumption diagnostics (Shapiro--Wilk rejects
residual normality; non-parametric path selected) and box plots with
Holm-adjusted pairwise Wilcoxon significance letters (\$\alpha =
0.05\$).](visStatistics_files/figure-html/kruskal-example-1.png)![Kruskal-Wallis
test applied to the \`iris\` dataset (\`Petal.Width\` vs.\\
\`Species\`). Assumption diagnostics (Shapiro--Wilk rejects residual
normality; non-parametric path selected) and box plots with
Holm-adjusted pairwise Wilcoxon significance letters (\$\alpha =
0.05\$).](visStatistics_files/figure-html/kruskal-example-2.png)

Kruskal-Wallis test applied to the `iris` dataset (`Petal.Width`
vs. `Species`). Assumption diagnostics (Shapiro–Wilk rejects residual
normality; non-parametric path selected) and box plots with
Holm-adjusted pairwise Wilcoxon significance letters (\\\alpha =
0.05\\).

### Route 2: Ordered response

#### Ordered response, categorical factor

##### Wilcoxon rank-sum test with ordered response

The `Titanic` dataset contains passenger counts by, among other
variables, passenger class and gender. After expanding the table to
individual rows, passenger class is treated as ordered and gender as a
two-level predictor.
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
selects the Wilcoxon rank-sum test. The result panel therefore displays
the rank-test comparison on the numeric level scores (see Figure
@ref(fig:ordinal-wilcoxon-kruskal-example), left).

``` r

titanic_df <- counts_to_cases(as.data.frame(Titanic))
titanic_df$Class <- ordered(titanic_df$Class,
                            levels = c("1st", "2nd", "3rd", "Crew"))
wilcox_ordered <- visstat(titanic_df$Sex, titanic_df$Class)
```

    ## Warning: Ordered response detected. Converting to integer level codes for
    ## non-parametric analysis.

##### Kruskal–Wallis test with ordered response

With three predictor groups,
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
routes to [`kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html)
followed by Holm-adjusted
[`pairwise.wilcox.test()`](https://rdrr.io/r/stats/pairwise.wilcox.test.html).
The result panel shows the Kruskal–Wallis comparison and Holm-adjusted
significance letters on the numeric level scores (see Figure
@ref(fig:ordinal-wilcoxon-kruskal-example), right). A synthetic survey
records perceived car comfort on a five-point scale across three
markets.

``` r

set.seed(123)
market <- factor(rep(c("Europe", "North America", "Asia"), each = 50))
comfort_numeric <- c(
  sample(1:5, 50, replace = TRUE, prob = c(0.30, 0.30, 0.20, 0.15, 0.05)),
  sample(1:5, 50, replace = TRUE, prob = c(0.10, 0.20, 0.40, 0.20, 0.10)),
  sample(1:5, 50, replace = TRUE, prob = c(0.05, 0.10, 0.20, 0.35, 0.30))
)
survey_data_3 <- data.frame(
  market = market,
  comfort = ordered(comfort_numeric)
)
kruskal_ordered <- visstat(comfort ~ market, data = survey_data_3)
```

    ## Warning: Ordered response detected. Converting to integer level codes for
    ## non-parametric analysis.

![Wilcoxon rank-sum test for ordered passenger class by sex in the
expanded \`Titanic\` data (left) and its multi-group generalisation, the
Kruskal-Wallis test for ordered car comfort ratings by market (right).
Holm-adjusted pairwise Wilcoxon post-hoc comparisons are shown as
significance letters for the Kruskal-Wallis example (\$\alpha =
0.05\$).](visStatistics_files/figure-html/ordinal-wilcoxon-kruskal-example-1.png)![Wilcoxon
rank-sum test for ordered passenger class by sex in the expanded
\`Titanic\` data (left) and its multi-group generalisation, the
Kruskal-Wallis test for ordered car comfort ratings by market (right).
Holm-adjusted pairwise Wilcoxon post-hoc comparisons are shown as
significance letters for the Kruskal-Wallis example (\$\alpha =
0.05\$).](visStatistics_files/figure-html/ordinal-wilcoxon-kruskal-example-2.png)

Wilcoxon rank-sum test for ordered passenger class by sex in the
expanded `Titanic` data (left) and its multi-group generalisation, the
Kruskal-Wallis test for ordered car comfort ratings by market (right).
Holm-adjusted pairwise Wilcoxon post-hoc comparisons are shown as
significance letters for the Kruskal-Wallis example (\\\alpha = 0.05\\).

### Route 3: Numeric response, numeric predictor

#### Linear regression

The `swiss` dataset records standardised fertility and socioeconomic
indicators for 47 French-speaking Swiss provinces in 1888. We examine
how the share of draftees achieving the highest army examination score
(`Examination`) predicts the fertility measure (`Fertility`), with
`conf.level = 0.99`. The diagnostic panel in Figure
@ref(fig:regression-example), left, shows that both normality tests pass
and the Breusch–Pagan test confirms homoscedasticity, supporting the
linear model. The assumption-diagnostic panel is displayed, but its
checks do not trigger automatic model replacement. The regression plot
shows the fitted line

\\\begin{equation} \hat{y}\_i = b_0 + b_1 x_i
(\\eq:simple-regression-fit) \end{equation}\\ with the point estimates
\\b_0\\ and \\b_1\\ for the unknown parameters \\\beta_0\\ and
\\\beta_1\\ of the linear regression model in Eq.  @ref(eq:glm) with one
predictor. It is displayed with pointwise confidence and prediction
bands at the specified `conf.level`.

The returned object contains the regression statistics,
residual-normality tests, pointwise confidence and prediction bands, and
the coefficient of determination \\R^2\\ (Eq. @ref(eq:r-squared)) as
effect size.

``` r

linreg_swiss <- visstat(swiss$Examination, swiss$Fertility, conf.level = 0.99)
```

![Simple linear regression of \`Fertility\` on \`Examination\` for the
\`swiss\` dataset (\`conf.level = 0.99\`). Assumption diagnostics
(Shapiro--Wilk, Anderson--Darling, Breusch--Pagan) and scatter plot with
fitted regression line, 99\\ confidence band (dark shading), and 99\\
prediction band (light
shading).](visStatistics_files/figure-html/regression-example-1.png)![Simple
linear regression of \`Fertility\` on \`Examination\` for the \`swiss\`
dataset (\`conf.level = 0.99\`). Assumption diagnostics (Shapiro--Wilk,
Anderson--Darling, Breusch--Pagan) and scatter plot with fitted
regression line, 99\\ confidence band (dark shading), and 99\\
prediction band (light
shading).](visStatistics_files/figure-html/regression-example-2.png)

Simple linear regression of `Fertility` on `Examination` for the `swiss`
dataset (`conf.level = 0.99`). Assumption diagnostics (Shapiro–Wilk,
Anderson–Darling, Breusch–Pagan) and scatter plot with fitted regression
line, 99% confidence band (dark shading), and 99% prediction band (light
shading).

The `airquality` ozone example shows the limits of the automated
approach when the default linear model is not an adequate final model.
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
identifies assumption violations and points to analyses outside the
automated decision tree. A default
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
call for ozone concentration (`Ozone`) as a function of wind speed
(`Wind`) fits the simple linear model.

``` r

ozone_lm <- visstat(airquality$Wind, airquality$Ozone)
```

    ## Warning: Statistical assumptions violated:
    ## Normality of residuals violated (Shapiro-Wilk p = 0.00621 )
    ## Homoscedasticity violated (Breusch-Pagan p = 0.00595 )
    ## Analysis proceeded but interpret results cautiously.

    ## RECOMMENDATION: Consider exploring alternatives outside visstat() such as data transformations,
    ## generalised linear models, or robust regression. For a non-causal alternative
    ## consider rerunning with correlation = TRUE.

![Default simple linear regression for \`Ozone\` by \`Wind\` in the
\`airquality\` dataset. Assumption diagnostics flag non-normal model
residuals and heteroscedasticity before alternative routes are
considered.](visStatistics_files/figure-html/ozone-lm-triage-1.png)![Default
simple linear regression for \`Ozone\` by \`Wind\` in the \`airquality\`
dataset. Assumption diagnostics flag non-normal model residuals and
heteroscedasticity before alternative routes are
considered.](visStatistics_files/figure-html/ozone-lm-triage-2.png)

Default simple linear regression for `Ozone` by `Wind` in the
`airquality` dataset. Assumption diagnostics flag non-normal model
residuals and heteroscedasticity before alternative routes are
considered.

The diagnostic output flags non-normal model residuals and
heteroscedasticity. The optional Spearman analysis for the same dataset
is shown in Section @ref(sec:examples-rank-correlation-mode). The
following example shows a Gamma generalised linear model outside
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md).

#### Model exploration outside `visstat()`

As a model outside of
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md),
we fit a Gamma generalised linear model with log link. The Gamma family
is suited here because Ozone is strictly positive and continuous, and
its variance grows with the mean — the structure detected by the
Breusch–Pagan test. The log link guarantees positive fitted values.

``` r

# Gamma model with log mapping
model_gamma <- glm(Ozone ~ Wind, data = airquality, family = Gamma(link = "log"))
model_gamma$aic
```

    ## [1] 1040.021

``` r

#Comparison with AIC of simple linear regression
model_lm <- glm(Ozone ~ Wind, data = airquality)
model_lm$aic
```

    ## [1] 1093.187

![Gamma GLM with log link fitted to the \`airquality\` dataset \`Ozone\`
vs. \`Wind\`. The red curve shows the fitted Gamma GLM; the y-axis is on
a log scale.](visStatistics_files/figure-html/gamma-glm-plot-1.png)

Gamma GLM with log link fitted to the `airquality` dataset `Ozone`
vs. `Wind`. The red curve shows the fitted Gamma GLM; the y-axis is on a
log scale.

For a Gamma generalised linear model with log link, standardised
deviance residuals are asymptotically standard normal; we use
Shapiro–Wilk and Anderson–Darling as approximate checks of the fitted
model:

``` r

# Extract standardised deviance residuals
std_dev_res <- rstandard(model_gamma, type = "deviance")
# Validate using the Shapiro-Wilk normality test
shapiro.test(std_dev_res)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  std_dev_res
    ## W = 0.99245, p-value = 0.7817

``` r

# Validate using the Anderson-Darling normality test
nortest::ad.test(std_dev_res)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  std_dev_res
    ## A = 0.198, p-value = 0.8853

The Gamma model improves the model fit according to the Akaike
Information Criterion ([Akaike 1974](#ref-Akaike:1974)), which decreases
from 1093.2 to 1040.0. The increase in the Shapiro–Wilk \\p\\-value from
\\p\_{SW} = 0.0052\\ in the simple linear regression to \\p\_{SW} =
0.78\\ is more consistent with residual normality. This comparison
illustrates how assumption warnings from
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
can motivate model exploration outside the automated decision tree.

### Route 4: Two unordered factors

The following examples are based on the `HairEyeColor` contingency
table, which is converted to the column-based data frame expected by
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
using the helper function
[`counts_to_cases()`](https://shhschilling.github.io/visStatistics/reference/counts_to_cases.md).

#### Pearson’s \\\chi^2\\ test

For a contingency table with \\R\\ response levels and \\C\\ predictor
levels, Pearson’s \\\chi^2\\ test (Eq. @ref(eq:pearson-chi)) shows a
grouped column plot of row percentages with the \\p\\-value in the
title, followed by a mosaic plot from `vcd` ([Meyer et al.
2006](#ref-Meyer:2006), [2024](#ref-Meyer:2024)). Each tile corresponds
to one cell of the contingency table. The tile colour represents the
Pearson residual value (Eq. @ref(eq:pearson-residual)) on a blue–red
colour scale; the tile size reflects the cell count.

With `Eye` and `Hair` from `HairEyeColor`, all expected cell counts
exceed the Cochran thresholds ([Cochran 1954](#ref-Cochran:1954)), so
the \\4 \times 4\\ \\\chi^2\\ approximation is used.

``` r

hair_eye_df <- counts_to_cases(as.data.frame(HairEyeColor))
visstat(hair_eye_df$Eye, hair_eye_df$Hair)
```

![Pearson's \$\chi^2\$ test applied to the \`HairEyeColor\` dataset.
Grouped bar chart of eye colour by hair colour and mosaic plot with
tiles coloured by Pearson residuals (blue: over-represented, red:
under-represented).](visStatistics_files/figure-html/chisq-example-1.png)![Pearson's
\$\chi^2\$ test applied to the \`HairEyeColor\` dataset. Grouped bar
chart of eye colour by hair colour and mosaic plot with tiles coloured
by Pearson residuals (blue: over-represented, red:
under-represented).](visStatistics_files/figure-html/chisq-example-2.png)

Pearson’s \\\chi^2\\ test applied to the `HairEyeColor` dataset. Grouped
bar chart of eye colour by hair colour and mosaic plot with tiles
coloured by Pearson residuals (blue: over-represented, red:
under-represented).

Here, cells for black hair and brown hair, as well as blond hair and
blue eyes, show counts above the expectation.

#### Pearson’s \\\chi^2\\ test with Yates’ continuity correction

Restricting `HairEyeColor` to black or brown hair and brown or blue eyes
yields a \\2 \times 2\\ table. Cochran’s rule is still satisfied, so
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
applies Pearson’s \\\chi^2\\ test with Yates’ continuity correction. The
resulting grouped column plot is shown in Figure
@ref(fig:yates-fisher-example), left.

``` r

hair_black_brown_eyes_brown_blue <- HairEyeColor[1:2, 1:2, ]
hair_black_brown_eyes_brown_blue_df <- counts_to_cases(
  as.data.frame(hair_black_brown_eyes_brown_blue))
yates_stats <- visstat(hair_black_brown_eyes_brown_blue_df$Eye,
                       hair_black_brown_eyes_brown_blue_df$Hair)
```

``` r

yates_stats$effect_size
```

    ## $name
    ## [1] "phi"
    ## 
    ## $estimate
    ## [1] 0.1709571
    ## 
    ## $effect_size_method
    ## [1] "Phi coefficient for 2 x 2 contingency table"

The returned effect size is \\\phi = 0.17\\, which, using Cohen’s
benchmarks for \\2 \times 2\\ tables ([Cohen 2013,
227](#ref-Cohen:2013)), is a small association. The p-value instead is
below \\\alpha = 0.05\\ (\\p = 0.0035\\) and thus significant. This
example underlines the importance of effect sizes: a significant p-value
can be accompanied with a small effect size measure.

#### Fisher’s exact test

Restricting `HairEyeColor` to male participants with black or brown hair
and hazel or green eyes yields a \\2 \times 2\\ table where one expected
frequency is less than 5, violating Cochran’s rule ([Cochran
1954](#ref-Cochran:1954)).
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
therefore applies Fisher’s exact test. The graphical output shows
absolute counts with count labels above each bar and the \\p\\-value in
the title, so the small cell counts that trigger the exact test remain
visible (see Figure @ref(fig:yates-fisher-example), right).

``` r

hair_eye_male <- HairEyeColor[, , 1]
black_brown_hazel_green <- hair_eye_male[1:2, 3:4]
black_brown_hazel_green_df <- counts_to_cases(
  as.data.frame(black_brown_hazel_green))
fisher_stats <- visstat(black_brown_hazel_green_df$Eye,
                        black_brown_hazel_green_df$Hair)
```

![Two \$2 \times 2\$ categorical routes in \`HairEyeColor\`:
Yates-corrected Pearson \$\chi^2\$ when Cochran's rule is satisfied
(black/brown hair and brown/blue eyes; left), and Fisher's exact test
when expected counts are too small (male participants, black/brown hair,
hazel/green eyes; right). The Yates-corrected plot shows row
percentages; the Fisher plot shows absolute
counts.](visStatistics_files/figure-html/yates-fisher-example-1.png)![Two
\$2 \times 2\$ categorical routes in \`HairEyeColor\`: Yates-corrected
Pearson \$\chi^2\$ when Cochran's rule is satisfied (black/brown hair
and brown/blue eyes; left), and Fisher's exact test when expected counts
are too small (male participants, black/brown hair, hazel/green eyes;
right). The Yates-corrected plot shows row percentages; the Fisher plot
shows absolute
counts.](visStatistics_files/figure-html/yates-fisher-example-2.png)

Two \\2 \times 2\\ categorical routes in `HairEyeColor`: Yates-corrected
Pearson \\\chi^2\\ when Cochran’s rule is satisfied (black/brown hair
and brown/blue eyes; left), and Fisher’s exact test when expected counts
are too small (male participants, black/brown hair, hazel/green eyes;
right). The Yates-corrected plot shows row percentages; the Fisher plot
shows absolute counts.

### Optional rank-correlation mode

Correlation analysis requires the explicit flag `correlation = TRUE`.

#### Kendall rank correlation with `correlation = TRUE`

A hypothetical survey of 150 secondary-school students records alcohol
consumption frequency and academic performance on five-point ordinal
scales. A negative monotone association is induced by construction:
students who consume alcohol more frequently tend to have lower academic
performance. The Kendall result is shown in Figure
@ref(fig:kendall-spearman-example), left.

``` r

set.seed(42)
n <- 150
xs <- sample(1:5, n, replace = TRUE)
ys <- pmin(5, pmax(1, (6 - xs) + sample(-1:1, n, replace = TRUE)))
likert_alc  <- c("never", "rarely", "sometimes", "often", "always")
likert_perf <- c("poor",  "fair",   "ok",        "good",  "great")
alcohol     <- ordered(likert_alc[xs],  levels = likert_alc)
performance <- ordered(likert_perf[ys], levels = likert_perf)
kendall_result <- visstat(performance, alcohol, correlation = TRUE)
spearman_air <- visstat(airquality$Wind, airquality$Ozone, correlation = TRUE)
```

![Rank-based correlations: Left: Kendall's \$\tau_b\$ for a hypothetical
survey (\$n = 150\$): alcohol consumption frequency vs.\\ academic
performance. Right: Spearman rank correlation of \`Wind\` and \`Ozone\`
from the \`airquality\` dataset (\`correlation = TRUE\`; right). Both
plots annotate the corresponding effect measure and
\$p\$-value.](visStatistics_files/figure-html/kendall-spearman-example-1.png)![Rank-based
correlations: Left: Kendall's \$\tau_b\$ for a hypothetical survey (\$n
= 150\$): alcohol consumption frequency vs.\\ academic performance.
Right: Spearman rank correlation of \`Wind\` and \`Ozone\` from the
\`airquality\` dataset (\`correlation = TRUE\`; right). Both plots
annotate the corresponding effect measure and
\$p\$-value.](visStatistics_files/figure-html/kendall-spearman-example-2.png)

Rank-based correlations: Left: Kendall’s \\\tau_b\\ for a hypothetical
survey (\\n = 150\\): alcohol consumption frequency vs. academic
performance. Right: Spearman rank correlation of `Wind` and `Ozone` from
the `airquality` dataset (`correlation = TRUE`; right). Both plots
annotate the corresponding effect measure and \\p\\-value.

#### Spearman rank correlation with `correlation = TRUE`

For the ozone example introduced in Section @ref(sec:lin-reg), staying
within `visstat(,)` with the flag `correlation = TRUE` gives the
Spearman analysis shown in Figure @ref(fig:kendall-spearman-example),
right.

## The `visstat` methods

Objects returned by
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
are of class `"visstat"` and support the S3 methods
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html), and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html).

### `print()`

[`print()`](https://rdrr.io/r/base/print.html) lists the returned
components.

``` r

anova_plantgrowth <- visstat(PlantGrowth$group, PlantGrowth$weight)
```

``` r

print(anova_plantgrowth)
```

    ## Object of class 'visstat'
    ## 
    ## Available components:
    ## [1] "summary statistics of ANOVA" "post-hoc analysis "         
    ## [3] "conf.level"                  "effect_size"

### `summary()`

[`summary()`](https://rdrr.io/r/base/summary.html) prints the full
returned object, including assumption tests, post-hoc comparisons,
confidence level, and `effect_size` where available.

``` r

summary(anova_plantgrowth)
```

    ## Summary of visstat object
    ## 
    ## --- Named components ---
    ## [1] "summary statistics of ANOVA" "post-hoc analysis "         
    ## [3] "conf.level"                  "effect_size"                
    ## 
    ## --- Contents ---
    ## 
    ## $summary statistics of ANOVA:
    ##             Df Sum Sq Mean Sq F value Pr(>F)  
    ## fact         2  3.766  1.8832   4.846 0.0159 *
    ## Residuals   27 10.492  0.3886                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## $post-hoc analysis :
    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = samples ~ fact)
    ## 
    ## $fact
    ##             diff        lwr       upr     p adj
    ## trt1-ctrl -0.371 -1.0622161 0.3202161 0.3908711
    ## trt2-ctrl  0.494 -0.1972161 1.1852161 0.1979960
    ## trt2-trt1  0.865  0.1737839 1.5562161 0.0120064
    ## 
    ## 
    ## $conf.level:
    ## [1] 0.95
    ## 
    ## $effect_size:
    ## $name
    ## [1] "omega-squared"
    ## 
    ## $estimate
    ## [1] 0.2040788
    ## 
    ## $effect_size_method
    ## [1] "Omega-squared for one-way ANOVA"

### `plot()`

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) lists available
plots by default. With `which`, it either replays a captured plot or
reports the selected saved file path.

#### Interactive mode

When
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
is called without a `graphicsoutput` defined (the default interactive
mode), the generated plots are captured internally. Calling
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) without `which`
lists all available plots; calling it with `which` replays the selected
plot in the interactive R session.

``` r

plot(anova_plantgrowth)
```

    ## Plot [1] captured. Use plot(obj, which = 1) to display.

    ## Plot [2] captured. Use plot(obj, which = 2) to display.

``` r

# Interactive only (not executed during vignette build):
plot(anova_plantgrowth, which = 2)
```

#### Saved graphics

When
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
is called with `graphicsoutput` specified,
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) lists the
generated file paths instead. All generated graphics can be saved in any
file format supported by `Cairo()` ([Urbanek and Horner
2025](#ref-Urbanek:2025)), including “png”, “jpeg”, “pdf”, “svg”, “ps”,
and “tiff”. If `plotName` is provided, the main result plot uses this
name. The assumption-diagnostic plot adds the prefix
`"glm_assumptions_"`. If `plotName` is not provided, file names are
generated from the selected plot type and the input variable names.

In the following example, we store the graphics in `png` format in the
`plotDirectory` [`tempdir()`](https://rdrr.io/r/base/tempfile.html)
using the `PlantGrowth` Fisher’s ANOVA example. Here, `plotName` is set
explicitly so that the output names are stable.

``` r

anova_plantgrowth_stored <- visstat(
  PlantGrowth$group,
  PlantGrowth$weight,
  graphicsoutput = "png",
  plotName = "anova_plantgrowth",
  plotDirectory = tempdir()
)
```

The full file paths of the generated graphics are stored as the
attribute `"plot_paths"` on the returned object of class `"visstat"`.

``` r

paths <- attr(anova_plantgrowth_stored, "plot_paths")
print(basename(paths))
```

    ## [1] "glm_assumptions_anova_plantgrowth.png"
    ## [2] "anova_plantgrowth.png"

## Discussion

The design of `visStatistics` prioritises transparent, reproducible
routing for common two-variable analyses ([Strasak et al.
2007](#ref-Strasak:2007); [Sato et al. 2017](#ref-Sato:2017); [Chicco et
al. 2025](#ref-Chicco:2025)) over broad model coverage.

This scope keeps the decision tree inspectable and the graphical output
consistent, but it also leaves several modelling choices (e.g. paired
tests, interaction terms, multiple linear regression) outside the
automated workflow. While one of R’s greatest strengths is the sheer
volume of statistical methods available, incorporating a wider array of
methods would require additional preliminary assumption checks, which in
turn would exacerbate the risk of overall Type I error inflation.
Furthermore, expanding the pipeline would result in a highly complex
decision tree, rendering the underlying statistical logic increasingly
opaque to the user.

`visStatistics` instead focuses on the *vis*ualisation of the chosen
test and, where applicable, its post-hoc and assumption tests.

For tests of central tendency, p-values from assumption tests of
normality and homoscedasticity are used as routing criteria, subject to
the large sample-size safeguards discussed below for normality testing.
However, no single assumption test maintains optimal Type I error rates
and statistical power across all distributions ([Olejnik and Algina
1987](#ref-Olejnik:1987)) and sample sizes, and p-values obtained from
these tests may be unreliable if their assumptions are violated.

Assumption tests are imperfect routing gates: In small samples,
substantial violations of the assumption may not reach statistical
significance, resulting in type II errors, whereas in large samples,
even minor, random deviations from the null hypothesis can result in
statistically significant p-values, leading to type I errors Franc
([2025](#ref-Franc:2025))

Only for residual normality, the Central Limit Theorem provides an exit
from this problem for large samples. This raises the practical question
of what should count as a “large” sample in normality testing. The
answer depends on the shape of the underlying distribution, especially
skewness and tail weight ([Lumley et al. 2002](#ref-Lumley:2002)).
Simulation studies suggest that moderately skewed distributions require
roughly 40–50 observations for adequate convergence of the sampling
distribution of the mean ([Fagerland 2012](#ref-Fagerland:2012)).

Therefore, based on the Central Limit Theorem, normality tests of
residuals do not steer automated routing in the decision logic of
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md),
if all group-specific sample sizes are greater than 50.

For smaller group-specific sample sizes, a Shapiro–Wilk test is used to
route between mean-based and rank-based methods, as simulation studies
suggest that it has the highest power among normality tests in small to
moderate (\\n = 10\\ to 100) sample sizes ([Razali and Wah
2011](#ref-Razali:2011)).

If the assumptions of parametric tests are violated,
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
automatically falls back to non-parametric tests. In the regression
branch, violated assumptions are solely flagged, and the package offers
Spearman rank correlation as a non-causal alternative to linear
regression. Further alternative methods such as data transformation,
generalised linear models, or robust regression are not implemented:
each requires user judgment – about the transformation family, the link
function, or the estimator – that cannot be automated without
substantially expanding the decision tree and increasing the risk of
Type I error inflation.

Assumption tests provide no information on the nature of deviations from
the expected distribution ([Shatz 2024](#ref-Shatz:2024)) and cannot
replace visual inspection of the diagnostic plots generated by
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md),
which may indicate cases where the automatic test choice should be
overridden.

Bootstrapping represents an alternative to assumption-guided routing. As
implemented for example in the R package `boot` ([Canty and Ripley
2025](#ref-boot:2025); [Davison and Hinkley 1997](#ref-Davison:1997)),
it can provide confidence intervals for a wide range of statistics.
However, bootstrapping often requires thousands of resamples and may
perform poorly with very small sample sizes. This runs counter to the
purpose of the `visStatistics` package, which is designed to offer a
rapid overview of the data, laying the groundwork for deeper analysis in
subsequent steps.

At the graphical level, the design is also kept deliberately
low-dependency. The package uses mostly R graphics, keeping the
transitive dependency footprint minimal. For more polished, annotated
plots of chosen statistical tests, we refer to packages such as
`ggstatsplot` ([Patil 2021](#ref-Patil:2021)) or `ggpubr` ([Kassambara
2026](#ref-Kassambara:2026)).

Taken together, these scope decisions define `visStatistics` as a rapid,
inspectable first-line workflow for routine two-variable inference
rather than a replacement for model-specific statistical analysis.

## Conclusion

`visStatistics` is useful where test selection should be reproducible,
visible, and easy to audit. Its value is not that it removes the user’s
statistical judgment, but that it exposes the assumptions, routing
decisions, effect sizes, and plots that should inform that judgment. The
package therefore serves as a transparent entry point for routine
two-variable analyses, leaving model-specific extensions to the analyst.

## (APPENDIX) Appendix

## Assumption tests

### Normality tests

The package displays both the Shapiro–Wilk test and the Anderson–Darling
test to check the normality of raw model residuals \\e_i\\.

#### Shapiro–Wilk test `shapiro.test()`

The Shapiro–Wilk test evaluates whether a sample \\x_1,\ldots,x_n\\
comes from a normal distribution. Let \\x\_{(1)}\le \cdots \le
x\_{(n)}\\ be its order statistics. Introduce a reference sample
\\Z_1,\ldots,Z_n\\ of independent standard normal random variables,
i.e. \\Z_i \sim N(0,1)\\ for all \\i\\, and let \\Z\_{(1)}\le \cdots \le
Z\_{(n)}\\ be their order statistics used to construct the Shapiro–Wilk
weights.

Let \\m_i = \operatorname{E}(Z\_{(i)})\\ and \\v\_{ij} =
\operatorname{Cov}(Z\_{(i)}, Z\_{(j)})\\ for \\i,j = 1,\ldots,n\\.
Define \\\mathbf{m} = (m_1,\ldots,m_n)^\top\\ and \\V =
(v\_{ij})\_{i,j=1}^n\\.

The vector \\\mathbf{m}\\ contains the expected standard-normal order
statistics, and \\V\\ is their covariance matrix. Let
\\\mathbf{a}=(a_1,\ldots,a_n)^\top\\ be the resulting vector of
normalised weights for the ordered observed sample values

\\\mathbf{a} =\frac{V^{-1}\mathbf{m}} {\sqrt{\left(\mathbf{m}^\top
V^{-1}V^{-1}\mathbf{m}\right)}}.\\ Then the Shapiro–Wilk statistic
([Shapiro and Wilk 1965](#ref-Shapiro:1965)) is

\\\begin{equation} W=\frac{\left(\sum\_{i=1}^{n} a_i x\_{(i)}\right)^2}
{\sum\_{i=1}^{n} (x_i-\bar{x})^2} (\\eq:shapiro-w) \end{equation}\\

\\W\\ takes values in \\(0, 1\]\\; values close to 1 indicate normality.

#### Anderson–Darling test `ad.test()`

The Anderson–Darling test ([Anderson and Darling
1952](#ref-Anderson:1952)) is particularly sensitive to deviations in
the tails of the distribution ([Razali and Wah 2011](#ref-Razali:2011);
[Yap and Sim 2011](#ref-Yap:2011)). Let \\z_i = (x\_{(i)} -
\bar{x})/s,\\ i=1,2,\ldots,n\\ be the standardised order statistics of
\\x_i\\, where \\s\\ is the sample standard deviation, and let \\\Phi\\
denote the standard normal cumulative distribution function. The test
statistic is

\\\begin{equation} A^2 = -n - \frac{1}{n}\sum\_{i=1}^{n}(2i-1)
\left\[\ln\Phi(z_i) + \ln\\\left(1 - \Phi(z\_{n+1-i})\right)\right\]
(\\eq:anderson-a2) \end{equation}\\

The implementation uses `ad.test()` from `nortest` ([Gross and Ligges
2015](#ref-Gross:2015)).

### Homoscedasticity tests

#### The mean-centred Levene test `levene.test()`

The package implementation uses Levene’s original mean-centred proposal
([Levene 1960](#ref-Levene:1960)).

The Levene test statistic is the one-way ANOVA \\F\\ statistic, computed
on the absolute residuals \\\|e\_{ij}\|\\ in place of the responses
\\x\_{ij}\\; the corresponding Fisher ANOVA formula is given in
Eq. @ref(eq:fisher-f):

\\\begin{equation} F_L = \frac{\displaystyle\sum\_{i=1}^{k} n_i
(\overline{\|e\|}\_i - \overline{\|e\|})^2\\/\\(k-1)}
{\displaystyle\sum\_{i=1}^{k}\sum\_{j=1}^{n_i}(\|e\_{ij}\| -
\overline{\|e\|}\_i)^2\\/\\(N-k)}, (\\eq:levene-f) \end{equation}\\

where \\\overline{\|e\|}\_i\\ is the within-group mean of the absolute
residuals and \\\overline{\|e\|}\\ is their overall mean.

#### Bartlett’s test `bartlett.test()`

Bartlett’s test statistic ([Bartlett 1937](#ref-Bartlett:1937)) is

\\\begin{equation} K^2 = \frac{(N-k)\ln s_p^2 -
\displaystyle\sum\_{i=1}^k (n_i-1)\ln s_i^2} {1 +
\dfrac{1}{3(k-1)}\\\left(\displaystyle\sum\_{i=1}^k \frac{1}{n_i-1} -
\frac{1}{N-k}\right)}, (\\eq:bartlett-k2) \end{equation}\\

where \\k\\ is the number of groups, \\N = \sum\_{i=1}^k n_i\\ is the
total sample size, \\n_i\\ is the sample size of group \\i\\, \\s_i^2\\
is the sample variance of group \\i\\, and \\s_p^2\\ is the pooled
variance

\\s_p^2 = \frac{1}{N-k}\sum\_{i=1}^k (n_i-1)\\s_i^2.\\

Under the null hypothesis the statistic approximately follows
\\\chi^2(k-1)\\.

#### Breusch–Pagan test `bp.test()`

For simple linear regression, group-based variance tests are not
applicable. The package implementation
[`bp.test()`](https://shhschilling.github.io/visStatistics/reference/bp.test.md)
performs the Koenker variant ([Koenker 1981](#ref-Koenker:1981)) of the
Breusch–Pagan test ([Breusch and Pagan 1979](#ref-Breusch:1979)), which
tests whether the \\N\\ squared residuals \\e_i^2\\ vary systematically
with the fitted values from the regression model \\\hat{y}\_i\\.

The Breusch–Pagan statistic is defined as:

\\\begin{equation} BP = N R^2\_\text{aux} (\\eq:breusch-pagan-bp),
\end{equation}\\

where \\R^2\_\text{aux}\\ denotes the coefficient of determination from
regressing \\e_i^2\\ on \\\hat{y}\_i\\:

\\R^2\_\text{aux} = 1 - \frac{\sum\_{i=1}^{N} (e_i^2 -
\widehat{e_i^2})^2} {\sum\_{i=1}^{N} (e_i^2 - \overline{e^2})^2}.\\

Here \\\widehat{e_i^2}\\ are the fitted values from this auxiliary
regression and \\\overline{e^2}\\ is the mean of the squared residuals.

Under the null hypothesis of homoscedasticity, \\BP\\ is compared
asymptotically to a \\\chi^2(k-1)\\ distribution.

## Parametric tests

In the numeric-response, categorical-predictor branch (Route 1),
parametric tests are selected when residual normality is not rejected,
or when all group-specific sample sizes are greater than 50. The Levene
variance gate then separates equal-variance tests from Welch-type tests.

### Student’s t-test and Fisher’s one-way ANOVA

#### Student’s t-test

Student’s t-test tests the null hypothesis that the means of two
unpaired groups are equal. The test statistic for Student’s t-test
(`t.test(..., var.equal = TRUE)`) is

\\\begin{equation} t = \frac{\bar{x}\_1 - \bar{x}\_2} {s_p
\sqrt{\dfrac{1}{n_1} + \dfrac{1}{n_2}}}, (\\eq:student-t)
\end{equation}\\

where \\\bar{x}\_1\\ and \\\bar{x}\_2\\ are the sample means, \\n_1\\
and \\n_2\\ the sample sizes, and \\s_p\\ the pooled standard deviation

\\s_p = \sqrt{\frac{(n_1-1)s_1^2 + (n_2-1)s_2^2}{n_1+n_2-2}},\\

with \\s_1^2\\ and \\s_2^2\\ the sample variances. The statistic follows
a \\t\\-distribution with \\\nu = n_1 + n_2 - 2\\ degrees of freedom.

#### Fisher’s one-way ANOVA

Fisher’s one-way ANOVA generalises the mean comparison to more than two
groups and tests the null hypothesis that the means of \\k\\ groups are
equal. Fisher’s ANOVA test statistic is

\\\begin{equation} \begin{aligned} F &=
\frac{MS\_\text{between}}{MS\_\text{within}} =
\frac{SS\_\text{between}/(k-1)}{SS\_\text{within}/(N-k)} =
\frac{\displaystyle\sum\_{i=1}^{k} n_i (\bar{x}\_i -
\bar{x})^2\\/\\(k-1)} {\displaystyle\sum\_{i=1}^{k}\sum\_{j=1}^{n_i}
(x\_{ij}-\bar{x}\_i)^2\\/\\(N-k)} \end{aligned}, (\\eq:fisher-f)
\end{equation}\\

where \\MS\_\text{between}\\ and \\MS\_\text{within}\\ are the
between-group and within-group mean squares, \\SS\_\text{between}\\ and
\\SS\_\text{within}\\ are the corresponding sums of squares, \\k\\ is
the number of groups, \\N = \sum\_{i=1}^k n_i\\ is the total sample
size, \\\bar{x}\_i\\ is the mean of group \\i\\, \\\bar{x}\\ is the
overall mean, and \\x\_{ij}\\ is observation \\j\\ in group \\i\\.

From Eq. @ref(eq:fisher-f) follows that in the two-sample case
(\\k=2\\), the squared test statistic of Student’s t-test equals the
Fisher ANOVA test statistic, \\t^2 = F\\, resulting in identical
\\p\\-values for `t.test(var.equal = TRUE)` and
[`aov()`](https://rdrr.io/r/stats/aov.html).

Under \\H_0: \mu_1 = \cdots = \mu_k\\, the statistic follows \\F(k-1,
N-k)\\.

##### Post-hoc comparison

[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
follows [`aov()`](https://rdrr.io/r/stats/aov.html) with Tukey’s Honest
Significant Differences procedure
[`TukeyHSD()`](https://rdrr.io/r/stats/TukeyHSD.html) ([Tukey
1949](#ref-Tukey:1949)). The procedure is designed for pairwise mean
comparisons following ANOVA.

[`TukeyHSD()`](https://rdrr.io/r/stats/TukeyHSD.html) returns adjusted
p-values and confidence intervals for all pairwise differences between
factor-level means. For two groups \\i\\ and \\j\\, let \\d\_{ij} =
\bar{x}\_i - \bar{x}\_j\\. The studentised range statistic is

\\\begin{equation} q\_{ij} = \frac{\|d\_{ij}\|}
{\sqrt{\dfrac{MS\_\text{within}}{2} \left(\dfrac{1}{n_i} +
\dfrac{1}{n_j}\right)}}, (\\eq:tukey-hsd-q) \end{equation}\\

where \\MS\_\text{within}\\ is defined in Eq. @ref(eq:fisher-f).
Adjusted p-values are computed from the studentised range distribution
with \\k\\ groups and \\N-k\\ residual degrees of freedom.

### Welch’s t-test and Welch’s heteroscedastic ANOVA

Welch’s heteroscedastic ANOVA generalises the unequal-variance mean
comparison to more than two groups.

#### Welch’s t-test

Welch’s t-test (`t.test(..., var.equal = FALSE)`) compares the means of
two independent groups when homogeneous variances cannot be assumed. Its
statistic is

\\\begin{equation} t = \frac{\bar{x}\_1 - \bar{x}\_2} {\sqrt{s_1^2/n_1 +
s_2^2/n_2}} (\\eq:welch-t) \end{equation}\\

with degrees of freedom approximated by the Welch–Satterthwaite equation
([Welch 1947](#ref-Welch:1947); [Satterthwaite
1946](#ref-Satterthwaite:1946)):

\\\begin{equation} \nu \approx \frac{\left(\dfrac{s_1^2}{n_1} +
\dfrac{s_2^2}{n_2}\right)^2} {\dfrac{(s_1^2/n_1)^2}{n_1-1} +
\dfrac{(s_2^2/n_2)^2}{n_2-1}}. (\\eq:welch-satterthwaite-df)
\end{equation}\\

Welch’s methods outperform their classical counterparts when variances
differ ([Moser and Stevens 1992](#ref-Moser:1992); [Fagerland and
Sandvik 2009](#ref-Fagerland:2009); [Delacre et al.
2017](#ref-Delacre:2017)).

#### Welch’s heteroscedastic ANOVA

Welch’s heteroscedastic ANOVA
([`oneway.test()`](https://rdrr.io/r/stats/oneway.test.html))
generalises Welch’s t-test to more than two groups by down-weighting
groups with large variance. It compares group means using weights based
on sample sizes and variances when homogeneous variances cannot be
assumed. Its test statistic is

\\\begin{equation} F_W = \frac{\displaystyle\sum\_{i=1}^{k} w_i
(\bar{x}\_i - \bar{x}\_w)^2\\/\\(k-1)} {1 + \dfrac{2(k-2)}{k^2-1}
\displaystyle\sum\_{i=1}^{k} \dfrac{(1-w_i/w)^2}{n_i-1}}, (\\eq:welch-f)
\end{equation}\\

where \\w_i = n_i/s_i^2\\ are the inverse-variance weights, \\w =
\sum\_{i=1}^{k} w_i\\, and \\\bar{x}\_w = \sum\_{i=1}^{k} w_i \bar{x}\_i
/ w\\ is the weighted grand mean. The numerator degree of freedom is
\\k-1\\; the denominator degree of freedom is the Satterthwaite-type
approximation returned by
[`oneway.test()`](https://rdrr.io/r/stats/oneway.test.html).

##### Post-hoc comparison

Post-hoc comparisons use the package implementation
[`games.howell()`](https://shhschilling.github.io/visStatistics/reference/games.howell.md)
([Games and Howell 1976](#ref-Games:1976)). The Games–Howell procedure
is used for pairwise mean comparisons under unequal variances and
unequal sample sizes. For each pairwise comparison, the two groups are
denoted as 1 and 2. The test statistic is

\\\begin{equation} t = \frac{d}{SE}, (\\eq:games-howell-t)
\end{equation}\\

where \\d = \bar{x}\_1 - \bar{x}\_2\\ is the mean difference and \\SE =
\sqrt{s_1^2/n_1 + s_2^2/n_2}\\ its standard error.

Eq. @ref(eq:games-howell-t) is evaluated against a \\t\\ distribution
with \\\nu\\ degrees of freedom from the Welch–Satterthwaite
approximation in Eq. @ref(eq:welch-satterthwaite-df). The resulting
two-sided \\p\\-values are adjusted with Holm’s method ([Holm
1979](#ref-Holm:1979)).

## Non-parametric tests

In the numeric-response, categorical-predictor branch, non-parametric
tests are selected when residual normality is rejected. They are also
used for an ordered response with a categorical predictor.

### Wilcoxon rank-sum test and Kruskal–Wallis test

The Wilcoxon rank-sum test is a two-group rank-based location test;
Kruskal–Wallis generalises this location comparison to more than two
groups.

#### Wilcoxon rank-sum test

The two-sample Wilcoxon rank-sum test, also known as the Mann–Whitney
test, tests for a difference in location between two independent
distributions.

To compare the two groups on one common rank scale, all observations are
pooled before ranking. For two independent groups with sample sizes
\\n_1\\ and \\n_2\\, all \\N = n_1 + n_2\\ observations are assigned
ranks \\1\\ to \\N\\. Let \\W_1 = \sum\_{i=1}^{n_1} R(x\_{1,i})\\ denote
the rank sum of the first group, where \\R(x\_{1,i})\\ is the rank of
observation \\x\_{1,i}\\ in the pooled sample. The test statistic
returned by [`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html)
is the Mann–Whitney \\U\\ statistic ([Mann and Whitney
1947](#ref-Mann:1947)) of the first group:

\\\begin{equation} W = U_1 = W_1 - \frac{n_1(n_1+1)}{2}
(\\eq:wilcoxon-w) \end{equation}\\

An exact \\p\\-value is computed when both groups contain fewer than 50
observations and the data contain no ties; otherwise a normal
approximation with continuity correction is used.

#### Kruskal–Wallis test

The Kruskal–Wallis test compares group distributions based on ranked
values and tests the null hypothesis that the groups come from the same
population, specifically that the distributions have the same location
([Kruskal and Wallis 1952](#ref-Kruskal:1952)). If the group
distributions are sufficiently similar in shape and scale, the test can
be interpreted as testing equality of medians across groups ([Hollander
et al. 2014](#ref-Hollander:2014)).

\\\begin{equation} H = \frac{12}{N(N+1)} \sum\_{i=1}^{k} n_i
\left(\bar{R}\_i - \bar{R}\right)^2, (\\eq:kruskal-h) \end{equation}\\

where \\n_i\\ is the sample size of group \\i\\, \\k\\ is the number of
groups, \\\bar{R}\_i\\ is the average rank of group \\i\\, \\N\\ is the
total sample size, and \\\bar{R} = (N+1)/2\\ is the expected average
rank under the null hypothesis. The statistic approximately follows
\\\chi^2(k-1)\\.

##### Post-hoc comparison

[`pairwise.wilcox.test()`](https://rdrr.io/r/stats/pairwise.wilcox.test.html)
compares each pair of factor levels via the Wilcoxon rank-sum test on
ranks rather than means. The resulting p-values are adjusted for
multiplicity using Holm’s step-down method ([Holm
1979](#ref-Holm:1979)).

## Tests for comparing proportions

For two unordered factors (route 4),
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
tests the null hypothesis that the variables are independent using
Pearson’s \\\chi^2\\ test or Fisher’s exact test, depending on expected
cell counts following Cochran’s rule ([Cochran
1954](#ref-Cochran:1954)).

### Pearson’s \\\chi^2\\ test

Pearson’s \\\chi^2\\ test evaluates the null hypothesis that two
categorical variables are independent.

Let \\O\_{ij}\\ and \\E\_{ij}\\ denote the observed and expected
frequencies in row \\i\\ and column \\j\\ of an \\R \times C\\
contingency table, where rows index the \\R\\ levels of the response
\\y\\ and columns the \\C\\ levels of the predictor \\x\\. The Pearson
residual for cell \\(i,j)\\ is \\\begin{equation} r\_{ij} =
\frac{O\_{ij} - E\_{ij}}{\sqrt{E\_{ij}}}, \quad i = 1,\ldots,R,\quad j =
1,\ldots,C. (\\eq:pearson-residual) \end{equation}\\ The test statistic
of Pearson’s \\\chi^2\\ test is

\\\begin{equation} \chi^2 = \sum\_{i=1}^{R}\sum\_{j=1}^{C} r\_{ij}^2,
(\\eq:pearson-chi) \end{equation}\\

Under the null hypothesis of independence, the statistic is compared
with a \\\chi^2\\ distribution with \\(R-1)(C-1)\\ degrees of freedom.

For \\2\times 2\\ tables, Yates’ continuity correction is applied by
default.

### Fisher’s exact test

Fisher’s exact test
([`fisher.test()`](https://rdrr.io/r/stats/fisher.test.html)) is applied
when Cochran’s rule ([Cochran 1954](#ref-Cochran:1954)) is violated. The
test calculates an exact \\p\\-value by conditioning on the observed
margins of an \\R \times C\\ contingency table under the null hypothesis
of independence. Let \\T = (n\_{ij})\\ denote the observed table. To
maintain consistency with the `y ~ x` (response ~ predictor) framework
used throughout `visStatistics`, rows (\\i=1,\ldots,R\\) represent the
levels of the response variable \\y\\ and columns (\\j=1,\ldots,C\\)
represent the levels of the predictor \\x\\. The row totals are
\\n\_{i\cdot} = \sum\_{j=1}^C n\_{ij}\\ and the column totals are
\\n\_{\cdot j} = \sum\_{i=1}^R n\_{ij}\\.

In the \\2 \times 2\\ case (\\R=2, C=2\\), the table is structured as
follows:

\\\begin{array}{c\|cc\|c} & x_1 & x_2 & \text{Row sums} \\ \hline y_1 &
n\_{11} & n\_{12} & n\_{1\cdot} \\ y_2 & n\_{21} & n\_{22} & n\_{2\cdot}
\\ \hline \text{Column sums} & n\_{\cdot 1} & n\_{\cdot 2} & N
\end{array}\\

The exact probability of observing this table under the null hypothesis
of independence, given the fixed margins, is given by the hypergeometric
probability mass function:

\\\begin{equation} \mathbb{P}(T \mid n\_{1\cdot}, n\_{2\cdot}, n\_{\cdot
1}, n\_{\cdot 2}) = \frac{\binom{n\_{1\cdot}}{n\_{11}}
\binom{n\_{2\cdot}}{n\_{21}}}{\binom{N}{n\_{\cdot 1}}},
(\\eq:fisher-exact) \end{equation}\\

where \\N = n\_{1\cdot} + n\_{2\cdot} = n\_{\cdot 1} + n\_{\cdot 2}\\ is
the total sample size. The \\p\\-value is computed by summing the
probabilities of all tables with the same margins whose probabilities
under the null are less than or equal to that of the observed table. For
general \\R \times C\\ tables,
[`fisher.test()`](https://rdrr.io/r/stats/fisher.test.html) generalises
this approach using the multivariate hypergeometric distribution. For
\\2 \times 2\\ tables,
[`fisher.test()`](https://rdrr.io/r/stats/fisher.test.html) additionally
returns the conditional maximum likelihood estimate of the odds ratio

\\\begin{equation} \widehat{\mathrm{OR}} =
n\_{11}n\_{22}/(n\_{12}n\_{21}) (\\eq:odds-ratio) \end{equation}\\

and its confidence interval.

## Rank correlations

Rank correlations are used when `correlation = TRUE`.

### Kendall rank correlation

Kendall’s \\\tau_b\\ tests the null hypothesis of no monotone
association between two ordered variables. For two ordinal variables
with \\n\\ joint observations, let \\C\\ denote the number of concordant
pairs (those whose ranks agree in both variables) and \\D\\ the number
of discordant pairs. Kendall’s \\\tau_b\\ is defined as

\\\begin{equation} \tau_b \\=\\ \frac{C - D} {\sqrt{\left(n_0 -
n_1\right)\left(n_0 - n_2\right)}}, (\\eq:kendall-tau-b)
\end{equation}\\

where \\n_0 = n(n-1)/2\\ is the total number of observation pairs, \\n_1
= \sum_i t_i(t_i-1)/2\\ is the number of pairs tied in the response, and
\\n_2 = \sum_j u_j(u_j-1)/2\\ is the number of pairs tied in the
predictor. The denominator correction makes \\\tau_b\\ attain \\\pm 1\\
even with ties, which Spearman’s \\\rho\\ does not ([Kendall
1945](#ref-Kendall:1945)). With few ordered levels (e.g., five-point
Likert items), ties are unavoidable; this is the principal reason to
prefer \\\tau_b\\ over Spearman’s \\\rho\\ in this setting ([Agresti
2010](#ref-Agresti:2010)).

[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
calls
`cor.test(as.numeric(y), as.numeric(x), method = "kendall", exact = FALSE)`
and reports \\\tau_b\\, the asymptotic test statistic \\z = \tau_b /
\operatorname{SE}(\tau_b)\\, and the two-sided \\p\\-value.

### Spearman rank correlation

For two numeric variables with `correlation = TRUE`,
[`visstat()`](https://shhschilling.github.io/visStatistics/reference/visstat.md)
calls `cor.test(x, y, method = "spearman")` to test for a monotone
association between \\x\\ and \\y\\ using ranks. Spearman’s \\\rho\\ is
Pearson’s \\r\\ applied to the ranks:

\\\begin{equation} \rho = r(\operatorname{rank}(x),
\operatorname{rank}(y)), (\\eq:spearman-rho) \end{equation}\\

where \\r(u, v)\\ denotes Pearson’s correlation coefficient:

\\r(u,v) = \frac{\sum\_{i=1}^{n}(u_i-\bar u)(v_i-\bar v)}
{\sqrt{\sum\_{i=1}^{n}(u_i-\bar u)^2}\\ \sqrt{\sum\_{i=1}^{n}(v_i-\bar
v)^2}}.\\

Here \\u_i = \operatorname{rank}(x_i)\\ and \\v_i =
\operatorname{rank}(y_i)\\ are the ranks of the \\n\\ paired
observations, and \\\bar{u}\\ and \\\bar{v}\\ are their sample means.

For inference, `cor.test(..., method = "spearman")` computes an exact
\\p\\-value for small samples without ties by evaluating all \\n!\\ rank
permutations. For larger samples or when ties are present, it uses an
approximation to the null distribution of the rank association measure
or its asymptotic transformation. No distributional assumptions on the
original data are required.

A separate Pearson-correlation branch is not implemented. In simple
linear regression with an intercept, the two-sided test of zero slope
and the two-sided test of zero Pearson correlation return the same
\\p\\-value. Pearson correlation would therefore not add a separate
inferential route to the default regression branch.

## Influence diagnostics: Cook’s distance

For simple linear regression, the leverage plot visualises whether
individual observations may exert disproportionate influence on the
fitted line. Cook’s distance combines residual size and leverage for
this purpose ([Cook and Weisberg 1982](#ref-Cook:1982)). In
`visStatistics`, the contours are drawn on the z residual scale used in
the diagnostic panel. With \\z_i\\ defined in Eq. @ref(eq:z-residual),
Cook’s distance for observation \\i\\ is

\\\begin{equation} D_i = \frac{z_i^2 h_i}{k(1-h_i)^2},
(\\eq:cooks-distance-z) \end{equation}\\

with leverage

\\\begin{equation} h_i = \frac{1}{N} +
\frac{(x_i-\bar{x})^2}{\sum\_{r=1}^{N}(x_r-\bar{x})^2}.
(\\eq:leverage-simple-regression) \end{equation}\\

Here \\x_i\\ is the predictor value of observation \\i\\, \\\bar{x}\\ is
the predictor mean, \\N\\ is the total sample size, and \\k = 2\\ is the
number of fitted model parameters.

## Effect size

Statistical significance is strongly affected by sample size, while
effect-size estimates are intended to support comparisons across studies
regardless of sample size ([Levine and Hullett 2002](#ref-Levine:2002)).

To avoid additional package dependencies,
[`effect_size()`](https://shhschilling.github.io/visStatistics/reference/effect_size.md)
extracts, where possible, the effect sizes from base R `stats` output,
Otherwise it implements the remaining formulae internally ([Hedges
1981](#ref-Hedges:1981); [Kerby 2014](#ref-Kerby:2014); [Olejnik and
Algina 2003](#ref-Olejnik:2003); [Ben-Shachar et al.
2020](#ref-BenShachar:2020); [Kelley 1935](#ref-Kelley:1935); [Bergsma
2013](#ref-Bergsma:2013)).

The following tables summarises the statistical analysis with their
respective effect sizes and formulae.

| Analysis | Effect size | Formula | Source |
|:---|:---|:---|:---|
| [Student’s \\t\\-test](#eq:student-t) | Hedges’ \\g\_{s_p}\\ (pooled) | \\g\_{s_p} = J\cdot(\bar{x}\_1-\bar{x}\_2)/s_p\\ | [Hedges 1981](https://doi.org/10.3102/10769986006002107) |
| [Welch’s \\t\\-test](#eq:welch-t) | Hedges’ \\g\_{s^{\*}}\\ (non-pooled) | \\g\_{s^{\*}} = J\cdot(\bar{x}\_1-\bar{x}\_2)/s^{\*}\\ | [Hedges 1981](https://doi.org/10.3102/10769986006002107) |
| [Wilcoxon rank-sum](#eq:wilcoxon-w) | rank-biserial \\r\\ | \\r = 2\cdot W/(n_1\cdot n_2) - 1\\ | [Kerby 2014](https://doi.org/10.2466/11.IT.3.1) |
| [Fisher’s ANOVA](#eq:fisher-f) | \\\omega^2\\ | \\\nu_1\cdot(F-1)/(\nu_1\cdot F + \nu_2 + 1)\\ | [Olejnik and Algina 2003](https://doi.org/10.1037/1082-989X.8.4.434) |
| [Welch’s ANOVA](#eq:welch-f) | \\\omega^2\\ (approx.) | \\\nu_1\cdot(F_W-1)/(\nu_1\cdot F_W + \nu_2 + 1)\\ | [Ben-Shachar et al. 2020](https://doi.org/10.21105/joss.02815) |
| [Kruskal–Wallis](#eq:kruskal-h) | Kelley-adjusted \\\eta_H^2\\ | \\(H - k + 1)/(N - k)\\ | [Kelley 1935](https://doi.org/10.1073/pnas.21.9.554) |
| [Simple linear regression](#sec:lin-reg) | \\R^2\\ | \\R^2 = 1 - SS\_\text{res}/SS\_\text{tot}\\, Eq.~@ref(eq:r-squared) | `summary(lm())$r.squared` |
| [Spearman](#eq:spearman-rho) | \\\rho\\ | \\\rho = r(\operatorname{rank}(x),\operatorname{rank}(y))\\, Eq.~@ref(eq:spearman-rho) | `cor.test()$estimate` |
| [Kendall](#eq:kendall-tau-b) | \\\tau_b\\ | \\\tau_b = (C-D)/\sqrt{\left(n_0-n_1\right)\left(n_0-n_2\right)}\\, Eq.~@ref(eq:kendall-tau-b) | `cor.test()$estimate` |
| [Pearson \\\chi^2\\ (\\R\times C\\)](#eq:pearson-chi) | Cramér’s \\V\\ | \\V\_{R\times C} = \sqrt{\chi^2/\left(N\cdot(\min(R,C)-1)\right)}\\ | [Cohen 2013, p. 223](https://doi.org/10.4324/9780203771587) |
| [Pearson \\\chi^2\\ (\\2\times 2\\)](#eq:pearson-chi) | \\\phi\\ | \\\phi = V\_{2\times 2} = \sqrt{\chi^2/N}\\ | [Cohen 2013, p. 223](https://doi.org/10.4324/9780203771587) |
| [Fisher’s exact (\\2\times 2\\)](#eq:fisher-exact) | odds ratio | \\\widehat{\mathrm{OR}} = n\_{11}n\_{22}/(n\_{12}n\_{21})\\, Eq.~@ref(eq:odds-ratio) | `fisher.test()$estimate` |

(#tab:effect-size-formulae) Effect sizes returned by
[`effect_size()`](https://shhschilling.github.io/visStatistics/reference/effect_size.md).
{.table}

Here, Hedges’ small-sample correction factor is

\\\begin{equation\*} J = \frac{\Gamma((N-2)/2)}
{\sqrt{(N-2)/2}\\\Gamma((N-3)/2)}, \end{equation\*}\\

where \\J\\ denotes Hedges’ correction factor.

The non-pooled average-variance standardizer is

\\\begin{equation\*} s^{\*} = \sqrt{\frac{s_1^2+s_2^2}{2}},
\end{equation\*}\\

where \\s^{\*}\\ denotes the average-variance standardizer.

\\\nu_1\\ and \\\nu_2\\ denote the numerator and denominator degrees of
freedom; for Fisher’s ANOVA, \\\nu_1=k-1\\ and \\\nu_2=N-k\\; for
Welch’s ANOVA, \\\nu_1=k-1\\ and \\\nu_2\\ is the usually fractional
denominator degree of freedom returned by
[`oneway.test()`](https://rdrr.io/r/stats/oneway.test.html).

For simple linear regression, the coefficient of determination is

\\\begin{equation} R^2 = 1 - \frac{SS\_\text{res}}{SS\_\text{tot}},
(\\eq:r-squared) \end{equation}\\

where \\SS\_\text{res}=\sum\_{i=1}^{N}(y_i-\hat{y}\_i)^2\\ is the
residual sum of squares, \\\hat{y}\_i\\ is the predicted value, and
\\SS\_\text{tot}=\sum\_{i=1}^{N}(y_i-\bar{y})^2\\ is the total sum of
squares.

All other variables used in Table @ref(tab:effect-size-formulae) are
defined in the corresponding “Analysis” section.

## References

Agresti, Alan. 2010. *Analysis of Ordinal Categorical Data*. 1st ed.
Wiley Series in Probability and Statistics. Wiley.
<https://doi.org/10.1002/9780470594001>.

Akaike, Hirotugu. 1974. “A New Look at the Statistical Model
Identification.” *IEEE Transactions on Automatic Control* 19 (6):
716–23. <https://doi.org/10.1109/TAC.1974.1100705>.

Allingham, David, and J. C. W. Rayner. 2012. “Testing Equality of
Variances for Multiple Univariate Normal Populations.” *Journal of
Statistical Theory and Practice* 6 (3): 524–35.
<https://doi.org/10.1080/15598608.2012.695703>.

Anderson, T. W., and D. A. Darling. 1952. “Asymptotic Theory of Certain
"Goodness of Fit" Criteria Based on Stochastic Processes.” *The Annals
of Mathematical Statistics* 23 (2): 193–212.
<https://doi.org/10.1214/aoms/1177729437>.

Bartlett, M. S. 1937. “Properties of Sufficiency and Statistical Tests.”
*Proceedings of the Royal Society of London. Series A, Mathematical and
Physical Sciences* 160 (901): 268–82.
<https://doi.org/10.1098/rspa.1937.0109>.

Ben-Shachar, Mattan S., Daniel Lüdecke, and Dominique Makowski. 2020.
“Effectsize: Estimation of Effect Size Indices and Standardized
Parameters.” *Journal of Open Source Software* 5 (56): 2815.
<https://doi.org/10.21105/joss.02815>.

Bergsma, Wicher. 2013. “A Bias-Correction for Cramer’s V and Tschuprow’s
T.” *Journal of the Korean Statistical Society* 42 (3): 323–28.
<https://doi.org/10.1016/j.jkss.2012.10.002>.

Bijlenga, Philippe, Renato Gondar, Sabine Schilling, et al. 2017.
“PHASES Score for the Management of Intracranial Aneurysm: A
Cross-Sectional Population-Based Retrospective Study.” *Stroke* 48 (8):
2105–12. <https://doi.org/10.1161/STROKEAHA.117.017391>.

Breusch, T. S., and A. R. Pagan. 1979. “A Simple Test for
Heteroscedasticity and Random Coefficient Variation.” *Econometrica* 47
(5): 1287–94. <https://doi.org/10.2307/1911963>.

Brodeur, Abel, Nikolai Cook, and Anthony Heyes. 2020. “Methods Matter:
P-Hacking and Publication Bias in Causal Analysis in Economics.”
*American Economic Review* 110 (11): 3634–60.
<https://doi.org/10.1257/aer.20190687>.

Canty, Angelo, and Brian Ripley. 2025. *Boot: Bootstrap Functions*.
Manual. <https://doi.org/10.32614/CRAN.package.boot>.

Chicco, Davide, Andrea Sichenze, and Giuseppe Jurman. 2025. “A Simple
Guide to the Use of Student’s t-Test, Mann-Whitney U Test, Chi-squared
Test, and Kruskal-Wallis Test in Biostatistics.” *BioData Mining* 18
(1): 56. <https://doi.org/10.1186/s13040-025-00465-6>.

Cochran, William G. 1954. “The Combination of Estimates from Different
Experiments.” *Biometrics* 10 (1): 101.
<https://doi.org/10.2307/3001666>.

Cohen, Jacob. 2013. *Statistical Power Analysis for the Behavioral
Sciences*. 2nd ed. Routledge. <https://doi.org/10.4324/9780203771587>.

Cook, R. Dennis, and Sanford Weisberg. 1982. *Residuals and Influence in
Regression*. New York: Chapman and Hall.

Davison, Anthony Christopher, and David Victor Hinkley. 1997. *Bootstrap
Methods and Their Applications*. Cambridge University Press.
<https://doi.org/10.1017/CBO9780511802843>.

Delacre, Marie, Daniël Lakens, and Christophe Leys. 2017. “Why
Psychologists Should by Default Use Welch’s t-Test Instead of Student’s
t-Test.” *International Review of Social Psychology* 30 (1): 92–101.
<https://doi.org/10.5334/irsp.82>.

Fagerland, Morten W. 2012. “T-Tests, Non-Parametric Tests, and Large
Studies—a Paradox of Statistical Practice?” *BMC Medical Research
Methodology* 12 (1): 78. <https://doi.org/10.1186/1471-2288-12-78>.

Fagerland, Morten W., and Leiv Sandvik. 2009. “Performance of Five
Two-Sample Location Tests for Skewed Distributions with Unequal
Variances.” *Contemporary Clinical Trials* 30 (5): 490–96.
<https://doi.org/10.1016/j.cct.2009.06.007>.

Franc, Jeffrey Michael. 2025. “The Misuse of Normality Tests as
Gatekeepers for Research in Prehospital and Disaster Medicine.”
*Prehospital and Disaster Medicine* 40 (5): 241–42.
<https://doi.org/10.1017/S1049023X25101465>.

Fritz, Catherine O., Peter E. Morris, and Jennifer J. Richler. 2012.
“Effect Size Estimates: Current Use, Calculations, and Interpretation.”
*Journal of Experimental Psychology: General* 141 (1): 2–18.
<https://doi.org/10.1037/a0024338>.

Games, Paul A., and John F. Howell. 1976. “Pairwise Multiple Comparison
Procedures with Unequal N’s and/or Variances: A Monte Carlo Study.”
*Journal of Educational Statistics* (US) 1 (2): 113–25.
<https://doi.org/10.2307/1164979>.

Garcia, Luiz. 2026. *autotestR: Automated Functions for Basic
Statistical Tests*. Manual.
<https://doi.org/10.32614/CRAN.package.autotestR>.

Ghasemi, Asghar, and Saleh Zahediasl. 2012. “Normality Tests for
Statistical Analysis: A Guide for Non-Statisticians.” *International
Journal of Endocrinology and Metabolism* 10 (2): 486–89.
<https://doi.org/10.5812/ijem.3505>.

Gross, Juergen, and Uwe Ligges. 2015. *Nortest: Tests for Normality*.
Manual. <https://doi.org/10.32614/CRAN.package.nortest>.

Hayat, Matthew J., Amanda Powell, Tessa Johnson, and Betsy L. Cadwell.
2017. “Statistical Methods Used in the Public Health Literature and
Implications for Training of Public Health Professionals.” *PLOS ONE* 12
(6): e0179032. <https://doi.org/10.1371/journal.pone.0179032>.

Hedges, Larry V. 1981. “Distribution Theory for Glass’s Estimator of
Effect Size and Related Estimators.” *Journal of Educational Statistics*
6 (2): 107–28. <https://doi.org/10.3102/10769986006002107>.

Hollander, Myles, Eric Chicken, and Douglas A. Wolfe. 2014.
*Nonparametric Statistical Methods*. Third edition. Wiley Series in
Probability and Statistics. John Wiley & Sons, Inc.

Holm, Sture. 1979. “A Simple Sequentially Rejective Multiple Test
Procedure.” *Scandinavian Journal of Statistics* 6 (2): 65–70.
<https://www.jstor.org/stable/4615733>.

Kassambara, Alboukadel. 2026. *Ggpubr: ’Ggplot2’ Based Publication Ready
Plots*. Manual. <https://doi.org/10.32614/CRAN.package.ggpubr>.

Kelley, Truman L. 1935. “An Unbiased Correlation Ratio Measure.”
*Proceedings of the National Academy of Sciences* 21 (9): 554–59.
<https://doi.org/10.1073/pnas.21.9.554>.

Kendall, M. G. 1945. “The Treatment of Ties in Ranking Problems.”
*Biometrika* 33 (3): 239–51. <https://doi.org/10.2307/2332303>.

Kerby, Dave S. 2014. “The Simple Difference Formula: An Approach to
Teaching Nonparametric Correlation.” *Comprehensive Psychology* 3.
<https://doi.org/10.2466/11.IT.3.1>.

Koenker, Roger. 1981. “A Note on Studentizing a Test for
Heteroscedasticity.” *Journal of Econometrics* 17 (1): 107–12.
<https://doi.org/10.1016/0304-4076(81)90062-2>.

Kozak, M., and H.-P. Piepho. 2018. “What’s Normal Anyway? Residual Plots
Are More Telling Than Significance Tests When Checking ANOVA
Assumptions.” *Journal of Agronomy and Crop Science* 204 (1): 86–98.
<https://doi.org/10.1111/jac.12220>.

Kruskal, William H., and W. Allen Wallis. 1952. “Use of Ranks in
One-Criterion Variance Analysis.” *Journal of the American Statistical
Association* 47 (260): 583–621. <https://doi.org/10.2307/2280779>.

Levene, Howard. 1960. “Robust Tests for Equality of Variances.” In
*Contributions to Probability and Statistics: Essays in Honor of Harold
Hotelling*, edited by Ingram Olkin. Stanford University Press.

Levine, Timothy R., and Craig R. Hullett. 2002. “Eta Squared, Partial
Eta Squared, and Misreporting of Effect Size in Communication Research.”
*Human Communication Research* 28 (4): 612–25.
<https://doi.org/10.1111/j.1468-2958.2002.tb00828.x>.

Lumley, Thomas, Paula Diehr, Scott Emerson, and Lu Chen. 2002. “The
Importance of the Normality Assumption in Large Public Health Data
Sets.” *Annual Review of Public Health* 23 (1): 151–69.
<https://doi.org/10.1146/annurev.publhealth.23.100901.140546>.

Mann, Henry B., and Donald R. Whitney. 1947. “On a Test of Whether One
of Two Random Variables Is Stochastically Larger Than the Other.” *The
Annals of Mathematical Statistics* 18 (1): 50–60.
<https://doi.org/10.1214/aoms/1177730491>.

Meyer, David, Achim Zeileis, and Kurt Hornik. 2006. “The Strucplot
Framework: Visualizing Multi-Way Contingency Tables with Vcd.” *Journal
of Statistical Software* 17 (3): 1–48.
<https://doi.org/10.18637/jss.v017.i03>.

Meyer, David, Achim Zeileis, Kurt Hornik, and Michael Friendly. 2024.
*vcd: Visualizing Categorical Data*. Manual.
<https://doi.org/10.32614/CRAN.package.vcd>.

Moser, B K, and G. R. Stevens. 1992. “Homogeneity of Variance in the
Two-Sample Means Test.” *The American Statistician*, February, 19–21.
<https://doi.org/10.1080/00031305.1992.10475839>.

Olejnik, Stephen F., and James Algina. 1987. “Type I Error Rates and
Power Estimates of Selected Parametric and Nonparametric Tests of
Scale.” *Journal of Educational Statistics* 12 (1): 45.
<https://doi.org/10.2307/1164627>.

Olejnik, Stephen, and James Algina. 2003. “Generalized Eta and Omega
Squared Statistics: Measures of Effect Size for Some Common Research
Designs.” *Psychological Methods* 8 (4): 434–47.
<https://doi.org/10.1037/1082-989X.8.4.434>.

Patil, Indrajeet. 2021. “Visualizations with Statistical Details: The
’Ggstatsplot’ Approach.” *Journal of Open Source Software* 6 (61): 3167.
<https://doi.org/10.21105/joss.03167>.

R Core Team. 2026. *R: A Language and Environment for Statistical
Computing*. Manual. R Foundation for Statistical Computing.
<https://doi.org/10.32614/R.manuals>.

Rasch, Dieter, Klaus D. Kubinger, and Karl Moder. 2011. “The Two-Sample
t Test: Pre-Testing Its Assumptions Does Not Pay Off.” *Statistical
Papers* 52 (1): 219–31. <https://doi.org/10.1007/s00362-009-0224-x>.

Razali, Nornadiah Mohd, and Yap Bee Wah. 2011. “Power Comparisons of
Shapiro-Wilk, Kolmogorov-Smirnov, Lilliefors and Anderson-Darling
Tests.” *Journal of Statistical Modeling and Analytics* 2 (1): 21–33.

Sato, Yasunori, Masahiko Gosho, Kengo Nagashima, Sho Takahashi, James H.
Ware, and Nan M. Laird. 2017. “Statistical Methods in the Journal; an
Update.” *New England Journal of Medicine* 376 (11): 1086–87.
<https://doi.org/10.1056/NEJMc1616211>.

Satterthwaite, F. E. 1946. “An Approximate Distribution of Estimates of
Variance Components.” *Biometrics Bulletin* 2 (6): 110–14.
<https://doi.org/10.2307/3002019>.

Sau, Arkaprabha, Santanu Phadikar, and Ishita Bhakta. 2025. *boxTest:
Boxplot and Significance Test for Two Groups*. Manual.
<https://doi.org/10.32614/CRAN.package.boxTest>.

Schilling, Sabine. 2026. *visStatistics: Automated Selection and
Visualisation of Statistical Hypothesis Tests*.
<https://doi.org/10.32614/CRAN.package.visStatistics>.

Shapiro, S. S., and M. B. Wilk. 1965. “An Analysis of Variance Test for
Normality (Complete Samples).” *Biometrika* 52 (3-4): 591–611.
<https://doi.org/10.1093/biomet/52.3-4.591>.

Shatz, Itamar. 2024. “Assumption-Checking Rather Than (Just) Testing:
The Importance of Visualization and Effect Size in Statistical
Diagnostics.” *Behavior Research Methods* 56 (2): 826–45.
<https://doi.org/10.3758/s13428-023-02072-x>.

Strasak, Alexander M., Qamruz Zaman, Gerhard Marinell, Karl P. Pfeiffer,
and Hanno Ulmer. 2007. “The Use of Statistics in Medical Research: A
Comparison of "The New England Journal of Medicine" and "Nature
Medicine".” *The American Statistician* 61 (1): 47–55.
<https://www.jstor.org/stable/27643837>.

Subirana, Isaac, Héctor Sanz, and Joan Vila. 2014. “Building Bivariate
Tables: The compareGroups Package for R.” *Journal of Statistical
Software* 57 (12): 1–16.

Thompson, Bruce. 2015. “The Case for Using the General Linear Model as a
Unifying Conceptual Framework for Teaching Statistics and Psychometric
Theory.” *Journal of Methods and Measurement in the Social Sciences* 6
(2). <https://doi.org/10.2458/v6i2.18801>.

Tukey, John W. 1949. “Comparing Individual Means in the Analysis of
Variance.” *Biometrics* 5 (2): 99. <https://doi.org/10.2307/3001913>.

Urbanek, Simon, and Jeffrey Horner. 2025. *Cairo: R Graphics Device
Using Cairo Graphics Library for Creating High-Quality Bitmap (PNG,
JPEG, TIFF), Vector (PDF, SVG, PostScript) and Display (X11 and Win32)
Output*. Manual. <https://doi.org/10.32614/CRAN.package.Cairo>.

Welch, B. L. 1947. “The Generalization of ‘Student’s’ Problem When
Several Different Population Variances Are Involved.” *Biometrika* 34
(1–2): 28–35. <https://doi.org/10.1093/biomet/34.1-2.28>.

Xu, Weichao, Yunhe Hou, Y. S. Hung, and Yuexian Zou. 2013. “A
Comparative Analysis of Spearman’s Rho and Kendall’s Tau in Normal and
Contaminated Normal Models.” *Signal Processing* 93 (1): 261–76.
<https://doi.org/10.1016/j.sigpro.2012.08.005>.

Yap, B. W., and C. H. Sim. 2011. “Comparisons of Various Types of
Normality Tests.” *Journal of Statistical Computation and Simulation* 81
(12): 2141–55. <https://doi.org/10.1080/00949655.2010.520163>.

Zeevat, Wouter. 2025. *Automatedtests: Automating Choosing Statistical
Tests*. Manual. <https://doi.org/10.32614/CRAN.package.automatedtests>.
