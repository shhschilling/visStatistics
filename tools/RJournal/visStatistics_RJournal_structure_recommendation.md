# Recommended R Journal Structure for `visStatistics`

Date: 2026-05-14  
Working folder: `/Users/sschilli/Library/CloudStorage/OneDrive-HochschuleLuzern/Forschung/visStatistics/tools/RJournal`

## Executive Recommendation

The paper should be positioned around a simple but strong argument:

> The most routine frequentist tests are also among the most frequently mis-selected. Analysts often know they need a t-test, Wilcoxon test, ANOVA, Kruskal-Wallis test, chi-squared test, Fisher test, regression, or rank correlation, but they often select among these tests without checking the assumptions that make the choice defensible.

That is the key selling point. `visStatistics` is not valuable because it implements rare methods. It is valuable because it makes the most common two-variable inferential decisions explicit, reproducible, visual, and harder to misuse.

The recommended central claim is:

> `visStatistics` provides a single-entry workflow for common two-variable frequentist inference: it detects variable classes, checks relevant assumptions, selects the corresponding test, returns the complete test result, and visualizes both the diagnostics and the selected result.

This is stronger than presenting the paper as a generic package overview or a derivation of classical tests. The paper should argue that "banal" tests still need disciplined routing, because assumptions are not cosmetic: normality, variance homogeneity, sample size, expected cell counts, and the distinction between regression and correlation determine which frequentist procedure is appropriate.

## Core Unique Selling Point

| Dimension | Why it matters | `visStatistics` claim |
|---|---|---|
| Routine frequentist tests | These are the tests users apply daily, often without enough statistical support. | The package focuses exactly on the high-volume tests where misuse is common. |
| Assumption-aware selection | Choosing t-test vs Wilcoxon, ANOVA vs Kruskal-Wallis vs Welch ANOVA, or chi-squared vs Fisher is assumption-dependent. | The routing logic explicitly checks the assumptions that determine the branch. |
| Residual-based normality | Group-wise normality testing can multiply decisions and inflate false rejection of normality as the number of groups grows. | Central-tendency routing uses normality of model residuals, aligning the decision with the linear-model framework. |
| Visual accountability | Automated choice is risky if users see only the final p-value. | Diagnostic plots are shown with the selected result, so the automation remains inspectable. |
| One entry point | Many tools require users to know the correct function or test before starting. | `visstat(x, y)` and the formula interface provide a single workflow for common two-variable settings. |
| Reproducible workflow | GUI-style test choice is hard to audit. | The decision, result, and plot can be reproduced from scripted R code. |

## Recommended Article Spine

| Order | Section | Purpose | Recommended Treatment |
|---:|---|---|---|
| 1 | Introduction | State the problem: frequentist tests are routine, but assumption-dependent selection is often done informally or incorrectly. | Keep the frequentist argument central. Avoid making it sound as if the package is merely for convenience. |
| 2 | Existing CRAN software and gap | Benchmark against CRAN packages that automate tests, produce tables, or add statistical annotations to plots. | Use the CRAN competitor table below. Make clear that the gap is assumption-aware, visual, single-call test routing. |
| 3 | Package interface and returned object | Introduce `visstat(x, y)`, formula input, the returned `"visstat"` object, and its `print()`, `summary()`, and `plot()` methods. | This is the only place where the methods should be introduced systematically. Keep concise and software-focused. |
| 4 | Decision framework | Present the overview figure and central-tendency tree. Explain the assumptions that drive each split. | This should be the intellectual center of the paper. |
| 5 | Worked examples | Demonstrate representative branches and show how diagnostics support the selected test. | Use fewer examples, but make each example explicitly show "assumption -> branch -> result". |
| 6 | Reproducibility and deployment | Explain reproducible reports, server-side use, teaching, consulting, and browser workflows. | Do not re-introduce `print()`, `summary()`, or `plot()` here; refer back to Section 3 only if needed. |
| 7 | Limitations | State what the package deliberately does not automate: paired designs, interactions, multiple regression visualization, GLMs, robust regression, transformations. | Frame this as a guardrail against false confidence in automatic testing. |
| 8 | Summary | Restate the package as assumption-aware automation for routine frequentist inference. | Short. |
| Appendix A | Linear-model framework | Explain why t-test, ANOVA, and regression belong together. | Keep; this supports the residual-normality argument. |
| Appendix B or supplement | Test-statistic formulas | Definitions of tests and association measures. | Move most formulas out of the main narrative unless directly needed for routing. |

## Suggested Main-Text Restructure

1. **Introduction**
   - Start with the applied failure mode: users frequently choose among common frequentist tests without formally checking assumptions.
   - State that `visStatistics` targets common two-variable inference, not advanced modeling.
   - Make the "ordinary tests, disciplined choice" argument explicit.

2. **Related CRAN software**
   - Compare against packages that automate tests (`automatedtests`, `autotestR`), generate inferential summary tables (`compareGroups`, `gtsummary`, `tableone`), provide tidy test wrappers (`rstatix`), annotate plots (`ggstatsplot`, `ggpubr`, `statsExpressions`), or report existing results (`report`).
   - The critical distinction: most competitors either execute a user-selected test, create tables, or annotate graphics; `visStatistics` makes the assumption-checking and branch decision visible.
   - Use package vignettes as structural inspiration: short "basic usage" examples first, then customization/edge cases, then interpretation.

3. **Package interface and returned object**
   - `visstat(x, y)`.
   - `visstat(y ~ x, data = df)`.
   - Returned `"visstat"` object.
   - `print()`: compact console report of selected test and main statistic.
   - `summary()`: complete statistical output, assumption checks, post-hoc results where applicable.
   - `plot()`: diagnostic and result graphics.
   - Optional graphics export.

4. **Decision logic**
   - Put the overview figure first.
   - Then central-tendency decision tree.
   - Add a routing table with columns: input classes, assumption check, selected test family, visualization.

5. **Examples**
   - Do not try to show every branch with equal weight.
   - Each example should read as: "The assumption diagnostics show X; therefore `visstat()` selects Y; the output visualizes Z."
   - Recommended main examples: Welch t-test, Kruskal-Wallis with post-hoc, simple regression, Spearman correlation, chi-squared with mosaic residuals, Fisher exact test.
   - Borrow the vignette convention of showing the minimum runnable call before explaining details. For each branch, show one short code chunk, one figure, and one paragraph interpreting the route.

6. **Reproducibility and deployment**
   - Explain how the single returned object supports scripted reports.
   - Discuss server-side/browser use, teaching, and consulting.
   - Do not repeat method documentation; this section should show why the interface matters in practice.

7. **Limitations and misuse prevention**
   - Put this before the summary, not as an afterthought.
   - Make clear that automatic test selection is a guide, not a substitute for study-design knowledge.

## CRAN Competitor Benchmark

The benchmark should be against CRAN packages, not only R Journal papers. These packages are similar in workflow scope, user audience, or output type.

| # | CRAN package | Main scope | How tests are selected / used | Visualization / diagnostics | Where `visStatistics` is different | USP implication |
|---:|---|---|---|---|---|---|
| 1 | [`automatedtests`](https://cran-e.com/package/automatedtests) | Automatically chooses and runs statistical tests. | Very close competitor: designed to select an appropriate test automatically. | Returns clear test results; visualization is not the central package identity. | `visStatistics` should differentiate through diagnostic plots, residual-based normality for central-tendency tests, and the combined visual result. | Treat this as the closest direct competitor. The paper must explain why `visStatistics` is statistically and visually more transparent. |
| 2 | [`autotestR`](https://cran.r-universe.dev/autotestR) | Automated functions for basic statistical tests, including t-test, Mann-Whitney, Pearson correlation, ANOVA, and diagnostic helpers. | Includes `pre.test()` and test-specific functions with automated checking. | Uses `ggplot2`/`dplyr`-compatible workflows and integrated visuals. | `visStatistics` can claim a more unified two-vector interface, explicit class-driven routing, residual-normality argument, and minimal dependency/base-graphics design. | Important competitor for "basic tests made accessible"; distinguish on one-call routing and assumption plots tied to branch decisions. |
| 3 | [`compareGroups`](https://search.r-project.org/CRAN/refmans/compareGroups/html/compareGroups.html) | Descriptive and bivariate comparison tables, common in biomedical reporting. | Computes different tests depending on variable type; documentation mentions t-test, ANOVA, Kruskal-Wallis, Fisher, log-rank, etc. | Has plots and normality plots in package documentation, but table/reporting is primary. | `visStatistics` is not a table generator; it makes the test-selection logic and diagnostic plots the main output. It also argues against group-wise normality routing. | Strong comparator because current manuscript already cites it; use it to sharpen the residual-normality USP. |
| 4 | [`gtsummary`](https://cran.r-universe.dev/gtsummary/doc/manual.html) | Publication-ready summary and analytic tables. | `add_p()` chooses defaults based on variable type and number of groups; users can override tests. | Tables, not diagnostic plots. | `visStatistics` focuses on test-choice justification and graphics, not presentation tables. | Good example of polished reporting, but `visStatistics` owns the assumption-aware decision path. |
| 5 | [`tableone`](https://www.rdocumentation.org/packages/tableone/versions/0.13.2/topics/CreateTableOne) | Table 1 baseline characteristics with optional group comparisons. | Performs statistical tests for stratified baseline variables; exact tests can be specified. | No central diagnostic visualization workflow. | `visStatistics` is for individual two-variable inference and diagnostic visualization, not baseline tables. | Position `visStatistics` as pre-table or teaching/consulting workflow where the user needs to understand why a test was selected. |
| 6 | [`rstatix`](https://search.r-project.org/CRAN/refmans/rstatix/html/00Index.html) | Pipe-friendly wrappers for basic statistical tests. | Users generally choose the function: `t_test()`, `wilcox_test()`, `anova_test()`, `kruskal_test()`, etc. | Helpers for significance labels and plot positions, but diagnostics are not the central decision workflow. | `visStatistics` chooses the branch for the user and visualizes assumptions plus result. | Useful contrast: `rstatix` makes chosen tests tidy; `visStatistics` helps choose the test. |
| 7 | [`ggstatsplot`](https://cran-e.com/package/ggstatsplot) | `ggplot2`-based plots with statistical details. | Supports parametric, nonparametric, robust, Bayesian tests, but users choose the plot/test context. | Very strong annotated visualization. | `visStatistics` is less polished graphically but stronger as a decision-routing tool with assumption diagnostics. | Acknowledge `ggstatsplot` for polished statistical graphics; differentiate on automatic assumption-aware route selection. |
| 8 | [`ggpubr`](https://cran.r-universe.dev/ggpubr/doc/manual.html) | Publication-ready `ggplot2` helpers and statistical comparison annotations. | Users specify statistical comparison methods such as t-test, Wilcoxon, ANOVA, etc. | Strong plot annotation; not an assumption-checking router. | `visStatistics` decides the test based on data and assumptions before plotting. | `ggpubr` is a plotting/output competitor, not a selection competitor. |
| 9 | [`statsExpressions`](https://search.r-project.org/CRAN/refmans/statsExpressions/html/00Index.html) | Creates tidy data frames and expressions with statistical details, often powering annotated plots. | Users choose statistical context; package formats results/expressions. | Supports statistical annotation workflows. | `visStatistics` performs the end-to-end decision and result plotting rather than only preparing statistical labels. | Useful to show that output annotation exists elsewhere; the unique claim remains assumption-aware routing. |
| 10 | [`report`](https://stat.ethz.ch/CRAN/web/packages/report/vignettes/report.html) | Automated reporting of R objects and statistical models. | Summarizes existing objects from tests/models; does not choose the test for the user. | Text/report output, not diagnostic route visualization. | `visStatistics` generates the test decision and result object; `report` could report the object afterward. | Complementary package; reinforces that `visStatistics` is upstream of reporting. |

## USP Against the CRAN Landscape

| Competitor family | What they do well | Gap left for `visStatistics` |
|---|---|---|
| Automated test selectors (`automatedtests`, `autotestR`) | Lower the barrier to choosing basic tests. | `visStatistics` must own transparent diagnostics, residual-based normality, and plots that show why the branch was selected. |
| Summary-table packages (`compareGroups`, `gtsummary`, `tableone`) | Produce publication-ready descriptive/inferential tables. | They often hide the decision logic in table defaults; `visStatistics` makes the branch visible and teachable. |
| Tidy test wrappers (`rstatix`) | Make common tests easier to run in tidy workflows. | They assume users know which test function to call; `visStatistics` helps select the function. |
| Statistical plot annotation packages (`ggstatsplot`, `ggpubr`, `statsExpressions`) | Produce polished plots with p-values/test labels. | They mostly start after the plot/test context is chosen; `visStatistics` chooses and explains the context. |
| Reporting packages (`report`) | Turn model/test objects into readable prose. | They report existing analyses; `visStatistics` creates the assumption-aware analysis path. |

## Vignette Patterns to Borrow

The article should not copy competitors' content, but their vignettes are useful for structure. The best vignettes make the user journey obvious: minimum call first, then explanation, then customization or caveats. `visStatistics` should use that pattern while keeping the R Journal paper more analytical.

| Package vignette/source pattern | What it does well | What to borrow for `visStatistics` |
|---|---|---|
| `automatedtests` vignette / README | Leads with the promise of one-line automated testing. | Acknowledge this direct competitor, then show that `visstat()` adds diagnostic visual accountability: not only "which test", but "why this test". |
| `autotestR` manual/vignette style | Separates automatic suggestion (`pre.test()`) from test-specific functions and examples. | Keep `visStatistics` simpler: one main entry point first. Avoid scattering the story across many function subsections. |
| `compareGroups` vignette/manual | Starts from biomedical-style bivariate tables and shows how many variables can be compared by group. | Use as contrast: `compareGroups` is table-oriented; `visStatistics` is decision- and diagnostic-oriented. Include only one table-style comparison in the related-software section, not in examples. |
| `gtsummary` vignettes | Excellent progressive structure: basic table, add p-values, customize tests, then reporting. | Borrow the progressive pedagogy: basic `visstat()` call, inspect selected route, then show `summary()`/`plot()` only once. |
| `tableone` examples | Clear medical-reporting use case and simple defaults. | Use the medical/scoring use case as motivation, but do not let "Table 1" framing dominate the article. |
| `rstatix` documentation | Function list maps directly onto common tests. | Use it as a foil: tidy wrappers require choosing `t_test()`, `kruskal_test()`, etc.; `visStatistics` chooses the branch and makes the assumptions visible. |
| `ggstatsplot` vignettes | Strong result plots with statistical annotations and a clear visual payoff. | Make `visStatistics` figures work harder: captions should state the selected test, assumption outcome, and interpretation. |
| `ggpubr` vignettes | Practical plotting examples with statistical comparison labels. | Do not compete mainly on plot polish. Emphasize that `visStatistics` determines the statistical route before drawing the plot. |
| `statsExpressions` documentation | Separates statistical computation from plot annotation. | Clarify that `visStatistics` is not just label generation; it performs routing, diagnostics, and plotting together. |
| `report` vignette | Shows how existing statistical objects can be transformed into readable prose. | In Section 6, mention that `visstat()` output could be reported downstream, but the paper should keep the focus on upstream test selection. |

## Vignette-Inspired Example Template

Use this repeated micro-structure for each worked example:

| Element | Content |
|---|---|
| Data situation | "Numeric response and two-level factor" or "two categorical variables". |
| Minimum call | One short code chunk, usually `visstat(x, y)` or `visstat(y ~ x, data)`. |
| Diagnostic trigger | One sentence: which assumption check mattered and what it implied. |
| Selected test | Name the test and why it follows from the diagnostic trigger. |
| Visual output | Figure caption states both diagnostic panel and result panel. |
| Returned object | Only where needed, show `summary()` for a branch-specific detail such as post-hoc adjusted p-values or odds ratio. |

This template prevents the examples from becoming a catalogue. It also keeps `summary()` and `plot()` in their proper role: introduced in Section 3, then used sparingly in examples when they support interpretation.

## Recommended Positioning Sentence

Use a version of this in the introduction:

> Although tests such as the t-test, Wilcoxon rank-sum test, ANOVA, Kruskal-Wallis test, chi-squared test, Fisher's exact test, regression, and rank correlation are among the most common tools in applied statistics, their correct use depends on assumptions that are often checked informally or not at all. `visStatistics` targets this gap by making routine frequentist test selection explicit, assumption-aware, visual, and reproducible.

## Recommended Abstract Revision

The CRAN `Description` has two phrases worth preserving in the article abstract: "The right test, visualised" and the claim that the workflow "shifts attention from ad-hoc test selection to visual diagnostic assessment and statistical interpretation." The abstract should be shorter than the CRAN Description, but it should keep that emphasis.

> `visStatistics` provides an assumption-aware workflow for routine two-variable frequentist inference in R: the right test, visualised. Given two vectors, or a formula and data frame, `visstat()` detects whether variables are numerical, categorical, or ordinal; evaluates the assumptions relevant to common parametric and non-parametric procedures; selects the corresponding hypothesis test; returns the complete result; and produces annotated diagnostic and result plots. The package covers central-tendency comparisons, simple linear regression, rank correlation, contingency-table independence tests, and ordinal association. Its central motivation is that even the most widely used frequentist tests are often selected without adequate assessment of residual normality, variance homogeneity, sample size, or expected cell counts. By combining automatic routing with residual plots, Q-Q plots, box plots, bar charts, regression lines with confidence bands, and mosaic plots where appropriate, `visStatistics` shifts attention from ad-hoc test selection to visual diagnostic assessment and statistical interpretation. This design supports reproducible reports, teaching, statistical consulting, and server-side R applications where users interact through a web interface.

## Concrete Editing Priorities

| Priority | Edit | Rationale |
|---:|---|---|
| 1 | Rewrite the opening around routine frequentist tests and assumption-dependent misuse. | This is the paper's strongest motivation. |
| 2 | Add a CRAN competitor table after the introduction. | Reviewers need to see how `visStatistics` differs from existing CRAN options. |
| 3 | Emphasize residual-normality routing in central-tendency tests. | This is the strongest technical differentiator versus table-style or group-wise normality workflows. |
| 4 | Add a routing table: input classes, assumption checks, selected test, plot output. | Makes the package logic easier to evaluate than prose alone. |
| 5 | Move long derivations to appendix/supplement. | The main paper should sell the software workflow and statistical rationale, not become a textbook. |
| 6 | In examples, explicitly state the assumption result that triggers each branch. | This trains readers to see `visStatistics` as assumption-aware, not just automated. |
| 7 | Use competitor vignettes as style references, not as content templates. | Borrow their progressive teaching structure: basic call, interpretation, customization/caveat. |
| 8 | Keep limitations strong. | Automated testing is sensitive; reviewers will trust the paper more if the limits are direct. |
| 9 | Clean language and typos. | Current issues include "frequentest", "decicison", "as well ass", "squred", and repeated wording. |

## Proposed Related-Software Paragraph

> Existing CRAN packages address adjacent parts of this workflow. `automatedtests` and `autotestR` explicitly aim to simplify or automate basic statistical testing. `compareGroups`, `gtsummary`, and `tableone` compute inferential comparisons while producing descriptive or publication-ready tables. `rstatix` offers pipe-friendly wrappers around common tests, while `ggstatsplot`, `ggpubr`, and `statsExpressions` add statistical information to visualizations. `report` converts existing statistical objects into readable text. `visStatistics` occupies a narrower but distinct position: it focuses on routine two-variable frequentist inference and integrates variable-class detection, assumption diagnostics, test routing, test execution, and diagnostic/result visualization in one reproducible call.

## Suggested Figure/Table Additions

1. **Workflow figure**: data input -> class detection -> assumption checks -> test route -> result object -> diagnostic/result plots.
2. **Routing table**: input class combination x assumption check x selected framework.
3. **CRAN competitor table**: package, primary task, who selects the test, diagnostics shown, visualization included, single-call routing.
4. **Object-output table in Section 3**: `print()`, `summary()`, `plot()`, saved graphics.
5. **Example route table**: example dataset, diagnostic trigger, selected test, figure/result shown.

## Sources Used

- `automatedtests`: <https://cran-e.com/package/automatedtests>
- `autotestR`: <https://cran.r-universe.dev/autotestR>
- `compareGroups`: <https://search.r-project.org/CRAN/refmans/compareGroups/html/compareGroups.html>
- `gtsummary`: <https://cran.r-universe.dev/gtsummary/doc/manual.html>
- `tableone`: <https://www.rdocumentation.org/packages/tableone/versions/0.13.2/topics/CreateTableOne>
- `rstatix`: <https://search.r-project.org/CRAN/refmans/rstatix/html/00Index.html>
- `ggstatsplot`: <https://cran-e.com/package/ggstatsplot>
- `ggpubr`: <https://cran.r-universe.dev/ggpubr/doc/manual.html>
- `statsExpressions`: <https://search.r-project.org/CRAN/refmans/statsExpressions/html/00Index.html>
- `report`: <https://stat.ethz.ch/CRAN/web/packages/report/vignettes/report.html>
