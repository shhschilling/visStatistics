# Motivation letter

Dear Editors of the R Journal,

We submit for your consideration the manuscript "visStatistics: Automated selection and visualisation of statistical hypothesis tests in R", describing the CRAN package visStatistics.

The package addresses a recurring problem in applied statistics: the selection of an appropriate hypothesis test requires evaluating variable types, sample sizes, and distributional assumptions simultaneously — a process that is error-prone and time-consuming. While several R packages automate parts of this workflow, none provides a single-function interface that covers the full pipeline from data-type detection through assumption checking to annotated visualisation for all common two-variable inferential frameworks.

The key distinguishing features of visStatistics are: (1) a single entry point `visstat(x, y)` that resolves the full decision pipeline without any user input beyond the data; (2) normality assessment on pooled GLM residuals rather than per group, which is statistically more powerful; (3) assumption diagnostics always displayed alongside results; and (4) a server-side deployment model that allows sensitive data to remain on a secure server while end users interact via a web interface.

The package is available on CRAN and has been applied in a medical scoring tool (Bijlenga et al., 2017) and used in statistical consulting and undergraduate teaching. The manuscript is fully reproducible: all code and figures are generated from the Rmd source using standard CRAN packages.

We believe this manuscript is well suited for the R Journal's audience of R practitioners and developers.

Sincerely,
Sabine Schilling
