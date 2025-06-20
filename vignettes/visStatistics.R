## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  eval = TRUE,
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5,
  out.width = "100%"
)

## ----setup--------------------------------------------------------------------
library(visStatistics)

## ----install-github, eval = FALSE---------------------------------------------
# install_github("shhschilling/visStatistics")

## ----load, eval = FALSE-------------------------------------------------------
# library(visStatistics)

## ----fig-decision-switch, echo=FALSE, results='asis'--------------------------
if (knitr::opts_knit$get("rmarkdown.pandoc.to") == "html") {
  cat('
<div style="border: 1px solid #666; padding: 10px; display: inline-block; text-align: center;">
  <img src="figures/decision_tree.png" width="100%" 
       alt="Decision tree used to select the appropriate statistical test.">
  <p style="font-style: italic; font-size: 90%; margin-top: 0.5em;">
    Decision tree used to select the appropriate statistical test for a categorical
    predictor and numeric response, based on the number of factor levels, normality,
    and homoscedasticity.
  </p>
</div>
')
} else {
  cat('
\\begin{center}
\\fbox{%
  \\begin{minipage}{0.95\\linewidth}
    \\centering
    \\includegraphics[width=\\linewidth]{../man/figures/decision_tree.png}\\\\
    \\vspace{0.5em}
    \\textit{Decision tree used to select the appropriate statistical test for a categorical predictor and numeric response, based on the number of factor levels, normality, and homoscedasticity.}
  \\end{minipage}
}
\\end{center}
')
}

