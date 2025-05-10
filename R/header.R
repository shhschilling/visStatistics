#' @description
#' Automated Visualization of Statistical Hypothesis Test
#' 
#' \code{visstat()} automatically visualizes the hypothesis test with the highest
#' statistical power between a dependent variable (response) and an independent
#' variable (feature) in a given \code{data.frame} named \code{dataframe}, based on the
#' sample size, distribution, and class of both the response and feature.
#' The data in \code{dataframe} must be structured column-wise, where \code{varsample}
#' and \code{varfactor} are \code{character} strings corresponding to the column names
#' of the response and feature variables, respectively.
#' The automatically generated output figures illustrate the selected statistical
#' hypothesis test, display the main test statistics, and include assumption checks and
#' post hoc comparisons when applicable. The primary test results are returned as a
#' list object.
#'
#'@details
#' Decision logic (for more details, please refer to the package's \code{vignette}).
#'
#' In the following, data of class \code{numeric} or \code{integer} are referred to
#' as numerical, while data of class \code{factor} are referred to as categorical.
#' The significance level \eqn{\alpha} is defined as one minus the confidence level,
#' given by the argument \code{conf.level}. 
#'
#' The choice of statistical tests performed by the function \code{visstat()}
#' depends on whether the data are numerical or categorical, the number of levels in
#' the categorical variable, the distribution of the data, and the chosen \code{conf.level()}.
#'
#' The function prioritizes interpretable visual output and tests that remain valid
#' under their assumptions, following the decision logic below:
#'
#' (1) When the response is numerical and the predictor is categorical, tests
#' of central tendency are performed. If the categorical predictor has two levels:
#' - Welch's t-test (\code{t.test()}) is used if both groups have more than 30
#'   observations (Lumley et al. (2002) <doi:10.1146/annurev.publheath.23.100901.140546>).
#' - For smaller samples, normality is assessed using \code{shapiro.test()}.
#'   If both groups return p-values greater than \eqn{\alpha}, Welch's t-test is applied;
#'   otherwise, the Wilcoxon rank-sum test (\code{wilcox.test()}) is used.
#'
#' For predictors with more than two levels:
#' - An ANOVA model (\code{aov()}) is initially fitted.
#' - Residual normality is tested with \code{shapiro.test()} and \code{ad.test()}.
#'   If \eqn{p > \alpha} for either test, normality is assumed.
#' - Homogeneity of variance is tested with \code{bartlett.test()}:
#'   - If \eqn{p > \alpha}, use ANOVA with \code{TukeyHSD()}.
#'   - If \eqn{p \le \alpha}, use \code{oneway.test()} with \code{TukeyHSD()}.
#' - If residuals are not normal, use \code{kruskal.test()} with
#'   \code{pairwise.wilcox.test()}.
#'
#' (2) When both the response and predictor are numerical, a linear model (\code{lm()})
#' is fitted, with residual diagnostics and a confidence band plot.
#'
#' (3) When both variables are categorical, \code{visstat()} uses \code{chisq.test()} or
#' \code{fisher.test()} depending on expected counts, following Cochran's rule
#' (Cochran (1954) <doi:10.2307/3001666>).
#' Implemented main tests: \code{t.test()}, \code{wilcox.test()},
#' \code{aov()}, \code{oneway.test()}, \code{lm()}, \code{kruskal.test()},
#' \code{fisher.test()}, \code{chisq.test()}.
#'
#' Implemented tests for assumptions:
#' \itemize{
#'   \item Normality: \code{shapiro.test()} and \code{ad.test()}.
#'   \item Heteroscedasticity: \code{bartlett.test()}.
#' }
#'
#' Implemented post hoc tests:
#' \itemize{
#'   \item \code{TukeyHSD()} for \code{aov()} and \code{oneway.test()}.
#'   \item \code{pairwise.wilcox.test()} for \code{kruskal.test()}.
#' } 
