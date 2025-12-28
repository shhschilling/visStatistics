# Header visstat -----
#'
#' Wrapper for visstat_core Allowing Three Different Input Styles
#'
#' @description \code{visstat()} is a wrapper around \code{\link{visstat_core}} 
#' that provides three alternative input styles: a formula interface, a 
#' standardised vector interface, and a backward-compatible data frame interface.
#' \code{\link{visstat_core}} defines the decision logic for statistical 
#' hypothesis testing and visualisation between two variables of class 
#' \code{"numeric"}, \code{"integer"}, or \code{"factor"}.
#'
#' @param x For the formula interface: a formula of the form \code{y ~ x}, 
#'   where \code{y} is the response variable and \code{x} is the predictor or 
#'   grouping variable (requires \code{data} argument). For the standardised 
#'   form: a vector of class \code{"numeric"}, \code{"integer"}, or 
#'   \code{"factor"} representing the predictor or grouping variable. For the 
#'   backward-compatible form: a \code{data.frame} containing the relevant 
#'   columns.
#' @param y For the formula interface: not used (variables are extracted from 
#'   the formula). For the standardised form: a vector of class \code{"numeric"}, 
#'   \code{"integer"}, or \code{"factor"} representing the response variable. 
#'   For the backward-compatible form: a \code{character} string specifying the 
#'   name of the response variable column in \code{x}.
#' @param ... For the backward-compatible form only: a \code{character} string 
#'   specifying the name of the predictor or grouping variable column in 
#'   \code{x}. Ignored for formula and standardised input styles.
#' @param data A \code{data.frame} containing the variables specified in the 
#'   formula. Required when using the formula interface. Ignored for other 
#'   input styles.
#' @param conf.level Confidence level for statistical inference; default is 
#'   \code{0.95}.
#' @param do_regression Logical. If \code{TRUE} (default), performs simple 
#'   linear regression analysis with confidence and prediction bands when both 
#'   variables are numeric. If \code{FALSE}, performs correlation analysis with 
#'   trend line only (no regression interpretation).
#' @param numbers Logical. Whether to annotate plots with numeric values.
#' @param minpercent Number between 0 and 1 indicating minimal fraction of 
#'   total count data of a category to be displayed in mosaic count plots.
#' @param graphicsoutput Saves plot(s) of type \code{"png"}, \code{"jpg"}, 
#'   \code{"tiff"} or \code{"bmp"} in directory specified in 
#'   \code{plotDirectory}. If \code{NULL}, no plots are saved.
#' @param plotName Graphical output is stored following the naming convention 
#'   \code{"plotName.graphicsoutput"} in \code{plotDirectory}. Without 
#'   specifying this parameter, \code{plotName} is automatically generated 
#'   following the convention \code{"statisticalTestName_varsample_varfactor"}.
#' @param plotDirectory Specifies directory where generated plots are stored. 
#'   Default is current working directory.
#'
#' @details This wrapper supports three input formats:
#' 
#' (1) Formula interface: \code{visstat(y ~ x, data = df)}, where the formula 
#' specifies the response (\code{y}) and predictor (\code{x}) variables, and 
#' \code{data} is a data frame containing these variables.
#'
#' (2) Standardised form: \code{visstat(x, y)}, where both \code{x} and \code{y} 
#' are vectors of class \code{"numeric"}, \code{"integer"}, or \code{"factor"}. 
#' Here \code{x} is the predictor or grouping variable and \code{y} is the 
#' response variable.
#'
#' (3) Backward-compatible form: \code{visstat(dataframe, "name_of_y", "name_of_x")}, 
#' where the first character string refers to the response variable and the 
#' second to the predictor or grouping variable. Both must be column names in 
#' \code{dataframe}.
#'
#' The interpretation of \code{x} and \code{y} depends on the variable classes. 
#' Throughout, data of class \code{numeric} or \code{integer} are referred to 
#' as numeric, while data of class \code{factor} are referred to as categorical:
#'
#' If one variable is numeric and the other a factor, the numeric vector is the 
#' response (\code{y}) and the factor is the grouping variable (\code{x}). This 
#' supports tests of central tendencies (e.g., t-test, Welch's ANOVA, Wilcoxon, 
#' Kruskal-Wallis).
#'
#' If both variables are numeric, a linear model is fitted with \code{y} as the 
#' response and \code{x} as the predictor.
#'
#' If both variables are factors, an association test (Chi-squared or Fisher's 
#' exact) is used. The test result is invariant to variable order, but 
#' visualisations (e.g., axis layout, bar orientation) depend on the roles of 
#' \code{x} and \code{y}.
#'
#' This wrapper standardises the input and calls \code{\link{visstat_core}}, 
#' which selects and executes the appropriate test with visual output and 
#' assumption diagnostics.
#'
#' @return An object of class \code{"visstat"} containing the results of 
#' the automatically selected statistical test. The specific contents depend on 
#' which test was performed. Additionally, the returned object includes two 
#' attributes:
#' \itemize{
#'   \item \code{plot_paths}: Character vector of file paths where plots were 
#'     saved (if \code{graphicsoutput} was specified)
#'   \item \code{captured_plots}: List of captured plot objects for programmatic 
#'     access
#' }
#'
#' In case of insufficient data, returns a list with an \code{error} element and 
#' basic input summary information.
#'
#' @note For best visualization, ensure the RStudio Plots pane is adequately 
#' sized. If you get "figure margins too large" errors, try expanding the Plots 
#' pane in RStudio, using \code{dev.new(width=10, height=6)} for a larger plot 
#' window, or reducing the \code{cex} parameter.
#'
#' @seealso \code{\link{visstat_core}} defining the decision logic, the 
#' package's vignette \code{vignette("visStatistics")} explaining the decision 
#' logic accompanied by illustrative examples, and the accompanying webpage 
#' \url{https://shhschilling.github.io/visStatistics/}.
#'
#' @examples
#' # Formula interface
#' mtcars$am <- as.factor(mtcars$am)
#' visstat(mpg ~ am, data = mtcars)
#'
#' # Standardised usage
#' visstat(mtcars$am, mtcars$mpg)
#'
#' # Backward-compatible usage (same result as above)
#' visstat(mtcars, "mpg", "am")
#'
#' ## Student's t-test (equal variances, two groups)
#' # When residuals are normally distributed and Levene's test indicates
#' # homoscedasticity, the classic Student's t-test with pooled variance is used
#' visstat(sleep$group, sleep$extra)
#'
#' ## Welch's t-test (unequal variances, two groups)
#' # When residuals are normally distributed but Levene's test indicates
#' # heteroscedasticity, Welch's t-test is used
#' visstat(mtcars$am, mtcars$mpg)
#'
#' ## Wilcoxon rank sum test (non-normal, two groups)
#' # When residuals are not normally distributed
#' grades_gender <- data.frame(
#'   Sex = as.factor(c(rep("Girl", 20), rep("Boy", 20))),
#'   Grade = c(
#'     19.3, 18.1, 15.2, 18.3, 7.9, 6.2, 19.4, 20.3, 9.3, 11.3,
#'     18.2, 17.5, 10.2, 20.1, 13.3, 17.2, 15.1, 16.2, 17.3, 16.5,
#'     5.1, 15.3, 17.1, 14.8, 15.4, 14.4, 7.5, 15.5, 6.0, 17.4,
#'     7.3, 14.3, 13.5, 8.0, 19.5, 13.4, 17.9, 17.7, 16.4, 15.6
#'   )
#' )
#' visstat(grades_gender$Sex, grades_gender$Grade)
#'
#' ## Fisher's ANOVA (equal variances, >2 groups)
#' # When residuals are normally distributed and Levene's test indicates
#' # homoscedasticity, classic Fisher's ANOVA with TukeyHSD post-hoc is used
#' visstat(PlantGrowth$group, PlantGrowth$weight)
#'
#' ## Welch's one-way ANOVA (unequal variances, >2 groups)
#' # When residuals are normally distributed but Levene's test indicates
#' # heteroscedasticity, Welch's ANOVA with Games-Howell post-hoc is used
#' visstat(npk$block, npk$yield) 
#'
#' ## Kruskal-Wallis (non-normal, >2 groups)
#' # When residuals are not normally distributed
#' visstat(iris$Species, iris$Petal.Width)
#'
#' ## Simple linear regression (both numeric)
#' visstat(trees$Height, trees$Girth, conf.level = 0.99)
#'
#' ## Pearson's Chi-squared test (both factors, large expected counts)
#' HairEyeColorDataFrame <- counts_to_cases(as.data.frame(HairEyeColor))
#' visstat(HairEyeColorDataFrame$Eye, HairEyeColorDataFrame$Hair)
#'
#' ## Fisher's exact test (both factors, small expected counts)
#' HairEyeColorMaleFisher <- HairEyeColor[, , 1]
#' blackBrownHazelGreen <- HairEyeColorMaleFisher[1:2, 3:4]
#' blackBrownHazelGreen <- counts_to_cases(as.data.frame(blackBrownHazelGreen))
#' visstat(blackBrownHazelGreen$Eye, blackBrownHazelGreen$Hair)
#'
#' ## Save PNG
#' visstat(blackBrownHazelGreen$Hair, blackBrownHazelGreen$Eye,
#'         graphicsoutput = "png", plotDirectory = tempdir())
#'
#' ## Custom plot name
#' visstat(iris$Species, iris$Petal.Width,
#'         graphicsoutput = "pdf", plotName = "kruskal_iris", plotDirectory = tempdir())
#'
#' @export
visstat <- function(x,
                    y,
                    ...,
                    data = NULL,
                    conf.level = 0.95,
                    do_regression = TRUE,
                    numbers = TRUE,
                    minpercent = 0.05,
                    graphicsoutput = NULL,
                    plotName = NULL,
                    plotDirectory = getwd()) {
  # store default graphical parameters------
  oldparvisstat <- par(no.readonly = TRUE)
  on.exit(par(oldparvisstat))
  
  check_visstat_input(x, y, ..., data = data)
  
  clean_name <- function(expr) {
    sub(".*\\$", "", deparse(expr))
  }
  
  # Case 0: formula interface
  if (inherits(x, "formula")) {
    if (is.null(data)) {
      stop("Formula input requires a 'data' argument.")
    }
    
    vars <- all.vars(x)
    if (length(vars) != 2) {
      stop("Formula must be of the form 'y ~ x'.")
    }
    
    yvar <- vars[1]
    xvar <- vars[2]
    
    return(visstat_core(
      dataframe = data,
      varsample = yvar,
      varfactor = xvar,
      conf.level = conf.level,
      do_regression = do_regression,
      numbers = numbers,
      minpercent = minpercent,
      graphicsoutput = graphicsoutput,
      plotName = plotName,
      plotDirectory = plotDirectory
    ))
  }
  
  # Case 1: legacy form: visstat(data, "Girth", "Height")
  if (is.data.frame(x) && is.character(y) && length(y) == 1) {
    mc <- match.call()
    args <- as.list(mc)[-1]
    if (length(args) < 3 || !is.character(eval(args[[3]], parent.frame()))) {
      stop("When using the backward-compatible form, provide two column names as character strings.")
    }
    varsample <- y                                    # first string: response
    varfactor <- eval(args[[3]], parent.frame())      # second string: predictor
    dataframe <- x
    return(visstat_core(
      dataframe = dataframe,
      varsample = varsample,
      varfactor = varfactor,
      conf.level = conf.level,
      do_regression = do_regression,
      numbers = numbers,
      minpercent = minpercent,
      graphicsoutput = graphicsoutput,
      plotName = plotName,
      plotDirectory = plotDirectory
    ))
  }
  
  # Case 2: new syntax: visstat(trees$Height, trees$Girth)
  factor_expr <- substitute(x)  # first argument = predictor/grouping variable
  sample_expr <- substitute(y)  # second argument = response
  
  factor_val <- eval(factor_expr, parent.frame())
  sample_val <- eval(sample_expr, parent.frame())
  
  factor_name <- clean_name(factor_expr)
  sample_name <- clean_name(sample_expr)
  
  dataframe <- data.frame(factor_val, sample_val)
  names(dataframe) <- c(factor_name, sample_name)
  
  return(invisible(visstat_core(
    dataframe = dataframe,
    varsample = sample_name,   # second argument = response
    varfactor = factor_name,   # first argument = predictor
    conf.level = conf.level,
    do_regression = do_regression,
    numbers = numbers,
    minpercent = minpercent,
    graphicsoutput = graphicsoutput,
    plotName = plotName,
    plotDirectory = plotDirectory
  )))
}