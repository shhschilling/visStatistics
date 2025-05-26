#' Wrapper for vistat_core allwoing two different input styles
#'
#' A wrapper around the core function \code{\link{visstat_core}} defining the
#' decision logic for statistical hypothesis testing and visualisation between
#' two variables of class \code{"numeric"}, \code{"integer"}, or \code{"factor"}.
#'
#' @param x A vector of class \code{"numeric"}, \code{"integer"}, or \code{"factor"}
#'   (standardised usage), or a \code{data.frame} containing the relevant columns
#'   (backward-compatible usage).
#' @param y A second vector (standardised usage), or a character string specifying
#'   the name of a column in \code{x} (backward-compatible usage).
#' @param ... If \code{x} is a data frame and \code{y} is a character string,
#'   an additional character string must follow, naming the second column.
#' @param conf.level Confidence level for statistical inference; default is \code{0.95}.
#' @param numbers Logical. Whether to annotate plots with numeric values.
#' @param minpercent Minimum proportion (between 0 and 1) required to display a category in plots.
#' @param graphicsoutput Optional. Output format for plots (e.g., \code{"pdf"}, \code{"png"}).
#' @param plotName Optional. File name prefix for saving plot output.
#' @param plotDirectory Directory in which to save plots; defaults to the current working directory.
#'
#' @details
#' This wrapper supports two input formats:
#'
#' \itemize{
#'   \item \strong{Standardised form:} \code{visstat(x, y)}, where both
#'         \code{x} and \code{y} are vectors of class \code{"numeric"},
#'         \code{"integer"}, or \code{"factor"}.
#'
#'   \item \strong{Backward-compatible form:}
#'         \code{visstat(dataframe, "name_of_x", "name_of_y")}, where both
#'         character strings refer to column names in \code{dataframe}.
#'         This is equivalent to:
#'         \code{visstat(dataframe[["name_of_x"]], dataframe[["name_of_y"]])}.
#' }
#'
#' The interpretation of \code{x} and \code{y} depends on the variable classes:
#'
#' \itemize{
#'   \item If one variable is numeric and the other a factor, the numeric
#'         vector must be passed as \code{y} and the factor as \code{x}.
#'         This supports group comparisons (e.g., t-test, ANOVA, Wilcoxon).
#'
#'   \item If both variables are numeric, a linear model is fitted with
#'         \code{y} as the response and \code{x} as the predictor.
#'
#'   \item If both variables are factors, an association test (Chi-squared
#'         or Fisher’s exact) is used. The test result is invariant to
#'         variable order, but visualisations (e.g., axis layout, bar
#'         orientation) depend on the roles of \code{x} and \code{y}.
#' }
#'
#' This wrapper standardises the input and calls \code{\link{visstat_core}},
#' which selects and executes the appropriate test with visual output and
#' assumption diagnostics.
#' @return A list as returned by \code{\link{visstat_core}}, containing statistical results and graphical outputs.
#'
#' @seealso \code{\link{visstat_core}}
#'
#' @examples

#' ## Standardised usage (preferred):
#' visstat(mtcars$am, mtcars$mpg)
#'
#' ## Backward-compatible usage (same result):
#' visstat(mtcars, "mpg", "am")
#'
#' ## Wilcoxon rank sum test
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
#' ## ANOVA
#' visstat(npk$block, npk$yield)
#'
#' ## Kruskal-Wallis
#' visstat(iris$Species, iris$Petal.Width)
#'
#' ## Linear regression
#' visstat(trees$Height, trees$Girth, conf.level = 0.99)
#'
#' ## Chi-squared
#' HairEyeColorDataFrame <- counts_to_cases(as.data.frame(HairEyeColor))
#' visstat(HairEyeColorDataFrame$Eye, HairEyeColorDataFrame$Hair)
#'
#' ## Fisher's test
#' HairEyeColorMaleFisher <- HairEyeColor[, , 1]
#' blackBrownHazelGreen <- HairEyeColorMaleFisher[1:2, 3:4]
#' blackBrownHazelGreen <- counts_to_cases(as.data.frame(blackBrownHazelGreen))
#' visstat(blackBrownHazelGreen$Eye, blackBrownHazelGreen$Hair)
#'
#' ## Save PNG
#' visstat(blackBrownHazelGreen$Hair, blackBrownHazelGreen$Eye,
#'         graphicsoutput = "png", plotDirectory = tempdir())
#'
#' ## Save PDF
#' visstat(iris$Petal.Width, iris$Species,
#'         graphicsoutput = "pdf", plotDirectory = tempdir())
#'
#' ## Custom plot name
#' visstat(iris$Petal.Width, iris$Species,
#'         graphicsoutput = "pdf", plotName = "kruskal_iris", plotDirectory = tempdir())
#'

#' @export
visstat <- function(x,
                    y,
                    ...,
                    conf.level = 0.95,
                    numbers = TRUE,
                    minpercent = 0.05,
                    graphicsoutput = NULL,
                    plotName = NULL,
                    plotDirectory = getwd()) {
  # Case 1: backward-compatible — x is data.frame, y is character, third arg must be character too
  if (is.data.frame(x) && is.character(y) && length(y) == 1) {
    mc <- match.call()
    args <- as.list(mc)[-1]
    if (length(args) < 3 || !is.character(eval(args[[3]], parent.frame()))) {
      stop("When using the backward-compatible form, provide two column names as character strings.")
    }
    varsample <- y
    varfactor <- eval(args[[3]], parent.frame())
    dataframe <- x
    return(visstat_core(
      dataframe = dataframe,
      varsample = varsample,
      varfactor = varfactor,
      conf.level = conf.level,
      numbers = numbers,
      minpercent = minpercent,
      graphicsoutput = graphicsoutput,
      plotName = plotName,
      plotDirectory = plotDirectory
    ))
  }

  # Case 2: standardised — x and y are both vectors
  if (is.atomic(x) && is.atomic(y)) {
    if (!inherits(x, c("numeric", "integer", "factor"))) {
      stop("Invalid input: x must be of class 'numeric', 'integer' or 'factor'.")
    }
    if (!inherits(y, c("numeric", "integer", "factor"))) {
      stop("Invalid input: y must be of class 'numeric', 'integer' or 'factor'.")
    }

    return(visstat_core(data.frame(x = x, y = y),
      varsample = "y", varfactor = "x",
      conf.level = conf.level,
      numbers = numbers,
      minpercent = minpercent,
      graphicsoutput = graphicsoutput,
      plotName = plotName,
      plotDirectory = plotDirectory
    ))
  }

  stop("Invalid input. Use either: visstat(x, y) with vectors x and y, or visstat(dataframe, \"y\", \"x\") with column names as character strings")
}
