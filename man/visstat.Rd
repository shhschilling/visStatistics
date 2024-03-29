% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visstat.R
\name{visstat}
\alias{visstat}
\title{Visualization of statistical hypothesis testing based on decision tree}
\usage{
visstat(
  dataframe,
  varsample,
  varfactor,
  conf.level = 0.95,
  numbers = TRUE,
  minpercent = 0.05,
  graphicsoutput = NULL,
  plotName = NULL,
  plotDirectory = getwd()
)
}
\arguments{
\item{dataframe}{\code{data.frame} containing at least two columns. Data must be column wise ordered.}

\item{varsample}{column name of the dependent variable in \code{dataframe}, datatype \code{character}. \code{varsample} must be one entry of the list \code{names(dataframe)}.}

\item{varfactor}{column name of the independent variable in \code{dataframe}, datatype \code{character}.\code{varsample} must be one entry of the list \code{names(dataframe)}.}

\item{conf.level}{confidence level of the interval.}

\item{numbers}{a logical indicating whether to show numbers in mosaic count plots.}

\item{minpercent}{number between 0 and 1 indicating minimal fraction of total count data of a category to be displayed    in mosaic count plots.}

\item{graphicsoutput}{saves plot(s) of type "png",  "jpg", "tiff" or  "bmp" in directory specified in \code{plotDirectory}.
If graphicsoutput=NULL, no plots are saved.}

\item{plotName}{graphical output is stored following the naming convention "plotName.graphicsoutput" in \code{plotDirectory}.
Without specifying this parameter, plotName is automatically generated following the convention "statisticalTestName_varsample_varfactor".}

\item{plotDirectory}{specifies directory, where generated plots are stored. Default is current working directory.}
}
\value{
\code{list} containing statistics of test with highest statistical power meeting assumptions. All values are returned as invisibly copies. Values can be accessed by assigning a return value to \code{visstat}.
}
\description{
Based on a decision tree, \code{visstat()} picks the statistical hypothesis
test with the highest statistical  power between the dependent variable
(response)  and the independent variable (feature) in a \code{data.frame}
named \code{dataframe}.
Data in the provided \code{dataframe} must be structured column wise,
where \code{varsample} and \code{varfactor} are \code{character} strings
corresponding to the column names of the dependent and independent variable
respectively. For each test \code{visstat()} returns both a graph with the
main test statistics in its title as well as  a list of the  test statistics 
including eventual post-hoc analysis.
}
\details{
Implemented tests: \code{lm()},\code{t.test()}, \code{wilcox.test()},
\code{aov()}, \code{kruskal.test()}, \code{fisher.test()}, \code{chisqu.test()}.
Implemented tests for normal distribution of standardized residuals: \code{shapiro.test()} and \code{ad.test()}.
Implemented post-hoc tests: \code{TukeyHSD()} for aov() and \code{pairwise.wilcox.test()} for \code{kruskal.test()}.

For the comparison of averages, the following algorithm  depends on the value of the parameter of \code{conf.level}, which defaults to 0.95.
If the p-values of the standardized residuals of  \code{shapiro.test()} or \code{ks.test()} are smaller
than the error probability 1-\code{conf.level}, \code{kruskal.test()} resp. \code{wilcox.test()} are performed, otherwise the \code{oneway.test()}
and \code{aov()} resp. \code{t.test()} are performed and displayed.
Exception: If the sample size of both levels is bigger than 30,  \code{wilcox.test()} is never executed,instead always the \code{t.test()} is performed (Lumley et al. (2002)
<doi:10.1146/annurev.publheath.23.100901.140546>).

For the test of independence of count data, Cochran's rule (Cochran (1954)
<doi:10.2307/3001666>) is implemented:
If more than 20 percent of all cells have an expected count smaller than 5 or
an expected cell count is zero,  \code{fisher.test()} is performed and displayed, otherwise the \code{chisqu.test()}.
In both cases case an additional mosaic plot showing Pearson's residuals is generated.
}
\examples{

## Welch Two Sample t-test (calling t.test())
visstat(mtcars, "mpg", "am")

## Wilcoxon rank sum test (calling wilcox.test())
grades_gender <- data.frame(
  Sex = as.factor(c(rep("Girl", 20), rep("Boy", 20))),
  Grade = c(
    19.3, 18.1, 15.2, 18.3, 7.9, 6.2, 19.4,
    20.3, 9.3, 11.3, 18.2, 17.5, 10.2, 20.1, 13.3, 17.2, 15.1, 16.2, 17.3,
    16.5, 5.1, 15.3, 17.1, 14.8, 15.4, 14.4, 7.5, 15.5, 6.0, 17.4,
    7.3, 14.3, 13.5, 8.0, 19.5, 13.4, 17.9, 17.7, 16.4, 15.6
  )
)
visstat(grades_gender, "Grade", "Sex")

## One-way analysis of means (oneway.test())
anova_npk <- visstat(npk, "yield", "block")
anova_npk # prints summary of tests

## Kruskal-Wallis rank sum test (calling kruskal.test())
visstat(iris, "Petal.Width", "Species")
visstat(InsectSprays, "count", "spray")

## Linear regression
visstat(trees, "Girth", "Height", conf.level = 0.99)

## Pearson's Chi-squared test and mosaic plot with Pearson residuals
### Transform array to data.frame
HairEyeColorDataFrame <- counts_to_cases(as.data.frame(HairEyeColor))
visstat(HairEyeColorDataFrame, "Hair", "Eye")

## 2x2 contingency tables with Fisher's exact test and mosaic plot with Pearson residuals
HairEyeColorMaleFisher <- HairEyeColor[, , 1]
### slicing out a 2 x2 contingency table
blackBrownHazelGreen <- HairEyeColorMaleFisher[1:2, 3:4]
blackBrownHazelGreen <- counts_to_cases(as.data.frame(blackBrownHazelGreen))
fisher_stats <- visstat(blackBrownHazelGreen, "Hair", "Eye")
fisher_stats # print out summary statistics



## Saving the graphical output in directory plotDirectory
## A) saving graphical output of type "png" in temporary directory tempdir()
##    with default naming convention:
visstat(blackBrownHazelGreen, "Hair", "Eye", graphicsoutput = "png", 
plotDirectory = tempdir())

## remove graphical output from plotDirectory
file.remove(file.path(tempdir(), "chi_squared_or_fisher_Hair_Eye.png"))
file.remove(file.path(tempdir(), "mosaic_complete_Hair_Eye.png"))

## B) Specifying pdf as output type:
visstat(iris, "Petal.Width", "Species", graphicsoutput = "pdf", 
plotDirectory = tempdir())

## remove graphical output from plotDirectory
file.remove(file.path(tempdir(), "kruskal_Petal_Width_Species.pdf"))

## C) Specifiying plotName overwrites default naming convention
visstat(iris, "Petal.Width", "Species",
  graphicsoutput = "pdf",
  plotName = "kruskal_iris", plotDirectory = tempdir()
)
## remove graphical output from plotDirectory
file.remove(file.path(tempdir(), "kruskal_iris.pdf"))

}
