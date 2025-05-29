# MIT License----
# Copyright (c) 2020 Sabine Schilling
# Feedback highly welcome: sabineschilling@gmx.ch

# Header visstat_core -----
#'
#' Automated Visualization of Statistical Hypothesis Testing
#'
#' @description \code{visstat_core()} provides automated selection and visualization 
#' of a statistical hypothesis test between a two vectors in
#' a given \code{data.frame} named \code{dataframe} based on the data's type, 
#' distribution, sample size, and the
#' specified \code{conf.level}.
#' \code{varsample} and \code{varfactor} are \code{character}
#' strings corresponding to the column names of the chosen vectors in \code{dataframe}. 
#' These vectors must be of type \code{integer}, \code{numeric} or \code{factor}.
#' The automatically generated output figures
#' illustrate the selected statistical hypothesis test, display the main test
#' statistics, and include assumption checks and post hoc comparisons when
#' applicable. The primary test results are returned as a list object.
#'
#' @details The decision logic for selecting a statistical test is described below.
#' For more details, please refer to the package's \code{vignette("visstat_coreistics")}.
#' Throughout, data of class \code{numeric} or \code{integer} are referred to as
#' numeric, while data of class \code{factor} are referred to as categorical.
#' The significance level \code{alpha} is defined as one minus the confidence
#' level, given by the argument \code{conf.level}. Assumptions of normality and
#' homoscedasticity are considered met when the corresponding test yields a
#' p-value greater than \code{alpha = 1 - conf.level}.
#' The choice of statistical tests performed by \code{visstat_core()} depends on
#' whether the data are numeric or categorical, the number of levels in the
#' categorical variable, the distribution of the data, and the chosen
#' \code{conf.level}. The function prioritises interpretable visual output and
#' tests that remain valid under their assumptions, following the logic below:
#' 
#' (1) When the response is numerical and the predictor is categorical, tests of
#' central tendency are performed. If the predictor has two levels:
#' \code{t.test()} is used if both groups have more than 30 observations (Lumley
#' et al. (2002) <doi:10.1146/annurev.publhealth.23.100901.140546>). For smaller
#' samples, normality is assessed using \code{shapiro.test()}. If both groups
#' return p-values greater than \code{alpha}, \code{t.test()} is applied;
#' otherwise, \code{wilcox.test()} is used.
#' For predictors with more than two levels, \code{aov()} is initially fitted.
#' Residual normality is tested with \code{shapiro.test()} and \code{ad.test()}.
#' If \code{p > alpha} for either test, normality is assumed. Homogeneity of
#' variance is tested with \code{bartlett.test()}. If \code{p > alpha},
#' \code{aov()} with \code{TukeyHSD()} is used. If \code{p <= alpha},
#' \code{oneway.test()} is applied with \code{TukeyHSD()}. If residuals are not
#' normal, \code{kruskal.test()} with \code{pairwise.wilcox.test()} is used.
#' 
#' (2): When both the response and predictor are numerical, a linear model
#' \code{lm()} is fitted, with residual diagnostics and a confidence band plot.
#' 
#' (3): When both variables are categorical, \code{visstat_core()} uses
#' \code{chisq.test()} or \code{fisher.test()} depending on expected counts,
#' following Cochran's rule (Cochran (1954) <doi:10.2307/3001666>).
#' 
#' Implemented main tests: 
#' 
#' \code{t.test()}, \code{wilcox.test()}, \code{aov()},
#' \code{oneway.test()}, \code{lm()}, \code{kruskal.test()},
#' \code{fisher.test()}, \code{chisq.test()}.
#' 
#' Implemented tests for assumptions:
#' \itemize{
#'   \item Normality: \code{shapiro.test()} and \code{ad.test()}
#'   \item Heteroscedasticity: \code{bartlett.test()}
#' }
#' 
#' Implemented post hoc tests:
#' \itemize{
#'   \item \code{TukeyHSD()} for \code{aov()} and \code{oneway.test()}
#'   \item \code{pairwise.wilcox.test()} for \code{kruskal.test()}
#' }
#' @seealso
#' See also the package's vignette
#' \code{vignette("visStatistics")} for the overview,
#' and the accompanying webpage
#' \url{https://shhschilling.github.io/visStatistics/}.
#'
#' @param dataframe \code{data.frame} with at least two columns.
#' @param varsample \code{character} string matching a column name in 
#'   \code{dataframe}. Interpreted as the response if the referenced column is 
#'   of class \code{numeric} or \code{integer} and the column named by 
#'   \code{varfactor} is of class \code{factor}.
#' @param varfactor \code{character} string matching a column name in 
#'   \code{dataframe}. Interpreted as the grouping variable if the referenced 
#'   column is of class \code{factor} and the column named by \code{varsample} 
#'   is of class \code{numeric} or \code{integer}.
#' @param conf.level Confidence level
#' @param numbers a logical indicating whether to show numbers in mosaic count
#'   plots.
#' @param minpercent number between 0 and 1 indicating minimal fraction of total
#'   count data of a category to be displayed	in mosaic count plots.
#' @param graphicsoutput saves plot(s) of type "png",  "jpg", "tiff" or  "bmp"
#'   in directory specified in \code{plotDirectory}. If graphicsoutput=NULL, no
#'   plots are saved.
#' @param plotName graphical output is stored following the naming convention
#'   "plotName.graphicsoutput" in \code{plotDirectory}. Without specifying this
#'   parameter, plotName is automatically generated following the convention
#'   "statisticalTestName_varsample_varfactor".
#' @param plotDirectory specifies directory, where generated plots are stored.
#'   Default is current working directory.
#' @return \code{list} containing statistics of automatically selected test
#'   meeting assumptions. All values are returned as invisible copies.
#'   Values can be accessed by assigning a return value to \code{visstat_core}.

#' @examples
#' # Welch Two Sample t-test (t.test())
#' visstat_core(mtcars, "mpg", "am")
#'
#' ## Wilcoxon rank sum test (wilcox.test())
#' grades_gender <- data.frame(
#'   Sex = as.factor(c(rep("Girl", 20), rep("Boy", 20))),
#'   Grade = c(
#'     19.3, 18.1, 15.2, 18.3, 7.9, 6.2, 19.4,
#'     20.3, 9.3, 11.3, 18.2, 17.5, 10.2, 20.1, 13.3, 17.2, 15.1, 16.2, 17.3,
#'     16.5, 5.1, 15.3, 17.1, 14.8, 15.4, 14.4, 7.5, 15.5, 6.0, 17.4,
#'     7.3, 14.3, 13.5, 8.0, 19.5, 13.4, 17.9, 17.7, 16.4, 15.6
#'   )
#' )
#' visstat_core(grades_gender, "Grade", "Sex")
#'
#' ## Welch's oneway ANOVA not assuming equal variances (oneway.test())
#' anova_npk <- visstat_core(npk, "yield", "block")
#' anova_npk # prints summary of tests
#'
#' ## Kruskal-Wallis rank sum test (kruskal.test())
#' visstat_core(iris, "Petal.Width", "Species")
#' visstat_core(InsectSprays, "count", "spray")
#'
#' ## Linear regression  (lm())
#' visstat_core(trees, "Girth", "Height", conf.level = 0.99)
#'
#' ## Pearson's Chi-squared test (chisq.test())
#' ### Transform array to data.frame
#' HairEyeColorDataFrame <- counts_to_cases(as.data.frame(HairEyeColor))
#' visstat_core(HairEyeColorDataFrame, "Hair", "Eye")
#'
#' ## Fisher's exact test (fisher.test())
#' HairEyeColorMaleFisher <- HairEyeColor[, , 1]
#' ### slicing out a 2 x2 contingency table
#' blackBrownHazelGreen <- HairEyeColorMaleFisher[1:2, 3:4]
#' blackBrownHazelGreen <- counts_to_cases(as.data.frame(blackBrownHazelGreen))
#' fisher_stats <- visstat_core(blackBrownHazelGreen, "Hair", "Eye")
#' fisher_stats # print out summary statistics
#'
#' ## Saving the graphical output in directory "plotDirectory"
#' ## A) Saving graphical output of type "png" in temporary directory tempdir()
#' ##    with default naming convention:
#' visstat_core(blackBrownHazelGreen, "Hair", "Eye",
#'   graphicsoutput = "png",
#'   plotDirectory = tempdir()
#' )
#'
#' ## Remove graphical output from plotDirectory
#' file.remove(file.path(tempdir(), "chi_squared_or_fisher_Hair_Eye.png"))
#' file.remove(file.path(tempdir(), "mosaic_complete_Hair_Eye.png"))
#'
#' ## B) Specifying pdf as output type:
#' visstat_core(iris, "Petal.Width", "Species",
#'   graphicsoutput = "pdf",
#'   plotDirectory = tempdir()
#' )
#'
#' ## Remove graphical output from plotDirectory
#' file.remove(file.path(tempdir(), "kruskal_Petal_Width_Species.pdf"))
#'
#' ## C) Specifying "plotName" overwrites default naming convention
#' visstat_core(iris, "Petal.Width", "Species",
#'   graphicsoutput = "pdf",
#'   plotName = "kruskal_iris", plotDirectory = tempdir()
#' )
#' ## Remove graphical output from plotDirectory
#' file.remove(file.path(tempdir(), "kruskal_iris.pdf"))
#'
#' @import vcd
#' @import Cairo
#' @import graphics
#' @import grDevices
#' @import grid
#' @import multcompView
#' @import stats
#' @import utils
#' @importFrom nortest ad.test
#' @export visstat_core


visstat_core <- function(dataframe,
                    varsample,
                    varfactor,
                    conf.level = 0.95,
                    numbers = TRUE,
                    minpercent = 0.05,
                    graphicsoutput = NULL,
                    plotName = NULL,
                    plotDirectory = getwd()) {
  stopifnot(is.data.frame(dataframe))
  stopifnot(varsample %in% names(dataframe))
  stopifnot(varfactor %in% names(dataframe))
  
  
  
  # store default graphical parameters------
  oldparvisstat_core <- par(no.readonly = TRUE)
  oldparvisstat_core$new <- FALSE # reset the default value
  on.exit(par(oldparvisstat_core))
  
  # Collect plot paths from plot_paths <- c(plot_paths, saveGraphVisstat())
  plot_paths <- character(0)
   
  # Set default values---------------------------
  alpha <- 1 - conf.level
  
  ## Get input variables---------------------------------
  input <-
    get_samples_fact_inputfile(dataframe, varsample, varfactor)
  # out of function get_groups_inputfile
  samples <- input$samples
  fact <- input$fact
  name_of_sample <- input$name_of_sample
  name_of_factor <- input$name_of_factor
  matchingCriteria <- input$matchingCriteria
  
  # dependent on samples, fact, name_of_sample, name_of_factor, conf.level,
  
  typesample <- class(samples)
  typefactor <-
    class(fact) # type of independent variable returned as a character vector
  
  
  # transform independent variable "fact" of class "character" to factor
  if (typefactor == "character") {
    fact <-
      as.factor(fact) # transform  "fact" of class "character" to factor
    typefactor <- class(fact) # store the class of type "factor"
  }
  
  #  Check order 
  
  if ((typefactor == "numeric" || typefactor == "integer") && typesample == "factor") {
    stop("A numeric or integer predictor with a factor response is ignored.")
  }
  
  maxlabels <- length(levels(samples))
  ## Comparison of all  possible combinations of input variables --------------
  ## A) median or mean-----
  # requirement: only two levels of factors
  # if the chosen "sample" is numeric or integer, we can perform parametric tests
  # like the t-test (if the assumption of normal distribution is met )
  # otherwise Wilcoxon test
  
  if ((
    # Wilcoxon or t-test -----
    typesample == "integer" | typesample == "numeric"
  ) &&
  (typefactor == "factor") && nlevels(fact) == 2) {
    # check if there is at least one entry in each group, if not return empty
    twosamples <-
      create_two_samples_vector(samples, fact) # returns list with three entries
    if (length(twosamples) < 3) {
      vis_sample_fact <-
        warning("In each group must be at least one member.")
    } else {
      # t-Test -----
      x1 <- twosamples$sample1
      x2 <- twosamples$sample2
      # The two-sample t-test is robust to non-normality due to
      # the central limit theorem.->
      # Checking for normality of samples not necessary,if sample size roughly >30
      # citation: THE IMPORTANCE OF THE NORMALITY ASSUMPTION IN LARGE PUBLIC
      # HEALTH DATA SETS
      # DOI: 10.1146/annurev.publhealth.23.100901.140546
      #
      # Check normality of both samples with Shapiro -Test-----
      # Check assumptions of Shapiro-Test:length between 3 and 5000,
      # at least one level returns TRUE if size between 3 and 50000
      #
      # There are two different ways to justify the use of the t-test:
      # 1.Data is normally distributed and you have at least two samples per group
      # 2. You have large (N>30)sample sizes in each group.
      
      shapiro_assumptions1 <- check_assumptions_shapiro(x1)
      shapiro_assumptions2 <- check_assumptions_shapiro(x2)
      
      if (shapiro_assumptions1 == TRUE) {
        p1 <- test_norm(twosamples$sample1)
      }
      
      if (shapiro_assumptions2 == TRUE) {
        p2 <- test_norm(twosamples$sample2)
      }
      # Check if normal distributions are given in both samples by Shapiro --
      # Assume normal distributions if the p-value is greater alpha
      # Perform always t-test if both samples are >30
      
      if (length(twosamples$sample1) > 30 &
          length(twosamples$sample2) > 30) {
        openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)
        vis_sample_fact <- two_sample_t_test(
          samples,
          fact,
          alternative = c("two.sided"),
          paired = FALSE,
          var.equal = FALSE,
          conf.level = conf.level,
          samplename = name_of_sample,
          factorname = name_of_factor
        )
        
        if (is.null(plotName)) {
          filename <-
            paste("ttest_", name_of_sample, "_", name_of_factor, sep = "")
        } else {
          filename <- plotName
        }
        plot_paths <- c(plot_paths, saveGraphVisstat(filename, type = graphicsoutput, fileDirectory = plotDirectory))
      }
      # 2. If assumptions of t-test are not met: Wilcoxon, else t-test
      else if (!exists("p1") |
               (if (exists("p1")) {
                 p1$p.value < alpha
               } else {
                 FALSE
               }) |
               !exists("p2") |
               (if (exists("p2")) {
                 (p2$p.value < alpha)
               } else {
                 FALSE
               })) {
        # case 1: Wilcoxon-Test:
        # normal distribution not given for n<limit
        openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)
        
        vis_sample_fact <- two_sample_wilcoxon_test(
          samples,
          fact,
          alternative = "two.sided",
          conf.level = conf.level,
          notchf = F,
          samplename = varsample,
          # factorname = matchingCriteria,
          factorname = varfactor,
          cex = 1
        )
        
        
        if (is.null(plotName)) {
          filename <-
            paste("wilcoxon-test_",
                  name_of_sample,
                  "_",
                  name_of_factor,
                  sep = "")
        } else {
          filename <- plotName
        }
        
        
        plot_paths <- c(plot_paths, saveGraphVisstat(fileName = filename,
                         type = graphicsoutput,
                         fileDirectory = plotDirectory))
      } else {
        openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)
        
        vis_sample_fact <- two_sample_t_test(
          samples,
          fact,
          alternative = "two.sided",
          paired = F,
          var.equal = F,
          conf.level = conf.level,
          samplename = varsample,
          factorname = varfactor
        )
        
        if (is.null(plotName)) {
          filename <-
            paste("ttest_", name_of_sample, "_", name_of_factor, sep = "")
        } else {
          filename <- plotName
        }
        
        plot_paths <- c(plot_paths, saveGraphVisstat(fileName = filename,
                         type = graphicsoutput,
                         fileDirectory = plotDirectory))
      }
      
      # attr(vis_sample_fact, "plot_paths") <- plot_paths
      # class(vis_sample_fact) <- "visstat"
      # return(invisible(vis_sample_fact))
    }
  }
  
  
  
  
  
  ## B) Chi2 and Mosaic-----
  
  if (typefactor == "factor" && typesample == "factor") {
    if (check_assumptions_count_data(samples, fact) == FALSE) {
      vis_sample_fact <-
        makeTable(samples, fact, name_of_sample, name_of_factor)
    } else {
      # Chi^2 Test-----
      openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)
      
      vis_chi <-
        vis_chi_squared_test(samples, fact, name_of_sample, "groups")
      if (is.null(plotName)) {
        filename <- paste("chi_squared_or_fisher_",
                          name_of_sample,
                          "_",
                          name_of_factor,
                          sep = "")
      } else {
        filename <- paste(plotName, "_", "chi_squared_or_fisher", sep = "")
      }
      
      plot_paths <- c(plot_paths, saveGraphVisstat(fileName = filename,
                       type = graphicsoutput,
                       fileDirectory = plotDirectory))
      # Mosaic plots -----
      # a) complete labeled mosaic graph
      
      if (maxlabels > 7) {
        numberflag <- F
      } else {
        numberflag <- T
      }
      
      openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)
      
      vis_mosaic_res <- vis_mosaic(
        samples,
        fact,
        name_of_sample = name_of_sample,
        name_of_factor = name_of_factor,
        minperc = 0,
        numbers = numberflag
      )
      
      
      
      if (is.null(plotName)) {
        filename <- paste("mosaic_complete_",
                          name_of_sample,
                          "_",
                          name_of_factor,
                          sep = "")
      } else {
        filename <- paste(plotName, "_", "mosaic_complete", sep = "")
      }
      
      
      
      plot_paths <- c(plot_paths, saveGraphVisstat(filename, type = graphicsoutput, fileDirectory = plotDirectory))
      
      # b) reduced plots if number of of levels>7
      # Display only categories with at least minpercent of entries
      
      if (maxlabels > 7) {
        openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)
        
        vis_mosaic_res <- vis_mosaic(
          samples,
          fact,
          name_of_sample = name_of_sample,
          name_of_factor = "groups",
          minperc = minpercent,
          numbers = T
        )
        plot_paths <- c(plot_paths, saveGraphVisstat(
          paste(
            "mosaic_reduced_",
            name_of_sample,
            "_",
            name_of_factor,
            sep = ""
          )),
          type = graphicsoutput,
          fileDirectory = plotDirectory
        )
      }
      
      vis_sample_fact <- c(vis_chi, vis_mosaic_res)
    }
  }
  # C) both types numerical: Regression-----
  
  # Both samples and fact of type integer or numeric
  # Regression
  #
  #
  if ((typefactor == "integer" |
       typefactor == "numeric") &&
      (typesample == "integer" | typesample == "numeric")) {
    # samples: independent variable, factor: dependent   variable
    # check normality
    normality_residual_assumptioon <-
      vis_normality_assumptions(samples, fact, conf.level = conf.level)
    
    openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)
    
    vis_sample_fact <- vis_regression(
      samples,
      # y: dependent
      fact,
      # x: independent
      name_of_factor = name_of_factor,
      name_of_sample = name_of_sample,
      conf.level = conf.level
    )
    if (is.null(plotName)) {
      filename <-
        paste("regression_", name_of_sample, "_", name_of_factor, sep = "")
    } else {
      filename <- paste(plotName)
    }
    
    
    
    
    plot_paths <- c(plot_paths, saveGraphVisstat(fileName = filename,
                     type = graphicsoutput,
                     fileDirectory = plotDirectory))
  }
  
  # D) more than two comparisons-----
  # A) sample is numeric or integer: ANOVA or Kruskal/Wallis
  
  # excellent tutorial
  # https://www.scribbr.com/statistics/anova-in-r/
  
  
  if (typefactor == "factor" &&
      (typesample == "integer" | typesample == "numeric") &&
      nlevels(fact) > 2) {
    visanova <- vis_anova_assumptions(
      samples,
      fact,
      conf.level = conf.level,
      samplename = varsample,
      factorname = varfactor
    )
    
    
    if (visanova$shapiro_test$p.value > alpha |
        visanova$ad_test$p.value > alpha) {
      openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)
      
      vis_sample_fact <- vis_anova(
        samples,
        fact,
        samplename = varsample,
        factorname = varfactor,
        conf.level = conf.level
      )
      
      
      if (is.null(plotName)) {
        filename <-
          paste("anova_", name_of_sample, "_", name_of_factor, sep = "")
      } else {
        filename <- paste(plotName)
      }
      
      
      plot_paths <- c(plot_paths, saveGraphVisstat(fileName = filename,
                       type = graphicsoutput,
                       fileDirectory = plotDirectory))
      
      
      
      
      # if p -values of both Shapiro-Wilk and Kruskall-Wallis-Test are smaller than 0.05, Kruskall-Wallis-Test
    } else {
      openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)
      
      vis_sample_fact <- vis_Kruskal_Wallis_clusters(
        samples,
        fact,
        conf.level = conf.level,
        samplename = varsample,
        factorname = varfactor,
        cex = 1,
        notch = F
      )
      
      if (is.null(plotName)) {
        filename <-
          paste("kruskal_", name_of_sample, "_", name_of_factor, sep = "")
      } else {
        filename <- paste(plotName)
      }
      
      
      plot_paths <- c(plot_paths, saveGraphVisstat(fileName = filename,
                       type = graphicsoutput,
                       fileDirectory = plotDirectory))
    }
  }
  
  
  
  # At the very end:
  attr(vis_sample_fact, "plot_paths") <- plot_paths
  class(vis_sample_fact) <- "visstat"
  
  return(invisible(vis_sample_fact))
  
  
}
# End of visstat_core function -------