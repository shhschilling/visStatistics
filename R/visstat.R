#MIT License----
#Copyright (c) 2018 Sabine Schilling
#Feedback highly welcome: sabineschilling@gmx.ch

# Header visstat -----

#' Visualization of the statistical hypothesis test between two or more groups of categorical or numerical data.
#'
#' \code{visstat()} \strong{vis}ualizes the \strong{stat}istical hypothesis testing between two or more groups of data, where the parameter \code{varsample} is the dependent variable (or response) and the paramter \code{varfactor} is the independent variable with two or more features.
#' The statistical hypothesis test with the highest statistical power and fulfilling the assumptions of the corresponding tests is performed and visualized.
#' A graph displaying the raw data accordingly to the chosen test as well as the test statistics is generated. Furthermore
#' \code{visstat()} returns the corresponding test statistics as text.
#' Implemented tests: \code{lm()},\code{t.test()}, \code{wilcox.test()}, \code{aov()}, \code{kruskal.test()}, \code{fisher.test()},\code{chisqu.test()}. If \code{varfactor} contains more than two features,
#' \code{visstat()} tests the underlying assumptions of \code{aov()} and \code{oneway.test()}: 
#'  If the p-values of the standardized residuals of both \code{shapiro.test()} or \code{ks.test()} are smaller  than 1-\code{conf.level}, \code{kruskal.test()} is performed.
#' otherwise the \code{oneway.test()} and \code{aov()} are performed.


#'
#' @param dataframe \code{data.frame} or \code{list} containing at least two columns with column headings of data type \code{character}. Data must be column wise ordered.
#' @param varsample column name of dependent variable in dataframe, dataype \code{character}
#' @param varfactor column name of independent variable in dataframe, dataype \code{character}
#' @param conf.level confidence level of the interval.
#' @param numbers	a logical indicating whether to show numbers in mosaic count plots
#' @param minpercent number indicating minimalfraction of total count data of a category to be displayed	in the mosaic count plots
#' @param graphicsoutput output format of plot. Allowed are the \code{character} strings "png","pdf","svg". Note that the graphics windows will be closed.
#'
#' @return Statistics of test with highest statistical power meeting assumptions.
#' @examples
#' visstat(iris,"Petal.Width", "Species")
#' visstat(InsectSprays,"count","spray")
#' visstat(ToothGrowth,"len", "supp")
#' visstat(mtcars,"mpg","am")
#' 
#' 
#' @import vcd
#' @import Cairo
#' @import graphics
#' @import grDevices
#' @import grid
#' @import multcompView 
#' @import stats
#' @import utils
#' @import vcd
#' 
#' @export visstat
visstat = function(dataframe,
                                   varsample,
                                   varfactor,
                                   conf.level = 0.95,
                                   numbers = TRUE,
                                   minpercent = 0.05,
                                   graphicsoutput = NULL)
{
  # visstat performs the statistical test with the hightest statistical between two columns of the input "dataframe".
  # Three variables must be provided:
  #  - dataframe of type data.frame or list (generated from json file)
  #  with headers which are either the dependent variable (varsamples)
  # or the independent variable (varfact)
  #  - varsample: dependent variable choosen by user out of columns of dataframe, varsample is the name given in the header
  #  - varfactor: independent variables choosen by user outout of columns of dataframe,  varfactor is the name given in the header
  #numbers: Boolean deciding if in mosaic plots counts of each category should be shown
  # minpercent=0.05 #minimal fraction of total count which has to be in each category of count data in order to be displayed in mosaic plot
  # graphicsoutput can be "png", "jpeg", "jpg", "tiff", "bmp"


  
  cexsize = 1


  #Set default values---------------------------
  alpha = 1 - conf.level
  ##Get input variables---------------------------------
  input = get_samples_fact_inputfile(dataframe, varsample, varfactor)
  #out of function get_groups_inputfile
  samples = input$samples
  fact = input$fact
  name_of_sample = input$name_of_sample
  name_of_factor = input$name_of_factor
  matchingCriteria = input$matchingCriteria


  # This part must become a function-------
  #dependent on samples, fact, name_of_sample, name_of_factor, conf.level,
  #paired=F,
  typesample = class(samples)
  typefactor = class(fact)
  maxlabels = length(levels(samples))
  ## Comparison of all  possible combinations of input variables ------------------
  ##A) median or mean-----
  #requirement: only two levels of factors
  #if the chosen "sample" is numeric or integer, we can perform parametric tests like the t-test
  #(if the assumption of normal distribution is met )
  #otherwise Wilcoxon test

  if ((#Wilcoxon or t-test -----
       typesample == "integer" | typesample == "numeric")
      &&
      (typefactor == "factor") && nlevels(fact) == 2)
  {
    #check if there is at least one entry in each group, if not return empty
    twosamples = create_two_samples_vector(samples, fact) #returns list with three entries
    if (length(twosamples) < 3) {
      vis_sample_fact = warning("In each group must be at least one member ")
    } else{
      # t-Test -----
      # rest = two_sample_tTest(samples, fact, alpha, side = "two.sided", samplename=varsample,factorname=matchingCriteria)
      x = twosamples$sample1and2
      x1 = twosamples$sample1
      x2 = twosamples$sample2
      #the two-sample t-test is robust to non-normality due to the central limit theorem
      #checking for normality of samples not necessary if sample size roughly >100
      #citation: THE IMPORTANCE OF THE NORMALITY ASSUMPTION IN LARGE PUBLIC HEALTH DATA SETS
      # DOI: 10.1146/annurev.publhealth.23.100901.140546
      #
      #Check normality of both samples with Shapiro -Test-----
      #Check assumptions of Shapiro-Test:length between 3 and 5000, at least one level
      #returns TRUE if size between 3 and 50000
      #

      # There are two different ways to justify the use of the t-test"
      # 1.Your data is normally distributed and you have at least two samples per group
      # 2. You have large (N>100)sample sizes in each group

      shapiro_assumptions1 = check_assumptions_shapiro(x1)
      shapiro_assumptions2 = check_assumptions_shapiro(x2)

      if (shapiro_assumptions1 == TRUE)
        p1 = test_norm(twosamples$sample1)

      if (shapiro_assumptions2 == TRUE)
        p2 = test_norm(twosamples$sample2)
      # Check if normal distributions are given in both samples by Shapiro and KS-Test --
      # Assume normal distributions if the p-value of at least one of the tests is greater alpha


      #Perform always t-test if both samples are >100

      if
      (length(twosamples$sample1) > 100 &length(twosamples$sample2)>100)
      {
        openGraphCairo()
        vis_sample_fact = two_sample_tTest(samples,
                                           fact,
                                           conf.level=conf.level,
                                           alternative = 'two.sided',var.equal=F,paired=F,
                                           samplename=varsample,factorname=matchingCriteria)
        saveGraphCairo(
          paste("ttest_", name_of_sample, "_", name_of_factor, sep = ""))
      }
      #2. If assumptions of t-test are not me: Wilcoxon, else t-test
      else
        if
      (
        !exists("p1")
        |
        (if (exists("p1"))
        {   p1$p.value< alpha
        }else{
          FALSE}
        )
        |
        !exists("p2")
        |
        (if (exists("p2"))
        {
          ( p2$p.value< alpha)
        }else{
          FALSE
        }
        )
      )
        {
        #case 1: Wilcoxon-Test:
        #normal distribution not given for n<limit
        openGraphCairo()
        vis_sample_fact = two_sample_WilcoxonTest(
          samples,
          fact,
          alternative ="two.sided",
          conf.level=conf.level,
          notchf = T,
          samplename=varsample,factorname=matchingCriteria
        )
        saveGraphCairo(
          paste(
            "wilcoxon-test_",
            name_of_sample,
            "_",
            name_of_factor,
            sep = ""
          ))
      } else{
        openGraphCairo()
        vis_sample_fact = two_sample_tTest(samples,
                                           fact,
                                           conf.level=conf.level,
                                           alternative = 'two.sided',var.equal=F,paired=F,
                                           samplename=varsample,factorname=matchingCriteria)
        saveGraphCairo(
          paste("ttest_", name_of_sample, "_", name_of_factor, sep = "")
        )
      }
      return(vis_sample_fact)
    }
  }





    ## B) Chi2 and Mosaic-----

    if (typefactor == "factor" && typesample == "factor")
    {
      if (check_assumptions_count_data(samples, fact) == FALSE)
      {
        vis_sample_fact = makeTable(samples, fact, name_of_sample, name_of_factor)

      } else{
        #Chi^2 Test-----
        openGraphCairo(type = graphicsoutput)
        vis_chi = vis_chi_squared_test(samples, fact, name_of_sample, "groups")
        saveGraphCairo(paste(
          "chi_squared_",
          name_of_sample,
          "_",
          name_of_factor,
          sep = ""
        ),
        type = graphicsoutput)
        #Mosaic plots -----
        #a) complete labeled mosaic graph

        if (maxlabels > 7)
        {
          numberflag = F
        } else{
          numberflag = T
        }
        openGraphCairo(type = graphicsoutput)
        vis_mosaic_res = vis_mosaic(
          samples,
          fact,
          name_of_sample = name_of_sample,
          name_of_factor = name_of_factor,
          minperc = 0,
          numbers = numberflag
        )

        saveGraphCairo(paste(
          "mosaic_complete_",
          name_of_sample,
          "_",
          name_of_factor,
          sep = ""
        ),
        type = graphicsoutput)

        #b) reduced plots if number of of levels>7
        #Display only categories with at least minpercent of entries

        if (maxlabels > 7) {
          openGraphCairo(type = graphicsoutput)
          vis_mosaic_res = vis_mosaic(
            samples,
            fact,
            name_of_sample = name_of_sample,
            name_of_factor = "groups",
            minperc = minpercent,
            numbers = T
          )
          saveGraphCairo(
            paste(
              "mosaic_reduced_",
              name_of_sample,
              "_",
              name_of_factor,
              sep = ""
            ),
            type = graphicsoutput
          )
        }
        vis_sample_fact=c(vis_chi,vis_mosaic_res)
      }
    }
    #C) both types numeric-----

    #Both samples and fact of type integer or numeric
    #Regression
    #
    #
    if ((typefactor == "integer" |
         typefactor == "numeric") &&
        (typesample == "integer" | typesample == "numeric"))
    {
      openGraphCairo(type = graphicsoutput)
      vis_sample_fact = vis_regression(fact,
                                       samples,
                                       name_of_factor = name_of_factor,
                                       name_of_sample = name_of_sample)
      saveGraphCairo(paste("regression_", name_of_sample, "_", name_of_factor, sep = ""),
                     type = graphicsoutput)
    }

    #D) more than two comparisons----
    #A) sample is numeric or integer: ANOVA or Kruskal/Wallis


    if (typefactor == "factor" &&
        (typesample == "integer" | typesample == "numeric") &&
        nlevels(fact) > 2)
    {
      visanova = vis_anova_assumptions(
        samples,
        fact,
        conf.level = 0.95,
        samplename = varsample,
        factorname = varfactor
      )


      if (visanova$shapiro_test$p.value > alpha |
          visanova$ks_test$p.value > alpha)

      {
        openGraphCairo(type = graphicsoutput)
        vis_sample_fact = vis_anova(samples,
                                    fact,
                                    samplename = varsample,
                                    factorname = varfactor)
        saveGraphCairo(paste("anova_", name_of_sample, "_", name_of_factor, sep = ""),
                       type = graphicsoutput)
        
        
        
        
        #if p -values of both Shapiro-Wilk and Kruskall-Wallis-Test are smaller than 0.05, Kruskall-Wallis-Test
      } else{
        openGraphCairo(type = graphicsoutput)
        vis_sample_fact = vis_Kruskal_Wallis_clusters(
          samples,
          fact,
          conf.level = conf.level,
          samplename = varsample,
          factorname = varfactor,
          cex = 1,
          notch = F
          
          
        )

        saveGraphCairo(paste("kruskal_", name_of_sample, "_", name_of_factor, sep = ""),
                       type = graphicsoutput)
        
        
        
      }

    }
  
  deleteRplotspdf()
    return(vis_sample_fact)
  }
  #End of vis_sample_fact function -------


  
