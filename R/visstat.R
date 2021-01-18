#MIT License----
#Copyright (c) 2020 Sabine Schilling
#Feedback highly welcome: sabineschilling@gmx.ch

# Header visstat -----

#' Visualization of statistical hypothesis testing based on decision tree
#'
#' \code{visstat()} \strong{vis}ualizes the \strong{stat}istical hypothesis testing between 
#' the dependent variable (or response)
#' \code{varsample} and the independent variable  \code{varfactor}. \code{varfactor} can have more than two features. 
#' \code{visstat()} runs a decision tree selecting the statistical hypothesis test with the highest statistical power 
#'  fulfilling the assumptions of the underlying test. For each test 
#'  \code{visstat()} returns a graph displaying the data with the main test statistics 
#' in the title and a list with the complete test statistics including eventual post-hoc analysis. 
#' Implemented tests: \code{lm()},\code{t.test()}, \code{wilcox.test()}, 
#' \code{aov()}, \code{kruskal.test()}, \code{fisher.test()}, \code{chisqu.test()}. 
#' Implemented tests for check of normal distribution of standardized residuals: \code{shapiro.test()} and \code{ad.test()}.
#' Implemented post-hoc tests: \code{TukeyHSD()} for aov() and \code{pairwise.wilcox.test()} for \code{kruskal.test()}.
#' 
#'  For the comparison of averages, the following algorithm is implemented: 
#'  If the p-values of the standardized residuals of  \code{shapiro.test()} or \code{ks.test()} are smaller 
#' than 1-conf.level, \code{kruskal.test()} resp. \code{wilcox.test()} are performed, otherwise the \code{oneway.test()}
#' and \code{aov()} resp. \code{t.test()} are performed and displayed.
#' Exception: If the sample size is bigger than 100,  \code{wilcox.test()} is never executed,instead always the \code{t.test()} is performed
#'  (Lumley et al. (2002) <doi:10.1146/annurev.publheath.23.100901.140546>).
#' For the test of independence of count data, Cochran's rule (Cochran (1954) <doi:10.2307/3001666>) is implemented: 
#' If more than 20 percent of all cells have a count smaller than 5, is performed and displayed, otherwise \code{chisqu.test()}. 
#' In both cases case an additional mosaic plot showing Pearson's residuals is generated. 


#' @param dataframe \code{data.frame} containing at least two columns. Data must be column wise ordered.
#'  Contingency tables can be transformed to column wise structure with helper function \code{counts_to_cases(as.data.frame())}. 
#' @param varsample column name of dependent variable in \code{dataframe}, datatype \code{character}.
#' @param varfactor column name of independent variable in \code{dataframe}, datatype \code{character}.
#' @param conf.level confidence level of the interval.
#' @param numbers	a logical indicating whether to show numbers in mosaic count plots. 
#' @param minpercent number between 0 and 1 indicating minimal fraction of total count data of a category to be displayed	in the mosaic count plots.
#' @param graphicsoutput saves plot of type "png", "jpeg", "jpg", "tiff" or  "bmp" in current working directory following the naming convention "statisticalTestName_varsample_varfactor.graphicsoutput"
#'
#' @return \code{list} containing statistics of test with highest statistical power meeting assumptions. All values are returned as invisibly copies. Values can be accessed by assigning a return value to \code{visstat}.
#' @examples
#' ## Kruskal-Wallis rank sum test
#' visstat(iris,"Petal.Width", "Species") 

#' 
#' ## ANOVA and One-way analysis of means
#' visstat(InsectSprays,"count","spray")  
#' 
#' ## Wilcoxon rank sum test
#' visstat(ToothGrowth,"len", "supp")
#' 
#' ## Welch Two Sample t-test
#' mtcars$am=as.factor(mtcars$am) # transform to categorical data of type "factor"
#' visstat(mtcars,"mpg","am") # Welch Two Sample t-test

#' ## Pearson's Chi-squared test
#' visstat(counts_to_cases(as.data.frame(HairEyeColor[,,1])),"Hair","Eye")
#'
#' ## Fisher test: Example transforming contingency table to data.frame.
#' HairEyeColorMaleFisher=HairEyeColor[,,1]
#' HairEyeColorMaleFisher[HairEyeColorMaleFisher<10]=4 #create example enforcing Cochran's rule
#' #transform contingency table to data.frame
#' HairEyeColorMaleFisher = counts_to_cases(as.data.frame(HairEyeColorMaleFisher)) 
#' visstat(HairEyeColorMaleFisher,"Hair","Eye") # Fisher test
#' remove(HairEyeColorMaleFisher)
#'
#
#' ## Linear regression
#' visstat(trees,"Girth","Height") 
#' 
#' ## A) printing statistical ouput 
#' stats_lin_reg=visstat(trees,"Girth","Height") #assign value to return values
#' stats_lin_reg #printing tatistical output 
#' remove(stats_lin_reg)
#' 
#' ## B) saving graphical output of type "png" to current directory with default 
#' ##    naming convention "statisticalTestName_varsample_varfactor.graphicsoutput"
#' visstat(trees,"Girth","Height",graphicsoutput = "png"); #creates file regression_Girth_Height.png in current directory
#' ## remove the example plot 
#' file.remove("regression_Girth_Height.png")
#' 
#' '## C) saving graphical output of type "pdf" to user chosen directory and user chosen file name 


#' @import vcd
#' @import Cairo
#' @import graphics
#' @import grDevices
#' @import grid
#' @import multcompView
#' @import stats
#' @import utils
#' @importFrom nortest ad.test

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
  # The function vistat() visualizes the statistical hypothesis testing between the dependent variable (response) varsample and the independent variable (feature) varfactor. 
  # The statistical hypothesis test (including the eventual corresponding post-hoc analysis) with the highest statistical power fulfilling
  # the assumptions of the corresponding test is performed. 
  # A graph displaying the raw data accordingly to the chosen test as well as the test statistics is generated and returned.
  # Implemented tests: lm(), t.test(), wilcox.test(), aov(), kruskal.test(), fisher.test(),chisqu.test().
  # Three variables must be provided:
  #  - dataframe of type data.frame or list (generated from json file)
  #  with headers which are either the dependent variable (varsamples)
  # or the independent variable (varfact)
  #  - varsample: dependent variable chosen by user out of columns of dataframe, varsample is the name given in the header
  #  - varfactor: independent variables chosen by user out of columns of dataframe,  varfactor is the name given in the header
  # Optional parameters with set default values: 
  # numbers: Boolean deciding if in mosaic plots counts of each category should be shown
  # minpercent: number between 0 and 1 indicating the minimal fraction of total count which has to be in each category of count data in order to be displayed in mosaic plot
  # graphicsoutput: character string indicating if a plot of type  "png", "jpeg", "jpg", "tiff", "bmp" should be saved to the current working directory following the 
  # naming convention "statisticalTestName_varsample_varfactor.graphicsoutput". The default "NULL" does not save the current plot.
  
  
  
  stopifnot(is.data.frame(dataframe) )
  stopifnot(varsample %in% names(dataframe))
  stopifnot(varfactor %in% names(dataframe))
  
  
  
  
  #store default graphical parameters------
 # if (length(dev.list())>0) dev.off() #closes all graphical devices 
  oldpar = par(no.readonly = TRUE)
  on.exit(par(oldpar))
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
        openGraphCairo(type = graphicsoutput)
        vis_sample_fact = two_sample_tTest(samples,
                                           fact,
                                           conf.level=conf.level,
                                           alternative = 'two.sided',var.equal=F,paired=F,
                                           samplename=varsample,factorname=matchingCriteria)
        saveGraphVisstat(
          paste("ttest_", name_of_sample, "_", name_of_factor, sep = ""),type = graphicsoutput)
      }
      #2. If assumptions of t-test are not met: Wilcoxon, else t-test
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
          ( p2$p.value < alpha)
        }else{
          FALSE
        }
        )
      )
        {
        #case 1: Wilcoxon-Test:
        #normal distribution not given for n<limit
        openGraphCairo(type = graphicsoutput)
        vis_sample_fact = two_sample_WilcoxonTest(
          samples,
          fact,
          alternative = "two.sided",
          conf.level= conf.level,
          notchf = T,
          samplename =varsample,factorname =matchingCriteria
        )
        saveGraphVisstat(
          paste(
            "wilcoxon-test_",
            name_of_sample,
            "_",
            name_of_factor,
            sep = ""
          ),type = graphicsoutput)
      } else{
        openGraphCairo(type = graphicsoutput)
        vis_sample_fact = two_sample_tTest(samples,
                                           fact,
                                           conf.level = conf.level,
                                           alternative = 'two.sided',var.equal = F,paired = F,
                                           samplename =varsample,factorname =matchingCriteria)
        saveGraphVisstat(
          paste("ttest_", name_of_sample, "_", name_of_factor, sep = ""),type = graphicsoutput)
        
      }
      return(invisible(vis_sample_fact))
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
        saveGraphVisstat(paste(
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

        saveGraphVisstat(paste(
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
          saveGraphVisstat(
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
        vis_sample_fact = c(vis_chi,vis_mosaic_res)
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
      saveGraphVisstat(paste("regression_", name_of_sample, "_", name_of_factor, sep = ""),
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
          visanova$ad_test$p.value > alpha)

      {
        openGraphCairo(type = graphicsoutput)
        vis_sample_fact = vis_anova(samples,
                                    fact,
                                    samplename = varsample,
                                    factorname = varfactor)
        saveGraphVisstat(paste("anova_", name_of_sample, "_", name_of_factor, sep = ""),
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

        saveGraphVisstat(paste("kruskal_", name_of_sample, "_", name_of_factor, sep = ""),
                       type = graphicsoutput)
        
        
        
      }

    }
  
  
  #setting back to default parameters-----
  #par(oldpar)
  
  
   
    return(invisible(vis_sample_fact))
  }
  #End of vis_sample_fact function -------


  
