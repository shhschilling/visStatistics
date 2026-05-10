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
#' specified \code{conf.level}. \code{visstat_core()} is called by the main
#'  wrapper function \code{visstat()}.
#' \code{varsample} and \code{varfactor} are \code{character}
#' strings corresponding to the column names of the chosen vectors in \code{dataframe}. 
#' These vectors must be of type \code{integer}, \code{numeric} or \code{factor}.
#' The automatically generated output figures
#' illustrate the selected statistical hypothesis test, display the main test
#' statistics, and include assumption checks and post hoc comparisons when
#' applicable. The primary test results are returned as a list object.
#'
#' @details The decision logic for selecting a statistical test is described below.
#' For more details, please refer to the package's \code{vignette("visStatistics")}.
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
#' central tendencies are performed. For the decision logic, please refer to the 
#' packages vignette \code{vignette("visStatistics")}
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
#'   \item Heteroscedasticity: \code{bartlett.test()} and \code{levene.test()} and \code{bp_test()}
#' }
#' 
#' Implemented post hoc tests:
#' \itemize{
#'   \item \code{TukeyHSD()} for \code{aov()} 
#'   \item \code{games.howell} for  \code{oneway.test()}
#'   \item \code{pairwise.wilcox.test()} for \code{kruskal.test()}
#' }
#' @seealso
#' The package's vignette
#' \code{vignette("visStatistics")} for a description of the 
#' decision logic, illustrated with numerous examples. The package is accompanied 
#' by its webpage
#' \url{https://shhschilling.github.io/visStatistics/}. The main function \code{\link{visstat}} for a detailed description of the return value.  
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
#' @param do_regression Logical. If TRUE (default), performs simple
#' linear regression analysis with confidence and prediction bands.
#' If FALSE, performs correlation analysis with trend line only
#'  (no regression interpretation).
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
#' @return An object of class \code{"visstat"} containing the results of 
#' the automatically selected statistical test. The specific contents depend on
#'  which test was performed.
#' Additionally, the returned object includes two attributes:
#' \itemize{
#'   \item \code{plot_paths}: Character vector of file paths where plots were 
#'     saved (if \code{graphicsoutput} was specified)
#'   \item \code{captured_plots}: List of captured plot objects for programmatic 
#'     access
#' }
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
#' ## Simple linear regression  (lm())
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
                         do_regression=TRUE,
                         numbers = TRUE,
                         minpercent = 0.05,
                         graphicsoutput = NULL,
                         plotName = NULL,
                         plotDirectory = getwd()) {
  stopifnot(is.data.frame(dataframe))
  stopifnot(varsample %in% names(dataframe))
  stopifnot(varfactor %in% names(dataframe))
  
  
  
  capture_env <- new.env()
  capture_env$captured_plots <- list() #restart list of caputre plots 
  #capture_env$capture_next_plot <- FALSE
  
  
  
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
  
  # Detect ordered x ordered case: needs its own pathway (branch B-pre,
  # Kendall's tau-b). For ordered response with non-ordered predictor we
  # keep the existing behaviour: convert to numeric ranks and route to the
  # non-parametric numeric/factor pathway (Wilcoxon/Kruskal-Wallis).
  ordinal_response <- FALSE
  both_ordered <- is.ordered(samples) && is.ordered(fact)
  if (is.ordered(samples) && !both_ordered) {
    warning("Ordered response (e.g., Likert scale) detected. Converting to numeric ranks for non-parametric analysis.")
    samples <- as.numeric(samples)
    ordinal_response <- TRUE  # Flag to force non-parametric
  }
  
  vis_sample_fact <- list()
  
  # dependent on samples, fact, name_of_sample, name_of_factor, conf.level,
  
  typesample <- class(samples)
  typefactor <-
    class(fact) # type of independent variable returned as a character vector
  
  
  # transform independent variable "fact" of class "character" to factor
  if (inherits(fact, "character")) {
    fact <-
      as.factor(fact) # transform  "fact" of class "character" to factor
    typefactor <- class(fact) # store the class of type "factor"
  }
  
  #  Check order 
  
  if ((inherits(fact, "numeric") || inherits(fact, "integer")) && inherits(samples, "factor")) {
    stop("A numeric or integer predictor with a factor response is ignored.")
  }
  
  maxlabels <- length(levels(samples))
  #  Comparison of all  possible combinations of input variables ----
  #  
  #  
  ## A) median or mean-------
  # --- Numeric vs Factor Logic with Original Error Catching ---
  if ((inherits(samples, "integer") | inherits(samples, "numeric")) &&
      inherits(fact, "factor") && nlevels(fact) >= 2) {
    
    # Pre-check: Original error handling for insufficient data
    counts_per_level <- table(fact)
    if (any(counts_per_level < 1) | length(samples) < 3) {
      warning("In each group must be at least one member and total sample size >= 3.")
      vis_sample_fact <- list(
        error = "Insufficient data",
        input_summary = list(sample_name = name_of_sample, factor_name = name_of_factor)
      )
      attr(vis_sample_fact, "plot_paths") <- plot_paths
      class(vis_sample_fact) <- "visstat"
      return(vis_sample_fact)
    }
    
    # Check if response was originally ordinal - force non-parametric
    if (ordinal_response) {
      warning("Ordinal response detected. Defaulting to non-parametric tests.")
      normality_met <- FALSE
    } else {
      # MANDATORY DIAGNOSTIC: Provide visual evidence for the decision pipeline
      openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory) 
      vis_lm_assumptions(samples, fact, cex = 0.8)
      
      if (is.null(plotName)) {
        filename <- paste("glm_assumptions_", name_of_sample, "_", name_of_factor, sep = "")
      } else {
        filename <- paste("glm_assumptions_", plotName, sep = "")
      }
      plot_paths <- c(plot_paths, saveGraphVisstat(fileName = filename, type = graphicsoutput, 
                                                   fileDirectory = plotDirectory, capture_env = capture_env))
      
      # Decision logic gate
      all_groups_large <- all(counts_per_level > 50)
      
      if (all_groups_large) {
        normality_met <- TRUE 
      } else {
        current_model <- lm(samples ~ fact)
        std_resids <- rstandard(current_model) #this is already part of the output of vis_lm_assumptions
        normality_met <- shapiro.test(std_resids)$p.value >= alpha
      }
    }
    
    # Testing and Visualization steps
    if (!normality_met) {
      # --- NON-PARAMETRIC BRANCH ---
      openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory) 
      if (nlevels(fact) == 2) {
        vis_sample_fact <- two_sample_wilcoxon_test(samples, fact, conf.level = conf.level, 
                                                    samplename = varsample, factorname = varfactor)
        if (is.null(plotName)) {
          filename <- paste("wilcoxon_", name_of_sample, "_", name_of_factor, sep = "")
        } else {
          filename <- paste(plotName)
        }
      } else {
        vis_sample_fact <- vis_Kruskal_Wallis(samples, fact, conf.level = conf.level, 
                                              samplename = varsample, factorname = varfactor)
        if (is.null(plotName)) {
          filename <- paste("kruskal_", name_of_sample, "_", name_of_factor, sep = "")
        } else {
          filename <- paste(plotName)
        }
      }
      plot_paths <- c(plot_paths, saveGraphVisstat(fileName = filename, type = graphicsoutput, 
                                                   fileDirectory = plotDirectory, capture_env = capture_env))
    } else {
      # --- PARAMETRIC BRANCH ---
      var_p <- levene.test(samples, fact)$p.value
      if (nlevels(fact) == 2) {
        # Group-wise normality diagnostics for Welch t-tests
        # visualization of normality assumption per group
        if (var_p < alpha) {
          openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)
          vis_group_normality(samples, fact, conf.level = conf.level, cex = 0.8)
          
          if (is.null(plotName)) {
            filename <- paste("ttest_assumptions_", name_of_sample, "_", name_of_factor, sep = "")
          } else {
            filename <- paste("ttest_assumptions_", plotName, sep = "")
          }
          plot_paths <- c(plot_paths, saveGraphVisstat(fileName = filename, type = graphicsoutput,
                                                       fileDirectory = plotDirectory, capture_env = capture_env))
        }
        # Final t-test execution
        openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory) 
        vis_sample_fact <- two_sample_t_test(samples, fact, var.equal = (var_p >= alpha), 
                                             conf.level = conf.level, samplename = varsample, 
                                             factorname = varfactor)
        if (is.null(plotName)) {
          filename <- paste("ttest_", name_of_sample, "_", name_of_factor, sep = "")
        } else {
          filename <- paste(plotName)
        }
        plot_paths <- c(plot_paths, saveGraphVisstat(fileName = filename, type = graphicsoutput, 
                                                     fileDirectory = plotDirectory, capture_env = capture_env))
      } else {
        # ANOVA execution (Fisher/Welch and Post-hoc handled internally)
        # 
        if (var_p < alpha) {
          # Unequal variances - will use Welch ANOVA, show normality per group
          openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)
          vis_group_normality(samples, fact, conf.level = conf.level, cex = 0.8)
          
          if (is.null(plotName)) {
            filename <- paste("anova_assumptions_", name_of_sample, "_", name_of_factor, sep = "")
          } else {
            filename <- paste("anova_assumptions_", plotName, sep = "")
          }
          plot_paths <- c(plot_paths, saveGraphVisstat(fileName = filename, type = graphicsoutput,
                                                       fileDirectory = plotDirectory, capture_env = capture_env))
        }
        openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory) 
        vis_sample_fact <- vis_anova(samples, fact, samplename = varsample, 
                                     factorname = varfactor, conf.level = conf.level)
        if (is.null(plotName)) {
          filename <- paste("anova_", name_of_sample, "_", name_of_factor, sep = "")
        } else {
          filename <- paste(plotName)
        }
        plot_paths <- c(plot_paths, saveGraphVisstat(fileName = filename, type = graphicsoutput, 
                                                     fileDirectory = plotDirectory, capture_env = capture_env))
      }
    }
  }
  
  
  ## B) Both variables of class factor -----
  ##
  ## "ordered" is a subclass of "factor", so the factor-x-factor branch
  ## handles two sub-cases:
  ##   B.1) both ordered  -> Kendall's tau-b rank correlation
  ##   B.2) at least one nominal -> Chi^2 / Fisher exact test
  ## In both sub-cases the visualisation includes a mosaic plot.

  if (inherits(fact, "factor") && inherits(samples, "factor")) {

    if (both_ordered) {
      ## ----- B.1) Both ordered: Kendall's tau-b -----
      ##
      ## Treating ordered levels as nominal would discard the ordering
      ## and lose power against a monotone trend. Kendall's tau-b
      ## handles tied ranks (unavoidable with few levels, e.g. Likert)
      ## more accurately than Spearman's rho (Agresti 2010, ch. 2;
      ## Kendall 1945).
      samples_num <- as.numeric(samples)
      fact_num    <- as.numeric(fact)

      kendall_test <- suppressWarnings(
        cor.test(samples_num, fact_num,
                 method = "kendall", exact = FALSE,
                 conf.level = conf.level)
      )

      # Plot 1: jittered rank-rank scatter
      # Title via mtext() (outer margin) to match the font used by all other
      # test functions; no "(n=...)" — no other test reports sample size there.
      openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)
      # Adaptive left margin: las=1 prints y-axis labels horizontally.
      # strwidth() measures actual rendered width in inches (device already open);
      # dividing by par("csi") converts to margin lines. +2 for tick gap + ylab.
      max_ylabel_in <- max(strwidth(as.character(levels(samples)), units = "inches"))
      label_lines   <- ceiling(max_ylabel_in / par("csi"))  # lines for tick text
      ylab_line     <- label_lines + 1                       # ylab 1 line beyond
      left_mar      <- max(5, ylab_line + 1)                 # margin + 1 buffer
      op <- par(oma = c(0, 0, 3, 0), mar = c(5, left_mar, 4, 2) + 0.1)

      # Colour by x-axis group (fact levels), consistent with boxplot/Kruskal.
      # Semi-transparency preserved so overlapping points show as darker shades.
      n_x <- length(levels(fact))
      if (n_x <= 2) {
        base_cols <- colorscheme(1)
      } else if (n_x <= length(colorscheme(3)) + 2) {
        base_cols <- c(colorscheme(1), head(colorscheme(3), n_x - 2))
      } else {
        base_cols <- rainbow(n_x, s = 0.4, alpha = 1)
      }
      point_cols <- adjustcolor(base_cols[fact_num], alpha.f = 0.6)

      # ylab placed via mtext() so it sits at the outer margin edge, clear of
      # the horizontal tick labels (las=1). Using plot(ylab=...) would anchor
      # it at mgp[1]=3 lines — inside the tick text for any label >2 lines wide.
      plot(jitter(fact_num,    amount = 0.15),
           jitter(samples_num, amount = 0.15),
           xlab = name_of_factor,
           ylab = "",
           xaxt = "n", yaxt = "n",
           pch = 19, col = point_cols)
      axis(1, at = seq_along(levels(fact)),    labels = levels(fact))
      axis(2, at = seq_along(levels(samples)), labels = levels(samples), las = 1)
      mtext(name_of_sample, side = 2, line = ylab_line, las = 0)
      mtext(bquote("Kendall's" ~ tau[b] ~ "=" ~
                     .(round(kendall_test$estimate, 3))), line = 2)
      mtext(bquote("p =" ~ .(signif(kendall_test$p.value, 3))),  line = 1)
      par(op)

      if (is.null(plotName)) {
        filename <- paste("kendall_", name_of_sample, "_", name_of_factor, sep = "")
      } else {
        filename <- paste(plotName, "_kendall", sep = "")
      }
      plot_paths <- c(plot_paths, saveGraphVisstat(fileName = filename,
                                                   type = graphicsoutput,
                                                   fileDirectory = plotDirectory,
                                                   capture_env = capture_env))

      # No mosaic for Kendall: shade=FALSE + ordered factors renders all tiles
      # black; and the jitter scatter already captures the rank structure.

      # cor.test returns class "htest" with $method, $p.value, ... so we put
      # it directly under $test for print.visstat / summary.visstat.
      kendall_test$data.name <- paste(name_of_sample, "and", name_of_factor)
      vis_sample_fact <- list(
        test             = kendall_test,
        n                = length(samples_num),
        levels_response  = levels(samples),
        levels_predictor = levels(fact)
      )

    } else {
      ## ----- B.2) At least one nominal: Chi^2 / Fisher -----
    if (check_assumptions_count_data(samples, fact) == FALSE) {
      # vis_sample_fact <-
      #   makeTable(samples, fact, name_of_sample, name_of_factor)
      vis_sample_fact <- tryCatch({
        makeTable(samples, fact, name_of_sample, name_of_factor)
      }, error = function(e) {
        list(error = paste("Failed to create contingency table:", e$message))
      })
      
      
    } else {
      # Chi^2 Test-----
      openGraphCairo(type = graphicsoutput,fileDirectory = plotDirectory) 
      
      vis_chi <-
        vis_chi_squared_test(samples, fact, name_of_sample, name_of_factor)
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
                                                   fileDirectory = plotDirectory,capture_env = capture_env))
      # Mosaic plots: only for Pearson chi-square without Yates correction
      # (not Fisher's exact test, not Yates-corrected 2x2 tables)
      is_fisher <- isTRUE(grepl("Fisher", vis_chi$method, ignore.case = TRUE))
      is_yates  <- isTRUE(grepl("Yates",  vis_chi$method, ignore.case = TRUE))
      vis_mosaic_res <- NULL

      if (!is_fisher && !is_yates) {
        # a) complete labeled mosaic graph
        if (maxlabels > 7) {
          numberflag <- FALSE
        } else {
          numberflag <- TRUE
        }

        openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)

        vis_mosaic_res <- vis_mosaic(
          samples,
          fact,
          name_of_sample = name_of_sample,
          name_of_factor = name_of_factor,
          minperc = 0,
          numbers = numberflag,
          shade = !grepl("Yates", vis_chi$method, ignore.case = TRUE)
        )

        if (is.null(plotName)) {
          filename <- paste("mosaic_complete_", name_of_sample, "_", name_of_factor, sep = "")
        } else {
          filename <- paste(plotName, "_", "mosaic_complete", sep = "")
        }

        plot_paths <- c(plot_paths, saveGraphVisstat(filename, type = graphicsoutput,
                                                     fileDirectory = plotDirectory,
                                                     capture_env = capture_env))

        # b) reduced mosaic if many levels
        if (maxlabels > 7) {
          openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)

          vis_mosaic_res <- vis_mosaic(
            samples,
            fact,
            name_of_sample = name_of_sample,
            name_of_factor = "groups",
            minperc = minpercent,
            numbers = TRUE,
            shade = !grepl("Yates", vis_chi$method, ignore.case = TRUE)
          )
          plot_paths <- c(plot_paths, saveGraphVisstat(
            paste("mosaic_reduced_", name_of_sample, "_", name_of_factor, sep = ""),
            type = graphicsoutput,
            fileDirectory = plotDirectory,
            capture_env = capture_env
          ))
        }
      }

      vis_sample_fact <- c(vis_chi, vis_mosaic_res)
    }
    }  # end B.2 (nominal Chi^2 / Fisher)
  }    # end B (factor x factor)
  # C) both types numerical: Regression-----
  
  # Both samples and fact of type integer or numeric
  # Regression
  #
  #
  if ((inherits(fact, "integer") | inherits(fact, "numeric")) &&
      (inherits(samples, "integer") | inherits(samples, "numeric"))) {
    
    # samples: independent variable, factor: dependent   variable
    # check normality
    # 
    openGraphCairo(type = graphicsoutput,fileDirectory = plotDirectory
    ) 
    
    
    normality_residual_assumption <-
      vis_lm_assumptions(samples, fact,cex = 0.8,regression = TRUE)
    
    
    
    
    if (is.null(plotName)) {
      filename <-
        paste("glm_assumptions_", varsample, "_", varfactor, sep = "")
    } else {
      filename <-  paste("glm_assumptions_",plotName)
    }
    
    plot_paths <- c(
      plot_paths,
      saveGraphVisstat(
        fileName = filename,
        type = graphicsoutput,
        fileDirectory = plotDirectory,
        capture_env = capture_env
      )
    )
    
    openGraphCairo(type = graphicsoutput,fileDirectory = plotDirectory) 
    
    vis_sample_fact <- vis_numeric(
      samples,
      # y: dependent
      fact,
      # x: independent
      name_of_factor = name_of_factor,
      name_of_sample = name_of_sample,
      conf.level = conf.level,
      do_regression=do_regression
    )
    if (is.null(plotName)) {
      filename <-
        paste("regression_", name_of_sample, "_", name_of_factor, sep = "")
    } else {
      filename <- paste(plotName)
    }
    
    plot_paths <- c(plot_paths, saveGraphVisstat(fileName = filename,
                                                 type = graphicsoutput,
                                                 fileDirectory = plotDirectory,capture_env = capture_env))
  }
  
  
  
  # At the very end:
  attr(vis_sample_fact, "plot_paths") <- plot_paths
  attr(vis_sample_fact, "captured_plots") <- capture_env$captured_plots
  class(vis_sample_fact) <- "visstat"
  
  # FORCE ALL CAIRO OPERATIONS TO COMPLETE
  if (!is.null(graphicsoutput)) {
    while (!is.null(dev.list())) {
      dev.off()
    }
  }
  
  if (!exists("vis_sample_fact") || is.null(vis_sample_fact)) {
    vis_sample_fact <- list(error = "Analysis completed but no results were generated")
  }
  
  return(invisible(vis_sample_fact))
}
# End of visstat_core function -------