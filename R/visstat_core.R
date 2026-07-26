# MIT License----
# Copyright (c) 2020 Sabine Schilling
# Feedback highly welcome: sabineschilling@gmx.ch

selected_test_title <- function(result) {
  if (!is.null(result[["t-test-statistics"]]$method)) {
    return(trimws(result[["t-test-statistics"]]$method))
  }
  if (!is.null(result[["statsWilcoxon"]]$method)) {
    return(trimws(result[["statsWilcoxon"]]$method))
  }
  if (!is.null(result[["Kruskal Wallis rank sum test"]]$method)) {
    return("Kruskal-Wallis test")
  }
  if (!is.null(result[["summary statistics of ANOVA"]])) {
    anova_result <- result[["summary statistics of ANOVA"]]
    if (
      inherits(anova_result, "htest") &&
        grepl("Welch", anova_result$method)
    ) {
      return("Welch's one-way ANOVA")
    }
    return("Fisher's one-way ANOVA")
  }
  if (!is.null(result$method)) {
    return(trimws(result$method))
  }
  if (!is.null(result$analysis_type)) {
    return(trimws(result$analysis_type))
  }
  if (!is.null(result$test$method)) {
    return(trimws(result$test$method))
  }
  "selected test"
}

# Warn about the one Route 1 configuration in which both automatic gates fail.
#
# In unbalanced designs whose smallest groups carry the largest standard
# deviations, the default gating leaves Bradley's liberal robustness bounds at
# every simulated sample size, because the Levene gate is too weak at small n
# and the rank branch inherits the sensitivity of rank tests to unequal
# variances at unequal group sizes; fixed Welch stays inside the bounds
# throughout. See the Route 1 simulations in vignette("visStatistics").
# The thresholds are heuristics matched to that simulated configuration, whose
# population standard deviation ratio is 2.2. The trigger is set lower because
# the ratio is estimated from the sample, and the group with the largest
# standard deviation is by construction the smallest one, so the estimate is
# both noisy and biased downwards: a population ratio of 2.2 is routinely
# observed as below 2.
detect_adverse_variance_pairing <- function(samples, fact,
                                            sd_ratio_min = 1.5) {
  none <- list(adverse = FALSE)
  n_i <- tapply(samples, fact, function(z) sum(is.finite(z)))
  s_i <- tapply(samples, fact, function(z) stats::sd(z[is.finite(z)]))
  keep <- is.finite(n_i) & is.finite(s_i)
  n_i <- n_i[keep]
  s_i <- s_i[keep]

  if (length(n_i) < 2 || length(unique(n_i)) < 2 || any(s_i <= 0)) {
    return(none)
  }

  sd_ratio <- max(s_i) / min(s_i)
  pairing <- suppressWarnings(
    stats::cor(as.numeric(n_i), as.numeric(s_i), method = "spearman")
  )

  if (!is.finite(pairing) || pairing >= 0 || sd_ratio < sd_ratio_min) {
    return(none)
  }

  list(adverse = TRUE, n = n_i, sd_ratio = sd_ratio)
}

# Only the vulnerable routes are flagged. In the simulated adverse design the
# default gating leaves Bradley's bounds either because the Levene gate is too
# weak and Fisher/Student is selected, or because the residual-normality gate
# sends the comparison to the rank branch, whose Type I error is itself
# sensitive to unequal variances at unequal group sizes. Welch, when it is the
# selected route, stays inside the bounds throughout, so it is not flagged.
warn_adverse_variance_pairing <- function(pairing, route) {
  if (!isTRUE(pairing$adverse)) {
    return(invisible(FALSE))
  }

  reason <- switch(route,
    fisher = paste(
      "the variance gate did not detect the heterogeneity and an",
      "equal-variance test was selected"
    ),
    rank = paste(
      "the residual-normality gate selected a rank-based test, whose Type I",
      "error is itself affected by unequal variances at unequal group sizes"
    )
  )

  warning(
    "Unbalanced group sizes with the largest standard deviations in the ",
    "smallest groups (group sizes ", paste(pairing$n, collapse = ", "),
    "; standard deviation ratio ",
    format(round(pairing$sd_ratio, 2), nsmall = 2), "). Here ", reason,
    ", so the reported test can exceed the nominal significance level. ",
    "Consider group_test = \"welch\" if the comparison is about population ",
    "means; see vignette(\"visStatistics\").",
    call. = FALSE
  )
  invisible(TRUE)
}

# Header visstat_core -----
#'
#' Automated Visualization of Statistical Hypothesis Testing
#'
#' @description \code{visstat_core()} implements the decision tree used by
#' \code{\link{visstat}}. It receives a \code{data.frame} and two column names,
#' determines the corresponding analysis route, creates the diagnostic and
#' result plots, and returns the selected test results as a \code{visstat}
#' object.
#'
#' @details The decision logic is organised into four routes. Route 1 handles a
#' numeric response with a categorical predictor. By default, Route 1 uses
#' residual-based test selection: Shapiro--Wilk on model residuals gates
#' mean-based versus rank-based analysis, and Levene gates equal-variance
#' versus Welch-type mean tests inside the mean branch. Above 5000
#' observations, where \code{shapiro.test()} is undefined, the
#' Anderson--Darling test takes over as the residual-normality gate.
#' Alternatively,
#' \code{group_test = "welch"} forces Welch-type mean tests, and
#' \code{group_test = "rank"} forces Wilcoxon/Kruskal--Wallis tests.
#'
#' Route 2 handles ordered responses with categorical predictors by converting
#' the ordered response to integer level codes and applying Wilcoxon or
#' Kruskal--Wallis tests. Route 3 handles two numeric variables by fitting
#' \code{lm()} by default, or Spearman rank correlation when
#' \code{correlation = TRUE}. Route 4 handles two unordered factors with
#' Pearson's \eqn{\chi^2} test or Fisher's exact test, depending on expected
#' counts. If both variables are ordered and \code{correlation = TRUE},
#' Kendall's \eqn{\tau_b} is used.
#'
#' The significance level \code{alpha} is defined as \code{1 - conf.level}.
#' Assumption tests are interpreted relative to this threshold.
#'
#' Under the default \code{group_test = NULL}, Route 1 issues a warning when the
#' group sizes are unbalanced, the largest group standard deviations occur in the
#' smallest groups, and the route selected is either the equal-variance test or a
#' rank-based test. In that configuration those two routes can exceed the nominal
#' significance level, whereas a selected Welch test does not and is therefore
#' not flagged; see the Route 1 simulations in \code{vignette("visStatistics")}.
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
#'   \item Heteroscedasticity: \code{bartlett.test()} and \code{levene.test()} and \code{bp.test()}
#' }
#'
#' For the general linear model the Shapiro-Wilk, Anderson-Darling, Levene and
#' Bartlett tests are applied to the internally studentised residuals
#' r_i = e_i / (SE_res sqrt(1 - h_i)), which remove the leverage-dependent
#' variance of the raw residuals (Var(e_i) = sigma^2 (1 - h_i)).
#'
#' Implemented post hoc tests:
#' \itemize{
#'   \item \code{TukeyHSD()} for \code{aov()}
#'   \item \code{games.howell()} for  \code{oneway.test()}
#'   \item \code{pairwise.wilcox.test()} for \code{kruskal.test()}
#' }
#' @seealso
#' The package's vignette
#' \code{vignette("visStatistics")} for a description of the
#' decision logic, illustrated with numerous examples. The package is accompanied
#' by its webpage
#' \url{https://shhschilling.github.io/visStatistics/}. The main function
#' \code{\link{visstat}} provides a detailed description of the return value.
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
#' @param correlation Logical. If FALSE (default), performs simple
#' linear regression analysis with confidence and prediction bands.
#' If TRUE, performs Spearman correlation analysis with trend line only
#'  (no regression interpretation).
#' @param numbers a logical indicating whether to show numbers in mosaic count
#'   plots.
#' @param minpercent number between 0 and 1 indicating minimal fraction of total
#'   count data of a category to be displayed	in mosaic count plots.
#' @param group_test Optional character. For Route 1 only, \code{NULL} keeps the
#'   default assumption gates, \code{"welch"} forces Welch-type mean tests, and
#'   \code{"rank"} forces Wilcoxon/Kruskal-Wallis rank tests.
#' @param graphicsoutput saves plot(s) of type "png", "jpeg", "pdf", "svg", "ps"
#'   or "tiff" in directory specified in \code{plotDirectory}. If
#'   graphicsoutput=NULL, no plots are saved. Any other value is not supported by
#'   \code{Cairo()}: it triggers a warning and no file is written.
#' @param plotName graphical output is stored following the naming convention
#'   "plotName.graphicsoutput" in \code{plotDirectory}. Without specifying this
#'   parameter, plotName is automatically generated following the convention
#'   "statisticalTestName_varsample_varfactor".
#' @param plotDirectory specifies directory, where generated plots are stored.
#'   Default is current working directory.
#' @param plot_args Optional named list of base graphics parameters.
#' @details
#' The Q-Q envelopes in the assumption diagnostics are simulated (see
#' \code{\link{qq_lm_envelope}}). The number of simulated refits is taken from
#' the option \code{visStatistics.qq_nsim} and defaults to 5000. As
#' \code{visstat_core()} has no corresponding argument, this option is the only
#' way to change it here; lower it to trade precision for speed, for instance
#' \code{options(visStatistics.qq_nsim = 1000L)}.
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
#' old_qq_nsim <- getOption("visStatistics.qq_nsim")
#' options(visStatistics.qq_nsim = 100L)
#'
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
#' options(visStatistics.qq_nsim = old_qq_nsim)

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
                         correlation = FALSE,
                         numbers = TRUE,
                         minpercent = 0.05,
                         group_test = NULL,
                         graphicsoutput = NULL,
                         plotName = NULL,
                         plotDirectory = getwd(),
                         plot_args = list()) {
  stopifnot(is.data.frame(dataframe))
  stopifnot(varsample %in% names(dataframe))
  stopifnot(varfactor %in% names(dataframe))
  if (is.null(plot_args)) plot_args <- list()


  capture_env <- new.env()
  capture_env$captured_plots <- list() # restart list of caputre plots
  # capture_env$capture_next_plot <- FALSE


  # store default graphical parameters------
  oldparvisstat_core <- par(no.readonly = TRUE)
  oldparvisstat_core$new <- FALSE # reset the default value
  on.exit(par(oldparvisstat_core))


  # Collect plot paths from plot_paths <- c(plot_paths, saveGraphVisstat())
  plot_paths <- character(0)

  # Set default values---------------------------
  alpha <- 1 - conf.level
  group_test <- if (is.null(group_test)) "automatic" else match.arg(group_test, c("welch", "rank"))

  ## Get input variables---------------------------------
  input <-
    get_samples_fact_inputfile(dataframe, varsample, varfactor)
  # out of function get_groups_inputfile
  samples <- input$samples
  fact <- input$fact
  name_of_sample <- input$name_of_sample
  name_of_factor <- input$name_of_factor
  # Detect ordered x ordered case:
  # Only route to Kendall's tau-b when correlation=TRUE; otherwise treat as
  # ordered response + factor predictor (Wilcoxon/Kruskal-Wallis).
  # For ordered response with non-ordered predictor we always convert to
  # numeric ranks and route to the non-parametric pathway.
  ordinal_response <- FALSE
  both_ordered <- is.ordered(samples) && is.ordered(fact)
  both_numeric_integer <- (is.numeric(samples) || is.integer(samples)) &&
    (is.numeric(fact) || is.integer(fact))
  use_kendall <- both_ordered && correlation
  correlation_ignored <- isTRUE(correlation) &&
    !both_ordered && !both_numeric_integer

  if (both_ordered && !correlation && nlevels(fact) > 4) {
    warning(
      "Ordered predictor with ", nlevels(fact), " levels detected. ",
      "Kruskal-Wallis discards the ordering of the predictor. ",
      "If a monotone association is of interest, consider correlation = TRUE ",
      "for Kendall's tau_b.",
      call. = FALSE
    )
  }

  if (is.ordered(samples) && (!both_ordered || !correlation)) {
    warning(
      "Ordered response detected. Converting to integer level codes for ",
      "non-parametric analysis.",
      call. = FALSE
    )
    samples <- as.numeric(samples)
    ordinal_response <- TRUE # Flag to force non-parametric
  }

  vis_sample_fact <- list()

  # dependent on samples, fact, name_of_sample, name_of_factor, conf.level,

  # transform independent variable "fact" of class "character" to factor
  if (inherits(fact, "character")) {
    fact <- as.factor(fact) # transform "fact" of class "character" to factor
  }

  #  Check order

  if (
    (inherits(fact, "numeric") || inherits(fact, "integer")) &&
      inherits(samples, "factor")
  ) {
    stop("A numeric or integer predictor with a factor response is ignored.")
  }

  maxlabels <- length(levels(samples))
  #  Comparison of all  possible combinations of input variables ----
  #
  #
  ## A) median or mean-------
  # --- Numeric vs Factor Logic with Original Error Catching ---
  if (
    (inherits(samples, "integer") || inherits(samples, "numeric")) &&
      inherits(fact, "factor") && nlevels(fact) >= 2
  ) {
    # Pre-check: Original error handling for insufficient data
    counts_per_level <- table(fact)
    if (any(counts_per_level < 1) || length(samples) < 3) {
      warning(
        "In each group must be at least one member and total sample size >= 3.",
        call. = FALSE
      )
      vis_sample_fact <- list(
        error = "Insufficient data",
        effect_size = effect_size_unavailable("Effect size unavailable for insufficient data."),
        input_summary = list(sample_name = name_of_sample, factor_name = name_of_factor)
      )
      attr(vis_sample_fact, "plot_paths") <- plot_paths
      class(vis_sample_fact) <- "visstat"
      return(vis_sample_fact)
    }

    # Explicit Route 1 overrides bypass route selection. The Welch override
    # still shows/checks the assumption diagnostics, but does not switch to
    # the rank branch when residual normality is rejected.
    if (group_test == "rank" || ordinal_response) {
      normality_met <- FALSE
    } else {
      # MANDATORY DIAGNOSTIC: Provide visual evidence for the decision pipeline
      openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)
      vis_lm_assumptions(samples, fact,
        cex = 0.8, conf.level = conf.level,
        plot_args = plot_args
      )

      if (is.null(plotName)) {
        filename <- paste("glm_assumptions_", name_of_sample, "_", name_of_factor, sep = "")
      } else {
        filename <- paste("glm_assumptions_", plotName, sep = "")
      }
      plot_paths <- c(plot_paths, saveGraphVisstat(
        fileName = filename, type = graphicsoutput,
        fileDirectory = plotDirectory, capture_env = capture_env
      ))

      # Decision logic gate
      # n > 50 CLT bypass DISABLED (commented out, not removed): the
      # Shapiro--Wilk test now routes the selection at all group sizes.
      # To restore the bypass, uncomment all_groups_large and the
      # `if (all_groups_large)` branch below.
      # all_groups_large <- all(counts_per_level > 50)
      current_model <- lm(samples ~ fact)
      raw_residuals <- residuals(current_model)
      # Internally studentised residuals for the normality gate, matching the
      # residual scale shown by vis_lm_assumptions().
      scaled_residuals <- rstandard(current_model)
      if (any(!is.finite(scaled_residuals))) {
        scaled_residuals <- raw_residuals / max(sigma(current_model), 1e-8)
      }

      # if (all_groups_large) {
      #   normality_met <- TRUE
      # } else if (length(raw_residuals) > 5000) {
      if (length(raw_residuals) > 5000) {
        # Shapiro--Wilk is undefined for n > 5000; route on Anderson--Darling,
        # which has no upper sample-size limit, so shape still decides.
        normality_test_name <- "Anderson-Darling"
        normality_p <- nortest::ad.test(scaled_residuals)$p.value
      } else {
        normality_test_name <- "Shapiro-Wilk"
        normality_p <- shapiro.test(scaled_residuals)$p.value
      }
      normality_met <- normality_p >= alpha

      if (group_test == "welch" && !normality_met) {
        warning(
          normality_test_name,
          " test p = ", format.pval(normality_p, digits = 3),
          " is below alpha = ",
          signif(alpha, 3),
          "; normality assumption violated. Consider switching to the ",
          "\"rank\" method.",
          call. = FALSE
        )
        normality_met <- TRUE
      }
    }

    # Flag the configuration in which the automatic route is unreliable, once
    # the route is known: only the equal-variance and rank routes are affected.
    adverse_pairing <- if (group_test == "automatic" && !ordinal_response) {
      detect_adverse_variance_pairing(samples, fact)
    } else {
      list(adverse = FALSE)
    }

    # Testing and Visualization steps
    if (!normality_met) {
      # --- NON-PARAMETRIC BRANCH ---
      warn_adverse_variance_pairing(adverse_pairing, "rank")
      openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)
      if (nlevels(fact) == 2) {
        vis_sample_fact <- two_sample_wilcoxon_test(samples, fact,
          conf.level = conf.level,
          samplename = varsample, factorname = varfactor,
          plot_args = plot_args
        )
        if (is.null(plotName)) {
          filename <- paste("wilcoxon_", name_of_sample, "_", name_of_factor, sep = "")
        } else {
          filename <- paste(plotName)
        }
      } else {
        vis_sample_fact <- vis_Kruskal_Wallis(samples, fact,
          conf.level = conf.level,
          samplename = varsample, factorname = varfactor,
          plot_args = plot_args
        )
        if (is.null(plotName)) {
          filename <- paste("kruskal_", name_of_sample, "_", name_of_factor, sep = "")
        } else {
          filename <- paste(plotName)
        }
      }
      plot_paths <- c(plot_paths, saveGraphVisstat(
        fileName = filename, type = graphicsoutput,
        fileDirectory = plotDirectory, capture_env = capture_env
      ))
    } else {
      # --- PARAMETRIC BRANCH ---
      if (!exists("scaled_residuals", inherits = FALSE)) {
        current_model <- lm(samples ~ fact)
        raw_residuals <- residuals(current_model)
        scaled_residuals <- rstandard(current_model)
        if (any(!is.finite(scaled_residuals))) {
          scaled_residuals <- raw_residuals / max(sigma(current_model), 1e-8)
        }
      }
      # Internally studentised residuals remove the leverage-induced
      # heteroscedasticity of the raw residuals (Var(e_i) = sigma^2 (1 - h_i)),
      # matching the |r_i| spread panel of vis_lm_assumptions().
      var_p <- levene.test(scaled_residuals, fact)$p.value
      use_fisher <- group_test == "automatic" && var_p >= alpha
      if (use_fisher) {
        warn_adverse_variance_pairing(adverse_pairing, "fisher")
      }
      if (nlevels(fact) == 2) {
        # Final t-test execution
        openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)
        vis_sample_fact <- two_sample_t_test(samples, fact,
          var.equal = use_fisher,
          conf.level = conf.level, samplename = varsample,
          factorname = varfactor,
          plot_args = plot_args
        )
        if (is.null(plotName)) {
          filename <- paste("ttest_", name_of_sample, "_", name_of_factor, sep = "")
        } else {
          filename <- paste(plotName)
        }
        plot_paths <- c(plot_paths, saveGraphVisstat(
          fileName = filename, type = graphicsoutput,
          fileDirectory = plotDirectory, capture_env = capture_env
        ))
      } else {
        # ANOVA execution (Fisher/Welch and Post-hoc handled internally)
        openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)
        vis_sample_fact <- vis_anova(samples, fact,
          samplename = varsample,
          factorname = varfactor, conf.level = conf.level,
          variance_route = if (group_test == "welch") "welch" else "levene",
          plot_args = plot_args
        )
        if (is.null(plotName)) {
          filename <- paste("anova_", name_of_sample, "_", name_of_factor, sep = "")
        } else {
          filename <- paste(plotName)
        }
        plot_paths <- c(plot_paths, saveGraphVisstat(
          fileName = filename, type = graphicsoutput,
          fileDirectory = plotDirectory, capture_env = capture_env
        ))
      }
    }
  }


  ## B) Both variables of class factor -----
  ##
  ## "ordered" is a subclass of "factor", so the factor-x-factor branch
  ## handles two sub-cases:
  ##   B.1) both ordered AND correlation=TRUE -> Kendall's tau-b rank correlation
  ##   B.2) at least one nominal, or both ordered but correlation=FALSE
  ##        -> Chi^2 / Fisher exact test (ordered response already converted above)

  if (inherits(fact, "factor") && inherits(samples, "factor")) {
    if (use_kendall) {
      ## ----- B.1) Both ordered + correlation=TRUE: Kendall's tau-b -----
      ##
      ## Treating ordered levels as nominal would discard the ordering
      ## and lose power against a monotone trend. Kendall's tau-b
      ## handles tied ranks (unavoidable with few levels, e.g. Likert)
      ## more accurately than Spearman's rho (Agresti 2010, ch. 2;
      ## Kendall 1945).
      samples_num <- as.numeric(samples)
      fact_num <- as.numeric(fact)

      kendall_test <- suppressWarnings(
        cor.test(samples_num, fact_num,
          method = "kendall", exact = FALSE,
          conf.level = conf.level
        )
      )

      # Plot 1: jittered rank-rank scatter
      # Title via mtext() (outer margin) to match the font used by all other
      # test functions; no "(n=...)" — no other test reports sample size there.
      openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)
      # Adaptive left margin: las=1 prints y-axis labels horizontally.
      # strwidth() measures actual rendered width in inches (device already open);
      # dividing by par("csi") converts to margin lines. +2 for tick gap + ylab.
      visstat_graphics_par(plot_args)
      max_ylabel_in <- max(strwidth(as.character(levels(samples)), units = "inches"))
      label_lines <- ceiling(max_ylabel_in / par("csi")) # lines for tick text
      ylab_line <- label_lines + 1 # ylab 1 line beyond
      left_mar <- max(5, ylab_line + 1) # margin + 1 buffer
      op <- par(mar = c(5, left_mar, 4, 2) + 0.1)

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
      plot(jitter(fact_num, amount = 0.15),
        jitter(samples_num, amount = 0.15),
        xlab = visstat_graphics_arg(plot_args, "xlab", name_of_factor),
        ylab = "",
        xaxt = "n", yaxt = "n",
        pch = visstat_graphics_arg(plot_args, "pch", 19),
        col = visstat_graphics_arg(plot_args, "col", point_cols)
      )
      axis(1, at = seq_along(levels(fact)), labels = levels(fact))
      axis(2, at = seq_along(levels(samples)), labels = levels(samples), las = 1)
      mtext(visstat_graphics_arg(plot_args, "ylab", name_of_sample),
        side = 2, line = ylab_line, las = 0
      )
      mtext(visstat_graphics_arg(
        plot_args,
        "main",
        bquote("Kendall's" ~ tau[b] ~ "=" ~
                 .(round(kendall_test$estimate, 3)) ~
                 ", p =" ~ .(signif(kendall_test$p.value, 3)))
      ))
      par(op)

      if (is.null(plotName)) {
        filename <- paste("kendall_", name_of_sample, "_", name_of_factor, sep = "")
      } else {
        filename <- paste(plotName, "_kendall", sep = "")
      }
      plot_paths <- c(plot_paths, saveGraphVisstat(
        fileName = filename,
        type = graphicsoutput,
        fileDirectory = plotDirectory,
        capture_env = capture_env
      ))

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
        vis_sample_fact <- tryCatch(
          {
            makeTable(samples, fact, name_of_sample, name_of_factor)
          },
          error = function(e) {
            list(error = paste("Failed to create contingency table:", e$message))
          }
        )
      } else {
        # Chi^2 Test-----
        openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)

        vis_chi <-
          vis_chi_squared_test(samples, fact, name_of_sample, name_of_factor,
            plot_args = plot_args
          )
        if (is.null(plotName)) {
          filename <- paste("chi_squared_or_fisher_",
            name_of_sample,
            "_",
            name_of_factor,
            sep = ""
          )
        } else {
          filename <- paste(plotName, "_", "chi_squared_or_fisher", sep = "")
        }

        plot_paths <- c(plot_paths, saveGraphVisstat(
          fileName = filename,
          type = graphicsoutput,
          fileDirectory = plotDirectory, capture_env = capture_env
        ))
        # Mosaic plots: only for Pearson chi-square without Yates correction
        # (not Fisher's exact test, not Yates-corrected 2x2 tables)
        is_fisher <- isTRUE(grepl("Fisher", vis_chi$method, ignore.case = TRUE))
        is_yates <- isTRUE(grepl("Yates", vis_chi$method, ignore.case = TRUE))
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
            shade = !grepl("Yates", vis_chi$method, ignore.case = TRUE),
            plot_args = plot_args
          )

          if (is.null(plotName)) {
            filename <- paste("mosaic_complete_", name_of_sample, "_", name_of_factor, sep = "")
          } else {
            filename <- paste(plotName, "_", "mosaic_complete", sep = "")
          }

          plot_paths <- c(plot_paths, saveGraphVisstat(filename,
            type = graphicsoutput,
            fileDirectory = plotDirectory,
            capture_env = capture_env
          ))

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
              shade = !grepl("Yates", vis_chi$method, ignore.case = TRUE),
              plot_args = plot_args
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
    } # end B.2 (nominal Chi^2 / Fisher)
  } # end B (factor x factor)
  # C) both types numerical: Regression-----

  # Both samples and fact of type integer or numeric
  # Regression
  #
  #
  if (
    (inherits(fact, "integer") || inherits(fact, "numeric")) &&
      (inherits(samples, "integer") || inherits(samples, "numeric"))
  ) {
    # samples: independent variable, factor: dependent   variable
    # check normality
    #
    openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)


    if (!correlation) {
      vis_lm_assumptions(
        samples,
        fact,
        cex = 0.8,
        correlation = FALSE,
        conf.level = conf.level,
        plot_args = plot_args
      )


      if (is.null(plotName)) {
        filename <-
          paste("glm_assumptions_", varsample, "_", varfactor, sep = "")
      } else {
        filename <- paste("glm_assumptions_", plotName)
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
    }

    openGraphCairo(type = graphicsoutput, fileDirectory = plotDirectory)

    vis_sample_fact <- vis_numeric(
      samples,
      # y: dependent
      fact,
      # x: independent
      name_of_factor = name_of_factor,
      name_of_sample = name_of_sample,
      conf.level = conf.level,
      correlation = correlation,
      plot_args = plot_args
    )
    if (is.null(plotName)) {
      filename <-
        paste("regression_", name_of_sample, "_", name_of_factor, sep = "")
    } else {
      filename <- paste(plotName)
    }

    plot_paths <- c(plot_paths, saveGraphVisstat(
      fileName = filename,
      type = graphicsoutput,
      fileDirectory = plotDirectory, capture_env = capture_env
    ))
  }


  # At the very end:
  if (!exists("vis_sample_fact") || is.null(vis_sample_fact)) {
    vis_sample_fact <- list(error = "Analysis completed but no results were generated")
  }
  if (is.list(vis_sample_fact) && is.null(vis_sample_fact$effect_size)) {
    vis_sample_fact$effect_size <- effect_size(vis_sample_fact, x = fact, y = samples)
  }
  if (isTRUE(correlation_ignored) && is.null(vis_sample_fact$error)) {
    selected_title <- selected_test_title(vis_sample_fact)
    warning(
      "correlation = TRUE was ignored; visstat() returned ",
      selected_title,
      ".",
      call. = FALSE
    )
  }
  attr(vis_sample_fact, "plot_paths") <- plot_paths
  attr(vis_sample_fact, "captured_plots") <- capture_env$captured_plots
  class(vis_sample_fact) <- "visstat"

  # FORCE ALL CAIRO OPERATIONS TO COMPLETE
  if (!is.null(graphicsoutput)) {
    while (!is.null(dev.list())) {
      dev.off()
    }
  }

  return(invisible(vis_sample_fact))
}
# End of visstat_core function -------
