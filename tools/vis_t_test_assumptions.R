### Header vis_anova_assumptions -----

#' Visualisation of the normality distribution of the standardised residuals of the ANOVA
#'
#' \code{vis_anova_assumptions} checks for normality of the standardised
#' residuals of the ANOVA. Both the Shapiro-Wilk test \code{shapiro.test()} and
#' the Anderson-Darling test \code{ad.test()} check the null that the
#' standardised residuals are normally distributed. It generates a scatter plot
#' of the standardised residuals versus the fitted mean values of the linear
#' models for each level of \code{fact}. Furthermore a normal QQ plot of the
#' standardised residuals is generated. The null of homogeneity of variances  of
#' each factor level is tested with the \code{bartlett.test()}.

#'
#' @param samples vector containing dependent variable, datatype numeric
#' @param fact vector containing independent variable, datatype factor
#' @param conf.level confidence level, 0.95=default
#' @param samplename name of sample used in graphical output, datatype character
#'   , ''=default
#' @param factorname name of sample used in graphical output, datatypecharacter,
#'   ''=default
#' @param cex number indicating the amount by which plotting text and symbols
#'   should be scaled relative to the default. 1=default, 1.5 is 50\% larger,
#'   0.5 is 50\% smaller, etc.

#'
#' @return \code{list} containing the test statistics of the anova, the p values
#'   generated by the Shapiro-Wilk test \code{shapiro.test()}, the
#'   Anderson-Darling test \code{ad.test()} and the \code{bartlett.test()}.

#' @examples
#' ToothGrowth$dose <- as.factor(ToothGrowth$dose)
#' vis_anova_assumptions(ToothGrowth$len, ToothGrowth$dose)
#'
#' vis_anova_assumptions(ToothGrowth$len, ToothGrowth$supp)
#' vis_anova_assumptions(iris$Petal.Width, iris$Species)
#'
#' @export vis_anova_assumptions

vis_anova_assumptions <- function(samples,
                                  fact,
                                  conf.level = 0.95,
                                  samplename = "",
                                  factorname = "",
                                  cex = 1) {
  oldparanovassum <- par(no.readonly = TRUE)
  on.exit(par(oldparanovassum))
  
  samples3 <- na.omit(samples)
  fact <- subset(fact, !is.na(samples))
  
  samples <- samples3
  
# check for normality of  residuals-----
  if (length(samples) > 7) {
    ad_test <- ad.test(rstandard(samples))
    p_AD <- signif(ad_test$p.value, 3)
  } else {
    ad_test <- "Anderson-Darling test requires sample size of at lest 7."
    p_AD <- NA
  }
  shapiro_test <- shapiro.test(samples)
  p_SH <- shapiro_test$p.value
  bartlett_test <- bartlett.test(samples ~ fact)
  p_bart <- bartlett_test$p.value
  
  
  
  #Plotting  residuals (left) and QQ-Plot right
  #
  # Plot  Residual analysis 
  # 
  # 
  par(mfrow = c(1, 2), oma = c(0, 0, 3, 0))
  #Plot 1: Residual analysis 
  plot(anova$fitted, rstandard(anova), main = "std. Residuals vs. Fitted")
  
  
  abline(h = 0, col = 1, lwd = 2)
  #Plot 2
  qqnorm(rstandard(anova))
  qqline(rstandard(anova), col = "red", lwd = 2)
  par(mfrow = c(1, 1))
  mtext(
    paste(
      "Check for homogeneity of variances: Bartlett: p = ",
      signif(p_bart, 2),
      "\n Check for normality of standardised residuals:",
      "\n Shapiro-Wilk: p = ",
      signif(p_SH, 2),
      ", Anderson-Darling: p = ",
      signif(p_AD, 2)
    ),
    outer = TRUE
  )
 
  
  
  # return statistic list 
  list_aov <- list(
    summary_anova = summary_anova,
    shapiro_test = shapiro_test,
    ad_test = ad_test,
    bartlett_test = bartlett_test
    
    
  )

  return(list_aov)
}
