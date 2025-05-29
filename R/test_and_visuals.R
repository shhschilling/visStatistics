# MIT License-----
# Copyright (c) 2021 Sabine Schilling

# Plotting functions----

# Testing for normality and visualization ----

test_norm_vis <- function(x, y_axis_hist = c(0, 0.04)) {
  # store default graphical parameters------
  oldparnormvis <- par(no.readonly = TRUE)
  on.exit(par(oldparnormvis))
  par(mfrow = c(1, 2), oma = c(0, 0, 3, 0))
  # Remove NA from x
  x <- x[!is.na(x)]
  norm_dens <- function(z) {
    dnorm(z, mean(x), sd(x))
  }
  
  ymax <- max(norm_dens(x))
  # Plot histogramm of raw data
  otto <- hist(
    x,
    freq = FALSE,
    col = "grey",
    breaks = "Sturges",
    xlim = c(
      mean(x, na.rm = T) - 5 * sd(x, na.rm = T),
      mean(x, na.rm = T) +
        5 * sd(x, na.rm = T)
    ),
    ylim = c(0, 1.2 * ymax)
  )
  
  # normal distribution with mean and sd of given distribution
  curve(norm_dens,
        col = "red",
        add = TRUE,
        lwd = 2)
  
  
  # par(new = TRUE) #the next high-level plotting command does not clean the frame before drawing
  # as if it were on a new device.
  lines(density(x), col = "blue")
  
  legend(
    "topright",
    c("fitted", "estimated"),
    lty = 1,
    lwd = 2,
    col = c("red", "blue"),
    bty = "n"
  )
  box() # frame around current plot
  
  qqnorm(x)
  qqline(x, col = "red", lwd = 2)
  
  KS <- ad.test(x)
  p_KS <- signif(KS$p.value, 2)
  SH <- shapiro.test(x)
  p_SH <- signif(SH$p.value, 2)
  
  mtext(
    paste(
      "Shapiro-Wilk: p = ",
      p_SH,
      "\n Anderson-Darling: p = ",
      p_KS,
      "\n Nullhypothesis: Data is normally distributed"
    ),
    outer = TRUE
  )
  my_list <- list("Anderson-Darling" = KS, "Shapiro" = SH)
  
  return(my_list)
}



###### Two-Sample t-Test ###############################
two_sample_t_test <- function(samples,
                              fact,
                              alternative = c("two.sided", "less", "greater"),
                              paired = FALSE,
                              var.equal = FALSE,
                              conf.level = conf.level,
                              samplename = "",
                              factorname = "") {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  alternative <- match.arg(alternative)
  
  
  if (!missing(conf.level) &&
      (length(conf.level) != 1 || !is.finite(conf.level) ||
       conf.level < 0 || conf.level > 1)) {
    return(warning("'conf.level' must be a single number between 0 and 1"))
  }
  
  if (missing(conf.level)) {
    conf.level <- 0.95
  }
  alpha <- 1 - conf.level
  levels <- unique(sort(fact))
  twosamples <- create_two_samples_vector(samples, fact)
  x <- twosamples$sample1and2
  x1 <- twosamples$sample1
  x2 <- twosamples$sample2
  # Check normality of both samples-----
  p1 <- test_norm(twosamples$sample1)
  p2 <- test_norm(twosamples$sample2)
  
  # margins of y -axis
  lower <- 0.2
  upper <- 0.2
  margins <- calc_min_max_of_y_axis(x, lower, upper)
  mi <- margins[[1]]
  ma <- margins[[2]]
  
  x <- cbind(x, factor(c(rep(1, length(
    x1
  )), rep(2, length(
    x2
  )))))
  
  b <- boxplot(
    samples ~ fact,
    lwd = 0.5,
    xlab = factorname,
    ylab = samplename,
    ylim = c(mi - 2, ma),
    varwidth = T,
    col = colorscheme(1)
  )
  
  
  stripchart(
    samples ~ fact,
    vertical = TRUE,
    xlim = c(0, 3),
    ylim = c(mi, ma),
    # col = c("grey70", "grey80"),
    col = colorscheme(2),
    axes = FALSE,
    method = "jitter",
    add = TRUE
  )
  
  
  
  points(1,
         mean(x1),
         col = 2,
         pch = 1,
         lwd = 3)
  points(2,
         mean(x2),
         col = 2,
         pch = 1,
         lwd = 3)
  
  # alpha_sidak = 1 - sqrt(1 - alpha)
  
  alpha_sidak <- alpha
  
  correction1 <- qt(1 - 0.5 * alpha_sidak, length(x1) - 1) * sd(x1) / sqrt(length(x1))
  correction2 <- qt(1 - 0.5 * alpha_sidak, length(x2) - 1) * sd(x2) / sqrt(length(x2))
  
  arrows(
    1,
    mean(x1, na.rm = T) + correction1,
    1,
    mean(x1, na.rm = T) - correction1,
    angle = 90,
    code = 3,
    # halbes Konfidenzintervall
    col = 2,
    lty = 1,
    lwd = 2,
    length = 0.1
  )
  
  arrows(
    2,
    mean(x2) + correction2,
    2,
    mean(x2) - correction2,
    angle = 90,
    code = 3,
    col = 2,
    lty = 1,
    lwd = 2,
    length = 0.1
  )
  
  # Sample sizes above box plot
  text(1:length(b$n), c(ma, ma), paste("N=", b$n))
  t <- t.test(
    x1,
    x2,
    alternative = alternative,
    conf.level = conf.level,
    paired = FALSE,
    var.equal = FALSE,
    na.action = na.omit
  )
  # Legend
  legend(
    "bottomleft",
    inset = 0.05,
    horiz = F,
    c(paste(
      "mean with", conf.level * 100, "% conf. intervall "
    )),
    col = c(colors()[552]),
    bty = "n",
    lwd = 3
  )
  
  p_value <- t$p.value
  p_value <- signif(p_value, 2)
  test_statistic <- t$statistic
  test_statistic <- round(test_statistic, 2)
  stat_name <- names(t$statistic)
  
  # Title general generation
  if (alternative == "two.sided") {
    ah <- "equals"
  } else {
    ah <- alternative
  }
  compare <- side_of_nh(alternative)
  
  
  
  
  mean_or_median <- "population mean"
  comparepvalue <- calculate_comparepvalue(p_value, conf.level)
  
  two_sample_title <-
    paste(
      t$method,
      ", alpha =",
      1 - conf.level,
      "\n Null hypothesis: ",
      mean_or_median,
      " ",
      samplename,
      " of ",
      factorname,
      " \"",
      unique(sort(fact))[1],
      "\" ",
      compare,
      " ",
      mean_or_median,
      " ",
      samplename,
      " of ",
      factorname,
      " \"",
      unique(sort(fact))[2],
      "\" ",
      "\n",
      stat_name,
      "=",
      test_statistic,
      ", p = ",
      p_value,
      ", p ",
      comparepvalue,
      "alpha",
      sep = ""
    )
  
  
  
  
  
  
  mtext(two_sample_title)
  
  my_list <-
    list(
      "dependent variable (response)" = samplename,
      "independent variables (features)" = unique(fact),
      "t-test-statistics" = t,
      "Shapiro-Wilk-test_sample1" = p1,
      "Shapiro-Wilk-test_sample2" = p2
    )
  
  return(my_list)
}


# Two-Sample Wilcoxon-Test  ###############################
# One function with flags for greater, less, two sided and notch
two_sample_wilcoxon_test <- function(samples,
                                     fact,
                                     alternative = c("two.sided", "less", "greater"),
                                     conf.level = conf.level,
                                     notchf = FALSE,
                                     samplename = "",
                                     factorname = "",
                                     cex = 1) {
  oldparwilcox <- par(no.readonly = TRUE) # make a copy of current values
  on.exit(par(oldparwilcox))
  
  alternative <- match.arg(alternative)
  # Error handling ----
  
  
  if (missing(conf.level)) {
    conf.level <- 0.95
  }
  
  if (missing(notchf)) {
    notchf <- FALSE
  }
  
  if (!((length(conf.level) == 1L) && is.finite(conf.level) &&
        (conf.level > 0) && (conf.level < 1))) {
    return(warning("'conf.level' must be a single number between 0 and 1"))
  }
  
  if (!is.numeric(samples)) {
    return(warning("'samples' must be numeric"))
  }
  if (!is.null(fact)) {
    if (!is.factor(fact)) {
      return(warning("'fact' must be factorial"))
    }
  }
  
  # Store default graphical parameter
  # Define color palette
  colortuple2 <- colorscheme(2)
  
  
  
  
  
  # Create to numeric vectors
  twosamples <- create_two_samples_vector(samples, fact)
  x <- twosamples$sample1and2
  x1 <- twosamples$sample1
  x2 <- twosamples$sample2
  
  upper <- 0.2
  lower <- 0.05
  res <- calc_min_max_of_y_axis(x, upper, lower)
  mi <- res[[1]]
  ma <- res[[2]]
  
  x <- cbind(x, factor(c(rep(1, length(
    x1
  )), rep(2, length(
    x2
  )))))
  
  par(oma = c(0, 0, 2, 0)) # outer margin above: 2 lines..
  # par(mar = c(8,4,4,2) + 0.1) #increse margin of labels below
  
  b <- boxplot(samples ~ fact, plot = 0) # holds  the counts
  
  
  
  stripchart(
    samples ~ fact,
    vertical = TRUE,
    xlim = c(0, 3),
    # ylim = c(mi, ma),
    method = "jitter",
    col = colorscheme(2),
    ylim = c(0, ma),
    ylab = samplename,
    xlab = factorname
  )
  boxplot(
    samples ~ fact,
    notch = notchf,
    varwidth = T,
    col = colorscheme(1),
    ylim = c(0, ma),
    add = T
  )
  # text(1:length(b$n), b$stats[5,]+1, paste("n=", b$n))
  text(1:length(b$n), c(ma, ma), paste("N =", b$n))
  t <- wilcox.test(samples ~ fact, alternative = alternative, na.action = na.omit)
  p_value <- t$p.value
  p_value <- formatC(signif(p_value, digits = 2))
  
  test_statistic <- t$statistic
  test_statistic <- round(test_statistic, 2)
  stat_name <- names(t$statistic)
  
  
  compare <- side_of_nh(alternative)
  if (factorname == "match") {
    prefix <- "of matched"
  } else {
    prefix <- character()
  }
  
  # Title general generation
  if (alternative == "two.sided") {
    ah <- "equals"
  } else {
    ah <- alternative
  }
  compare <- side_of_nh(alternative)
  
  
  
  
  mean_or_median <- "population median"
  comparepvalue <- calculate_comparepvalue(p_value, conf.level)
  
  two_sample_title <-
    paste(
      t$method,
      ", alpha = ",
      1 - conf.level,
      "\n Null hypoth.: ",
      mean_or_median,
      " ",
      samplename,
      " of ",
      factorname,
      " ",
      unique(sort(fact))[1],
      " ",
      compare,
      " ",
      mean_or_median,
      " ",
      samplename,
      " of ",
      factorname,
      " ",
      unique(sort(fact))[2],
      "\n",
      stat_name,
      "=",
      test_statistic,
      ", p = ",
      p_value,
      ", p ",
      comparepvalue,
      " alpha",
      sep = ""
    )
  
  
  
  
  mtext(two_sample_title)
  
  
  
  my_list <-
    list(
      "dependent variable (response)" = samplename,
      "indepedent variables (features)" = unique(fact),
      "statsWilcoxon" = t,
      "statsBoxplot" = b
    )
  
  return(my_list)
}

# Two-Sample F-Test ###############################
# subtract means; two lines according to variances.
two_sample_FTest <- function(samples,
                             fact,
                             conf.int = conf.int,
                             alternative = "two.sided") {
  # if (missing(conf.int)) conf.int = 0.95
  #  if (missing(alternative)) alternative = "two.sided"
  # Store default graphical parameter
  oldparftest <- par(no.readonly = TRUE)
  on.exit(par(oldparftest))
  if (missing(conf.int)) {
    conf.int <- 0.95
  }
  
  alpha <- 1 - conf.int
  levels <- unique(sort(fact))
  
  x1 <- samples[fact == levels[1]]
  x2 <- samples[fact == levels[2]]
  
  x1 <- x1 - mean(x1, na.rm = T)
  
  x2 <- x2 - mean(x2)
  
  
  x <- c(x1, x2)
  spread <- max(x) - min(x)
  spread <- max(spread, var(x1), var(x2))
  
  mi <- min(x) - 0.3 * spread
  ma <- max(x) + 0.3 * spread
  
  x <- cbind(x, factor(c(rep(1, length(
    x1
  )), rep(2, length(
    x2
  )))))
  
  par(oma = c(0, 0, 3, 0))
  stripchart(
    x[, 1] ~ x[, 2],
    vertical = TRUE,
    xlim = c(0.5, 3),
    ylim = c(mi, ma),
    col = c("grey70", "grey80"),
    ylab = "centered samples",
    xlab = "",
    axes = FALSE
  )
  
  axis(side = 2)
  axis(side = 1,
       at = c(1, 2),
       labels = levels)
  box()
  
  lines(
    x = c(1.1, 1.1),
    y = c(-0.5 * var(x1), 0.5 * var(x1)),
    col = "blue",
    lwd = 5
  )
  lines(
    x = c(1.9, 1.9),
    y = c(-0.5 * var(x2), 0.5 * var(x2)),
    col = "blue",
    lwd = 5
  )
  
  legend(
    "topright",
    inset = 0.05,
    c("variances"),
    col = c("blue"),
    lwd = 2
  )
  
  t <- var.test(x1, x2, alternative = alternative)
  p_value <- t$p.value
  p_value <- signif(p_value, 3)
  
  test_statistic <- t$statistic
  test_statistic <- formatC(signif(test_statistic, digits = 2))
  
  
  mtext(
    paste(
      "Two Sample F-Test (",
      alternative,
      "): P = ",
      p_value,
      "\n Confidence Level = ",
      1 - alpha
    ),
    outer = TRUE
  )
}



# Chisquared Test of Fisher-test  ----


# vis_chi_squared_test: implemented in vis_samples_fact -----
vis_chi_squared_test <- function(samples,
                                 fact,
                                 samplename,
                                 factorname,
                                 cex = 1) {
  oldparchi <- par(no.readonly = TRUE)
  on.exit(par(oldparchi))
  
  colortuple <- colorscheme(1)
  ColorPalette <- colorscheme(3)
  if (missing(samplename)) {
    samplename <- character()
  }
  if (missing(factorname)) {
    factorname <- character()
  }
  
  counts <- makeTable(samples, fact, samplename, factorname)
  # check for minimal size of 2x2
  check_assumptions_chi <- check_assumptions_count_data(samples, fact)
  
  if (check_assumptions_chi == FALSE) {
    fisher_chi <- counts
    return(fisher_chi)
   
  } else {
    row_sum <- rowSums(counts)
    col_sum <- colSums(counts)
    count_labels <- dimnames(counts)[2]
    count_labels <- as.character(unlist(count_labels))
    
    category_names <- dimnames(counts)[1]
    category_names <- as.character(unlist(category_names))
    
    norm_counts <- (counts / row_sum) * 100 # 100 %percentage in each group
    max_val_y <- max(norm_counts, na.rm = T)
    # col_vec_browser=c(colortuple,rainbow(nrow(counts)-2, s = 0.5))
    if (nrow(counts) < (length(ColorPalette) + 2)) {
      col_vec_browser <- c(colortuple, head(ColorPalette, n = nrow(counts) - 2))
    } else {
      col_vec_browser <- c(colortuple, rainbow(nrow(counts) - 2, s = 0.4, alpha = 1))
    }
    
    
    # creates new plot for barplot
    
    par(mfrow = c(1, 1), oma = c(0, 0, 3, 0))
    
    maxlabels <- length(levels(samples))
    if (maxlabels > 7 |
        grepl("basis", samplename) | grepl("source", samplename) |
        grepl("basis", factorname) | grepl("source", factorname) |
        grepl("genotyped", samplename) |
        grepl("genotyped", factorname)) {
      labelsize <- 0.3 * cex
    } else if (maxlabels > 5) {
      labelsize <- 0.7 * cex
    } else {
      labelsize <- cex
    }
    
    
    fisher_chi <- fisher_chi(counts) # checks if Cochran requirements for chi2 are met, if not: only fisher exact test allowed
    
    # titletext <- paste0(fisher_chi$method,
    #                    "\n Chi-squared = ",signif(fisher_chi$p.statistic, 2),
    #                    ", p-value = ",
    #                    signif(fisher_chi$p.value, 3),
    #                    sep = "")
    #                    
    if (fisher_chi$method != "Fisher's Exact Test for Count Data") {
      chi2_fisher_text <- paste0(
        fisher_chi$method,
        "\nChi-squared = ", round(fisher_chi$statistic, 2),
        ", p-value = ", signif(fisher_chi$p.value, 3)
      )
    } else {
      chi2_fisher_text <- paste0(
        fisher_chi$method,
        ",\np-value = ", signif(fisher_chi$p.value, 3)
      )
    }
    
    # titletext <- paste0(
    #   fisher_chi$method,
    #   "\n Chi-squared = ", round(fisher_chi$statistic, 2),
    #   ", p-value = ", signif(fisher_chi$p.value, 3)
    # )
    
    titletext = chi2_fisher_text
    
    
    if (nrow(counts) > 3) {
      ma <- max(1.3 * max_val_y)
      legendsize <- 0.7 * cex
    } else {
      ma <- ma <- max(1.1 * max_val_y)
      legendsize <- cex
    }
    
    barplot(
      norm_counts,
      names.arg = count_labels,
      xlim = c(-0.5, ncol(counts) + 1),
      ylim = c(0, ma),
      width = 1 / (nrow(counts) + 1),
      space = c(0, 1),
      col = col_vec_browser,
      ylab = "%",
      xlab = samplename,
      beside = TRUE,
      cex.axis = 1,
      cex.names = labelsize # size of labels of barplot
    )
    
    box()
    mtext(titletext)
    category_names <- as.character(category_names)
    legend(
      "topright",
      inset = 0.05,
      category_names,
      col = col_vec_browser,
      bty = "n",
      lwd = 2,
      cex = legendsize
    )
    
    return(fisher_chi)
  }
}

###### Visualize ANOVA ###############################
## performs ANOVA or  oneway test and corresponding post hoc tests
vis_anova <- function(samples,
                      fact,
                      conf.level = conf.level,
                      samplename = "",
                      factorname = "",
                      cex = 1) {
  if (missing(conf.level)) {
    conf.level <- 0.95
  }
  
  oldparanova <- par(no.readonly = TRUE)
  on.exit(par(oldparanova))
  
  alpha <- 1 - conf.level
  
  samples3 <- na.omit(samples)
  fact <- subset(fact, !is.na(samples))
  samples <- samples3
  n_classes <- length(unique(fact))
  # https://en.wikipedia.org/wiki/Bonferroni_correction
  number_of_pairwise_comparisons <- n_classes * (n_classes - 1) / 2
  alpha_sidak <- 1 - conf.level^(1 / number_of_pairwise_comparisons) # Sidak correction, https://en.wikipedia.org/wiki/%C5%A0id%C3%A1k_correction
  # alpha_sidak=alpha #do not apply sidak correction
  sdna <- function(x) {
    sd(x, na.rm = T)
  }
  meanna <- function(x) {
    mean(x, na.rm = T)
  }
  
  s <- tapply(samples, fact, sdna)
  m <- tapply(samples, fact, meanna)
  
  samples_per_class <- integer(n_classes)
  for (i in 1:n_classes) {
    samples_per_class[i] <- sum(fact == unique(fact)[i])
  }
  # tests
  an <- aov(samples ~ fact)
  summaryAnova <- summary(an)
  oneway <- oneway.test(samples ~ fact)
  # check for homogeneity
  bartlett_test <- bartlett.test(samples ~ fact)
  p_bart <- bartlett_test$p.value
  
  if (p_bart > 1 - conf.level) {
    p_aov <- summaryAnova[[1]][["Pr(>F)"]][1]
    F_value <- round(summaryAnova[[1]]$`F value`[1],2)
    label_aov <- "Fisher's one-way ANOVA"
    summarystat <- summaryAnova
  } else {
    p_aov <- oneway$p.value
    F_value=round(oneway$statistic,2)
    label_aov <- "Welch's heteroscedastic one-way ANOVA"
    # label_aov <-oneway$method
    summarystat <- oneway
  }
  
  
  
  
  maximum <- max(samples, na.rm = T)
  minimum <- min(samples, na.rm = T)
  
  spread <- maximum - minimum
  
  mi <- minimum - 1.2 * spread
  ma <- maximum + 1.2 * spread
  par(mfrow = c(1, 1), oma = c(0, 0, 3, 0))
  
  stripchart(
    samples ~ fact,
    vertical = TRUE,
    xlim = c(0, n_classes + 1),
    ylim = c(mi, ma),
    col = rep("grey30", n_classes),
    ylab = samplename,
    xlab = factorname,
    las = 2
  )
  
  
  
  # sd:
  for (i in 1:n_classes) {
    sn <- qt(1 - alpha_sidak / 2, samples_per_class[i] - 1) * s[[i]] / sqrt(samples_per_class[i])
    lines(
      x = c(i - 0.2, i - 0.2),
      # y = c(m[[i]] - s[[i]], m[[i]] + s[[i]]),
      y = c(m[[i]] - sn, m[[i]] + sn),
      col = colors()[131],
      lwd = 5
    )
  }
  
  
  for (i in 1:n_classes) {
    lines(
      x = c(i - 0.1, i + 0.1),
      y = c(m[[i]], m[[i]]),
      col = colors()[552],
      lwd = 3
    )
    arrows(
      i,
      m[[i]] + qt(1 - alpha / 2, samples_per_class[i] - 1) * s[[i]] / sqrt(samples_per_class[i]),
      # m[[i]] + qt(1 - alpha_sidak/2, samples_per_class[i] - 1) * s[[i]] / sqrt(samples_per_class[i]),
      i,
      m[[i]] - qt(1 - alpha / 2, samples_per_class[i] - 1) * s[[i]] / sqrt(samples_per_class[i]),
      # m[[i]] - qt(1 - alpha_sidak/2, samples_per_class[i] - 1) * s[[i]] / sqrt(samples_per_class[i]),
      angle = 90,
      code = 3,
      col = colors()[552],
      lty = 1,
      lwd = 2,
      length = 0.1
    )
  }
  
  tuk <- TukeyHSD(an, conf.level = conf.level)
  
  s <- multcompLetters(tuk[[1]][, 4], threshold = alpha)
  
  ord <- c()
  
  v <- attributes(s$Letters)$names
  f_levels <- sort(unique(fact))
  for (i in 1:n_classes) {
    ord[i] <- which(v == f_levels[i])
  }
  
  text(seq(1:n_classes + 1),
       mi,
       s$Letters[ord],
       col = colors()[81],
       lwd = 2)
  
  
  mtext(
    paste0(label_aov, "\nF = ", F_value, ", p = ", signif(p_aov, 2)),
    outer = TRUE)
  
 
  
  legend(
    "topleft",
    inset = 0.05,
    horiz = F,
    c(
      paste("mean with", conf.level * 100, "% conf. intervall "),
      paste("Sidak corrected" , round((1 - alpha_sidak) * 100, 2), "% conf. interval")
    ),
    col = c(colors()[131], colors()[552]),
    bty = "n",
    lwd = 3
  )
  
  
  
  
  my_list <-
    list(
        "summary statistics of Fisher's one-way ANOVA" = summaryAnova,
        "summary statistics of Welch's one-way ANOVA (not assuming equal variances)" = oneway,
      #"summary statistics" =summarystat,
      "post-hoc analysis of TuckeyHSD" = tuk,
      "conf.level" = conf.level
    )
  
  return(my_list)
}



###### Visualize Kruskal_Wallis ###############################
## performs Kruskal Wallis and post-hoc Wilcoxon:

vis_Kruskal_Wallis_clusters <- function(samples,
                                        fact,
                                        conf.level = conf.level,
                                        samplename = "",
                                        factorname = "",
                                        cex = 1,
                                        notch = F) {
  oldparkruskal <- par(no.readonly = TRUE)
  on.exit(par(oldparkruskal))
  
  if (missing(conf.level)) {
    conf.level <- 0.95
  }
  alpha <- 1 - conf.level
  # remove rows with NAs in samples
  samples3 <- na.omit(samples)
  fact <- subset(fact, !is.na(samples))
  samples <- samples3
  n_classes <- length(unique(fact))
  # define color scheme dependent on number of classes

  mc <- rainbow(n_classes, alpha = 1)
  # mc=ColorPalette(n_classes)
  
  s <- tapply(samples, fact, sd)
  m <- tapply(samples, fact, mean)
  
  samples_per_class <- c()
  for (i in 1:n_classes) {
    samples_per_class[i] <- sum(fact == unique(fact)[i])
  }
  
  kk <- kruskal.test(samples ~ fact)
  
  extramargin <- 0.1
  margins <- calc_min_max_of_y_axis(samples, extramargin, extramargin)
  mi <- margins[[1]]
  ma <- margins[[2]]
  
  par(mfrow = c(1, 1), oma = c(1, 0, 1, 0)) # oma: outer margin sout, west, north, east
  
  if (notch == TRUE) {
    b <- boxplot(
      samples ~ fact,
      notch = TRUE,
      col = mc,
      las = 1,
      xlim = c(0, n_classes + 1),
      ylim = c(mi, ma),
      xlab = factorname,
      ylab = samplename,
      # changes group names size
      cex.lab = cex,
      cex.axis = 0.8 * cex,
      cex.main = cex,
      cex.sub = cex,
      boxwex = 0.5
    )
  } else {
    b <- boxplot(
      samples ~ fact,
      notch = FALSE,
      col = mc,
      las = 1,
      xlim = c(0, n_classes + 1),
      ylim = c(mi, ma),
      xlab = factorname,
      ylab = samplename,
      boxwex = 0.5
    )
  }
  
  
  stripchart(
    samples ~ fact,
    vertical = TRUE,
    # method="jitter",
    col = rep("grey50", n_classes),
    # ylab = ylab,
    # xlab = xlab,
    las = 1,
    # horizontal legend,
    add = TRUE
  )
  
  mtext(c("N = ", b$n), at = c(0.7, seq(1, n_classes)), las = 1) # nmber of cases in each group
  tuk <- sig_diffs_nongauss(samples, fact, conf.level = conf.level)
  
  s <- multcompLetters(tuk[[1]][, 4], threshold = alpha)
  
  ord <- c()
  
  v <- attributes(s$Letters)$names
  f_levels <- sort(unique(fact))
  for (i in 1:n_classes) {
    ord[i] <- which(v == f_levels[i])
  }
  (ma)
  text(
    seq(1:n_classes + 1),
    mi,
    s$Letters[ord],
    col = "darkgreen",
    cex = cex,
    lwd = 2
  )
  
  
  kk_value <- round(as.numeric(kk$statistic),2)
  
  title(paste0(kk$method,
               "\n H = ",kk_value,
              ", p = ", signif(kk$p.value, digits = 3)))
  my_list <-
    list("Kruskal Wallis rank sum test" = kk,
         "post-hoc by pairwise Wilcoxon rank sum test " = tuk)
  return(my_list)
}


##### Visualize Regression und trumpet curves ###############################
vis_regr_trumpets <- function(x, y, conf.level) {
  if (missing(conf.level)) {
    conf.level <- 0.95
  }
  oldparreg <- par(no.readonly = TRUE)
  on.exit(par(oldparreg))
  reg <- lm(y ~ x)
  summary(reg)
  
  ## error bands:
  y_conf_low <- conf_band(x, reg, conf.level, -1)
  y_conf_up <- conf_band(x, reg, conf.level, 1)
  
  ma <- max(y, reg$fitted)
  mi <- min(y, reg$fitted)
  spread <- ma - mi
  
  lower <- 0.1
  upper <- 0.4
  margins <- calc_min_max_of_y_axis(y, lower, upper)
  mi <- margins[[1]]
  ma <- margins[[2]]
  
  
  par(oma = c(0, 0, 5, 0))
  plot(x, y, ylim = c(mi, ma))
  points(x,
         reg$fitted,
         type = "l",
         col = 2,
         lwd = 2)
  
  points(
    x,
    y_conf_low,
    type = "l",
    lwd = 2,
    lty = 2,
    col = colors()[84]
  )
  points(
    x,
    y_conf_up,
    type = "l",
    lwd = 2,
    lty = 2,
    col = colors()[84]
  )
  legend(
    "bottomright",
    c(
      "regr. line",
      paste("confidence band for alpha=", 1 - conf.level)
    ),
    lwd = 2,
    col = c(2, colors()[84], colors()[85]),
    lty = c(1, 2, 3),
    bty = "n"
  )
  s <- summary(reg)
  mtext(
    paste(
      "Regression: ax + b. confidence band for alpha = ",
      1 - conf.level,
      "\n \n"
    ),
    outer = TRUE,
    cex = 1.5
  )
  mtext(
    paste(
      "\n \n a = ",
      signif(reg$coefficients[2], 2),
      ", p = ",
      signif(s$coefficients[2, 4], 2),
      "\n b = ",
      signif(reg$coefficients[1], 2),
      ", p = ",
      signif(s$coefficients[1, 4], 2),
      "\n R^2 = ",
      signif(summary(reg)$r.squared, 4)
    ),
    outer = TRUE
  )
  
  par(mfrow = c(1, 2), oma = c(0, 0, 3, 0))
  plot(
    reg$fitted,
    residuals(reg),
    main = "Residuals vs. Fitted",
    xlab = "Fitted Values",
    ylab = "Residuals"
  )
  abline(h = 0, col = 1, lwd = 2)
  
  qqnorm(residuals(reg), ylab = "Sample Quantiles of  Residuals")
  qqline(residuals(reg), col = "red", lwd = 2)
  
  KS <- ad.test(residuals(reg))
  p_KS <- signif(KS$p.value, 2)
  SH <- shapiro.test(residuals(reg))
  p_SH <- signif(SH$p.value, 2)
  
  mtext(
    paste(
      "Residual Analysis\n Shapiro-Wilk: p = ",
      p_SH,
      "\n  Anderson-Darling: p = ",
      p_KS
    ),
    outer = TRUE
  )
}

###### Visualize Residuals ###############################
vis_resid <- function(resid, fitted) {
  oldparresid <- par(no.readonly = TRUE)
  on.exit(par(oldparresid))
  
  par(mfrow = c(1, 2), oma = c(0, 0, 3, 0))
  plot(fitted, resid, main = "Residuals vs. Fitted")
  abline(h = 0, col = 1, lwd = 2)
  
  qqnorm(resid)
  qqline(resid, col = "red", lwd = 2)
  
  KS <- ad.test(resid)
  p_KS <- signif(KS$p.value, 2)
  SH <- shapiro.test(resid)
  p_SH <- signif(SH$p.value, 2)
  
  mtext(
    paste(
      "Residual Analysis\n Shapiro-Wilk: p = ",
      p_SH,
      "\n Anderson-Darling: p = ",
      p_KS
    ),
    outer = TRUE
  )
}


###### Visualize Regression ###############################

# only normality assumptions of standardised residuals
vis_normality_assumptions <- function(y, x, conf.level = 0.95) {
  oldparreg <- par(no.readonly = TRUE)
  on.exit(par(oldparreg))
  alpha <- 1 - conf.level
  # P = alpha
  
  # remove all NAs from both vectors
  xna <- x[!is.na(y) & !is.na(x)]
  yna <- y[!is.na(y) & !is.na(x)]
  
  x <- xna
  y <- yna
  
  ord <- order(x)
  x <- sort(x)
  y <- y[ord]
  
  reg <- lm(y ~ x)
  resreg <- summary(reg)
  
  
  par(mfrow = c(1, 2), oma = c(0, 0, 4, 0))
  plot(
    reg$fitted,
    rstandard(reg),
    main = "std. Residuals vs. Fitted",
    xlab = "Fitted Values",
    ylab = "standardised Residuals"
  )
  abline(h = 0, col = 1, lwd = 2)
  
  qqnorm(rstandard(reg), ylab = "Sample Quantiles of Std. Residuals")
  qqline(rstandard(reg), col = "red", lwd = 2)
  
  KS <- ad.test(rstandard(lm(y ~ x)))
  p_KS <- signif(KS$p.value, 2)
  SH <- shapiro.test(rstandard(lm(y ~ x)))
  p_SH <- signif(SH$p.value, 2)
  if (p_SH < alpha) {
    mtext(
      paste(
        "Residual Analysis\n Shapiro-Wilk: p = ",
        p_SH,
        ", Anderson-Darling: p = ",
        p_KS,
        "\n Requirement of normally distributed residuals not met "
      ),
      outer = TRUE
    )
  } else {
    mtext(
      paste(
        "Residual Analysis\n Shapiro-Wilk: p = ",
        p_SH,
        ", Anderson-Darling: p = ",
        p_KS
      ),
      outer = TRUE
    )
  }
  
  my_list <- list(
    "summary_regression" = resreg,
    "shapiro_test_residuals" = SH,
    "ad_test_residuals" = KS
  )
  
  
  return(my_list)
}




vis_regression <- function(y,
                           x,
                           conf.level = conf.level,
                           name_of_factor = character(),
                           name_of_sample = character()) {
  if (missing(conf.level)) {
    conf.level <- 0.95
  }
  oldparregr <- par(no.readonly = TRUE)
  on.exit(par(oldparregr))
  
  alpha <- 1 - conf.level
  
  # remove all NAs from both vectors
  xna <- x[!is.na(y) & !is.na(x)]
  yna <- y[!is.na(y) & !is.na(x)]
  
  x <- xna
  y <- yna
  
  ord <- order(x)
  x <- sort(x)
  y <- y[ord]
  ylim <- 1.1 * max(y, na.rm <- T)
  
  
  
  # di
  reg <- lm(y ~ x)
  resreg <- summary(reg)
  
  
  ## error bands:
  
  # #old cold-depreciated, replaced by predict function------
  # y_conf_lowa = conf_band(x, reg, conf.level, -1)
  # y_conf_upa = conf_band(x, reg, conf.level, 1)
  # y_progn_lowa = progn_band(x, reg, conf.level, -1)
  # y_progn_upa = progn_band(x, reg, conf.level, 1)
  # #-------------------
  
  conf.int_prediction <- predict(reg, interval = "confidence", level = conf.level) # confidence band
  pred.int_prediction <- suppressWarnings(predict(reg, interval = "prediction", level = conf.level)) # prediction band
  
  y_conf_low <- conf.int_prediction[, 2]
  y_conf_up <- conf.int_prediction[, 3]
  y_progn_low <- pred.int_prediction[, 2]
  y_progn_up <- pred.int_prediction[, 3]
  
  predicted_value <- conf.int_prediction[, 1]
  
  error_bands <- cbind(predicted_value,
                       y_conf_low,
                       y_conf_up,
                       y_progn_low,
                       y_progn_up)
  
  ma <- max(y, reg$fitted, y_progn_up, na.rm <- T)
  mi <- min(y, reg$fitted, y_progn_low, na.rm <- T)
  
  spread <- ma - mi
  
  par(mfrow = c(1, 1), oma = c(0, 0, 5, 0))
  plot(
    x,
    y,
    ylim = c(mi - 0.1 * spread, ma + 0.4 * spread),
    xlab = name_of_factor,
    ylab = name_of_sample
  )
  
  points(
    x,
    reg$fitted,
    type = "l",
    col = colorscheme(2)[1],
    # dark green
    lwd = 2
  )
  # plot confidence band, lower boundary
  points(
    x,
    y_conf_low,
    type = "l",
    lwd = 2,
    lty = 2,
    col = colorscheme(1)[1]
  )
  # plot confidence band, upper boundary
  points(
    x,
    y_conf_up,
    type = "l",
    lwd = 2,
    lty = 2,
    col = colorscheme(1)[1]
  )
  # plot prognosis band, lower boundary
  points(
    x,
    y_progn_low,
    type = "l",
    lwd = 2,
    lty = 3,
    col = colorscheme(1)[2]
  )
  # plot prognosis band, upper boundary
  points(
    x,
    y_progn_up,
    type = "l",
    lwd = 2,
    lty = 3,
    col = colorscheme(1)[2]
  )
  
  legend(
    "topleft",
    horiz = FALSE,
    text.width = 0.75,
    c("regr. line", "confidence band", "prognosis band"),
    lwd = 2,
    # line width
    col = c(colorscheme(2)[1], colorscheme(1)[1], colorscheme(1)[2]),
    lty = c(1, 2, 3),
    # line types of legend
    bty = "n",
    # no box around legend
    cex = 0.8 # reduces the legend size
  )
  
  
  s <- summary(reg)
  
  conf_intervall_regression <- confint(reg, level = conf.level) # conf.int confidence interval of slope and intercept
  
  AD <- ad.test(rstandard(lm(y ~ x)))
  
  SH <- shapiro.test(rstandard(lm(y ~ x)))
  
  mtext(
    paste(
      "y = a*x +b, confidence level = ",
      conf.level,
      ", adjusted R2 = ",
      signif(s$adj.r.squared, 2),
      " \n slope a = ",
      signif(reg$coefficients[2], 2),
      ", conf. interval [",
      signif(conf_intervall_regression[2, 1], 2),
      ",",
      signif(conf_intervall_regression[2, 2], 2),
      "]",
      ", p = ",
      signif(s$coefficients[2, 4], 2),
      "\n intercept b = ",
      signif(reg$coefficients[1], 2),
      ", conf. interval [",
      signif(conf_intervall_regression[1, 1], 2),
      ",",
      signif(conf_intervall_regression[1, 2], 2),
      "]",
      ", p = ",
      signif(s$coefficients[1, 4], 2)
    ),
    outer = TRUE
  )
  
  my_list <- list(
    "independent variable x" = name_of_factor,
    "dependent variable y" = name_of_sample,
    "summary_regression" = resreg,
    "shapiro_test_residuals" = SH,
    "anderson_darling_test_residuals" = AD,
    "error_bands" = error_bands
  )
  
  return(my_list)
}


# Mosaic plots-----
vis_mosaic <- function(samples,
                       fact,
                       name_of_sample = character(),
                       name_of_factor = character(),
                       minperc = 0.05,
                       numbers = TRUE) {
  oldparmosaic <- par(no.readonly = TRUE)
  oldparmosaic$new <- FALSE
  on.exit(par(oldparmosaic))
  
  if (missing(minperc)) {
    # minperc is the minimum percehntage a column has to contribute to be displayed
    minperc <- 0.05
  }
  if (missing(numbers)) {
    # numbers are shown in rectangle of category
    numbers <- TRUE
  }
  
  counts <- makeTable(samples, fact, name_of_sample, name_of_factor)
  check_assumptions <- check_assumptions_count_data(samples, fact)
  if (check_assumptions == FALSE) {
    my_list <- counts
    return(my_list)
  } else {
    ## Mosaic plot
    ## The height  of the box is the same for all boxes in the same row and
    # is equal to the total count in that row.
    #
    # The width of the box is the proportion of individuals in the row which fall into that cell.
    # #Full mosaic plot with all data only if unique number of samples and fact below threshold
    maxfactors <- max(length(unique(samples)), length(unique(fact)))
    threshold <- 6
    
    if (length(unique(samples)) < threshold &
        length(unique(fact)) < threshold) {
      res <- mosaic(
        counts,
        shade = TRUE,
        legend = TRUE,
        # shows pearsons residual
        pop = F
        # ,main = titletext
      )
      
      tab <-
        as.table(ifelse(counts < 0.005 * sum(counts), NA, counts))
      # puts numbers on count
      if (numbers == TRUE) {
        labeling_cells(text = tab, margin = 0)(counts)
      }
    } else {
      #
      ## Elimintate rows and columns distributing less than minperc total number of counts
      rowSum <- rowSums(counts)
      colSum <- colSums(counts)
      total <- sum(counts)
      
      
      countscolumn_row_reduced <- as.table(counts[which(rowSum > minperc * total), which(colSum > minperc * total)])
      
      # check dimensions after reduction: must be a contingency table
      test <- dim(as.table(countscolumn_row_reduced))
      if (is.na(test[2])) {
        countsreduced <- counts
      } else {
        countsreduced <- countscolumn_row_reduced
      }
      res <- mosaic(
        countsreduced,
        shade = TRUE,
        legend = TRUE,
        cex.axis = 50 / maxfactors,
        labeling_args = list(gp_labels = (gpar(
          fontsize = 70 / maxfactors
        ))),
        # main = titletext,
        pop = F
      )
      if (numbers == TRUE) {
        labeling_cells(text = countsreduced, margin = 0)(countsreduced)
      }
    }
    my_list <-
      list("mosaic_stats" = res)
    
    return(my_list)
  }
}




# Helper functions--------------------------------------

# Check for type of samples and fact
type_sample_fact <- function(samples, fact) {
  typesample <- class(samples)
  typefactor <- class(fact)
  listsf <- list("typesample" = typesample, "typefactor" = typefactor)
  return(listsf)
}

# helper function odds ratio
# calculation of odds ratio
odds_ratio <- function(a, b, c, d, alpha, zerocorrect) {
  attr(odds_ratio, "help") <-
    "odds_ratio calculates odds ratio OR=(a/b)/(c/d) and corresponding upper and lower confidence intervalls\n INPUT: a = group 1 positive, c = group 2 positive, b=group 1 non positive, d = group 2 non positive, 1-alpha: confidence level, default alpha=0.05"
  
  # "odds_ratio calculates odds ratio OR=(a/b)/(c/d) and corresponding upper and lower confidence intervalls\n
  # INPUT: a=number of positives in  group 1, c=group 2 positive, b=group 1 non positive, d =group 2 non positive,default alpha=0.05, OR=(a/b)/(c/d)"\n
  # a,b,c,d can be vectors, elementwise calculation
  #
  
  
  if (missing(alpha)) {
    alpha <- 0.05
  }
  if (missing(zerocorrect)) {
    zerocorrect <- TRUE
  }
  # odds ratio:=OR=a/b/(c/d)
  
  # eliminate columns with zeros
  # a=c=0 or b=d 0: no positive or no negative cases in both groups
  # Higgins and Green 2011:
  
  if (zerocorrect == TRUE) {
    # eliminate columns with zeros, if
    # a=c=0 or b=d=0: no positive or no control cases in BOTH groups
    # Higgins and Green 2011:
    doublezero <- which(a == 0 &
                          c == 0 | b == 0 & d == 0, arr.ind = T)
    a[doublezero] <- NaN
    b[doublezero] <- NaN
    c[doublezero] <- NaN
    d[doublezero] <- NaN
    # Where zeros cause problems with computation of effects or standard errors, 0.5 is added to all cells (a, b, c, d)
    singlezero <- which(a == 0 |
                          b == 0 | c == 0 | d == 0, arr.ind = T)
    a[singlezero] <- a[singlezero] + 0.5
    b[singlezero] <- b[singlezero] + 0.5
    c[singlezero] <- c[singlezero] + 0.5
    d[singlezero] <- d[singlezero] + 0.5
  }
  
  oddA <- a / b
  oddB <- c / d
  
  OR <- oddA / oddB
  
  # confidence intervall
  # SE of ln(OR)
  SE <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
  alpha <- 0.05
  zalph <- qnorm(1 - alpha / 2)
  
  log_low <- log(OR) - zalph * SE
  log_up <- log(OR) + zalph * SE
  
  lowconf <- exp(log_low) # lower confidence
  upconf <- exp(log_up)
  
  output <- rbind(OR, lowconf, upconf, SE)
  my_list <- ("odds_ratio_statistics" <- output)
  return(my_list)
}

# create sorted table
makeTable <- function(samples, fact, samplename, factorname) {
  counts <- data.frame(fact, samples)
  colnames(counts) <- c(factorname, samplename)
  counts2 <- table(counts)
  # sort by column sums
  counts3 <- counts2[, order(colSums(counts2), decreasing = T)]
  # sort by row sums
  counts4 <- counts3[order(rowSums(counts3), decreasing = T), ]
  # remove columnns with all entries zero
  counts4 <- counts4[, colSums(counts4 != 0) > 0]
  
  return(counts4)
}

fisher_chi <- function(counts) {
  # if Cochran requirements for chi2 not given: fisher test is performed
  # if more than 20% of cells have EXPECTED count smaller 5 or one cell has expected count smaller than 1
  #
  
  suppressWarnings(chisq <- chisq.test(counts))
  expected_counts <- chisq$expected
  
  if (any(expected_counts < 1) # at least one cell with expectation value smaller 1
      |
      sum(expected_counts < 5) / length(expected_counts) > 0.2 # more than 20% of cells have expected count smaller 5
      &
      # Fisher Tests breaks down for too large tables
      dim(counts)[2] < 7) {
    # fisher.test
    testFisherChi <- fisher.test(
      counts,
      workspace = 1e9,
      simulate.p.value = T,
      hybrid = F,
      B = 1e5
    )
  } else {
    testFisherChi <- chisq
  }
  
  return(testFisherChi)
}

side_of_nh <- function(alternative) {
  if (alternative == "less") {
    compare <- c(">=")
  } else if (alternative == "greater") {
    compare <- c("<=")
  } else {
    compare <- c("equals")
  }
  return(compare)
}

create_two_samples_vector <- function(samples, fact) {
  # Creates column vector built out of two samples
  # samples all in one column and sorted
  levels <- unique(sort(fact))
  # two levels
  if (length(levels) > 2) {
    return(warning(
      "warning: create_two_samples_vector: only two level input allowed"
    ))
  } else {
    samples1 <- samples[fact == levels[1]]
    samples1 <- samples1[!is.na(samples1)]
    if (length(samples1) == 0) {
      return(warning("each group needs at least one entry"))
    } else {
      samples2 <- samples[fact == levels[2]]
      samples2 <- samples2[!is.na(samples2)]
      if (length(samples2) == 0) {
        return(warning("each group needs at least one entry"))
      } else {
        x <- c(samples1, samples2)
        my_list <- list(
          "sample1" = samples1,
          "sample2" = samples2,
          "sample1and2" = x
        )
        return(my_list)
      }
    }
  }
}

calc_min_max_of_y_axis <- function(samples,
                                   lowerExtramargin,
                                   upperExtramargin) {
  maximum <- max(samples, na.rm = T)
  minimum <- min(samples, na.rm = T)
  spread <- maximum - minimum
  min_y_axis <- minimum - lowerExtramargin * spread
  max_y_axis <- maximum + upperExtramargin * spread
  return(list(min_y_axis, max_y_axis))
}


check_assumptions_shapiro <- function(x) {
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  rng <- x[n] - x[1L] # 1L is integer
  checkSize <- !(is.na(n) || n < 3L || n > 5000L) # FALSE or TRUE
  if (checkSize == FALSE) {
    warning("sample size must be between 3 and 5000")
    return(FALSE)
  }
  if (rng == 0) {
    warning("all 'x' values are identical")
    return(FALSE)
  }
  
  
  
  return(TRUE)
}

check_assumption_shapiro_size_range_two_samples <- function(x1, x2) {
  boolean1 <- check_assumptions_shapiro(x1)
  boolean2 <- check_assumptions_shapiro(x2)
  if (boolean1 == TRUE & boolean2 == TRUE) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


check_assumptions_count_data <- function(samples, fact) {
  counts <- table(samples, fact)
  sr <- rowSums(counts)
  sc <- colSums(counts)
  counts <- counts[sr > 0, sc > 0, drop = FALSE]
  nr <- as.integer(nrow(counts))
  nc <- as.integer(ncol(counts))
  if (is.null(dim(counts))) {
    warning("no entries in count table ")
    return(FALSE)
  } else if (is.na(nr) || is.na(nc) || is.na(nr * nc)) {
    warning("invalid nrow  or ncol in count data ", domain = NA)
    return(FALSE)
  } else if (nr <= 1L) {
    warning("need 2 or more non-zero row marginals")
    return(FALSE)
  } else if (nc <= 1L) {
    warning("need 2 or more non-zero column marginals")
    return(FALSE)
  } else {
    return(TRUE)
  }
}

sig_diffs_nongauss <- function(samples, fact, conf.level = conf.level) {
  # function to produce a table similar to that produced for TukeyHSD,
  # but for non-normally distributed data
  # calculate p values for each data classification based on pairwise.wilcox.test
  
  if (missing(conf.level)) {
    conf.level <- 0.95
  }
  
  ufactor <- levels(fact)
  pwt <- pairwise.wilcox.test(samples, fact, conf.level = conf.level)
  factormeans <- matrix(0, length(ufactor), 1)
  for (ii in 1:length(ufactor)) {
    pos <- which(fact == ufactor[ii])
    
    factormeans[ii] <- mean(samples[pos])
  }
  
  # make a matrix with a row for every possible combination of
  # 2 data classifications and populate it with the calculated
  # p values
  
  xcomb <- combn(length(ufactor), 2)
  tukeylike <- matrix(0, ncol(xcomb), 4)
  colnames(tukeylike) <- c("diff", "lwr", "upr", "p adj")
  tukeynames <- vector("list", ncol(xcomb))
  for (ii in 1:ncol(xcomb)) {
    tukeynames[ii] <-
      paste(ufactor[xcomb[2, ii]], "-", ufactor[xcomb[1, ii]], sep = "")
    
    p_value <- pwt$p.value[xcomb[2, ii] - 1, xcomb[1, ii]]
    
    if (is.na(p_value)) {
      p_value <- 1
    }
    
    tukeylike[ii, 4] <- p_value
    tukeylike[ii, 1] <- NA
    tukeylike[ii, 2] <- NA
    tukeylike[ii, 3] <- NA
  }
  rownames(tukeylike) <- tukeynames
  
  # re-format the table slightly so it is the same as that produced
  # by TukeyHSD and output
  
  tukeylike2 <- list(tukeylike)
  
  return(tukeylike2)
}


conf_band <- function(x, reg, conf.level = conf.level, up) {
  # reg: result of linear regression lm
  # up: fact plus or minus
  if (missing(conf.level)) {
    conf.level <- 0.95
  }
  
  if (missing(up)) {
    up <- 1
  }
  alpha <- 1 - conf.level
  a <- reg$coefficients[2] # slope
  b <- reg$coefficients[1] # constant
  md <- x - mean(x)
  
  result <- x # initialization
  
  # formula standard error of the regression line at point x:
  # https://stats.stackexchange.com/questions/101318/understanding-shape-and-calculation-of-confidence-bands-in-linear-regression
  # See also statistics script page 124: konfidenzint4erval
  
  # http://www.sthda.com/english/articles/40-regression-analysis/166-predict-in-r-model-predictions-and-confidence-intervals/
  for (i in 1:length(x)) {
    result[i] <- a * x[i] + b +
      up * qt(1 - alpha / 2, length(x) - 2) *
      # Standard error of the estimate
      sqrt(sum(reg$resid * reg$resid) / (length(x) - 2)) *
      #
      sqrt(1 / (length(x)) + md[i]^2 / sum(md * md))
  }
  return(result)
}

progn_band <- function(x, reg, conf.level, up) {
  if (missing(conf.level)) {
    conf.level <- 0.95
  }
  alpha <- 1 - conf.level
  if (missing(up)) {
    up <- 1
  }
  a <- reg$coefficients[2]
  b <- reg$coefficients[1]
  md <- x - mean(x)
  
  result <- x
  
  for (i in 1:length(x)) {
    result[i] <- a * x[i] + b +
      up * qt(1 - alpha / 2, length(x) - 2) *
      # Standard error of the estimate
      sqrt(sum(reg$resid * reg$resid) / (length(x) - 2)) *
      #
      sqrt(1 + 1 / (length(x)) + md[i]^2 / sum(md * md))
  }
  return(result)
}
calculate_comparepvalue <- function(p_value, conf.level) {
  if (p_value < 1 - conf.level) {
    comparepvalue <- "<"
  } else {
    comparepvalue <- ">"
  }
  return(comparepvalue)
}

# Check for normality with Shapiro-Wilk test without visualization----
test_norm <- function(x) {
  # Remove NA from x
  x <- x[!is.na(x)]
  #  KS = ks.test(x, pnorm, mean(x), sd(x))
  shapiro_wilk_test <- shapiro.test(x)
  # my_list = list("Kolmogorov-Smirnoff" = KS, "Shapiro" =SH)
  return(shapiro_wilk_test)
}

# Check length of distributions for t-test----
check_assumption_sample_size_t_test <- function(x1, x2, minimum_size) {
  # x1 sample 1
  # x2 sample 2
  # minimum_size:return TRUE if length> minimum_size
  if (length(x1) > minimum_size & length(x2) > minimum_size) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Define color scheme-----

#' \code{colorscheme(x)} selects color scheme of graphical output. Function parameter NULL lists all available color schemes, 1 a color tuple of green and blue
#' 2 a color tuple of dark green and turquoi, 3 a colorplaette as defined by RcolorBrewer

#'
#' @param colorcode selects color scheme. parameters NULL: list of all available color schemes, 1: colortuple, 2, colortuple2, 3, ColorPalette
#' @return selected color scheme, colors are given with their Hex Code #RRGGBB names
#' @noRd

colorscheme <- function(colorcode = NULL) {
  browserLightGreen <- "#B8E0B8" # matched part group0
  browserLightBlue <- "#B3D1EF" # matched part group1
  browserLightTurquois <- "#B3E1EF" # light turquois
  browserDarkGreen <- "#5CB85C" # dark green
  colortuple <- c(browserLightGreen, browserLightBlue)
  colortuple2 <- c(browserDarkGreen, browserLightTurquois)
  # from package RColorBrewer Set 3
  ColorPalette <- c(
    "#8DD3C7",
    "#FFFFB3",
    "#BEBADA",
    "#FB8072",
    "#80B1D3",
    "#FDB462",
    "#B3DE69",
    "#FCCDE5",
    "#D9D9D9",
    "#BC80BD",
    "#CCEBC5",
    "#FFED6F"
  )
  
  
  my_list <- list(
    "colortuple" = colortuple,
    "colortuple2" = colortuple2,
    "ColorPalette" = ColorPalette
  )
  
  if (is.null(colorcode)) {
    return(my_list)
  } else if (colorcode == 1) {
    return(colortuple)
  } else if (colorcode == 2) {
    return(colortuple2)
  } else if (colorcode == 3) {
    return(ColorPalette)
  } else {
    message("Choose valid parameter: NULL, 1,2 or 3")
  }
}

resetPar <- function() {
  dev.new
  while (!is.null(dev.list()))
    dev.off() # restores to default values
  oldpar <- par(no.readonly = TRUE)
  return(oldpar)
}
