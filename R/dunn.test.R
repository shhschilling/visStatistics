#' Dunn's Post-Hoc Test for Kruskal-Wallis
#'
#' Performs pairwise comparisons on the rank sums of a single, combined ranking
#' of all groups, as proposed by Dunn (1964). It is the post-hoc procedure
#' matched to \code{kruskal.test()}: both rank the observations globally, so a
#' pairwise decision here concerns the same quantity the omnibus test rejected.
#'
#' @param samples numeric vector; the dependent variable.
#' @param groups factor or vector; the grouping variable.
#' @param conf.level numeric; confidence level (default: 0.95). Used only for
#'   the \code{significant} column; the p-values do not depend on it.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{group1}{First group in comparison}
#'   \item{group2}{Second group in comparison}
#'   \item{mean_rank_diff}{Difference in mean ranks (group1 - group2)}
#'   \item{se}{Standard error of the difference in mean ranks}
#'   \item{z}{Standard normal test statistic}
#'   \item{p_value}{Unadjusted two-sided p-value}
#'   \item{p_adj}{Holm-adjusted p-value for multiple comparisons}
#'   \item{significant}{Logical; TRUE if p_adj < (1 - conf.level)}
#' }
#'
#' @details
#' All \eqn{k} samples are combined and ranked from smallest to largest, ties
#' receiving the average rank. Writing \eqn{\bar R_i} for the mean rank of group
#' \eqn{i} and \eqn{N} for the total number of observations, the statistic for
#' groups \eqn{i} and \eqn{j} is
#' \deqn{z_{ij} = (\bar R_i - \bar R_j) / \sigma_{ij},}
#' with
#' \deqn{\sigma_{ij}^2 = \left[\frac{N(N+1)}{12} -
#'   \frac{\sum_{s=1}^{r}(t_s^3 - t_s)}{12(N-1)}\right]
#'   \left(\frac{1}{n_i} + \frac{1}{n_j}\right),}
#' where the \eqn{r} groups of tied scores contain \eqn{t_s} observations each;
#' the subtracted term is zero without ties. This is Eq. (3) of Dunn (1964).
#' The function returns two-sided p-values adjusted by Holm's step-down procedure
#'  over all \eqn{p = k(k-1)/2} pairwise comparisons.
#' Note that all pairwise comparisons are performed, so \eqn{p} is not chosen in
#' advance as Dunn's formulation assumes.
#'
#' @references
#' Dunn, O. J. (1964). Multiple Comparisons Using Rank Sums.
#' \emph{Technometrics}, 6(3), 241-252. doi:10.2307/1266041.
#'
#' @examples
#' # Convert dose to factor
#' ToothGrowth$dose <- as.factor(ToothGrowth$dose)
#'
#' # Perform Dunn's test
#' result <- dunn.test(ToothGrowth$len, ToothGrowth$dose)
#' print(result)
#'
#' @export
dunn.test <- function(samples, groups, conf.level = 0.95) {
  # Input validation
  if (!is.numeric(samples)) {
    stop("samples must be numeric")
  }

  if (length(samples) != length(groups)) {
    stop("samples and groups must have the same length")
  }

  # Clean data
  complete_cases <- complete.cases(samples, groups)
  samples <- samples[complete_cases]
  groups <- droplevels(as.factor(groups[complete_cases]))

  group_levels <- levels(groups)
  k <- length(group_levels)

  if (k < 2) {
    stop("At least 2 groups required")
  }

  # Single combined ranking of all groups, mid-ranks for ties. This is what
  # makes the test the counterpart of kruskal.test(): the same ranking.
  ranks <- rank(samples)
  n_total <- length(samples)
  n <- tapply(ranks, groups, length)
  mean_ranks <- tapply(ranks, groups, mean)

  # Tie correction, Dunn (1964) Eq. (3). Zero when all values are distinct.
  tie_sizes <- table(samples)
  tie_sizes <- tie_sizes[tie_sizes > 1]
  tie_term <- if (length(tie_sizes) > 0) {
    sum(tie_sizes^3 - tie_sizes) / (12 * (n_total - 1))
  } else {
    0
  }
  sigma_sq <- n_total * (n_total + 1) / 12 - tie_term

  comparisons <- combn(k, 2)
  n_comparisons <- ncol(comparisons)

  results <- data.frame(
    group1 = character(n_comparisons),
    group2 = character(n_comparisons),
    mean_rank_diff = numeric(n_comparisons),
    se = numeric(n_comparisons),
    z = numeric(n_comparisons),
    p_value = numeric(n_comparisons),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(n_comparisons)) {
    g1 <- group_levels[comparisons[1, i]]
    g2 <- group_levels[comparisons[2, i]]

    mean_rank_diff <- mean_ranks[[g1]] - mean_ranks[[g2]]
    se <- sqrt(sigma_sq * (1 / n[[g1]] + 1 / n[[g2]]))
    z_stat <- mean_rank_diff / se

    results[i, ] <- list(
      group1 = g1,
      group2 = g2,
      mean_rank_diff = mean_rank_diff,
      se = se,
      z = z_stat,
      p_value = 2 * pnorm(abs(z_stat), lower.tail = FALSE)
    )
  }

  results$p_adj <- p.adjust(results$p_value, method = "holm")
  results$significant <- results$p_adj < (1 - conf.level)

  class(results) <- c("dunn.test", "data.frame")

  return(results)
}

#' @exportS3Method
print.dunn.test <- function(x, digits = 4, ...) {
  cat("\nDunn's Post-Hoc Test (Holm-adjusted)\n")
  cat("Global ranking of all groups; matched to kruskal.test()\n\n")

  out <- as.data.frame(x)
  num_cols <- vapply(out, is.numeric, logical(1))
  out[num_cols] <- lapply(out[num_cols], round, digits = digits)

  print(out, row.names = FALSE, ...)
  invisible(x)
}
