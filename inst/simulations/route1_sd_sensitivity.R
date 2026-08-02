## Does the normality gate respond to the SD vector itself, or to the residual
## excess kurtosis that mixing several scales induces?
##
## For each cell this records both quantities: the share of replications the
## Shapiro-Wilk gate sends to the rank branch, and the mean excess kurtosis of
## the standardised residuals it is applied to. Input is exactly normal in
## every cell, so any residual non-normality comes from the scale mixture alone.
##
## Not part of the published grid: this is a sensitivity check on the choice of
## the SD vector, run at B = 10000, where the Monte Carlo error of a routing
## share near 50 % is 0.5 percentage points.
##
## Usage: Rscript --vanilla route1_sd_sensitivity.R [B] [cores]

RNGkind("L'Ecuyer-CMRG")
set.seed(20260801)
.rng_stream <- .Random.seed

cell_seeds <- function(n) {
  seeds <- vector("list", n)
  for (i in seq_len(n)) {
    seeds[[i]] <- .rng_stream
    .rng_stream <<- parallel::nextRNGStream(.rng_stream)
  }
  seeds
}

args <- commandArgs(trailingOnly = TRUE)
NREP <- if (length(args) >= 1) as.integer(args[1]) else 10000L
NCORES <- if (length(args) >= 2) {
  as.integer(args[2])
} else {
  max(1L, min(8L, parallel::detectCores(logical = FALSE) - 1L))
}
ALPHA <- 0.05

## SD vectors, from homoscedastic to stronger than the published grid, plus the
## Delacre construction in which a single group differs from k-1 equal ones.
SD_VECTORS <- list(
  "equal 1.0"                 = c(1.0, 1.0, 1.0, 1.0),
  "mild 1:1.6"                = c(1.0, 1.2, 1.4, 1.6),
  "published 1:2.2"           = c(1.0, 1.3, 1.7, 2.2),
  "strong 1:3.4"              = c(1.0, 1.6, 2.4, 3.4),
  "one group 1:2 (Delacre)"   = c(1.0, 1.0, 1.0, 2.0),
  "one group 1:4 (Delacre)"   = c(1.0, 1.0, 1.0, 4.0)
)

SIZE_DESIGNS <- list(
  "balanced"                     = c(1, 1, 1, 1),
  "unbalanced, larger n larger SD" = c(0.5, 0.8, 1.2, 1.5),
  "unbalanced, larger n smaller SD" = c(1.5, 1.2, 0.8, 0.5)
)

MEAN_NS <- c(10L, 20L, 50L, 100L)

excess_kurtosis <- function(x) {
  x <- x - mean(x)
  n <- length(x)
  m2 <- sum(x^2) / n
  m4 <- sum(x^4) / n
  m4 / m2^2 - 3
}

standardised_residuals <- function(y, g) {
  model <- stats::aov(y ~ g)
  rs <- suppressWarnings(stats::rstandard(model))
  if (any(!is.finite(rs))) {
    rs <- stats::residuals(model) / max(stats::sigma(model), 1e-8)
  }
  rs
}

one_cell <- function(n_vec, sd_vec) {
  seeds <- cell_seeds(NREP)
  g <- factor(rep(seq_along(n_vec), times = n_vec))
  out <- parallel::mclapply(seq_len(NREP), function(i) {
    assign(".Random.seed", seeds[[i]], envir = globalenv())
    y <- unlist(lapply(seq_along(n_vec), function(j) stats::rnorm(n_vec[j], 0, sd_vec[j])))
    rs <- standardised_residuals(y, g)
    c(
      routed_rank = as.numeric(stats::shapiro.test(rs)$p.value < ALPHA),
      resid_kurt = excess_kurtosis(rs)
    )
  }, mc.cores = NCORES)
  m <- do.call(rbind, out)
  share <- mean(m[, "routed_rank"])
  c(
    route_rank_probability = share,
    route_mc_se = sqrt(share * (1 - share) / NREP),
    mean_resid_excess_kurtosis = mean(m[, "resid_kurt"]),
    sd_resid_excess_kurtosis = stats::sd(m[, "resid_kurt"])
  )
}

rows <- list()
idx <- 1L
for (sd_name in names(SD_VECTORS)) {
  for (design_name in names(SIZE_DESIGNS)) {
    for (mean_n in MEAN_NS) {
      n_vec <- as.integer(round(mean_n * SIZE_DESIGNS[[design_name]]))
      stopifnot(mean(n_vec) == mean_n)
      sd_vec <- SD_VECTORS[[sd_name]]
      res <- one_cell(n_vec, sd_vec)
      rows[[idx]] <- data.frame(
        sd_label = sd_name,
        sd_per_group = paste(format(sd_vec, nsmall = 1), collapse = ", "),
        sd_ratio = max(sd_vec) / min(sd_vec),
        design = design_name,
        mean_n_per_group = mean_n,
        n_per_group = paste(n_vec, collapse = ", "),
        route_rank_probability = res["route_rank_probability"],
        route_mc_se = res["route_mc_se"],
        mean_resid_excess_kurtosis = res["mean_resid_excess_kurtosis"],
        sd_resid_excess_kurtosis = res["sd_resid_excess_kurtosis"],
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
      cat(sprintf(
        "%-26s | %-32s | n_bar=%3d | routed %.3f | resid kurt %+.3f\n",
        sd_name, design_name, mean_n,
        res["route_rank_probability"], res["mean_resid_excess_kurtosis"]
      ))
      utils::flush.console()
    }
  }
}

out <- do.call(rbind, rows)
rownames(out) <- NULL
write.csv(out, "route1_sd_sensitivity.csv", row.names = FALSE)
cat("\nwritten: route1_sd_sensitivity.csv (", nrow(out), " cells )\n", sep = "")
