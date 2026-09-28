# Fresh reconstruction of the 32 normal, equal-variance scenarios described
# in "Continue simulation results". The Android seeds/files were unavailable.
# Run from the repository root: Rscript dev/2026-09-28_pretesting_power_replay.R
# Optional arguments: replications (default 1000), worker processes (default 4).
# The unbalanced allocation is taken from the local simulation design.
args <- commandArgs(trailingOnly = TRUE)
B <- if (length(args)) as.integer(args[1]) else 1000L
workers <- if (length(args) > 1L) as.integer(args[2]) else 4L
stopifnot(B > 1L, workers >= 1L)
source("R/levene.test.R")
alpha <- 0.05
outdir <- file.path("dev", paste0("2026-09-28_pretesting_power_replay_B", B))
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
design <- expand.grid(balance = c("balanced", "unbalanced"),
                      n_bar = c(10L, 20L, 30L, 50L, 100L),
                      shift = c("ordered", "onepoint_0.5", "onepoint_1"),
                      stringsAsFactors = FALSE)
design <- rbind(design, data.frame(balance = c("balanced", "unbalanced"),
                                  n_bar = 200L, shift = "ordered"))
design$cell <- seq_len(nrow(design))
design$seed <- 20260928L + design$cell
shifts <- list(ordered = c(0, 0.25, 0.5, 0.75),
               onepoint_0.5 = c(0, 0, 0, 0.5),
               onepoint_1 = c(0, 0, 0, 1))
design$n_vector <- vapply(seq_len(nrow(design)), function(i) {
  mult <- if (design$balance[i] == "balanced") rep(1, 4) else c(.5, .8, 1.2, 1.5)
  paste(as.integer(round(design$n_bar[i] * mult)), collapse = ",")
}, character(1))
write.csv(design, file.path(outdir, "design.csv"), row.names = FALSE)

run_cell <- function(i) {
  path <- file.path(outdir, sprintf("cell_%02d.rds", i))
  if (file.exists(path)) return(readRDS(path))
  set.seed(design$seed[i])
  n <- as.integer(strsplit(design$n_vector[i], ",", fixed = TRUE)[[1]])
  g <- factor(rep(LETTERS[1:4], n))
  mu <- rep(shifts[[design$shift[i]]], n)
  records <- lapply(seq_len(B), function(b) {
    y <- rnorm(sum(n)) + mu
    model <- lm(y ~ g)
    r <- rstandard(model)
    pL <- levene.test(r, g)$p.value
    pSW <- shapiro.test(r)$p.value
    group_p <- vapply(split(y, g), function(z) shapiro.test(z)$p.value,
                      numeric(1))
    pHolm <- min(p.adjust(group_p, method = "holm"))
    pF <- anova(model)[["Pr(>F)"]][1]
    pW <- oneway.test(y ~ g, var.equal = FALSE)$p.value
    pKW <- kruskal.test(y ~ g)$p.value
    route <- if (pL < alpha) {
      if (pHolm < alpha) "KW" else "W"
    } else if (pSW < alpha) "KW" else "F"
    p <- c(F = pF, W = pW, KW = pKW)
    data.frame(replication = b, pL = pL, pSW = pSW, pHolm = pHolm,
               pF = pF, pW = pW, pKW = pKW,
               F = pF < alpha, W = pW < alpha, KW = pKW < alpha,
               route = route, gate = unname(p[route]) < alpha)
  })
  records <- do.call(rbind, records)
  stopifnot(nrow(records) == B, !anyNA(records))
  saveRDS(records, path)
  message("Saved cell ", i, "/", nrow(design))
  records
}
records <- parallel::mclapply(seq_len(nrow(design)), run_cell, mc.cores = workers)
stopifnot(all(vapply(records, is.data.frame, logical(1))))

power <- comparisons <- branch <- list()
for (i in seq_len(nrow(design))) {
  d <- records[[i]]
  base <- design[i, ]
  for (method in c("F", "W", "KW", "gate")) {
    p <- mean(d[[method]])
    power[[length(power) + 1L]] <- cbind(base, method, B,
      power_pct = 100 * p, mcse_pp = 100 * sqrt(p * (1 - p) / B),
      route_F_pct = 100 * mean(d$route == "F"),
      route_W_pct = 100 * mean(d$route == "W"),
      route_KW_pct = 100 * mean(d$route == "KW"))
  }
  for (method in c("F", "W", "KW")) {
    loss <- sum(d[[method]] & !d$gate)
    gain <- sum(!d[[method]] & d$gate)
    difference <- as.integer(d$gate) - as.integer(d[[method]])
    se <- 100 * sd(difference) / sqrt(B)
    delta <- 100 * mean(difference)
    stopifnot(abs(delta - 100 * (gain - loss) / B) < 1e-10)
    comparisons[[length(comparisons) + 1L]] <- cbind(base, fixed = method, B,
      fixed_power_pct = 100 * mean(d[[method]]),
      gate_power_pct = 100 * mean(d$gate), lost = loss, gained = gain,
      gate_minus_fixed_pp = delta, paired_mcse_pp = se,
      mc95_lower_pp = delta - 1.96 * se, mc95_upper_pp = delta + 1.96 * se)
    for (destination in c("F", "W", "KW")) {
      selected <- d$route == destination
      branch[[length(branch) + 1L]] <- cbind(base, fixed = method,
        destination, B, routed = sum(selected),
        lost = sum(selected & d[[method]] & !d$gate),
        gained = sum(selected & !d[[method]] & d$gate))
    }
  }
}
write.csv(do.call(rbind, power), file.path(outdir, "power_and_routing.csv"),
          row.names = FALSE)
write.csv(do.call(rbind, comparisons), file.path(outdir, "paired_comparisons.csv"),
          row.names = FALSE)
write.csv(do.call(rbind, branch), file.path(outdir, "paired_counts_by_destination.csv"),
          row.names = FALSE)
capture.output(sessionInfo(), file = file.path(outdir, "sessionInfo.txt"))
message("Completed: ", outdir)
