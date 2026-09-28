# Full vignette power replay with Levene-first routing.
# Run from repository root:
# Rscript inst/simulations/route1_power_levene_first_holm.R 1000 4
# Independent output; existing simulation data and package routing are untouched.
args <- commandArgs(trailingOnly = TRUE)
B <- if (length(args)) as.integer(args[1]) else 1000L
workers <- if (length(args) > 1L) as.integer(args[2]) else 4L
stopifnot(B > 1L, workers > 0L)
simdir <- "inst/simulations"
source("R/levene.test.R")
source(file.path(simdir, "fleishman_route1_residual_helpers.R"))
outdir <- file.path(simdir, paste0("2026-09-28_power_levene_first_holm_B", B))
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
alpha <- 0.05

# Read all source grids. Retain their cell coverage, including n=200 for
# ordered shifts. SDs use the exact constants in the generating script,
# rather than the rounded strings in the saved CSVs.
files <- c("fleishman_4groups_power.csv",
           "fleishman_4groups_power_design_brunner_B50000.csv",
           "fleishman_4groups_power_design_brunner_onepoint_d050_B50000.csv",
           "fleishman_4groups_power_design_brunner_onepoint_d100_B50000.csv")
inputs <- lapply(file.path(simdir, files), read.csv, stringsAsFactors = FALSE)
cols <- c("design", "n_per_group", "n_vector", "panel", "skew",
          "excess_kurtosis", "group_mean_offsets")
hom <- inputs[[1]][inputs[[1]]$design %in%
  c("balanced n, equal SD", "unbalanced n, equal SD"), cols]
design <- rbind(transform(hom, shift = "ordered"),
                transform(inputs[[2]][, cols], shift = "ordered"),
                transform(inputs[[3]][, cols], shift = "onepoint_0.5"),
                transform(inputs[[4]][, cols], shift = "onepoint_1"))
design$cell <- seq_len(nrow(design))
design$seed <- 202609280L + design$cell
stopifnot(nrow(design) == 400L,
          !anyDuplicated(design[c("design", "n_per_group", "panel", "shift")]))
sd_for <- function(name) {
  if (name %in% c("balanced n, equal SD", "unbalanced n, equal SD")) rep(1, 4)
  else if (name == "unbalanced n, larger n with smaller SD")
    c(sqrt(5), 2, sqrt(2), 1)
  else c(1, sqrt(2), 2, sqrt(5))
}
design$sd_vector <- vapply(design$design, function(x)
  paste(format(sd_for(x), digits = 16), collapse = ","), character(1))
for (i in seq_len(nrow(design))) {
  case <- fleishman_cases[fleishman_cases$panel == design$panel[i], ]
  stopifnot(case$skew == design$skew[i],
            case$excess_kurtosis == design$excess_kurtosis[i])
  n <- as.integer(strsplit(design$n_vector[i], ",", fixed = TRUE)[[1]])
  mult <- if (startsWith(design$design[i], "balanced")) rep(1, 4)
          else c(.5, .8, 1.2, 1.5)
  stopifnot(identical(n, as.integer(round(design$n_per_group[i] * mult))))
}
manifest <- list(design = design, B = B, alpha = alpha,
  gate = "Levene on rstandard; pooled SW if pL>=alpha; groupwise Holm SW otherwise",
  source_files = files,
  source_mtime = file.info(file.path(simdir, files))$mtime,
  source_md5 = tools::md5sum(file.path(simdir, files)),
  code_md5 = tools::md5sum(c("R/levene.test.R",
                           file.path(simdir, "fleishman_route1_residual_helpers.R"))),
  distribution_parameters = fleishman_cases)
manifest_path <- file.path(outdir, "manifest.rds")
if (file.exists(manifest_path)) {
  stopifnot(identical(readRDS(manifest_path), manifest))
} else {
  saveRDS(manifest, manifest_path)
}
write.csv(design, file.path(outdir, "design.csv"), row.names = FALSE)
write.csv(fleishman_cases, file.path(outdir, "distribution_parameters.csv"),
          row.names = FALSE)

run_cell <- function(i) {
  path <- file.path(outdir, sprintf("cell_%03d.rds", i))
  if (file.exists(path)) return(path)
  set.seed(design$seed[i])
  n <- as.integer(strsplit(design$n_vector[i], ",", fixed = TRUE)[[1]])
  mu <- as.numeric(strsplit(design$group_mean_offsets[i], ",", fixed = TRUE)[[1]])
  s <- sd_for(design$design[i])
  g <- factor(rep(LETTERS[1:4], n))
  records <- lapply(seq_len(B), function(b) {
    y <- unlist(lapply(seq_len(4), function(j)
      s[j] * draw_fleishman_panel(n[j], design$panel[i]) + mu[j]))
    model <- aov(y ~ g)
    r <- rstandard(model)
    pL <- levene.test(r, g)$p.value
    pSW <- shapiro.test(r)$p.value
    group_p <- vapply(split(y, g), function(z) shapiro.test(z)$p.value,
                      numeric(1))
    # SW is invariant under group-specific centring and positive scaling.
    pHolm <- min(p.adjust(group_p, method = "holm"))
    p <- c(F = summary(model)[[1]][["Pr(>F)"]][1],
           W = oneway.test(y ~ g, var.equal = FALSE)$p.value,
           KW = kruskal.test(y ~ g)$p.value)
    route <- if (pL < alpha) {
      if (pHolm < alpha) "KW" else "W"
    } else if (pSW < alpha) "KW" else "F"
    data.frame(replication = b, pL, pSW, pHolm,
               pSW_A = group_p[1], pSW_B = group_p[2],
               pSW_C = group_p[3], pSW_D = group_p[4],
               pF = p["F"], pW = p["W"], pKW = p["KW"],
               F = p["F"] < alpha, W = p["W"] < alpha,
               KW = p["KW"] < alpha, route,
               gate = unname(p[route]) < alpha, row.names = NULL)
  })
  d <- do.call(rbind, records)
  stopifnot(nrow(d) == B, !anyNA(d),
            all(d$gate == ifelse(d$route == "F", d$F,
                                ifelse(d$route == "W", d$W, d$KW))))
  tmp <- paste0(path, ".partial")
  saveRDS(d, tmp)
  stopifnot(file.rename(tmp, path))
  message("Saved cell ", i, "/", nrow(design))
  path
}
paths <- parallel::mclapply(seq_len(nrow(design)), run_cell, mc.cores = workers)
stopifnot(all(vapply(paths, function(x) is.character(x) && file.exists(x), logical(1))))
power <- comparisons <- branches <- list()
for (i in seq_len(nrow(design))) {
  d <- readRDS(paths[[i]])
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
    delta <- as.integer(d$gate) - as.integer(d[[method]])
    se <- 100 * sd(delta) / sqrt(B)
    diff <- 100 * mean(delta)
    stopifnot(abs(diff - 100 * (gain - loss) / B) < 1e-10)
    comparisons[[length(comparisons) + 1L]] <- cbind(base, fixed = method, B,
      fixed_power_pct = 100 * mean(d[[method]]), gate_power_pct = 100 * mean(d$gate),
      lost = loss, gained = gain, gate_minus_fixed_pp = diff,
      paired_mcse_pp = se, mc95_lower_pp = diff - 1.96 * se,
      mc95_upper_pp = diff + 1.96 * se)
    for (destination in c("F", "W", "KW")) {
      selected <- d$route == destination
      branches[[length(branches) + 1L]] <- cbind(base, fixed = method,
        destination, B, routed = sum(selected),
        lost = sum(selected & d[[method]] & !d$gate),
        gained = sum(selected & !d[[method]] & d$gate))
    }
  }
}
write.csv(do.call(rbind, power), file.path(outdir, "power_and_routing.csv"), row.names = FALSE)
write.csv(do.call(rbind, comparisons), file.path(outdir, "paired_comparisons.csv"), row.names = FALSE)
write.csv(do.call(rbind, branches), file.path(outdir, "paired_counts_by_destination.csv"), row.names = FALSE)
capture.output(sessionInfo(), file = file.path(outdir, "sessionInfo.txt"))
message("Completed: ", outdir)
