## ---------------------------------------------------------------------------
## eta_H^2 per (grid, design, panel), for every power grid in this directory.
##
## !! NOT A CITABLE QUANTITY !!
##
## The expression evaluated here,
##
##     eta_H^2 = 12 * sum_i q_i (p_i - 1/2)^2,
##     q_i = n_i / N,   p_i = int Fbar_q dF_i,   Fbar_q = sum_i q_i F_i,
##
## is a DERIVATION OF OUR OWN, written up in the "Population effect size of the
## rank-based test" subsection of vignettes/children/_effect_size_table.Rmd.
## No source we have read states it. Two of its three steps are algebraic
## identities checked against kruskal.test() to machine precision; the limit
## itself is supported by simulation, not by a citation.
##
## It exists because omega^2 is blind to distribution shape and therefore cannot
## explain why the rank branch behaves so differently across panels of the same
## design. See AUDIT_what_is_verified.md, section B.
##
## Design constants are read from each grid's OWN results file rather than
## re-declared here, so a grid can never be labelled with an effect size
## belonging to inputs it did not simulate. The homoscedastic
## designs of the brunner grid were not rerun and are taken, as everywhere else,
## from the legacy grid whose inputs are identical.
##
## p_i is estimated by the estimator it is the limit of, (Rbar_i - 1/2)/N, on one
## very large sample drawn in the design's own allocation proportions.
##
## Output: eta_h_own_by_design_panel.csv, with a `grid` column.
## ---------------------------------------------------------------------------

SIMDIR <- local({
  here <- getwd()
  if (file.exists(file.path(here, "fleishman_route1_residual_helpers.R"))) here
  else {
    installed <- system.file("simulations", package = "visStatistics")
    if (!nzchar(installed)) stop("Run from inst/simulations/.")
    installed
  }
})
source(file.path(SIMDIR, "fleishman_route1_residual_helpers.R"))

set.seed(20260811)
N_TOTAL <- 8e5

parse_vec <- function(s) as.numeric(strsplit(as.character(s), ",[ ]*")[[1]])

## eta_H^2 for one (multipliers, sd, shifts, panel). Everything else in this
## file only assembles arguments for this.
eta_h_sq_own <- function(mult, sd_vec, shifts, panel) {
  q <- mult / sum(mult)
  n <- as.integer(round(q * N_TOTAL))
  k <- length(n)
  y <- unlist(lapply(seq_len(k), function(i) {
    sd_vec[i] * draw_fleishman_panel(n[i], panel) + shifts[i]
  }))
  g <- rep(seq_len(k), times = n)
  R <- rank(y)
  N <- length(y)
  p_i <- (tapply(R, g, mean) - 0.5) / N
  q_eff <- n / N
  list(eta = 12 * sum(q_eff * (p_i - 0.5)^2),
       check = sum(q_eff * p_i) - 0.5,
       p = p_i)
}

## Multipliers are not stored in the result files, only the realised n vector,
## so they are recovered from it: n_vec / mean(n_vec).
multipliers_from <- function(n_vector) {
  n <- parse_vec(n_vector)
  n / mean(n)
}

## Each grid contributes (design, panel) -> (multipliers, sd, shifts).
grids <- list(
  legacy = list(
    file = "fleishman_4groups_power.rds",
    read = function(f) {
      d <- readRDS(file.path(SIMDIR, f))
      d[!duplicated(paste(d$design, d$panel)),
        c("design", "panel", "n_vector", "sd_per_group", "group_mean_offsets")]
    }
  ),
  brunner = list(
    file = "fleishman_4groups_power_design_brunner_B50000.csv",
    read = function(f) {
      d <- read.csv(file.path(SIMDIR, f), stringsAsFactors = FALSE)
      d[!duplicated(paste(d$design, d$panel)),
        c("design", "panel", "n_vector", "sd_per_group", "group_mean_offsets")]
    }
  ),
  omega_fixed = list(
    file = "fleishman_4groups_power_omega_fixed_B50000.csv",
    read = function(f) {
      d <- read.csv(file.path(SIMDIR, f), stringsAsFactors = FALSE)
      d[!duplicated(paste(d$design, d$panel)),
        c("design", "panel", "n_vector", "sd_per_group", "group_mean_offsets")]
    }
  ),
  etaH_fixed = list(
    file = "fleishman_4groups_power_etaH_fixed_B50000.csv",
    read = function(f) {
      d <- read.csv(file.path(SIMDIR, f), stringsAsFactors = FALSE)
      d[!duplicated(paste(d$design, d$panel)),
        c("design", "panel", "n_vector", "sd_per_group", "group_mean_offsets")]
    }
  ),
  ## The Type I grid: all group means are zero by construction, so omega^2 is
  ## zero in every cell. eta_H^2 is NOT, because scaling a skewed distribution
  ## moves its mass even when its mean stays put. It is therefore the quantity
  ## that says which columns of that figure are a genuine Type I check for the
  ## rank branch and which are power -- the distinction the `rank_null_true`
  ## flag of rankfd_route1_typeI.R:137 makes qualitatively.
  typeI = list(
    file = "rankfd_route1_typeI_B50000.csv",
    read = function(f) {
      d <- read.csv(file.path(SIMDIR, f), stringsAsFactors = FALSE)
      d <- d[!duplicated(paste(d$design, d$panel)),
             c("design", "panel", "n_per_group", "sd_per_group")]
      names(d)[names(d) == "n_per_group"] <- "n_vector"
      d$group_mean_offsets <- "0, 0, 0, 0"
      d
    }
  ),
  ## The two grids below are appended LAST so that every grid above them draws
  ## from the same stream positions as before and reproduces bit-identically.

  ## The one-point alternative, mu = (0, 0, 0, delta).
  brunner_onepoint = list(
    file = "fleishman_4groups_power_design_brunner_onepoint_d100_B50000.csv",
    read = function(f) {
      d <- read.csv(file.path(SIMDIR, f), stringsAsFactors = FALSE)
      d[!duplicated(paste(d$design, d$panel)),
        c("design", "panel", "n_vector", "sd_per_group", "group_mean_offsets")]
    }
  ),
  ## The Type I grid on the SD vectors (1, sqrt(2), 2, sqrt(5)) and its reverse.
  typeI_brunner = list(
    file = "route1_typeI_design_brunner_B50000.csv",
    read = function(f) {
      d <- read.csv(file.path(SIMDIR, f), stringsAsFactors = FALSE)
      d <- d[!duplicated(paste(d$design, d$panel)),
             c("design", "panel", "n_per_group", "sd_per_group")]
      names(d)[names(d) == "n_per_group"] <- "n_vector"
      d$group_mean_offsets <- "0, 0, 0, 0"
      d
    }
  ),
  ## The one-point alternative at delta = 0.5.
  brunner_onepoint_d050 = list(
    file = "fleishman_4groups_power_design_brunner_onepoint_d050_B50000.csv",
    read = function(f) {
      d <- read.csv(file.path(SIMDIR, f), stringsAsFactors = FALSE)
      d[!duplicated(paste(d$design, d$panel)),
        c("design", "panel", "n_vector", "sd_per_group", "group_mean_offsets")]
    }
  )
)

rows <- list()
i <- 1
for (gname in names(grids)) {
  gr <- grids[[gname]]
  if (!file.exists(file.path(SIMDIR, gr$file))) {
    message("skipping grid '", gname, "': ", gr$file, " not found")
    next
  }
  spec <- gr$read(gr$file)
  cat("\n=== grid:", gname, "(", nrow(spec), "design-panel cells ) ===\n")
  for (r in seq_len(nrow(spec))) {
    s <- spec[r, ]
    res <- eta_h_sq_own(
      multipliers_from(s$n_vector),
      parse_vec(s$sd_per_group),
      parse_vec(s$group_mean_offsets),
      s$panel
    )
    rows[[i]] <- data.frame(
      grid = gname, design = s$design, panel = s$panel,
      sd_per_group = s$sd_per_group,
      group_mean_offsets = s$group_mean_offsets,
      eta_h_sq_own = res$eta,
      rel_effects = paste(sprintf("%.4f", res$p), collapse = ", "),
      constraint_residual = res$check,
      row.names = NULL
    )
    cat(sprintf("  %-40s panel %d  eta_H^2 = %.5f\n", s$design, s$panel, res$eta))
    i <- i + 1
  }
}

out <- do.call(rbind, rows)
write.csv(out, "eta_h_own_by_design_panel.csv", row.names = FALSE)
message("\nWrote eta_h_own_by_design_panel.csv (", nrow(out), " rows)")
