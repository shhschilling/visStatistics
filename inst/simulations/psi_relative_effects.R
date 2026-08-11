## ---------------------------------------------------------------------------
## Unweighted relative effects psi_i for every design in the power and Type I
## grids.
##
##     H^psi(x) = (1/a) sum_j F_j(x),      psi_i = int H^psi dF_i
##
## Zimmermann, Happ & Bathke (2021), The American Statistician 75(2), 121-131,
## eqs. (2) and (4). Unlike the weighted relative effect theta_i estimated by
## kruskal.test(), psi_i does not depend on the group sizes: p. 125 states that
## theta_i "depends on the relative sample sizes n_i and, thus, is not a
## parameter nor an effect size of a statistical model in a strict sense, in
## contrast to psi_i".
##
## Their remaining caveat (Sect. 3.3, p. 127) is that the range of psi_i depends
## on the NUMBER OF GROUPS a, so deviations from 1/2 are not comparable across
## studies with different a. Every design in this package's grids has a = 4, so
## the comparison the figures make is inside that constraint.
##
## Because psi_i is allocation-free, it is a function of the SD vector, the
## shift vector and the input distribution only. Balanced and unbalanced designs
## sharing those therefore share psi -- which is the point: the effect does not
## change when only the group sizes do.
##
## Also reported: c_KW^psi = psi' P_a psi = sum_i (psi_i - 1/2)^2, the
## consistency non-centrality of the PSEUDO-RANK Kruskal-Wallis test
## (Brunner, Bathke & Konietschke 2018, Result 4.14, p. 201). Note this is the
## pseudo-rank statistic's quantity, not kruskal.test()'s.
##
## psi_i is estimated by its plug-in estimator on one large EQUALLY allocated
## sample, since the reference distribution H^psi is the unweighted average.
##
## Output: psi_by_design_panel.csv
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
N_PER_GROUP <- 2e5

parse_vec <- function(s) as.numeric(strsplit(as.character(s), ",[ ]*")[[1]])

## Equal allocation, because H^psi weights every group equally.
psi_of <- function(sd_vec, shifts, panel) {
  a <- length(sd_vec)
  n <- rep(N_PER_GROUP, a)
  y <- unlist(lapply(seq_len(a), function(i) {
    sd_vec[i] * draw_fleishman_panel(n[i], panel) + shifts[i]
  }))
  g <- rep(seq_len(a), times = n)
  R <- rank(y)
  N <- length(y)
  as.vector((tapply(R, g, mean) - 0.5) / N)
}

grids <- list(
  legacy      = "fleishman_4groups_power.rds",
  brunner     = "fleishman_4groups_power_design_brunner_B50000.csv",
  omega_fixed = "fleishman_4groups_power_omega_fixed_B50000.csv",
  etaH_fixed  = "fleishman_4groups_power_etaH_fixed_B50000.csv",
  typeI       = "rankfd_route1_typeI_B50000.csv"
)

read_spec <- function(gname, f) {
  path <- file.path(SIMDIR, f)
  d <- if (grepl("\\.rds$", f)) readRDS(path) else read.csv(path, stringsAsFactors = FALSE)
  if (gname == "typeI") {
    d <- d[!duplicated(paste(d$design, d$panel)), c("design", "panel", "sd_per_group")]
    d$group_mean_offsets <- "0, 0, 0, 0"
  } else {
    d <- d[!duplicated(paste(d$design, d$panel)),
           c("design", "panel", "sd_per_group", "group_mean_offsets")]
  }
  d
}

rows <- list(); i <- 1
for (gname in names(grids)) {
  f <- grids[[gname]]
  if (!file.exists(file.path(SIMDIR, f))) {
    message("skipping grid '", gname, "': ", f, " not found"); next
  }
  spec <- read_spec(gname, f)
  cat("\n=== grid:", gname, "(", nrow(spec), "cells ) ===\n")
  for (r in seq_len(nrow(spec))) {
    s <- spec[r, ]
    psi <- psi_of(parse_vec(s$sd_per_group), parse_vec(s$group_mean_offsets), s$panel)
    rows[[i]] <- data.frame(
      grid = gname, design = s$design, panel = s$panel,
      sd_per_group = s$sd_per_group, group_mean_offsets = s$group_mean_offsets,
      psi = paste(sprintf("%.4f", psi), collapse = ", "),
      c_kw_psi = sum((psi - 0.5)^2),
      row.names = NULL
    )
    cat(sprintf("  %-40s panel %d  psi = %s   c = %.5f\n", s$design, s$panel,
                paste(sprintf("%.3f", psi), collapse = " "), sum((psi - 0.5)^2)))
    i <- i + 1
  }
}
out <- do.call(rbind, rows)
write.csv(out, "psi_by_design_panel.csv", row.names = FALSE)
message("\nWrote psi_by_design_panel.csv (", nrow(out), " rows)")
