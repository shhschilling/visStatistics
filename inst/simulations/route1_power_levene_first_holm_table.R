# Render saved results only; no simulations run here.
# Rscript inst/simulations/route1_power_levene_first_holm_table.R 1000
args <- commandArgs(trailingOnly = TRUE)
B <- if (length(args)) as.integer(args[1]) else 1000L
outdir <- file.path("inst/simulations",
                    paste0("2026-09-28_power_levene_first_holm_B", B))
p <- read.csv(file.path(outdir, "power_and_routing.csv"))
d <- read.csv(file.path(outdir, "paired_comparisons.csv"))
design <- read.csv(file.path(outdir, "design.csv"))
stopifnot(nrow(p) == 1600L, nrow(d) == 1200L, all(p$B == B), all(d$B == B))
rows <- lapply(design$cell, function(i) {
  x <- p[p$cell == i, ]
  y <- d[d$cell == i, ]
  row <- design[design$cell == i, ]
  for (m in c("F", "W", "KW", "gate")) {
    row[[paste0(m, "_power_pct")]] <- x$power_pct[x$method == m]
    row[[paste0(m, "_mcse_pp")]] <- x$mcse_pp[x$method == m]
  }
  for (r in c("F", "W", "KW")) {
    row[[paste0("route_", r, "_pct")]] <- x[[paste0("route_", r, "_pct")]][1]
    row[[paste0("gate_minus_", r, "_pp")]] <- y$gate_minus_fixed_pp[y$fixed == r]
    row[[paste0("paired_mcse_vs_", r, "_pp")]] <- y$paired_mcse_pp[y$fixed == r]
  }
  row
})
wide <- do.call(rbind, rows)
write.csv(wide, file.path(outdir, "comparison_table.csv"), row.names = FALSE)

# Counts are by cell; do not pool power across sample sizes or distributions.
keys <- unique(d[c("design", "shift", "panel", "skew", "excess_kurtosis", "fixed")])
summary_rows <- lapply(seq_len(nrow(keys)), function(i) {
  k <- keys[i, ]
  z <- d[d$design == k$design & d$shift == k$shift &
           d$panel == k$panel & d$fixed == k$fixed, ]
  cbind(k, cells = nrow(z),
        losses_larger_than_one_mcse = sum(z$gate_minus_fixed_pp < -z$paired_mcse_pp),
        losses_mc95_excludes_zero = sum(z$mc95_upper_pp < 0),
        gains_mc95_excludes_zero = sum(z$mc95_lower_pp > 0),
        largest_observed_loss_pp = max(0, -min(z$gate_minus_fixed_pp)),
        largest_observed_gain_pp = max(0, max(z$gate_minus_fixed_pp)))
})
write.csv(do.call(rbind, summary_rows), file.path(outdir, "summary_by_design_and_distribution.csv"),
          row.names = FALSE)

esc <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
html <- c('<!doctype html><html lang="en"><meta charset="utf-8">',
  '<title>Power: fixed tests and Levene-first gate</title>',
  '<style>body{font:16px Arial,sans-serif;color:#222;margin:28px;line-height:1.45}',
  'h1{font-size:24px}h2{font-size:20px}table{border-collapse:collapse;width:100%;font-size:14px}',
  'th,td{padding:8px 10px;border-bottom:1px solid #ddd;text-align:right;white-space:nowrap}',
  'thead{background:#eef2f4}th:first-child,td:first-child{text-align:left}',
  '.sep{border-left:2px solid #888}details{margin:18px 0}summary{cursor:pointer;font-weight:bold}',
  '.scroll{overflow-x:auto}caption{text-align:left;padding:12px 0}p{max-width:1100px}',
  '</style><body><h1>Power: fixed tests and Levene-first gate</h1>',
  sprintf('<p>%s replications per cell; four groups; significance level 5%%. All tests use the same simulated samples within each cell.</p>', format(B, big.mark = ',')),
  '<p>L+SW: Levene on internally studentised model residuals first. If Levene does not reject, pooled residual Shapiro-Wilk selects F or KW. If Levene rejects, Holm-adjusted groupwise Shapiro-Wilk selects W or KW.</p>',
  '<p>F: Fisher ANOVA; W: Welch ANOVA; KW: Kruskal-Wallis. Positive differences mean higher rejection probability for the gate; negative differences mean lower rejection probability. Differences are in percentage points, with &plusmn; one paired Monte Carlo standard error. These simulations alone do not establish Type I error control.</p>',
  '<p>The inputs are the vignette\'s normal and Fleishman distributions. Skewness and excess kurtosis identify each base distribution. SD factors and mean shifts are listed for every block. n denotes each group size in balanced designs and average group size in unbalanced designs; the complete group-size vector is shown. Routing percentages are separate from final-test rejection percentages.</p>')
for (shift in unique(wide$shift)) {
  html <- c(html, paste0('<h2>', esc(shift), '</h2>'))
  for (name in unique(wide$design)) {
    z <- wide[wide$shift == shift & wide$design == name, ]
    z <- z[order(z$panel, z$n_per_group), ]
    html <- c(html, paste0('<details open><summary>', esc(name), '</summary><div class="scroll"><table>'),
      paste0('<caption>Means: (', esc(z$group_mean_offsets[1]), '); SD: (', esc(z$sd_vector[1]), ').</caption>'),
      '<thead><tr><th colspan="4">Simulation inputs</th><th colspan="4" class="sep">Final-test rejection (%)</th><th colspan="3" class="sep">Gate routing (%)</th><th colspan="3" class="sep">Gate minus fixed (pp &plusmn; MCSE)</th></tr>',
      '<tr><th>Skew; excess kurtosis</th><th>n</th><th>Group sizes A-D</th><th>Input</th><th class="sep">F</th><th>W</th><th>KW</th><th>L+SW</th><th class="sep">F</th><th>W</th><th>KW</th><th class="sep">vs F</th><th>vs W</th><th>vs KW</th></tr></thead><tbody>')
    for (j in seq_len(nrow(z))) {
      r <- z[j, ]
      vals <- c(sprintf('%g; %g', r$skew, r$excess_kurtosis), r$n_per_group,
        r$n_vector, if (r$panel == 1) 'Normal' else 'Fleishman',
        sprintf('%.1f', unlist(r[c('F_power_pct','W_power_pct','KW_power_pct','gate_power_pct')])),
        sprintf('%.1f', unlist(r[c('route_F_pct','route_W_pct','route_KW_pct')])),
        vapply(c('F','W','KW'), function(m) sprintf('%+.2f &plusmn; %.2f',
          r[[paste0('gate_minus_',m,'_pp')]], r[[paste0('paired_mcse_vs_',m,'_pp')]]), character(1)))
      tags <- ifelse(seq_along(vals) %in% c(5,9,12), '<td class="sep">', '<td>')
      html <- c(html, paste0('<tr>', paste0(tags, vals, '</td>', collapse=''), '</tr>'))
    }
    html <- c(html, '</tbody></table></div></details>')
  }
}
html <- c(html, '</body></html>')
writeLines(html, file.path(outdir, "comparison_table.html"))
message("Table: ", file.path(outdir, "comparison_table.html"))
