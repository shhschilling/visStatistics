# Reuse the vignette's plotting functions with the new saved power results.
# Run from repository root; does not rerun simulations or overwrite figures.
SIMDIR <- normalizePath("inst/simulations")
OUTDIR <- file.path(SIMDIR, "2026-09-28_power_levene_first_holm_B1000")
source(file.path(SIMDIR, "fleishman_route1_residual_helpers.R"))
source(file.path(SIMDIR, "fleishman_figure_typography.R"))
source(file.path(SIMDIR, "omega_scaling_helpers.R"))
ggplot2 <- asNamespace("ggplot2")
patchwork <- asNamespace("patchwork")
scales <- asNamespace("scales")

# Import named constants and complete plotting functions, not the old data
# reads or figure-writing calls. There is one source for the visual design.
imports <- c("D_BAL_EQ", "D_UNB_EQ", "D_BAL_HET", "D_POS", "D_NEG",
  "DESIGN_WORDS", "NMULT", "SD_EQ", "SD_POS", "SD_NEG", "sd_label",
  "sd_vector_label", "H_PDF", "H_POWER", "BAND_SCALE", "groups",
  "xlim", "y_cap", "panel_title", "make_pdf_panel", "make_power_panel",
  "pdf_title", "IN_PER_UNIT", "lambda_of", "symbol_of")
for (e in parse(file.path(SIMDIR, "figure_power_brunner_sd.R"))) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      is.name(e[[2]]) && as.character(e[[2]]) %in% imports) eval(e)
}
names(DESIGN_WORDS) <- names(NMULT) <- c(D_BAL_EQ, D_UNB_EQ, D_BAL_HET, D_POS, D_NEG)
SDS <- setNames(list(SD_EQ, SD_EQ, SD_POS, SD_POS, SD_NEG), names(DESIGN_WORDS))
PANELS <- 1:5
ETA <- NULL
USE <- c("Fisher", "Welch", "Kruskal-Wallis", "Levene-first gate")
STRATS <- setNames(c("F", "W", "KW", "L+SW"), USE)
COLS <- setNames(c("#B79F00", "#56B4E9", "#000000", "#0072B2"), USE)
SHP <- setNames(c(0, 2, 4, 1), USE)
SZ <- setNames(c(4, 3.2, 3.8, 7.2), USE)
COLUMN <- setNames(c("fisher_power", "welch_power", "rank_power", "gate_power"), USE)
saved <- read.csv(file.path(OUTDIR, "comparison_table.csv"))
manifest <- readRDS(file.path(OUTDIR, "manifest.rds"))
stopifnot(nrow(saved) == 400, all(saved$cell == manifest$design$cell))

power_panel <- function(design_name, letter) {
  p <- make_power_panel(design_name, letter)
  for (j in seq_along(p$layers)) {
    d <- p$layers[[j]]$data
    if (is.data.frame(d) && "t" %in% names(d)) {
      d$t <- "L+SW selection (%)"
      p$layers[[j]]$data <- d
    }
  }
  balanced <- startsWith(design_name, "balanced")
  description <- sprintf(
    "%s; %s; SD = (%s)", DESIGN_WORDS[[design_name]],
    if (balanced) "n<sub>i</sub> = n, i = 1,...,4" else
      "(n<sub>1</sub>, n<sub>2</sub>, n<sub>3</sub>, n<sub>4</sub>) = n&#772;(0.5, 0.8, 1.2, 1.5)",
    sd_vector_label(SDS[[design_name]]))
  suppressMessages(p +
    ggplot2$scale_x_log10(breaks = NS_TO_PLOT,
                          limits = c(5.5, max(NS_TO_PLOT) * 1.3)) +
    ggplot2$labs(title = fleishman_panel_title(letter, description),
      x = if (balanced) "Group size n" else expression("Average group size " * bar(n)),
      y = "Final-test rejection (%)"))
}

for (scenario in unique(saved$shift)) {
  power <- saved[saved$shift == scenario, ]
  SHIFTS <- as.numeric(strsplit(power$group_mean_offsets[1], ",", fixed = TRUE)[[1]])
  stopifnot(length(unique(power$group_mean_offsets)) == 1)
  for (i in seq_len(nrow(power))) {
    s <- as.numeric(strsplit(power$sd_vector[i], ",", fixed = TRUE)[[1]])
    stopifnot(isTRUE(all.equal(s, SDS[[power$design[i]]], tolerance = 1e-12)))
    case <- fleishman_cases[fleishman_cases$panel == power$panel[i], ]
    stopifnot(case$skew == power$skew[i], case$excess_kurtosis == power$excess_kurtosis[i])
  }
  NS_TO_PLOT <- sort(unique(power$n_per_group))
  power$power_panel <- factor(paste0(power$panel, ")"), levels = paste0(PANELS, ")"))
  power$fisher_power <- power$F_power_pct / 100
  power$welch_power <- power$W_power_pct / 100
  power$rank_power <- power$KW_power_pct / 100
  power$gate_power <- power$gate_power_pct / 100
  power$route_fisher_probability <- power$route_F_pct / 100
  power$route_welch_probability <- power$route_W_pct / 100
  power$route_rank_probability <- power$route_KW_pct / 100

  # ================= OPTIONAL OVERALL TITLE AND CAPTION =================
  # Remove plot_annotation() below to omit these, retaining all panel labels.
  title <- sprintf("F, W, KW and Levene-first L+SW | group means = (%s) | B = %s",
                   paste(SHIFTS, collapse = ", "), format(manifest$B, big.mark = ","))
  caption <- paste(
    "F: Fisher ANOVA; W: Welch ANOVA; KW: Kruskal-Wallis. Points are final-test rejection rates; no connecting lines.",
    "L+SW: Levene on rstandard residuals first; pooled residual SW when Levene does not reject; Holm-adjusted groupwise SW otherwise.",
    "Normality non-rejection selects F or W respectively; normality rejection selects KW. Insets give gate routing percentages.",
    "All strategies use the same samples, at alpha = 5%. B = 1,000 per cell; individual-rate MCSE is at most 1.58 percentage points.",
    sep = "\n")
  annotation <- patchwork$plot_annotation(title = title, caption = caption,
    theme = ggplot2$theme(plot.title = ggplot2$element_text(size = 20),
                         plot.caption = ggplot2$element_text(size = 13, hjust = 0)))
  # ======================================================================
  homo <- patchwork$wrap_plots(
    make_pdf_panel(SD_EQ, "A", pdf_title(SD_EQ)),
    power_panel(D_BAL_EQ, "B"), power_panel(D_UNB_EQ, "C"),
    ncol = 1, heights = c(H_PDF, H_POWER, H_POWER)) + annotation
  hetero <- patchwork$wrap_plots(
    make_pdf_panel(SD_POS, "A", pdf_title(SD_POS)),
    power_panel(D_BAL_HET, "B"), power_panel(D_POS, "C"),
    make_pdf_panel(SD_NEG, "D", pdf_title(SD_NEG)),
    power_panel(D_NEG, "E"), ncol = 1,
    heights = c(H_PDF, H_POWER, H_POWER, H_PDF, H_POWER)) + annotation
  for (kind in c("homoscedastic", "heteroscedastic")) {
    fig <- if (kind == "homoscedastic") homo else hetero
    height <- if (kind == "homoscedastic") 24 else 38
    path <- file.path(OUTDIR, paste0("power_", scenario, "_", kind, ".png"))
    ggplot2$ggsave(path, fig, width = 20, height = height, dpi = FLEISHMAN_DPI,
                   limitsize = FALSE)
    message("Saved ", path)
  }
}
