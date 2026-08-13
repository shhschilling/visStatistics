## ---------------------------------------------------------------------------
## Type I figures on the SD vectors (1, sqrt(2), 2, sqrt(5)) and its reverse.
##
## All group means are zero, so every rejection is a false positive for the
## parametric branch and omega^2 is zero in every cell. It is stated once in
## each row header rather than printed per column, where it would say nothing.
##
## eta_H^2 is NOT zero. Scaling a skewed distribution moves its mass even when
## its mean does not, so the rank null holds in the symmetric columns and fails
## in the skewed ones. The column strips carry it, which is what says which
## columns of this figure are a Type I check for the rank branch and which are
## power. Its zeros in the symmetric columns are a check on the computation:
## under symmetry the relative effects are 1/2 whatever the scale.
##
## Kruskal-Wallis is the only rank test drawn, matching what visstat_core()
## selects.
##
## Usage:
##   Rscript figure_typeI_brunner.R
##
## Output:
##   fleishman_4groups_typeI_brunner_kw_{homo,hetero}scedastic.png
## ---------------------------------------------------------------------------

for (pkg in c("ggplot2", "patchwork", "scales", "ggtext")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Package '", pkg, "' is required.")
}

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
source(file.path(SIMDIR, "fleishman_figure_typography.R"))
source(file.path(SIMDIR, "omega_scaling_helpers.R"))

ggplot2 <- asNamespace("ggplot2"); patchwork <- asNamespace("patchwork")
scales <- asNamespace("scales")

ALPHA <- 0.05

D_BAL_EQ  <- "balanced n, equal SD"
D_UNB_EQ  <- "unbalanced n, equal SD"
D_BAL_HET <- "balanced n, unequal SD"
D_POS     <- "unbalanced n, larger n with larger SD"
D_NEG     <- "unbalanced n, larger n with smaller SD"

DESIGN_WORDS <- c(
  "balanced homoscedastic", "unbalanced homoscedastic", "balanced heteroscedastic",
  "unbalanced heteroscedastic (positive pairing)",
  "unbalanced heteroscedastic (negative pairing)"
)
names(DESIGN_WORDS) <- c(D_BAL_EQ, D_UNB_EQ, D_BAL_HET, D_POS, D_NEG)
NMULT <- c("1, 1, 1, 1", "0.5, 0.8, 1.2, 1.5", "1, 1, 1, 1",
           "0.5, 0.8, 1.2, 1.5", "0.5, 0.8, 1.2, 1.5")
names(NMULT) <- names(DESIGN_WORDS)

SD_EQ  <- c(1, 1, 1, 1)
SD_POS <- c(1, sqrt(2), 2, sqrt(5))
SD_NEG <- rev(SD_POS)
SDS <- list(SD_EQ, SD_EQ, SD_POS, SD_POS, SD_NEG)
names(SDS) <- names(DESIGN_WORDS)

## Equal means in every design: this is what makes the grid a Type I study.
SHIFTS <- c(0, 0, 0, 0)

## The SD vectors are written as (1, sqrt(2), 2, sqrt(5)) because the variances
## behind them are the integers sigma^2 = (1, 2, 4, 5). Printing 1.414 and 2.236
## loses that, so every label goes through this formatter.
sd_label <- function(x) {
  vapply(x, function(v) {
    if (isTRUE(all.equal(v, round(v)))) return(format(round(v)))
    sq <- v^2
    if (isTRUE(all.equal(sq, round(sq)))) return(paste0("√", format(round(sq))))
    format(round(v, 3), trim = TRUE, drop0trailing = TRUE)
  }, character(1))
}
sd_vector_label <- function(x) paste(sd_label(x), collapse = ", ")

## ---- assemble the grid ------------------------------------------------------
TYPEI_FILE <- file.path(SIMDIR, "route1_typeI_design_brunner_B50000.csv")
if (!file.exists(TYPEI_FILE)) {
  stop("Input grid not found: ", basename(TYPEI_FILE))
}
typeI <- read.csv(TYPEI_FILE, stringsAsFactors = FALSE)

EXPECTED_CELLS <- 150
if (nrow(typeI) != EXPECTED_CELLS) {
  stop(sprintf("%s holds %d cells, expected %d -- the run is incomplete.",
               basename(TYPEI_FILE), nrow(typeI), EXPECTED_CELLS))
}
missing_designs <- setdiff(names(DESIGN_WORDS), unique(typeI$design))
if (length(missing_designs)) {
  stop("designs absent from the grid: ", paste(missing_designs, collapse = "; "))
}

## Confirm the grid really is a null: any non-zero group mean would make this a
## power figure mislabelled as a Type I one.
nonzero <- typeI$group_means[vapply(typeI$group_means, function(s)
  any(abs(as.numeric(strsplit(s, ",[ ]*")[[1]])) > 0), logical(1))]
if (length(nonzero)) stop("group means are not all zero: ", nonzero[1])

keep <- c("design", "mean_n_per_group", "panel", "fisher_rejection",
          "welch_rejection", "levene_route_rejection", "rank_rejection",
          "sw_rejection", "sw_gate_rejection", "route_fisher_probability",
          "route_welch_probability", "route_rank_probability")
missing_cols <- setdiff(keep, names(typeI))
if (length(missing_cols)) {
  stop("columns absent from ", basename(TYPEI_FILE), ": ",
       paste(missing_cols, collapse = ", "))
}
typeI <- typeI[, keep]
names(typeI)[names(typeI) == "mean_n_per_group"] <- "n_per_group"

regime_of <- function(d) omega_sq_regime(
  as.numeric(strsplit(NMULT[[d]], ",[ ]*")[[1]]), SDS[[d]])

## eta_H^2 per (design, panel) for this grid, from eta_h_own_derivation.R.
ETA_FILE <- file.path(SIMDIR, "eta_h_own_by_design_panel.csv")
if (!file.exists(ETA_FILE)) {
  stop("eta_h_own_by_design_panel.csv not found; run eta_h_own_derivation.R first.")
}
ETA <- read.csv(ETA_FILE, stringsAsFactors = FALSE)
ETA <- ETA[ETA$grid == "typeI_brunner", , drop = FALSE]
EXPECTED_ETA_ROWS <- length(DESIGN_WORDS) * length(unique(typeI$panel))
if (nrow(ETA) != EXPECTED_ETA_ROWS) {
  stop("eta_h_own_by_design_panel.csv holds ", nrow(ETA),
       " typeI_brunner rows, expected ", EXPECTED_ETA_ROWS,
       " -- rerun eta_h_own_derivation.R now that this grid exists.")
}

H_PDF <- 1
H_POWER <- 2
BAND_SCALE <- 2 / H_POWER

PANELS <- sort(unique(typeI$panel))
typeI$power_panel <- factor(paste0(typeI$panel, ")"), levels = paste0(PANELS, ")"))
NS_TO_PLOT <- c(10, 20, 30, 50, 100)
groups <- c("A", "B", "C", "D")
xlim <- c(-2.5, 5); y_cap <- 0.7

STRATS <- c("1. Fisher always" = "F", "2. Welch always" = "W",
            "3. Levene-gated Fisher/Welch" = "L", "4. Kruskal-Wallis always" = "KW",
            "5. Shapiro-Wilk routed Welch/KW" = "SW",
            "6. Shapiro-Wilk plus Levene" = "SW+L")
COLS <- c("#B79F00", "#56B4E9", "#009E73", "#000000", "#D55E00", "#0072B2")
SHP <- c(0, 2, 5, 4, 1, 1); SZ <- c(4, 3.2, 3.6, 3.8, 5.8, 7.2)
names(COLS) <- names(SHP) <- names(SZ) <- names(STRATS)
COLUMN <- c("fisher_rejection", "welch_rejection", "levene_route_rejection",
            "rank_rejection", "sw_rejection", "sw_gate_rejection")
names(COLUMN) <- names(STRATS)
USE <- names(STRATS)

panel_title <- function(p) {
  one <- fleishman_cases[fleishman_cases$panel == p, , drop = FALSE]
  if (p == 1) return("N(0, 1)\n\n\nskew = 0\nexcess kurtosis = 0")
  sprintf(paste("Fleishman polynomial", "a = %.3f, b = %.3f", "c = -a, d = %.3f",
                "skew = %s", "excess kurtosis = %s", sep = "\n"),
          one$a, one$b, one$d, one$skew, one$excess_kurtosis)
}

## All four groups share a mean, so under equal SD the four densities coincide
## and a single curve is visible; under unequal SD they differ in scale alone.
make_pdf_panel <- function(sd_vec, letter, description) {
  lab <- sprintf("%s (mean shift = 0, SD = %s)", groups, sd_label(sd_vec))
  names(lab) <- groups
  lev <- vapply(PANELS, panel_title, character(1))
  rows <- list(); i <- 1
  for (p in PANELS) for (j in seq_along(SHIFTS)) {
    x <- seq(xlim[1], xlim[2], length.out = 700)
    d <- fleishman_scaled_density(x, p, sd = sd_vec[j], shift = SHIFTS[j])
    d[!is.finite(d)] <- NA_real_
    rows[[i]] <- data.frame(distribution = panel_title(p), group = groups[j],
                            x = x, density = d, stringsAsFactors = FALSE); i <- i + 1
  }
  dat <- do.call(rbind, rows); dat$distribution <- factor(dat$distribution, levels = lev)
  nums <- data.frame(distribution = factor(lev, levels = lev),
                     number = paste0(PANELS, ")"), x = xlim[1], y = y_cap)
  refs <- do.call(rbind, lapply(PANELS, function(p) {
    one <- fleishman_cases[fleishman_cases$panel == p, , drop = FALSE]
    data.frame(distribution = factor(panel_title(p), levels = lev),
               group = factor(rep(groups, 2), levels = groups),
               value = c(SHIFTS, one$a * sd_vec + SHIFTS),
               line_type = factor(rep(c("mean", "median"), each = 4),
                                  levels = c("mean", "median")))
  }))
  ggplot2$ggplot(dat, ggplot2$aes(x = x, y = density, colour = group, group = group)) +
    ggplot2$geom_vline(data = refs,
      ggplot2$aes(xintercept = value, colour = group, linetype = line_type),
      linewidth = 0.42, alpha = 0.75, inherit.aes = FALSE,
      show.legend = c(colour = FALSE, linetype = TRUE)) +
    ggplot2$geom_line(linewidth = 0.7, na.rm = TRUE) +
    ggplot2$geom_text(data = nums, ggplot2$aes(x = x, y = y, label = number),
      inherit.aes = FALSE, family = FLEISHMAN_FONT_FAMILY, hjust = 0, vjust = -2.0,
      size = FLEISHMAN_GEOM_TEXT$panel_number) +
    ggplot2$facet_wrap(~distribution, nrow = 1) +
    ggplot2$coord_cartesian(xlim = xlim, ylim = c(0, y_cap), clip = "off") +
    ggplot2$scale_colour_manual(values = fleishman_group_cols, breaks = groups,
      labels = lab, name = "group,\ngroup mean offset") +
    ggplot2$scale_linetype_manual(values = c(mean = "dashed", median = "dotted"),
      name = "reference line") +
    ggplot2$labs(title = fleishman_panel_title(letter, description),
                 x = "Response value", y = "Theoretical density") +
    ggplot2$theme_minimal(base_size = 10) +
    ggplot2$theme(
      strip.text = ggplot2$element_text(size = FLEISHMAN_TEXT$strip,
        family = FLEISHMAN_FONT_FAMILY, lineheight = FLEISHMAN_LINEHEIGHT$panel_title),
      legend.title = ggplot2$element_text(size = FLEISHMAN_TEXT$legend),
      legend.text = ggplot2$element_text(size = FLEISHMAN_TEXT$legend),
      axis.title = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_title),
      axis.text = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_text),
      plot.title = ggtext::element_markdown(hjust = 0,
        size = FLEISHMAN_TEXT$panel_letter, family = FLEISHMAN_FONT_FAMILY),
      plot.title.position = "plot",
      text = ggplot2$element_text(family = FLEISHMAN_FONT_FAMILY))
}

make_typeI_panel <- function(design_name, letter) {
  dat <- typeI[typeI$design == design_name, , drop = FALSE]
  if (!nrow(dat)) stop("no rows for ", design_name)

  ## Column strips carry this design's eta_H^2 for each panel, as plotmath, so
  ## the subscript H and the exponent 2 are typeset rather than printed.
  e <- ETA[ETA$design == design_name, , drop = FALSE]
  if (nrow(e) != length(PANELS)) stop("eta_H^2 missing for ", design_name)
  e <- e[order(e$panel), ]
  eta_lab <- setNames(sprintf('"%d)"~~eta[H]^2 == "%.4f"', e$panel, e$eta_h_sq_own),
                      paste0(e$panel, ")"))
  dat$power_panel <- factor(unname(eta_lab[as.character(dat$power_panel)]),
                            levels = unname(eta_lab[paste0(PANELS, ")")]))

  long <- do.call(rbind, lapply(USE, function(s) {
    v <- dat[[COLUMN[[s]]]]
    if (all(is.na(v))) return(NULL)
    transform(dat, strategy = s, rejection = v)
  }))
  long$strategy <- factor(long$strategy, levels = names(STRATS))
  long <- subset(long, n_per_group %in% NS_TO_PLOT)

  ## Rejection rates sit near alpha in most cells, so the routing inset always
  ## goes in a reserved band below zero rather than inside the plotting area.
  base <- subset(dat, n_per_group %in% NS_TO_PLOT)
  y_top <- max(0.25, min(1, 1.15 * max(long$rejection, na.rm = TRUE)))
  gate_ys <- y_top * c(-0.075, -0.125, -0.175)
  title_y <- y_top * -0.038
  y_min <- y_top * -0.21

  gv <- rbind(
    transform(base, gate_row = "F",  gate_y = gate_ys[1], gate_rate = route_fisher_probability),
    transform(base, gate_row = "W",  gate_y = gate_ys[2], gate_rate = route_welch_probability),
    transform(base, gate_row = "KW", gate_y = gate_ys[3], gate_rate = route_rank_probability))
  gv$lab <- sprintf("%.0f", 100 * gv$gate_rate)
  grows <- unique(gv[c("power_panel", "gate_row", "gate_y")])
  gtit <- unique(base["power_panel"]); gtit$t <- "SW+L selection (%)"

  header <- sprintf(paste0("Type I simulations, %s; (n<sub>1</sub>, n<sub>2</sub>, ",
      "n<sub>3</sub>, n<sub>4</sub>) = n&#772;(%s); (SD<sub>1</sub>, SD<sub>2</sub>, ",
      "SD<sub>3</sub>, SD<sub>4</sub>) = (%s); all group means 0, so ",
      "&omega;<sup>2</sup><sub>%s</sub> = 0"),
    DESIGN_WORDS[[design_name]], NMULT[[design_name]],
    sd_vector_label(SDS[[design_name]]), regime_of(design_name))

  ggplot2$ggplot() +
    ggplot2$geom_vline(xintercept = NS_TO_PLOT, colour = "grey88", linewidth = 0.35) +
    ggplot2$geom_hline(yintercept = seq(0, y_top, by = 0.05), colour = "grey88",
                       linewidth = 0.35) +
    ## nominal level: every marker above this line is an inflated rejection rate
    ggplot2$geom_hline(yintercept = ALPHA, colour = "grey30", linewidth = 0.4,
                       linetype = "dashed") +
    ggplot2$geom_hline(yintercept = -0.02 * y_top, colour = "grey60",
                       linewidth = 0.3) +
    ggplot2$geom_point(data = long,
      ggplot2$aes(x = n_per_group, y = rejection, colour = strategy, shape = strategy,
                  size = strategy, group = strategy), stroke = 1.15) +
    ggplot2$geom_text(data = gtit, ggplot2$aes(x = 26, y = title_y, label = t),
      colour = "grey25", family = FLEISHMAN_FONT_FAMILY, size = FLEISHMAN_GEOM_TEXT$inset) +
    ggplot2$geom_text(data = grows, ggplot2$aes(x = 5.8, y = gate_y, label = gate_row),
      colour = "grey25", family = FLEISHMAN_FONT_FAMILY, size = FLEISHMAN_GEOM_TEXT$inset) +
    ggplot2$geom_text(data = gv,
      ggplot2$aes(x = n_per_group, y = gate_y, label = lab), colour = "grey25",
      family = FLEISHMAN_FONT_FAMILY, size = FLEISHMAN_GEOM_TEXT$inset) +
    ggplot2$facet_grid(stats::as.formula(". ~ power_panel"),
      labeller = ggplot2$label_parsed) +
    ggplot2$scale_y_continuous(limits = c(y_min, y_top),
                               breaks = seq(0, y_top, by = 0.05),
                               labels = scales::percent) +
    ggplot2$scale_x_log10(breaks = NS_TO_PLOT, limits = c(5.5, 130)) +
    ggplot2$scale_shape_manual(values = SHP, breaks = USE, labels = STRATS[USE],
                               name = "test strategy") +
    ggplot2$scale_size_manual(values = SZ, breaks = USE, labels = STRATS[USE],
                              name = "test strategy") +
    ggplot2$scale_colour_manual(values = COLS, breaks = USE, labels = STRATS[USE],
                                name = "test strategy") +
    ggplot2$labs(title = fleishman_panel_title(letter, header),
                 x = "n per group", y = "simulated rejection rate") +
    ggplot2$theme_minimal(base_size = 10) +
    ggplot2$theme(
      legend.position = "right",
      legend.title = ggplot2$element_text(size = FLEISHMAN_TEXT$legend),
      legend.text = ggplot2$element_text(size = FLEISHMAN_TEXT$legend),
      axis.title = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_title),
      axis.text = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_text),
      axis.text.x = ggplot2$element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2$element_blank(),
      panel.border = ggplot2$element_rect(colour = "black", fill = NA, linewidth = 0.25),
      strip.background = ggplot2$element_blank(),
      strip.text.x = ggplot2$element_text(size = FLEISHMAN_TEXT$power_strip,
        face = "plain", family = FLEISHMAN_FONT_FAMILY, hjust = 0),
      plot.title = ggtext::element_markdown(hjust = 0,
        size = FLEISHMAN_TEXT$panel_letter, family = FLEISHMAN_FONT_FAMILY),
      plot.title.position = "plot",
      text = ggplot2$element_text(family = FLEISHMAN_FONT_FAMILY))
}

## ---- pooled layout ----------------------------------------------------------
## One density strip per SD vector, serving every panel beneath it.
pdf_title <- function(sd_vec) sprintf("input distributions, SD = (%s)",
                                      sd_vector_label(sd_vec))

IN_PER_UNIT <- 27.5 / 6
homo_h <- c(H_PDF, H_POWER, H_POWER)
het_h <- c(H_PDF, H_POWER, H_POWER, H_PDF, H_POWER)

homo_fig <- patchwork$wrap_plots(
  make_pdf_panel(SD_EQ, "A", pdf_title(SD_EQ)),
  make_typeI_panel(D_BAL_EQ, "B"),
  make_typeI_panel(D_UNB_EQ, "C"),
  ncol = 1, heights = homo_h)

het_fig <- patchwork$wrap_plots(
  make_pdf_panel(SD_POS, "A", pdf_title(SD_POS)),
  make_typeI_panel(D_BAL_HET, "B"),
  make_typeI_panel(D_POS, "C"),
  make_pdf_panel(SD_NEG, "D", pdf_title(SD_NEG)),
  make_typeI_panel(D_NEG, "E"),
  ncol = 1, heights = het_h)

f1 <- "fleishman_4groups_typeI_brunner_kw_homoscedastic.png"
f2 <- "fleishman_4groups_typeI_brunner_kw_heteroscedastic.png"

ggplot2$ggsave(f1, homo_fig, width = 20, height = sum(homo_h) * IN_PER_UNIT,
               dpi = FLEISHMAN_DPI)
ggplot2$ggsave(f2, het_fig, width = 20, height = sum(het_h) * IN_PER_UNIT,
               dpi = FLEISHMAN_DPI)
message("Saved:\n  ", f1, "\n  ", f2)
